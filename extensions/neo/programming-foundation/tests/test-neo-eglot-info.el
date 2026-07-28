;;; test-neo-eglot-info.el --- Tests for neo-eglot-info -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)
(require 'eglot)
(require 'easymenu)

(defconst neo--eglot-info-test-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing `test-neo-eglot-info.el'.")

(load-file
 (expand-file-name "../neo-eglot-info.el"
                   neo--eglot-info-test-directory))

(defun neo--eglot-info-test-menu-labels (menu)
  "Return visible item labels from MENU in keymap order."
  (let (labels)
    (map-keymap
     (lambda (_event binding)
       (when (eq (car-safe binding) 'menu-item)
         (push (nth 1 binding) labels)))
     menu)
    (nreverse labels)))

(defun neo--eglot-info-test-row (name rows)
  "Return the capability named NAME from ROWS."
  (cl-find name rows :key (lambda (row) (plist-get row :name))))

(describe "neo-eglot-info"
  (describe "menu integration"
    (it "installs one LSP info item before the first separator"
      (let ((eglot-menu
             (easy-menu-create-menu
              "Eglot"
              '(["Customize Eglot" ignore]
                "--"
                ["Other command" ignore]))))
        (neo--eglot-info-install-menu-item)
        (let ((first-labels
               (neo--eglot-info-test-menu-labels eglot-menu))
              (first-length (length eglot-menu)))
          (neo--eglot-info-install-menu-item)
          (expect (length eglot-menu) :to-equal first-length)
          (expect first-labels
                  :to-equal
                  '("Customize Eglot" "LSP info" "Other command"))
          (expect (neo--eglot-info-test-menu-labels eglot-menu)
                  :to-equal first-labels)))))

  (describe "capability states"
    (it "sorts returned capabilities and distinguishes enabled state"
      (let* ((rows
              (neo--eglot-info-capability-rows
               '(:renameProvider :json-false
                 :hoverProvider t
                 :completionProvider nil)
               '(:hoverProvider)))
             (completion
              (neo--eglot-info-test-row :completionProvider rows))
             (hover (neo--eglot-info-test-row :hoverProvider rows))
             (rename (neo--eglot-info-test-row :renameProvider rows)))
        (expect (mapcar (lambda (row) (plist-get row :name)) rows)
                :to-equal
                '(:completionProvider :hoverProvider :renameProvider))
        (expect (plist-get completion :available) :to-be t)
        (expect (plist-get completion :enabled) :to-be t)
        (expect (plist-get hover :available) :to-be t)
        (expect (plist-get hover :enabled) :to-be nil)
        (expect (plist-get rename :available) :to-be nil)
        (expect (plist-get rename :enabled) :to-be nil)))

    (it "omits capabilities the server did not return"
      (expect (neo--eglot-info-capability-rows nil nil) :to-be nil)))

  (describe "information mode"
    (it "provides refresh and quit bindings"
      (expect (lookup-key neo/eglot-info-mode-map (kbd "g"))
              :to-be #'revert-buffer)
      (expect (lookup-key neo/eglot-info-mode-map (kbd "q"))
              :to-be #'quit-window))

    (it "renders effective configuration and refreshes live capabilities"
      (let ((capabilities '(:hoverProvider t)))
        (with-temp-buffer
          (neo/eglot-info-mode)
          (setq neo--eglot-info-server 'server)
          (setq neo--eglot-info-source-buffer (current-buffer))
          (setq neo--eglot-info-ignored-capabilities nil)
          (cl-letf (((symbol-function 'jsonrpc-name)
                     (lambda (_server) "test-server"))
                    ((symbol-function 'jsonrpc-running-p)
                     (lambda (_server) t))
                    ((symbol-function 'jsonrpc--process)
                     (lambda (_server) nil))
                    ((symbol-function 'eglot--server-info)
                     (lambda (_server)
                       '(:name "Test LSP" :version "1.2.3")))
                    ((symbol-function 'eglot--project)
                     (lambda (_server) 'project))
                    ((symbol-function 'project-root)
                     (lambda (_project) "/tmp/project/"))
                    ((symbol-function 'eglot--languages)
                     (lambda (_server)
                       '((python-mode . "python"))))
                    ((symbol-function 'eglot-initialization-options)
                     (lambda (_server) '(:trace "messages")))
                    ((symbol-function
                      'eglot--workspace-configuration-plist)
                     (lambda (_server) '(:python (:analysis t))))
                    ((symbol-function 'eglot--capabilities)
                     (lambda (_server) capabilities)))
            (neo--eglot-info-render)
            (expect major-mode :to-be 'neo/eglot-info-mode)
            (expect (buffer-string) :to-match "Test LSP")
            (expect (buffer-string) :to-match "Initialization options")
            (expect (buffer-string) :to-match "Workspace configuration")
            (expect (buffer-string) :to-match ":hoverProvider")
            (setq capabilities '(:renameProvider t))
            (revert-buffer)
            (expect (buffer-string) :to-match ":renameProvider")
            (expect (buffer-string) :not :to-match ":hoverProvider"))))))

  (describe "interactive display"
    (it "rejects buffers without a current Eglot server"
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda () nil)))
        (expect (neo/eglot-info) :to-throw 'user-error)))

    (it "opens a focused dedicated bottom window"
      (let (display-action dedicated-window selected-window info-buffer)
        (unwind-protect
            (cl-letf (((symbol-function 'eglot-current-server)
                       (lambda () 'server))
                      ((symbol-function 'jsonrpc-name)
                       (lambda (_server) "test-server"))
                      ((symbol-function 'neo--eglot-info-render)
                       (lambda () nil))
                      ((symbol-function 'display-buffer)
                       (lambda (buffer action)
                         (setq info-buffer buffer)
                         (setq display-action action)
                         'test-window))
                      ((symbol-function 'set-window-dedicated-p)
                       (lambda (window dedicated)
                         (setq dedicated-window
                               (and dedicated window))))
                      ((symbol-function 'select-window)
                       (lambda (window &optional _norecord)
                         (setq selected-window window))))
              (let ((eglot-ignored-server-capabilities nil))
                (neo/eglot-info))
              (expect (buffer-live-p info-buffer) :to-be t)
              (expect (cdr (assq 'side display-action))
                      :to-be 'bottom)
              (expect (cdr (assq 'window-height display-action))
                      :to-equal 0.4)
              (expect dedicated-window :to-be 'test-window)
              (expect selected-window :to-be 'test-window))
          (when (buffer-live-p info-buffer)
            (kill-buffer info-buffer)))))))

;;; test-neo-eglot-info.el ends here
