;;; -*- lexical-binding: t -*-

;;; Commentary:

;; Read-only inspector for the effective configuration and negotiated
;; capabilities of an Eglot language server.

;;; Code:

(require 'easymenu)
(require 'pp)
(require 'project)
(require 'subr-x)
(require 'vtable)

(declare-function eglot--capabilities "eglot")
(declare-function eglot--languages "eglot")
(declare-function eglot--project "eglot")
(declare-function eglot--server-info "eglot")
(declare-function eglot--workspace-configuration-plist "eglot")
(declare-function eglot-current-server "eglot")
(declare-function eglot-initialization-options "eglot")
(declare-function jsonrpc--process "jsonrpc")
(declare-function jsonrpc-name "jsonrpc")
(declare-function jsonrpc-running-p "jsonrpc")

(defvar eglot-ignored-server-capabilities)
(defvar eglot-menu)

(defvar-local neo--eglot-info-server nil
  "Eglot server displayed in the current LSP information buffer.")

(defvar-local neo--eglot-info-source-buffer nil
  "Managed buffer from which the current LSP information view was opened.")

(defvar-local neo--eglot-info-ignored-capabilities nil
  "Snapshot of ignored capabilities for the current LSP information view.")

(defvar-keymap neo/eglot-info-mode-map
  :doc "Keymap for `neo/eglot-info-mode'."
  :parent special-mode-map
  "g" #'revert-buffer
  "q" #'quit-window)

;;;###autoload
(define-derived-mode neo/eglot-info-mode special-mode "LSP Info"
  "Display effective Eglot server configuration and capabilities."
  (setq-local revert-buffer-function #'neo--eglot-info-revert)
  (setq truncate-lines nil))

(defun neo--eglot-info-revert (_ignore-auto _noconfirm)
  "Refresh the current LSP information buffer."
  (neo--eglot-info-render))

(defun neo--eglot-info-safe-value (function)
  "Call FUNCTION and return its value or a visible error string."
  (condition-case error-data
      (funcall function)
    (error (format "<error: %s>" (error-message-string error-data)))))

(defun neo--eglot-info-format-value (value)
  "Return a compact readable representation of VALUE."
  (string-trim-right (pp-to-string value)))

(defun neo--eglot-info-capability-rows (capabilities ignored)
  "Build sorted rows from CAPABILITIES using IGNORED capability names.

CAPABILITIES is Eglot's server-capabilities plist.  A capability is
available when its key is present and its value is not `:json-false'.
It is enabled when it is available and absent from IGNORED."
  (let (rows)
    (while capabilities
      (let* ((name (pop capabilities))
             (value (pop capabilities))
             (available (not (eq value :json-false)))
             (enabled (and available (not (memq name ignored)))))
        (push (list :name name
                    :available available
                    :enabled enabled
                    :value value)
              rows)))
    (sort rows
          (lambda (left right)
            (string-lessp
             (symbol-name (plist-get left :name))
             (symbol-name (plist-get right :name)))))))

(defun neo--eglot-info-effective-ignored-capabilities ()
  "Return ignored capabilities effective for the inspected server."
  (if (buffer-live-p neo--eglot-info-source-buffer)
      (buffer-local-value 'eglot-ignored-server-capabilities
                          neo--eglot-info-source-buffer)
    neo--eglot-info-ignored-capabilities))

(defun neo--eglot-info-running-state (server)
  "Return a readable connection state for SERVER."
  (condition-case error-data
      (if (jsonrpc-running-p server) "running" "stopped")
    (error (format "<error: %s>" (error-message-string error-data)))))

(defun neo--eglot-info-process-command (server)
  "Return SERVER's process command, or a transport description."
  (let ((process
         (neo--eglot-info-safe-value
          (lambda () (jsonrpc--process server)))))
    (cond
     ((stringp process) process)
     ((and (processp process) (process-command process))
      (mapconcat #'identity (process-command process) " "))
     ((processp process) "<network connection>")
     (t "<not exposed>"))))

(defun neo--eglot-info-project-root (server)
  "Return the project root associated with SERVER."
  (neo--eglot-info-safe-value
   (lambda () (abbreviate-file-name
               (project-root (eglot--project server))))))

(defun neo--eglot-info-languages (server)
  "Return a readable summary of languages managed by SERVER."
  (neo--eglot-info-safe-value
   (lambda ()
     (mapconcat
      (lambda (language)
        (format "%s (%s)" (car language) (cdr language)))
      (eglot--languages server)
      ", "))))

(defun neo--eglot-info-insert-heading (heading)
  "Insert section HEADING into the current information buffer."
  (insert (propertize heading 'face 'bold) "\n"))

(defun neo--eglot-info-insert-field (label value)
  "Insert a LABEL and VALUE field into the current information buffer."
  (insert (propertize (format "%-22s" (concat label ":"))
                      'face 'font-lock-keyword-face)
          (format "%s" value)
          "\n"))

(defun neo--eglot-info-insert-object (label value)
  "Insert LABEL followed by a pretty-printed VALUE."
  (insert (propertize (concat label ":\n") 'face 'font-lock-keyword-face)
          (neo--eglot-info-format-value value)
          "\n\n"))

(defun neo--eglot-info-insert-capabilities (server)
  "Insert SERVER's capability table."
  (neo--eglot-info-insert-heading "Server capabilities")
  (let* ((capabilities
          (neo--eglot-info-safe-value
           (lambda () (eglot--capabilities server))))
         (rows
          (and (listp capabilities)
               (neo--eglot-info-capability-rows
                capabilities
                (neo--eglot-info-effective-ignored-capabilities)))))
    (if (null rows)
        (insert (if (stringp capabilities)
                    capabilities
                  "No server capabilities were reported.")
                "\n")
      (make-vtable
       :use-header-line nil
       :separator-width 2
       :columns
       `((:name "Capability" :width 36
                :getter ,(lambda (row _column)
                           (symbol-name (plist-get row :name))))
         (:name "Available" :width 9
                :getter ,(lambda (row _column)
                           (if (plist-get row :available) "yes" "no")))
         (:name "Enabled" :width 7
                :getter ,(lambda (row _column)
                           (if (plist-get row :enabled) "yes" "no")))
         (:name "Value" :width "45%"
                :getter ,(lambda (row _column)
                           (neo--eglot-info-format-value
                            (plist-get row :value)))))
       :objects rows))))

(defun neo--eglot-info-render ()
  "Render the current buffer's Eglot server information."
  (unless neo--eglot-info-server
    (user-error "No Eglot server is associated with this buffer"))
  (let* ((server neo--eglot-info-server)
         (server-info
          (neo--eglot-info-safe-value
           (lambda () (eglot--server-info server))))
         (server-name
          (or (and (listp server-info) (plist-get server-info :name))
              (neo--eglot-info-safe-value
               (lambda () (jsonrpc-name server)))))
         (server-version
          (and (listp server-info) (plist-get server-info :version)))
         (initialization-options
          (neo--eglot-info-safe-value
           (lambda () (eglot-initialization-options server))))
         (workspace-configuration
          (neo--eglot-info-safe-value
           (lambda () (eglot--workspace-configuration-plist server))))
         (inhibit-read-only t))
    (erase-buffer)
    (neo--eglot-info-insert-heading "LSP server")
    (neo--eglot-info-insert-field "Name" server-name)
    (neo--eglot-info-insert-field "Version" (or server-version "<not reported>"))
    (neo--eglot-info-insert-field
     "Connection" (neo--eglot-info-running-state server))
    (neo--eglot-info-insert-field
     "Process command" (neo--eglot-info-process-command server))
    (neo--eglot-info-insert-field
     "Project root" (neo--eglot-info-project-root server))
    (neo--eglot-info-insert-field
     "Languages" (neo--eglot-info-languages server))
    (when (buffer-live-p neo--eglot-info-source-buffer)
      (neo--eglot-info-insert-field
       "Opened from" (buffer-name neo--eglot-info-source-buffer)))
    (insert "\n")
    (neo--eglot-info-insert-heading "Effective configuration")
    (neo--eglot-info-insert-object
     "Initialization options" initialization-options)
    (neo--eglot-info-insert-object
     "Workspace configuration" workspace-configuration)
    (neo--eglot-info-insert-capabilities server)
    (goto-char (point-min))))

(defun neo--eglot-info-buffer-name (server)
  "Return the inspector buffer name for SERVER."
  (format "*LSP Info: %s*"
          (neo--eglot-info-safe-value
           (lambda () (jsonrpc-name server)))))

;;;###autoload
(defun neo/eglot-info ()
  "Display effective information for the current Eglot server."
  (interactive)
  (unless (fboundp 'eglot-current-server)
    (user-error "Eglot is not loaded"))
  (let ((server (eglot-current-server))
        (source-buffer (current-buffer)))
    (unless server
      (user-error "No Eglot server manages the current buffer"))
    (let ((buffer (get-buffer-create
                   (neo--eglot-info-buffer-name server))))
      (with-current-buffer buffer
        (unless (derived-mode-p 'neo/eglot-info-mode)
          (neo/eglot-info-mode))
        (setq neo--eglot-info-server server)
        (setq neo--eglot-info-source-buffer source-buffer)
        (setq neo--eglot-info-ignored-capabilities
              (with-current-buffer source-buffer
                (copy-sequence eglot-ignored-server-capabilities)))
        (neo--eglot-info-render))
      (when-let* ((window
                   (display-buffer
                    buffer
                    '((display-buffer-in-side-window)
                      (side . bottom)
                      (slot . 0)
                      (window-height . 0.4)))))
        (set-window-dedicated-p window t)
        (select-window window)))))

(defun neo--eglot-info-install-menu-item ()
  "Install the LSP information command in `eglot-menu'."
  (easy-menu-add-item
   'eglot-menu nil
   '["LSP info" neo/eglot-info
     :help "Inspect the current LSP server configuration and capabilities"
     :active (eglot-current-server)]
   "--"))

(with-eval-after-load 'eglot
  (neo--eglot-info-install-menu-item))

(provide 'neo-eglot-info)

;;; neo-eglot-info.el ends here
