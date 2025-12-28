;;; -*- lexical-binding: t -*-

(require 'neo-framework)

(neo/use-package svg-lib
  :config
  (svg-lib-button-mode 1))

(defface neo/divider-face
  '((t :overline unspecified
       :foreground unspecified
       :background unspecified))
  "Face for UI dividers.")

(defface neo/manager-info-label-face
  '((t :inherit font-lock-keyword-face
       :weight bold
       :height 0.7))
  "Face for info block labels in the NEO manager.")

(defface neo/manager-info-value-face
  '((t :inherit default
       :height 0.7))
  "Face for info block values in the NEO manager.")

(defface neo/manager-header-face
  '((t :inherit mode-line
       :height 0.9))
  "Face for the NEO manager header.")

;; (defface neo/header-button-active
;;   '((t :inherit neo/manager-header-face
;;        :weight bold
;;        :underline t))
;;   "Active header button.")

;; (defface neo/header-button-inactive
;;   '((t :inherit neo/manager-header-face)))

(defface neo/header-button-active
  `((t :weight bold
       :underline t
       :foreground ,(face-foreground 'neo/manager-header-face)
       :background ,(face-background 'neo/manager-header-face)))
  "Face for active header buttons in the NEO extension manager."
  :group 'neo)

(defface neo/header-button-inactive
  '((t :inherit header-line))
  "Face for inactive header buttons in the NEO extension manager."
  :group 'neo)

;; (defun neo/header-button (label action active)
;;   (propertize
;;    label
;;    'face (if active
;;              'neo/header-button-active
;;            'neo/header-button-inactive)
;;    'mouse-face 'highlight
;;    'help-echo label
;;    'keymap (let ((map (make-sparse-keymap)))
;;              (define-key map [header-line mouse-1] action)
;;              map)))

(defun neo/header-button (label action active)
  (propertize
   label
   'face (if active
             'neo/header-button-active
           'neo/header-button-inactive)
   'mouse-face 'highlight
   'help-echo label
   'keymap (let ((map (make-sparse-keymap)))
             (define-key map [header-line mouse-1] action)
             map)))

(defun neo/update-divider-face ()
  "Update `neo/divider-face` colors to match the current theme."
  (let ((fg (face-foreground 'default nil t))
        (bg (face-background 'default nil t)))
    (set-face-attribute 'neo/divider-face nil
                        :overline fg
                        :foreground bg
                        :background bg)))

(add-hook 'neo/after-theme-load-hook #'neo/update-divider-face)

(defun neo/insert-thin-divider (&optional color)
  "Insert a thin horizontal divider using underline, aligned to window width.
If COLOR is nil, use the theme's default foreground color."
  (unless (bolp)
    (insert "\n"))
  (insert "\n")
  (let* ((line-color (or color (face-foreground 'default nil t)))
         (bg-color   (face-background 'default nil t)))
    (insert
     (propertize " "
                 'display '(space :align-to right)
                 'face 'neo/divider-face))
    (insert "\n")))

(cl-defgeneric neo/extension-render-card (object)
  "Render OBJECT at point and return a point marker.")

(defun neo/manager--info-label (text)
  (propertize text 'face 'neo/manager-info-label-face))

(defun neo/manager--info-value (text)
  (propertize text 'face 'neo/manager-info-value-face))


(defconst neo--dir
  (file-name-directory
   (or load-file-name buffer-file-name))
  "Directory of the NEO package.")

(defun neo--local-file (file)
  (expand-file-name file neo--dir))

(cl-defmethod neo/extension-render-card ((ext neo/extension) framework)
  "Render EXT in the current buffer. Return (start . end) position."
  (let ((start (point))
	(debug-on-error t)
        (inhibit-message nil)
        (message-log-max t)
	)
    ;; Insert image using overlay
    (let* ((emblem (neo/extension-emblem ext))
	   (img (if (stringp emblem)
		    (create-image emblem 'png t :data-p t)
		  (create-image (neo--local-file "default-emblem64.png") 'png))))
          ;; Insert a space and record its bounds *after* insertion
          (insert " ")
          (let ((ov (make-overlay (1- (point)) (point))))
            (overlay-put ov 'display img)
            (overlay-put ov 'neo-image t)
            ;; Store it in the extension struct
            (setf (neo/extension-summary-overlay ext) ov)))
    
    (insert " ")

    (insert (propertize (or (neo/extension-title ext) "Unnamed")
                        'face '(:weight bold :height 1.2)))
    (insert "\n\n")

    ;; Description
    (when-let ((desc (neo/extension-description ext)))
      (insert (propertize desc 'face '(:slant italic :height 0.95)))
      (insert "\n\n"))

    (insert (svg-lib-button "[check-bold] OK"
                        (lambda () (interactive) (message "OK"))))
    (insert "\n")
    
    (let ((repo (neo/extension-repository ext))
	  (used-packages (neo--get-extension-info ext)))
;  "Alist mapping keys to list render specs."))
      (dolist (pair `(("Publisher" ,(neo/extension-publisher ext))
                      ("Type"      ,(neo/repository-type repo))
                      ("URL"       ,(neo/repository-url repo))
                      ("Path"      ,(neo/repository-path repo))
		      ("Required"  ,(neo/extension-requires ext))
		      ("Recommended" ,(neo/extension-depends-on ext))
		      ("Packages"  ,used-packages)
		      ))
	(when-let ((key (car pair))
		   (value (cadr pair)))
	  (insert (neo/manager--info-label (format "%-12s" key)) (neo/manager--info-value ": "))

	  (cond
	   ((listp value)
	    (neo--manager-dispatch-list key value))
	   (t
	    (insert (neo/manager--info-value value) "\n"))))))

	  ;; (if (listp value)
	  ;;     (if (string= key "Packages")
	  ;; 	  (neo--insert-packages-desc value))
;;	    (insert (neo/manager--info-value value))))))

    (neo/insert-thin-divider)
    
    ;; Final range
    (let ((end (point)))
      (put-text-property start end 'neo-extension ext)
      (let ((ov (make-overlay start (point))))
	(overlay-put ov 'neo/card t)
	(overlay-put ov 'neo/extension ext)
	(overlay-put ov 'evaporate t)
	ov)
      ;;; we probably don't use the return value or the text-property any more
      (cons start end))))

(defun neo--manager-dispatch-list (key value)
  (let* ((list-handlers
	  '(("Packages"    neo--insert-packages-desc)
	    ("Required"    neo--insert-extensions-desc)
	    ("Recommended" neo--insert-extensions-desc)))
	 (spec (assoc key list-handlers)))
    (if spec
	(funcall (cadr spec) value))))
;      (insert (mapconcat #'identity value ", "))))))

(defun neo--insert-extensions-desc (extensions)
  (dolist (slug (sort extensions))
    (let ((ext (neo/get-extension (neo--framework-instance) slug)))
      (indent-to 14)
      (insert (neo/manager--info-value (format "%s" slug)))
      (indent-to 40)
      (if ext
	  (insert (neo/manager--info-value (format "%s\n" (neo/extension-title ext))))
	(insert (neo/manager--info-value (format "Not available\n" slug))))
      )))

(defun neo--insert-package-desc (package-name package-info)
  (let* ((package (cadr (assoc-string package-name package-info)))
	 (desc (if package
		   (package-desc-summary package)
		 (if (string= package-name "emacs")
		     "Low-level Emacs configuration, not part of any package"
		   (format "No description available for package '%s'" package-name)))))
    (indent-to 14)
    (insert (neo/manager--info-value (format "%s" package-name)))
    (indent-to 40)
    (insert (neo/manager--info-value (format "%s\n" desc)))))

(defun neo--insert-packages-desc (package-info)
  (if package-info
      (sort package-info (lambda (a b) (string< (car a) (car b)))))
  (dolist (p package-info)
    (let* ((package-name (car p)))
      (neo--insert-package-desc package-name package-info))))

(defvar-local neo/current-filter 'all)

;; TODO: move these to neo-extension-manager.el
;(define-key neo/manager-mode-map (kbd "a") #'neo/filter-all)
;(define-key neo/manager-mode-map (kbd "e") #'neo/filter-enabled)
;(define-key neo/manager-mode-map (kbd "d") #'neo/filter-disabled)

(defun neo/filter-all ()
  (interactive)
  (setq neo/current-filter 'all)
  (neo/extensions-refresh-all))

(defun neo/filter-installed ()
  (interactive)
  (setq neo/current-filter 'installed)
  (neo/extensions-refresh-all))

(defun neo/filter-recommended ()
  (interactive)
  (setq neo/current-filter 'recommended)
  (neo/extensions-refresh-all))

(defun neo/filter-disabled ()
  (interactive)
  (setq neo/current-filter 'disabled)
  (neo/extensions-refresh-all))

(defun neo/manager-update-header ()
  (setq-local
   header-line-format
   (list
    (propertize " " 'face 'neo/manager-header-face)
    (svg-lib-button "[check-bold] All"
                        (lambda () (interactive) (message "All")))
    (propertize " " 'face 'neo/manager-header-face)
    (neo/header-button "[All]" #'neo/filter-all
                       (eq neo/current-filter 'all))
    (propertize "  " 'face 'neo/manager-header-face)
    (neo/header-button "[Installed]" #'neo/filter-installed
                       (eq neo/current-filter 'installed))
    (propertize "  " 'face 'neo/manager-header-face)
    (neo/header-button "[Recommended]" #'neo/filter-recommended
                       (eq neo/current-filter 'recommended))
    (propertize "  " 'face 'neo/manager-header-face)
    (neo/header-button "[Disabled]" #'neo/filter-disabled
                       (eq neo/current-filter 'disabled))
    (propertize
     " "
     'display '(space :align-to right)
     'face 'neo/manager-header-face))))

(defun neo/extensions-render ()
  (let ((inhibit-read-only t)
        (framework (neo--framework-instance)))
    (erase-buffer)

    (maphash (lambda (_ v)
               (neo/extension-render-card v framework))
             (neo-framework-available-extensions framework))
    (goto-char (point-min))))

(defun neo/extensions-refresh-all ()
  (interactive)
  (neo/manager-update-header)
  (neo/extensions-render))

;; (defun neo/extensions-render ()
;;   (setq-local
;;    header-line-format
;;    (list
;;     (propertize " " 'face 'neo/manager-header-face)
;;     (neo/header-button "[All]" #'neo/filter-all (eq neo/current-filter 'all))
;;     (propertize "  " 'face 'neo/manager-header-face)
;;     (neo/header-button "[Installed]" #'neo/filter-installed (eq neo/current-filter 'installed))
;;     (propertize "  " 'face 'neo/manager-header-face )
;;     (neo/header-button "[Recommended]" #'neo/filter-recommended (eq neo/current-filter 'recommended))
;;     (propertize "  " 'face 'neo/manager-header-face)
;;     (neo/header-button "[Disabled]" #'neo/filter-disabled (eq neo/current-filter 'disabled))
;;     (propertize
;;      " "
;;      'display '(space :align-to right)
;;      'face 'neo/manager-header-face)))

;;   (let ((inhibit-read-only t))
;;     (let* ((framework  (neo--framework-instance)))
;;       (message "running rendering")
;;       (erase-buffer)
;;       (maphash (lambda (k v)
;; 		 (neo/extension-render-card v))
;;                (neo-framework-available-extensions framework))
;;       (goto-char (point-min)))))

(provide 'neo-extension-manager-render)
