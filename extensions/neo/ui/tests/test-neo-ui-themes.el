;;; tests/test-neo-ui-themes.el --- Tests for neo-ui-themes -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)

(defvar neo/after-framework-bootstrap-hook nil)
(defvar neo/after-theme-load-hook nil)

(defvar neo--test-config nil)
(defvar neo--saved-frame-snapshots 0)

(defmacro neo/use-package (&rest _args)
  nil)

(defun neo/get-config (key)
  (alist-get key neo--test-config nil nil #'string=))

(defun neo/set-config (key value)
  (setf (alist-get key neo--test-config nil nil #'string=) value))

(defun neo/save-initial-frame-properties ()
  (setq neo--saved-frame-snapshots (1+ neo--saved-frame-snapshots)))

(provide 'neo-config)

(load-file (expand-file-name "../neo-ui-themes.el"
                             (file-name-directory (or load-file-name buffer-file-name))))

(defconst neo--default-after-framework-bootstrap-hook
  neo/after-framework-bootstrap-hook)

(defconst neo--default-after-theme-load-hook
  neo/after-theme-load-hook)

(describe "neo-ui-themes"
  (before-each
    (setq neo--test-config nil
          neo--saved-frame-snapshots 0
          neo/current-theme nil
          custom-enabled-themes nil
          neo/after-framework-bootstrap-hook neo--default-after-framework-bootstrap-hook
          neo/after-theme-load-hook neo--default-after-theme-load-hook))

  (it "keeps the default Emacs theme when no theme has been persisted"
    (let (load-calls)
      (cl-letf (((symbol-function 'custom-available-themes)
                 (lambda () '(ef-summer ef-winter)))
                ((symbol-function 'load-theme)
                 (lambda (&rest args)
                   (push args load-calls)
                   (neo/run-after-theme-load (car args)))))
        (expect (neo/restore-persisted-theme) :to-be nil)
        (expect load-calls :to-be nil)
        (expect neo/current-theme :to-be nil)
        (expect neo--saved-frame-snapshots :to-equal 0))))

  (it "restores a persisted theme from the config DB"
    (let (load-calls)
      (setq neo--test-config '(("theme" . "ef-summer")))
      (cl-letf (((symbol-function 'custom-available-themes)
                 (lambda () '(ef-summer ef-winter)))
                ((symbol-function 'load-theme)
                 (lambda (theme no-confirm)
                   (push (list theme no-confirm) load-calls)
                   (neo/run-after-theme-load theme))))
        (expect (neo/restore-persisted-theme) :to-equal 'ef-summer)
        (expect load-calls :to-equal '((ef-summer t)))
        (expect neo/current-theme :to-equal 'ef-summer)
        (expect neo--saved-frame-snapshots :to-equal 1))))

  (it "persists an explicit theme change"
    (let (disabled-themes)
      (setq custom-enabled-themes '(ef-winter))
      (cl-letf (((symbol-function 'custom-available-themes)
                 (lambda () '(ef-summer ef-winter)))
                ((symbol-function 'disable-theme)
                 (lambda (theme)
                   (push theme disabled-themes)))
                ((symbol-function 'load-theme)
                 (lambda (theme _no-confirm)
                   (neo/run-after-theme-load theme))))
        (expect (neo/load-theme-internal 'ef-summer t) :to-equal 'ef-summer)
        (expect disabled-themes :to-equal '(ef-winter))
        (expect (alist-get "theme" neo--test-config nil nil #'string=)
                :to-equal "ef-summer")
        (expect neo/current-theme :to-equal 'ef-summer)
        (expect neo--saved-frame-snapshots :to-equal 1))))

  (it "ignores an unavailable persisted theme without overwriting the DB"
    (let (load-calls)
      (setq neo--test-config '(("theme" . "missing-theme")))
      (cl-letf (((symbol-function 'custom-available-themes)
                 (lambda () '(ef-summer ef-winter)))
                ((symbol-function 'load-theme)
                 (lambda (&rest args)
                   (push args load-calls)
                   (neo/run-after-theme-load (car args)))))
        (expect (neo/restore-persisted-theme) :to-be nil)
        (expect load-calls :to-be nil)
        (expect (alist-get "theme" neo--test-config nil nil #'string=)
                :to-equal "missing-theme")
        (expect neo/current-theme :to-be nil)
        (expect neo--saved-frame-snapshots :to-equal 0))))

  (it "restores the previous theme when a new theme fails to load"
    (let (re-enabled-themes)
      (setq custom-enabled-themes '(ef-winter))
      (cl-letf (((symbol-function 'custom-available-themes)
                 (lambda () '(ef-summer ef-winter)))
                ((symbol-function 'disable-theme)
                 (lambda (_theme) nil))
                ((symbol-function 'load-theme)
                 (lambda (_theme _no-confirm)
                   (error "boom")))
                ((symbol-function 'enable-theme)
                 (lambda (theme)
                   (push theme re-enabled-themes)
                   (neo/run-after-theme-load theme))))
        (expect (neo/load-theme-internal 'ef-summer t) :to-be nil)
        (expect re-enabled-themes :to-equal '(ef-winter))
        (expect neo/current-theme :to-equal 'ef-winter)
        (expect (alist-get "theme" neo--test-config nil nil #'string=)
                :to-be nil)))))

(provide 'test-neo-ui-themes)
;;; test-neo-ui-themes.el ends here
