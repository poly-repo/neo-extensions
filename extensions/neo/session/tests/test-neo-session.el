;;; test-neo-session.el --- Tests for neo-session -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)
(require 'savehist)

(defconst neo--session-test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing `test-neo-session.el'.")

(defmacro neo/use-package (_name &rest arguments)
  "Apply the relevant custom and config forms from ARGUMENTS in order."
  (let ((custom-form (cadr (memq :custom arguments)))
        (config-form (cadr (memq :config arguments))))
    `(progn
       (setq ,(car custom-form) ,(cadr custom-form))
       ,config-form)))

(describe "neo-session"
  (it "configures the profile cache before enabling Savehist"
    (let ((global-map (copy-keymap global-map))
          (savehist-file "original-history")
          cache-argument
          activation-path)
      (cl-letf (((symbol-function 'neo/cache-file-path)
                 (lambda (filename)
                   (setq cache-argument filename)
                   "/tmp/neo-profile/savehist.el"))
                ((symbol-function 'savehist-mode)
                 (lambda (&optional _argument)
                   (setq activation-path savehist-file))))
        (load-file
         (expand-file-name "../neo-session.el" neo--session-test-dir))
        (expect cache-argument :to-equal "savehist.el")
        (expect savehist-file :to-equal "/tmp/neo-profile/savehist.el")
        (expect activation-path :to-equal savehist-file)))))

(provide 'test-neo-session)
;;; test-neo-session.el ends here
