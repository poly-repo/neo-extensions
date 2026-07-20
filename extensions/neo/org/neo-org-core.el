;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)

(defgroup neo-org nil
  "Customization options for the org extension."
  :group 'neo-extensions)

(defcustom neo/org-directory nil
  "Base directory for Org files.
When nil, fall back to `org-directory' if it is already set, else `~/org/'."
  :type '(choice (const :tag "Use Org default" nil)
                 string)
  :group 'neo-org)

(defcustom neo/org-default-notes-file-name "default-notes.org"
  "Filename for `org-default-notes-file' relative to `neo/org-directory'."
  :type 'string
  :group 'neo-org)

(defcustom neo/org-capture-notes-file-name "notes.org"
  "Filename used by the default capture templates.
Relative paths are resolved under `neo/org-directory'."
  :type 'string
  :group 'neo-org)

(defcustom neo/org-protocol-default-template-key "p"
  "Default `org-protocol' template key for captures without an explicit key."
  :type 'string
  :group 'neo-org)

(defcustom neo/org-babel-languages
  '((emacs-lisp . t)
    (python . t)
    (shell . t)
    (C . t))
  "Languages enabled for Org Babel."
  :type '(alist :key-type symbol :value-type boolean)
  :group 'neo-org)

(defcustom neo/org-babel-cpp-compiler "clang++"
  "Compiler used for C and C++ Org Babel source blocks."
  :type 'string
  :group 'neo-org)

(defcustom neo/org-babel-cpp-flags "-std=c++23"
  "Compiler flags used for C++ Org Babel source blocks."
  :type 'string
  :group 'neo-org)

(defcustom neo/org-enable-modern t
  "When non-nil, enable `org-modern' in Org buffers and agendas."
  :type 'boolean
  :group 'neo-org)

(defcustom neo/org-prettify-haskell-header t
  "When non-nil, display `haskell' as lambda in Org src headers.
This only affects the language token in `#+begin_src haskell' lines."
  :type 'boolean
  :group 'neo-org)

(defcustom neo/org-prettify-mlody-header t
  "When non-nil, display `mlody' as music in Org src headers.
This only affects the language token in `#+begin_src mlody' lines."
  :type 'boolean
  :group 'neo-org)

(defcustom neo/org-enable-crypt nil
  "When non-nil, enable `org-crypt' before-save encryption support."
  :type 'boolean
  :group 'neo-org)

(defcustom neo/org-crypt-key nil
  "GPG key used by `org-crypt'.
When nil, the crypt integration stays inert even if enabled."
  :type '(choice (const :tag "Disabled" nil)
                 string)
  :group 'neo-org)

(defcustom neo/org-crypt-tag "crypt"
  "Tag that marks subtrees for `org-crypt' encryption."
  :type 'string
  :group 'neo-org)

(defconst neo--org-structure-templates
  '(("h" . "src haskell")
    ("m" . "src mlody")
    ("sh" . "src sh")
    ("ml" . "src mlody")
    ("mlody" . "src mlody")
    ("b" . "src bash")
    ("el" . "src emacs-lisp")
    ("py" . "src python")
    ("cpp" . "src C++")
    ("c++" . "src C++")
    ("yaml" . "src yaml")
    ("json" . "src json"))
  "Additional structure templates installed by the org extension.")

(defconst neo--org-prettify-symbols
  '(("haskell" . ?λ)
    ("mlody" . ?🎼))
  "Prettified symbols used by the org extension.")

(defvar-local neo--org-prettify-symbols-parent-predicate nil
  "Original `prettify-symbols-compose-predicate' for the current Org buffer.")

(defun neo--org-string-present-p (value)
  "Return non-nil when VALUE is a non-empty string."
  (and (stringp value)
       (not (string-empty-p value))))

(defun neo--org-normalize-path (path &optional base-directory)
  "Return PATH as an absolute path, resolving symlinks when possible.
If PATH is relative, resolve it under BASE-DIRECTORY."
  (let ((expanded (expand-file-name path base-directory)))
    (if (file-exists-p expanded)
        (file-truename expanded)
      expanded)))

(defun neo--org-base-directory ()
  "Return the absolute base directory for Org files."
  (file-name-as-directory
   (neo--org-normalize-path
    (or (and (neo--org-string-present-p neo/org-directory)
             neo/org-directory)
        (and (boundp 'org-directory)
             (neo--org-string-present-p org-directory)
             org-directory)
        "~/org/"))))

(defun neo--org-file-in-base-directory (filename)
  "Return FILENAME resolved under `neo--org-base-directory'."
  (neo--org-normalize-path filename (neo--org-base-directory)))

(defun neo--org-default-notes-file ()
  "Return the default notes file path for Org captures."
  (neo--org-file-in-base-directory neo/org-default-notes-file-name))

(defun neo--org-capture-notes-file ()
  "Return the capture notes file path."
  (neo--org-file-in-base-directory neo/org-capture-notes-file-name))

(defun neo--org-prettify-symbols-compose-p (start _end match)
  "Return non-nil when MATCH should be prettified between START and _END."
  (and (or (null neo--org-prettify-symbols-parent-predicate)
           (funcall neo--org-prettify-symbols-parent-predicate start _end match))
       (cond
        ((string= match "haskell")
         (and neo/org-prettify-haskell-header
              (save-excursion
                (goto-char start)
                (beginning-of-line)
                (looking-at-p "^[[:blank:]]*#\\+begin_src[[:blank:]]+haskell\\(?:[[:blank:]].*\\)?$"))))
        ((string= match "mlody")
         (and neo/org-prettify-mlody-header
              (save-excursion
                (goto-char start)
                (beginning-of-line)
                (looking-at-p "^[[:blank:]]*#\\+begin_src[[:blank:]]+mlody\\(?:[[:blank:]].*\\)?$"))))
        (t t))))

(defun neo--org-configure-prettify-symbols ()
  "Enable Org-specific symbol prettification in the current buffer."
  (unless (eq prettify-symbols-compose-predicate
              #'neo--org-prettify-symbols-compose-p)
    (setq-local neo--org-prettify-symbols-parent-predicate
                prettify-symbols-compose-predicate))
  (dolist (entry neo--org-prettify-symbols)
    (cl-pushnew entry prettify-symbols-alist :test #'equal))
  (setq-local prettify-symbols-compose-predicate
              #'neo--org-prettify-symbols-compose-p)
  (setq-local prettify-symbols-unprettify-at-point 'right-edge)
  (prettify-symbols-mode 1))

(defun neo--org-mode-setup ()
  "Apply NEO defaults for Org buffers."
  (electric-pair-mode -1)
  (when (or neo/org-prettify-haskell-header
            neo/org-prettify-mlody-header)
    (neo--org-configure-prettify-symbols)))

(defun neo--org-capture-templates ()
  "Return the default Org capture templates for the extension."
  (let ((notes-file (neo--org-capture-notes-file)))
    `(("p" "org-protocol-capture" entry
       (file+headline ,notes-file "Inbox")
       "* %^{Title}\nSource: %u, %c\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?"
       :immediate-finish t)
      ("L" "org-protocol-store-link" entry
       (file+headline ,notes-file "Inbox")
       "* %? [[%:link][%:description]]\nCaptured On: %U"
       :immediate-finish t))))

(defun neo--org-configure-babel ()
  "Enable Org Babel languages configured by `neo/org-babel-languages'."
  (org-babel-do-load-languages
   'org-babel-load-languages
   neo/org-babel-languages)
  (when (alist-get 'C neo/org-babel-languages)
    (setq org-babel-C-compiler neo/org-babel-cpp-compiler
          org-babel-C++-flags neo/org-babel-cpp-flags
          org-babel-default-header-args:C++
          `((:compiler . ,neo/org-babel-cpp-compiler)
            (:flags . ,neo/org-babel-cpp-flags)))))

(defun neo--org-configure-structure-templates ()
  "Install additional structure templates for Org blocks."
  (dolist (entry neo--org-structure-templates)
    (cl-pushnew entry org-structure-template-alist :test #'equal)))

(defun neo--org-configure-src-editing ()
  "Teach Org source editing to use Neo's language-specific modes."
  (require 'org-src)
  ;; Org stores the mode name without the `-mode' suffix.
  (setf (alist-get "mlody" org-src-lang-modes nil nil #'equal) 'neo-mlody))

(defun neo--org-crypt-enabled-p ()
  "Return non-nil when org-crypt should be configured."
  (and neo/org-enable-crypt
       (neo--org-string-present-p neo/org-crypt-key)))

(defun neo--org-configure-crypt ()
  "Configure org-crypt using the extension's custom variables."
  (org-crypt-use-before-save-magic)
  (cl-pushnew neo/org-crypt-tag org-tags-exclude-from-inheritance :test #'equal)
  (setq org-crypt-key neo/org-crypt-key
        org-crypt-disable-auto-save 'encrypt))

(neo/use-package org
  :builtin t
  :custom
  (org-directory (neo--org-base-directory))
  (org-default-notes-file (neo--org-default-notes-file))
  (org-protocol-default-template-key neo/org-protocol-default-template-key)
  (org-capture-templates (neo--org-capture-templates))
  :hook
  (org-mode . neo--org-mode-setup)
  :config
  (neo--org-configure-babel))

(neo/use-package org-protocol
  :builtin t
  :after org)

(neo/use-package org-src
  :builtin t
  :after org
  :config
  (neo--org-configure-src-editing))

(neo/use-package org-tempo
  :builtin t
  :after org
  :config
  (neo--org-configure-structure-templates))

(neo/use-package org-crypt
  :builtin t
  :after org
  :if (neo--org-crypt-enabled-p)
  :config
  (neo--org-configure-crypt))

(neo/use-package ob-python
  :builtin t
  :after org)

(neo/use-package ob-shell
  :builtin t
  :after org)

(neo/use-package ob-C
  :builtin t
  :after org)

(neo/use-package org-modern
  :if neo/org-enable-modern
  :after org
  :hook
  ((org-mode . org-modern-mode)
   (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-hide-emphasis-markers t))

(provide 'neo-org-core)
;;; neo-org-core.el ends here
