;;; tests/test-neo-org.el --- Tests for neo-org -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)

(defmacro neo/use-package (&rest _args)
  "Ignore package declarations while loading extension code in tests."
  nil)

(defvar org-directory nil)
(defvar org-structure-template-alist nil)
(defvar prettify-symbols-alist nil)
(defvar prettify-symbols-compose-predicate nil)
(defvar prettify-symbols-unprettify-at-point nil)

(defun prettify-symbols-mode (&optional _arg)
  "Test stub for `prettify-symbols-mode'."
  nil)

(defun neo-mlody-mode ()
  "Test stub for `neo-mlody-mode'."
  (interactive)
  (setq major-mode 'neo-mlody-mode
        mode-name "MLody"))

(defun neo--test-org-write-file (path content)
  "Write CONTENT to PATH, creating parent directories as needed."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert content))
  path)

(defun neo--test-org-prepare-score-fixture (root)
  "Create the minimal shared score fixture under ROOT."
  (let ((score-root (expand-file-name neo--org-haskell-score-relative-directory root)))
    (make-directory (expand-file-name ".rules" score-root) t)
    (make-directory (expand-file-name "images" score-root) t)
    (make-directory (expand-file-name "chapters" score-root) t)
    (neo--test-org-write-file
     (expand-file-name "preamble.tex" score-root)
     "% test preamble\n")
    (neo--test-org-write-file
     (expand-file-name "theme.tex" score-root)
     "% test theme\n")
    (neo--test-org-write-file
     (expand-file-name "main.bib" score-root)
     "")
    (neo--test-org-write-file
     (expand-file-name ".rules/mlodylualatex.yaml" score-root)
     "!config\n")
    score-root))

(load-file (expand-file-name "../neo-org.el"
                             (file-name-directory (or load-file-name buffer-file-name))))

(describe "neo-org"
  (it "prefers the extension's Org directory override"
    (let ((neo/org-directory "~/notes")
          (org-directory "~/ignored"))
      (expect (neo--org-base-directory)
              :to-equal
              (file-name-as-directory (expand-file-name "~/notes")))))

  (it "falls back to Org's directory when no override is set"
    (let ((neo/org-directory nil)
          (org-directory "~/fallback"))
      (expect (neo--org-base-directory)
              :to-equal
              (file-name-as-directory (expand-file-name "~/fallback")))))

  (it "builds protocol capture templates against the configured notes file"
    (let* ((neo/org-directory "~/notes")
           (neo/org-capture-notes-file-name "capture.org")
           (templates (neo--org-capture-templates))
           (protocol-template (assoc "p" templates))
           (store-link-template (assoc "L" templates)))
      (expect neo/org-protocol-default-template-key :to-equal "p")
      (expect protocol-template :not :to-be nil)
      (expect store-link-template :not :to-be nil)
      (expect (nth 3 protocol-template)
              :to-equal
              `(file+headline ,(expand-file-name "~/notes/capture.org") "Inbox"))))

  (it "overrides Org's default <h shorthand with a Haskell source block"
    (require 'org-tempo)
    (let ((org-structure-template-alist (copy-tree org-structure-template-alist))
          (org-tempo-tags nil))
      (neo--org-configure-structure-templates)
      (with-temp-buffer
        (org-mode)
        (insert "<h")
        (expect (org-tempo-complete-tag) :to-be-truthy)
        (expect (buffer-string)
                :to-equal
                "#+begin_src haskell\n\n#+end_src"))))

  (it "keeps the extended Haskell Org tempo shorthands"
    (let (org-structure-template-alist)
      (neo--org-configure-structure-templates)
      (expect (cdr (assoc "hs" org-structure-template-alist))
              :to-equal
              "src haskell")
      (expect (cdr (assoc "haskell" org-structure-template-alist))
              :to-equal
              "src haskell")))

  (it "adds the Mlody Org tempo shorthand"
    (let (org-structure-template-alist)
      (neo--org-configure-structure-templates)
      (expect (cdr (assoc "m" org-structure-template-alist))
              :to-equal
              "src mlody")))

  (it "configures Org prose for 100-column auto-fill with proportional text"
    (let ((neo/org-fill-column 100)
          (neo/org-auto-fill t)
          (neo/org-use-variable-pitch t)
          (neo/org-code-block-font-height 0.9))
      (with-temp-buffer
        (org-mode)
        (neo--org-mode-setup)
        (expect fill-column :to-equal 100)
        (expect auto-fill-function :to-equal #'org-auto-fill-function)
        (expect (assq 'default face-remapping-alist) :not :to-be nil)
        (expect (assq 'org-block face-remapping-alist)
                :to-equal
                '(org-block (:inherit fixed-pitch :height 0.9) org-block))
        (expect (assq 'org-table face-remapping-alist)
                :to-equal
                '(org-table (:inherit fixed-pitch) org-table))
        (expect (length neo--org-fixed-pitch-cookies)
                :to-equal
                (length neo--org-fixed-pitch-faces)))))

  (it "can keep Org fully fixed-pitch when proportional prose is disabled"
    (let ((neo/org-use-variable-pitch nil))
      (with-temp-buffer
        (org-mode)
        (neo--org-mode-setup)
        (expect (assq 'default face-remapping-alist) :to-be nil)
        (expect neo--org-fixed-pitch-cookies :to-be nil))))

  (it "registers the Kaobook LaTeX class for Haskell notebooks"
    (let ((org-latex-classes '(("article" "\\documentclass{article}"))))
      (neo--org-haskell-register-latex-class)
      (let ((entry (assoc neo--org-haskell-latex-class-name org-latex-classes)))
        (expect entry :not :to-be nil)
        (expect (cadr entry) :to-equal neo--org-haskell-latex-documentclass)
        (expect (cadr entry)
                :to-match
                (regexp-quote neo--org-haskell-latex-preamble-input))
        (expect (cadr entry)
                :to-match
                "\\\\newminted\\[neoNotebookHaskellCode\\]{haskell}")
        (expect (caddr entry) :to-equal '("\\chapter{%s}" . "\\chapter*{%s}")))))

  (it "adds the optional Org Roam project template only when configured"
    (let ((neo/org-directory "~/notes")
          (neo/org-roam-project-template-file nil))
      (expect (neo--org-roam-project-capture-template) :to-be nil))
    (let ((neo/org-directory "~/notes")
          (neo/org-roam-project-template-file "templates/project.org"))
      (expect (car (neo--org-roam-project-capture-template)) :to-equal "p")))

  (it "resolves the Org Roam database through Neo's data path"
    (cl-letf (((symbol-function 'neo/data-file-path)
               (lambda (filename) (concat "/neo-data/" filename))))
      (expect (neo--org-roam-db-location) :to-equal "/neo-data/org-roam.db")))

  (it "resolves org-download images relative to the Org base directory"
    (let ((neo/org-directory "~/notes")
          (neo/org-download-image-dir "images"))
      (expect (neo--org-download-image-dir)
              :to-equal
              (file-name-as-directory (expand-file-name "~/notes/images")))))

  (it "enables org-crypt only when both the flag and key are present"
    (let ((neo/org-enable-crypt nil)
          (neo/org-crypt-key "ABC123"))
      (expect (neo--org-crypt-enabled-p) :to-be nil))
    (let ((neo/org-enable-crypt t)
          (neo/org-crypt-key nil))
      (expect (neo--org-crypt-enabled-p) :to-be nil))
    (let ((neo/org-enable-crypt t)
          (neo/org-crypt-key "ABC123"))
      (expect (neo--org-crypt-enabled-p) :to-be-truthy)))

  (it "prettifies haskell only in begin_src headers"
    (with-temp-buffer
      (insert "#+begin_src haskell\n")
      (search-backward "haskell")
      (let ((start (point))
            (neo--org-prettify-symbols-parent-predicate nil))
        (expect (neo--org-prettify-symbols-compose-p start (+ start 7) "haskell")
                :to-be-truthy))))

  (it "does not prettify plain haskell text"
    (with-temp-buffer
      (insert "haskell\n")
      (goto-char (point-min))
      (let ((neo--org-prettify-symbols-parent-predicate nil))
        (expect (neo--org-prettify-symbols-compose-p (point) (+ (point) 7) "haskell")
                :to-be nil))))

  (it "prettifies mlody only in begin_src headers"
    (with-temp-buffer
      (insert "#+begin_src mlody\n")
      (search-backward "mlody")
      (let ((start (point))
            (neo--org-prettify-symbols-parent-predicate nil))
        (expect (neo--org-prettify-symbols-compose-p start (+ start 5) "mlody")
                :to-be-truthy))))

  (it "maps mlody Org source editing to neo-mlody-mode"
    (let ((org-src-window-setup 'current-window))
      (neo--org-configure-src-editing)
      (expect (org-src-get-lang-mode "mlody") :to-equal 'neo-mlody-mode)
      (let (edit-buffer)
        (unwind-protect
            (with-temp-buffer
              (org-mode)
              (insert "#+begin_src mlody\npipeline example\n#+end_src\n")
              (goto-char (point-min))
              (search-forward "pipeline")
              (cl-letf (((symbol-function 'pop-to-buffer-same-window)
                         (lambda (buffer &rest _args)
                           (setq edit-buffer buffer)
                           (set-buffer buffer)
                           buffer))
                        ((symbol-function 'pop-to-buffer)
                         (lambda (buffer &rest _args)
                           (setq edit-buffer buffer)
                           (set-buffer buffer)
                           buffer)))
                (expect (org-edit-special) :to-be-truthy)
                (expect edit-buffer :not :to-be nil)
                (with-current-buffer edit-buffer
                  (expect major-mode :to-equal 'neo-mlody-mode))))
          (when (buffer-live-p edit-buffer)
            (kill-buffer edit-buffer))))))

  (it "uses the notebook LaTeX class in Haskell notebook buffers"
    (with-temp-buffer
      (neo/org-haskell-notebook-mode)
      (neo--org-haskell-configure-export)
      (expect org-latex-default-class
              :to-equal
              neo--org-haskell-latex-class-name)
      (expect org-latex-src-block-backend :to-equal 'minted)
      (expect (cadr (assq 'mlody org-latex-minted-langs))
              :to-equal
              "mlody")
      (expect (member #'neo--org-haskell-prefix-final-output
                      org-export-filter-final-output-functions)
              :not :to-be nil)
      (expect (member #'neo--org-haskell-style-src-block
                      org-export-filter-src-block-functions)
              :not :to-be nil)
      (expect (local-variable-p 'org-latex-default-class) :to-be-truthy)))

  (it "prefixes LaTeX exports with build-mode detection"
    (expect
     (neo--org-haskell-prefix-final-output "\\documentclass{article}\n" 'latex nil)
     :to-match
     (concat "\\`"
             (regexp-quote neo--org-haskell-latex-build-mode-prefix)
             (regexp-quote "\\documentclass{article}\n"))))

  (it "leaves non-LaTeX exports unchanged"
    (expect (neo--org-haskell-prefix-final-output "plain text" 'ascii nil)
            :to-equal
            "plain text"))

  (it "exports notebook LaTeX without a local-hook sentinel in final-output filters"
    (with-temp-buffer
      (insert "#+title: Demo\n\nHello, notebook export.\n")
      (neo/org-haskell-notebook-mode)
      (neo--org-haskell-register-latex-class)
      (neo--org-haskell-configure-export)
      (expect org-export-filter-final-output-functions :not :to-contain t)
      (let ((latex (org-export-as 'latex nil nil nil nil)))
        (expect (string-prefix-p neo--org-haskell-latex-build-mode-prefix latex)
                :to-be-truthy)
        (expect (string-match-p
                 (regexp-quote neo--org-haskell-latex-preamble-input)
                 latex)
                :not :to-be nil))))

  (it "exports Haskell source blocks with notebook shading"
    (with-temp-buffer
      (insert "#+title: Demo\n\n#+begin_src haskell\nf x = x + 1\n#+end_src\n")
      (neo/org-haskell-notebook-mode)
      (neo--org-haskell-register-latex-class)
      (neo--org-haskell-configure-export)
      (let* ((latex (org-export-as 'latex nil nil nil nil))
             (body (substring latex
                              (string-match
                               (regexp-quote "\\begin{document}")
                               latex))))
        (expect body :to-match (regexp-quote "\\begin{neoNotebookHaskellCode}[]"))
        (expect latex :to-match "neoNotebookHaskellBg")
        (expect body :not :to-match (regexp-quote "\\begin{minted}"))
        (expect body :not :to-match (regexp-quote "\\begin{verbatim}")))))

  (it "exports notebook blocks with language-specific shaded wrappers"
    (with-temp-buffer
      (insert "#+title: Demo\n\n"
              "#+begin_src mlody\nvalue(name=\"greeting\")\n#+end_src\n\n"
              "#+begin_src haskell\nf x = x + 1\n#+end_src\n\n"
              "#+begin_src python\nprint('hi')\n#+end_src\n")
      (neo/org-haskell-notebook-mode)
      (neo--org-haskell-register-latex-class)
      (neo--org-haskell-configure-export)
      (let* ((latex (org-export-as 'latex nil nil nil nil))
             (body (substring latex
                              (string-match
                               (regexp-quote "\\begin{document}")
                               latex))))
        (expect body :to-match (regexp-quote "\\begin{neoNotebookMlodyCode}[]"))
        (expect body :to-match (regexp-quote "\\begin{neoNotebookHaskellCode}[]"))
        (expect body :to-match (regexp-quote "\\begin{neoNotebookCode}[]{python}"))
        (expect latex :to-match "neoNotebookMlodyBg")
        (expect body :not :to-match (regexp-quote "\\begin{minted}")))))

  (it "runs a second LuaLaTeX pass for fast notebook arara profiles"
    (let* ((profile (neo--org-haskell-render-arara-build-profile "online" t t))
           (lines (split-string profile "\n" t))
            (latex-lines
             (cl-remove-if-not
              (lambda (line)
                (string-match-p "mlodylualatex" line))
              lines)))
      (expect (length latex-lines) :to-equal 2)
      (expect (car latex-lines) :to-equal (cadr latex-lines))
      (expect (car latex-lines) :to-match "jobname: .*online")
      (expect (car latex-lines) :to-match "pythonpath: \\.latexminted")
      (expect (car latex-lines) :to-match "draft: true")
      (expect profile :not :to-match "biber")
      (expect profile :not :to-match "makeindex")))

  (it "stages notebook LaTeX exports for arara without hardwired score jobnames"
    (let ((repo-root (make-temp-file "neo-org-haskell-repo" t))
          (temp-dir (make-temp-file "neo-org-haskell-temp" t)))
      (unwind-protect
          (with-temp-buffer
            (let ((notebook-file (expand-file-name "notes/demo.org" repo-root))
                  (neo/org-haskell-temporary-directory temp-dir))
              (neo--test-org-prepare-score-fixture repo-root)
              (setq default-directory repo-root
                    buffer-file-name notebook-file)
              (insert "#+title: Demo\n\nHello, notebook export.\n")
              (neo/org-haskell-notebook-mode)
              (cl-letf (((symbol-function 'org-export-to-file)
                         (lambda (_backend file &rest _args)
                           (with-temp-file file
                             (insert "LATEX\n"))
                           file)))
                (let* ((path (neo/org-haskell-export-latex))
                       (build-directory (neo--org-haskell-pdf-build-directory))
                       (config-path
                        (expand-file-name
                         neo--org-haskell-arara-config-file-name
                         build-directory))
                       (config
                        (with-temp-buffer
                          (insert-file-contents config-path)
                          (buffer-string))))
                  (expect path
                          :to-equal
                          (expand-file-name
                           neo--org-haskell-latex-entry-file-name
                           build-directory))
                  (expect path
                          :to-match
                          (concat "\\`"
                                  (regexp-quote
                                   (file-name-as-directory
                                    (expand-file-name temp-dir)))))
                  (expect (file-symlink-p (expand-file-name "images" build-directory))
                          :not :to-be nil)
                  (expect (file-symlink-p (expand-file-name "chapters" build-directory))
                          :not :to-be nil)
                  (expect (file-symlink-p
                           (expand-file-name
                            neo--org-haskell-score-preamble-relative-path
                            build-directory))
                          :not :to-be nil)
                  (expect (file-symlink-p (expand-file-name "theme.tex" build-directory))
                          :not :to-be nil)
                  (expect (file-symlink-p (expand-file-name "main.bib" build-directory))
                          :not :to-be nil)
                  (expect (file-symlink-p
                           (expand-file-name ".rules/mlodylualatex.yaml" build-directory))
                          :not :to-be nil)
                  (expect (file-symlink-p
                           (expand-file-name
                            neo--org-haskell-latexminted-support-relative-directory
                            build-directory))
                          :not :to-be nil)
                  (expect config :to-match "demo-print")
                  (expect config :to-match "demo-online")
                  (expect config :to-match "pythonpath: \\.latexminted")
                  (expect config :not :to-match "the-score-print")
                  (expect config :not :to-match "the-score-online")))))
        (delete-directory repo-root t)
        (delete-directory temp-dir t))))

  (it "compiles notebook PDFs from the staged LaTeX via arara"
    (let ((repo-root (make-temp-file "neo-org-haskell-repo" t))
          (temp-dir (make-temp-file "neo-org-haskell-temp" t)))
      (unwind-protect
          (with-temp-buffer
            (let ((notebook-file (expand-file-name "notes/demo.org" repo-root))
                  (neo/org-haskell-temporary-directory temp-dir)
                  compile-call)
              (neo--test-org-prepare-score-fixture repo-root)
              (setq default-directory repo-root
                    buffer-file-name notebook-file)
              (insert "#+title: Demo\n\nHello, notebook export.\n")
              (neo/org-haskell-notebook-mode)
              (cl-letf (((symbol-function 'org-export-to-file)
                         (lambda (_backend file &rest _args)
                           (with-temp-file file
                             (insert "LATEX\n"))
                           file))
                        ((symbol-function 'compile)
                         (lambda (command)
                           (setq compile-call (list default-directory command))
                           'compilation-buffer))
                        ((symbol-function 'message)
                         (lambda (&rest _args) nil)))
                (let ((build-directory
                       (file-name-as-directory
                        (neo--org-haskell-pdf-build-directory))))
                  (expect (neo/org-haskell-export-pdf)
                          :to-equal
                          'compilation-buffer)
                  (expect compile-call
                          :to-equal
                          (list build-directory
                                "arara --preamble draft-fast-online main.tex"))
                  (expect (file-exists-p
                           (expand-file-name
                            neo--org-haskell-latex-entry-file-name
                            build-directory))
                          :to-be-truthy)))))
        (delete-directory repo-root t)
        (delete-directory temp-dir t))))

  (it "routes Org's standard PDF export for notebooks through the staged arara build"
    (let ((repo-root (make-temp-file "neo-org-haskell-repo" t))
          (temp-dir (make-temp-file "neo-org-haskell-temp" t)))
      (unwind-protect
          (with-temp-buffer
            (let ((notebook-file (expand-file-name "notes/demo.org" repo-root))
                  (neo/org-haskell-temporary-directory temp-dir)
                  export-call
                  compiled-tex)
              (neo--test-org-prepare-score-fixture repo-root)
              (setq default-directory repo-root
                    buffer-file-name notebook-file)
              (insert "#+title: Demo\n\nHello, notebook export.\n")
              (neo/org-haskell-notebook-mode)
              (cl-letf (((symbol-function 'org-export-to-file)
                         (lambda (backend file async subtreep visible-only body-only ext-plist post-process)
                           (setq export-call
                                 (list backend file async subtreep visible-only body-only ext-plist))
                           (with-temp-file file
                             (insert "LATEX\n"))
                           (funcall post-process file)))
                        ((symbol-function 'neo--org-haskell-compile-pdf-file)
                         (lambda (file)
                           (setq compiled-tex file)
                           "/tmp/demo-online.pdf")))
                (expect (neo--org-haskell-export-to-pdf nil t nil nil '(:foo bar))
                        :to-equal
                        "/tmp/demo-online.pdf")
                (let ((entry-path
                       (expand-file-name
                        neo--org-haskell-latex-entry-file-name
                        (neo--org-haskell-pdf-build-directory))))
                  (expect export-call
                          :to-equal
                          (list 'latex entry-path nil t nil nil '(:foo bar)))
                  (expect compiled-tex :to-equal entry-path)))))
        (delete-directory repo-root t)
        (delete-directory temp-dir t))))

  (it "intercepts Org's standard PDF export only for notebook buffers"
    (let (notebook-call fallback-call)
      (with-temp-buffer
        (neo/org-haskell-notebook-mode)
        (cl-letf (((symbol-function 'neo--org-haskell-export-to-pdf)
                   (lambda (&rest args)
                     (setq notebook-call args)
                     "/tmp/notebook.pdf")))
          (expect
           (neo--org-haskell-around-latex-export-to-pdf
            (lambda (&rest args)
              (setq fallback-call args)
              "/tmp/fallback.pdf")
            nil t nil nil '(:demo t))
           :to-equal
           "/tmp/notebook.pdf")))
      (expect notebook-call :to-equal '(nil t nil nil (:demo t)))
      (expect fallback-call :to-be nil)
      (setq notebook-call nil
            fallback-call nil)
      (with-temp-buffer
        (org-mode)
        (expect
         (neo--org-haskell-around-latex-export-to-pdf
          (lambda (&rest args)
            (setq fallback-call args)
            "/tmp/fallback.pdf")
          nil nil t nil '(:demo t))
         :to-equal
         "/tmp/fallback.pdf"))
      (expect notebook-call :to-be nil)
      (expect fallback-call :to-equal '(nil nil t nil (:demo t)))))

  (it "collects Haskell notebook blocks in document order"
    (with-temp-buffer
      (org-mode)
      (insert "#+begin_src haskell\none = 1\n#+end_src\n\n"
              "#+begin_src python\nprint('skip')\n#+end_src\n\n"
              "#+begin_src haskell\ntwo = one + 1\n#+end_src\n")
      (let ((blocks (neo--org-haskell-collect-document-blocks)))
        (expect (mapcar (lambda (block) (plist-get block :body)) blocks)
                :to-equal
                '("one = 1"
                  "two = one + 1")))))

  (it "drops indentation common to every non-blank line in a block body"
    (expect (neo--org-haskell-normalize-block-body
             "  foo = do\n    pure 1\n\n  bar = foo\n")
            :to-equal
            "foo = do\n  pure 1\n\nbar = foo\n"))

  (it "sends the current Haskell block through multiline GHCi input"
    (let ((calls nil)
          (repl-buffer (generate-new-buffer " *neo-org-ghci*")))
      (unwind-protect
          (with-temp-buffer
            (org-mode)
            (insert "#+begin_src haskell\n  foo x =\n    x + 1\n#+end_src\n")
            (search-backward "foo")
            (let ((source-buffer (current-buffer)))
              (cl-letf (((symbol-function 'neo--haskell-ensure-standalone-repl)
                         (lambda ()
                           repl-buffer))
                        ((symbol-function 'get-buffer-process)
                         (lambda (buffer)
                           (expect buffer :to-equal repl-buffer)
                           'ghci-process))
                        ((symbol-function 'comint-send-string)
                         (lambda (process string)
                           (push (list process string) calls)))
                        ((symbol-function 'message)
                         (lambda (&rest _args) nil)))
                (neo/org-haskell-send-block)
                (with-current-buffer repl-buffer
                  (expect neo--haskell-standalone-repl-source-buffer
                          :to-equal source-buffer))
                (expect calls
                        :to-equal
                        '((ghci-process ":{\nfoo x =\n  x + 1\n:}\n"))))))
        (kill-buffer repl-buffer))))

  (it "tangles and loads all Haskell blocks into the notebook repl"
    (let ((calls nil)
          (repl-buffer (generate-new-buffer " *neo-org-ghci*"))
          (temp-dir (make-temp-file "neo-org-haskell-temp" t)))
      (unwind-protect
          (with-temp-buffer
            (org-mode)
            (let ((neo/org-haskell-temporary-directory temp-dir))
              (setq buffer-file-name "/tmp/notebooks/demo.org")
              (insert "* Notes\n"
                      "#+begin_src haskell\n  import Data.List\n#+end_src\n\n"
                      "#+begin_src haskell\n  main = print (sort [3, 1, 2])\n#+end_src\n")
              (goto-char (point-min))
              (cl-letf (((symbol-function 'neo--haskell-ensure-standalone-repl)
                         (lambda ()
                           repl-buffer))
                        ((symbol-function 'get-buffer-process)
                         (lambda (buffer)
                           (expect buffer :to-equal repl-buffer)
                           'ghci-process))
                        ((symbol-function 'comint-send-string)
                         (lambda (process string)
                           (push (list process string) calls)))
                        ((symbol-function 'message)
                         (lambda (&rest _args) nil)))
                (let* ((repl (neo/org-haskell-load-document))
                       (command (cadar calls))
                       (path (read (substring command 6 -1)))
                       (generated
                        (with-temp-buffer
                          (insert-file-contents path)
                          (buffer-string))))
                  (expect repl :to-equal repl-buffer)
                  (expect path
                          :to-match
                          (concat "\\`"
                                  (regexp-quote
                                   (file-name-as-directory
                                    (expand-file-name temp-dir)))))
                  (expect generated
                          :to-match
                          "-- Generated from /tmp/notebooks/demo\\.org by neo/org-haskell-notebook-mode\\.")
                  (expect generated
                          :to-match
                          "{-# LINE 3 \"/tmp/notebooks/demo\\.org\" #-}\nimport Data\\.List")
                  (expect generated
                          :to-match
                          (concat "{-# LINE 7 \"/tmp/notebooks/demo\\.org\" #-}\n"
                                  (regexp-quote "main = print (sort [3, 1, 2])")))
                  (expect calls :to-equal (list (list 'ghci-process command)))))))
        (delete-directory temp-dir t)
        (kill-buffer repl-buffer))))

  (it "deletes the frame after the requested number of capture exits"
    (let ((neo/org-capture-delete-frame-depth 2)
          (deleted 0))
      (cl-letf (((symbol-function 'delete-frame)
                 (lambda (&rest _) (setq deleted (1+ deleted)))))
        (neo--org-capture-delete-frame-if-needed)
        (expect neo/org-capture-delete-frame-depth :to-equal 1)
        (expect deleted :to-equal 0)
        (neo--org-capture-delete-frame-if-needed)
        (expect neo/org-capture-delete-frame-depth :to-equal 0)
        (expect deleted :to-equal 1)))))

(provide 'test-neo-org)
;;; test-neo-org.el ends here
