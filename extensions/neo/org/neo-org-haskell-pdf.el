;;; -*- lexical-binding: t -*-

(require 'compile)
(require 'ox)
(require 'neo-org-haskell)
(require 'neo-org-haskell-export)

(defcustom neo/org-haskell-arara-command "arara"
  "Command used to compile exported notebook LaTeX with arara."
  :type 'string
  :group 'neo-org)

(defcustom neo/org-haskell-arara-default-preamble "draft-fast-online"
  "Default arara preamble used by `neo/org-haskell-export-pdf'."
  :type '(choice
          (const "print")
          (const "online")
          (const "fast-print")
          (const "fast-online")
          (const "draft-print")
          (const "draft-online")
          (const "draft-fast-print")
          (const "draft-fast-online"))
  :group 'neo-org)

(defconst neo--org-haskell-pdf-cache-directory "neo-org-haskell-pdf"
  "Temporary subdirectory used for generated notebook PDF builds.")

(defconst neo--org-haskell-latex-entry-file-name "main.tex"
  "Entry file name used for generated notebook LaTeX builds.")

(defconst neo--org-haskell-arara-config-file-name "arararc.yaml"
  "Arara config file written for notebook PDF builds.")

(defconst neo--org-haskell-latexminted-support-relative-directory ".latexminted"
  "Build-relative directory that exposes repo-local latexminted helpers.")

(defconst neo--org-haskell-latexminted-pythonpath ".latexminted"
  "PYTHONPATH value used by notebook latexminted runs.")

(defconst neo--org-haskell-latexminted-support-source-directory
  (expand-file-name
   "support/latexminted"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Source directory containing repo-local latexminted helpers.")

(defconst neo--org-haskell-pdf-log-buffer-name "*Org PDF LaTeX Output*"
  "Buffer used for synchronous notebook PDF compilation output.")

(defconst neo--org-haskell-arara-build-preambles
  '("print"
    "online"
    "fast-print"
    "fast-online"
    "draft-print"
    "draft-online"
    "draft-fast-print"
    "draft-fast-online")
  "Arara preambles that produce a notebook PDF.")

(defconst neo--org-haskell-arara-all-preambles
  (append neo--org-haskell-arara-build-preambles '("clean"))
  "All arara preambles emitted into notebook build configs.")

(defconst neo--org-haskell-arara-pass-clean-extensions
  '("aux" "idx" "toc" "out" "log" "bbl" "bcf" "blg" "run.xml")
  "Intermediate extensions cleaned before non-fast notebook builds.")

(defconst neo--org-haskell-arara-clean-extensions
  '("pdf"
    "aux"
    "bbl"
    "bcf"
    "blg"
    "fdb_latexmk"
    "fls"
    "glg"
    "idx"
    "ilg"
    "ind"
    "glstex"
    "log"
    "out"
    "run.xml"
    "synctex.gz"
    "auxlock"
    "lof"
    "lot"
    "toc")
  "Generated extensions removed by the notebook clean preamble.")

(defconst neo--org-haskell-score-rule-relative-path
  (concat neo--org-haskell-score-relative-directory "/.rules/mlodylualatex.yaml")
  "Repository-relative path to the score arara rule reused by notebooks.")

(defconst neo--org-haskell-score-optional-root-entries
  '("examples" "img")
  "Score root entries staged into notebook PDF builds when present.")

(defun neo--org-haskell-ensure-notebook-buffer ()
  "Raise a user error unless the current buffer is a Haskell notebook."
  (unless (derived-mode-p 'neo/org-haskell-notebook-mode)
    (user-error "neo-org: current buffer is not a Haskell notebook")))

(defun neo--org-haskell-score-root ()
  "Return the repository root containing the shared score assets."
  (neo--org-haskell-ensure-notebook-buffer)
  (let* ((start (or (and buffer-file-name
                         (file-name-directory buffer-file-name))
                    default-directory))
         (root
          (locate-dominating-file
           start
           (lambda (directory)
             (file-exists-p
              (expand-file-name
               neo--org-haskell-score-preamble-relative-path
               directory))))))
    (unless root
      (user-error
       "neo-org: could not locate %s from %s"
       neo--org-haskell-score-preamble-relative-path
       start))
    root))

(defun neo--org-haskell-score-directory ()
  "Return the absolute score directory reused by notebook PDF builds."
  (expand-file-name
   neo--org-haskell-score-relative-directory
   (neo--org-haskell-score-root)))

(defun neo--org-haskell-pdf-build-directory ()
  "Return the build directory for the current notebook PDF export."
  (expand-file-name
   (neo--org-haskell-notebook-slug)
   (neo--org-haskell-artifact-root neo--org-haskell-pdf-cache-directory)))

(defun neo--org-haskell-pdf-entry-path ()
  "Return the generated LaTeX entry file for the current notebook."
  (expand-file-name
   neo--org-haskell-latex-entry-file-name
   (neo--org-haskell-pdf-build-directory)))

(defun neo--org-haskell-arara-jobname (variant)
  "Return the arara job name for VARIANT."
  (format "%s-%s" (neo--org-haskell-buffer-stem) variant))

(defun neo--org-haskell-arara-output-variant (preamble)
  "Return the output variant selected by PREAMBLE."
  (cond
   ((string-suffix-p "online" preamble) "online")
   ((string-suffix-p "print" preamble) "print")
   (t
    (user-error "neo-org: cannot infer output variant from preamble %s"
                preamble))))

(defun neo--org-haskell-pdf-output-path (preamble &optional directory)
  "Return the notebook PDF path for PREAMBLE under DIRECTORY."
  (expand-file-name
   (format "%s.pdf"
           (neo--org-haskell-arara-jobname
            (neo--org-haskell-arara-output-variant preamble)))
   (or directory
       (neo--org-haskell-pdf-build-directory))))

(defun neo--org-haskell-render-arara-clean-directive (&optional files extensions)
  "Return an arara clean directive for FILES and EXTENSIONS."
  (concat
   "% arara: clean: { "
   (string-join
    (delq nil
          (list
           (when files
             (format "files: [ %s ]" (string-join files ", ")))
           (when extensions
             (format "extensions: [ %s ]" (string-join extensions ", ")))))
    ", ")
   " }"))

(defun neo--org-haskell-render-arara-lualatex-directive (variant &optional draft)
  "Return the notebook LuaLaTeX directive for VARIANT.
When DRAFT is non-nil, set the MLody draft flag for the run."
  (format
   "%% arara: mlodylualatex: { jobname: %s, shell: true, pythonpath: %s%s, synctex: yes }"
   (neo--org-haskell-arara-jobname variant)
   neo--org-haskell-latexminted-pythonpath
   (if draft ", draft: true" "")))

(defun neo--org-haskell-render-arara-build-profile (variant &optional draft fast)
  "Return the arara profile body for VARIANT.
When DRAFT is non-nil, set `MLODY_DRAFT'.  When FAST is non-nil, skip the
bibliography, index, and extra LuaLaTeX passes."
  (let* ((jobname (neo--org-haskell-arara-jobname variant))
         (latex (neo--org-haskell-render-arara-lualatex-directive variant draft)))
    (string-join
     (append
      (unless fast
        (list
         (neo--org-haskell-render-arara-clean-directive
          nil
          neo--org-haskell-arara-pass-clean-extensions)))
      (list latex)
      ;; minted v3 generates highlight files on the first TeX pass and needs
      ;; one more pass to typeset them instead of placeholder markers.
      (when fast
        (list latex))
      (unless fast
        (list
         (format "%% arara: biber: { options: [ %S ] }" jobname)
         (format "%% arara: makeindex: { files: [ %s ] }" jobname)
         latex
         latex)))
     "\n")))

(defun neo--org-haskell-render-arara-clean-profile ()
  "Return the arara clean profile body for the current notebook build."
  (string-join
   (list
    (neo--org-haskell-render-arara-clean-directive
     (list "main"
           (neo--org-haskell-arara-jobname "print")
           (neo--org-haskell-arara-jobname "online"))
     neo--org-haskell-arara-clean-extensions)
    (neo--org-haskell-render-arara-clean-directive
     '("arara.xml" "arara.yaml" "arara.log")))
   "\n"))

(defun neo--org-haskell-indent-block (block prefix)
  "Return BLOCK with PREFIX inserted at the start of each line."
  (mapconcat
   (lambda (line) (concat prefix line))
   (split-string block "\n" t)
   "\n"))

(defun neo--org-haskell-render-arara-config ()
  "Return the arara config for the current notebook PDF build."
  (let ((profiles
         `(("print" . ,(neo--org-haskell-render-arara-build-profile "print"))
           ("online" . ,(neo--org-haskell-render-arara-build-profile "online"))
           ("fast-print" . ,(neo--org-haskell-render-arara-build-profile "print" nil t))
           ("fast-online" . ,(neo--org-haskell-render-arara-build-profile "online" nil t))
           ("draft-print" . ,(neo--org-haskell-render-arara-build-profile "print" t))
           ("draft-online" . ,(neo--org-haskell-render-arara-build-profile "online" t))
           ("draft-fast-print" . ,(neo--org-haskell-render-arara-build-profile "print" t t))
           ("draft-fast-online" . ,(neo--org-haskell-render-arara-build-profile "online" t t))
           ("clean" . ,(neo--org-haskell-render-arara-clean-profile)))))
    (concat
     "!config\n"
     "paths:\n"
     "- './.rules'\n"
     "logging: true\n"
     "preambles:\n"
     (mapconcat
      (lambda (profile)
        (format "  %s: |\n%s"
                (car profile)
                (neo--org-haskell-indent-block (cdr profile) "    ")))
      profiles
      "\n"))))

(defun neo--org-haskell-write-file (path content)
  "Write CONTENT to PATH, creating parent directories as needed."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert content))
  path)

(defun neo--org-haskell-stage-score-path (source relative-path build-directory)
  "Expose SOURCE inside BUILD-DIRECTORY at RELATIVE-PATH."
  (unless (file-exists-p source)
    (user-error "neo-org: missing required score path %s" source))
  (let ((target (expand-file-name relative-path build-directory)))
    (make-directory (file-name-directory target) t)
    (make-symbolic-link source target)
    target))

(defun neo--org-haskell-stage-score-path-if-present (source relative-path build-directory)
  "Expose SOURCE inside BUILD-DIRECTORY at RELATIVE-PATH when SOURCE exists."
  (when (file-exists-p source)
    (neo--org-haskell-stage-score-path source relative-path build-directory)))

(defun neo--org-haskell-prepare-pdf-build-directory ()
  "Reset and repopulate the current notebook PDF build directory."
  (let* ((build-directory (neo--org-haskell-pdf-build-directory))
         (score-directory (neo--org-haskell-score-directory)))
    (when (file-directory-p build-directory)
      (delete-directory build-directory t))
    (make-directory build-directory t)
    (neo--org-haskell-stage-score-path
     (expand-file-name "images" score-directory)
     "images"
     build-directory)
    (neo--org-haskell-stage-score-path
     (expand-file-name "chapters" score-directory)
     "chapters"
     build-directory)
    (neo--org-haskell-stage-score-path
     (expand-file-name "preamble.tex" score-directory)
     neo--org-haskell-score-preamble-relative-path
     build-directory)
    (neo--org-haskell-stage-score-path
     (expand-file-name "theme.tex" score-directory)
     "theme.tex"
     build-directory)
    (neo--org-haskell-stage-score-path
     (expand-file-name "main.bib" score-directory)
     "main.bib"
     build-directory)
    (neo--org-haskell-stage-score-path
     (expand-file-name ".rules/mlodylualatex.yaml" score-directory)
     ".rules/mlodylualatex.yaml"
     build-directory)
    (neo--org-haskell-stage-score-path
     neo--org-haskell-latexminted-support-source-directory
     neo--org-haskell-latexminted-support-relative-directory
     build-directory)
    (dolist (entry neo--org-haskell-score-optional-root-entries)
      (neo--org-haskell-stage-score-path-if-present
       (expand-file-name entry score-directory)
       entry
       build-directory))
    (neo--org-haskell-write-file
     (expand-file-name neo--org-haskell-arara-config-file-name build-directory)
     (neo--org-haskell-render-arara-config))
    build-directory))

(defun neo--org-haskell-export-latex-file
    (&optional async subtreep visible-only body-only ext-plist post-process)
  "Export the current notebook to the staged LaTeX entry file.
ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, EXT-PLIST, and POST-PROCESS are
passed through to `org-export-to-file'."
  (neo--org-haskell-ensure-notebook-buffer)
  (neo--org-haskell-register-latex-class)
  (neo--org-haskell-configure-export)
  (let ((build-directory (neo--org-haskell-prepare-pdf-build-directory))
        (org-export-show-temporary-export-buffer nil))
    (org-export-to-file
     'latex
     (expand-file-name neo--org-haskell-latex-entry-file-name build-directory)
     async
     subtreep
     visible-only
     body-only
     ext-plist
     post-process)))

(defun neo--org-haskell-compile-pdf-file (tex-path)
  "Compile TEX-PATH synchronously with arara and return the resulting PDF."
  (let* ((preamble neo/org-haskell-arara-default-preamble)
         (build-directory (file-name-directory tex-path))
         (pdf-path (neo--org-haskell-pdf-output-path preamble build-directory))
         (command (neo--org-haskell-arara-command preamble))
         (buffer (get-buffer-create neo--org-haskell-pdf-log-buffer-name)))
    (when (file-exists-p pdf-path)
      (delete-file pdf-path))
    (with-current-buffer buffer
      (setq default-directory build-directory)
      (setq buffer-read-only nil)
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (let ((exit-code
           (let ((default-directory build-directory))
             (call-process
              shell-file-name
              nil
              buffer
              nil
              shell-command-switch
              command))))
      (with-current-buffer buffer
        (compilation-mode)
        (setq default-directory build-directory))
      (unless (and (integerp exit-code)
                   (zerop exit-code)
                   (file-exists-p pdf-path))
        (error
         "neo-org: file \"%s\" wasn't produced  See \"%s\" for details"
         (file-name-nondirectory pdf-path)
         neo--org-haskell-pdf-log-buffer-name))
      pdf-path)))

(defun neo--org-haskell-export-to-pdf
    (&optional async subtreep visible-only body-only ext-plist)
  "Export the current notebook to PDF through the staged arara build."
  (when async
    (user-error
     "neo-org: async LaTeX PDF export is not supported for Haskell notebooks"))
  (neo--org-haskell-export-latex-file
   nil
   subtreep
   visible-only
   body-only
   ext-plist
   #'neo--org-haskell-compile-pdf-file))

(defun neo--org-haskell-around-latex-export-to-pdf
    (orig &optional async subtreep visible-only body-only ext-plist)
  "Route notebook PDF exports away from Org's default LaTeX compiler."
  (if (derived-mode-p 'neo/org-haskell-notebook-mode)
      (neo--org-haskell-export-to-pdf
       async
       subtreep
       visible-only
       body-only
       ext-plist)
    (funcall orig async subtreep visible-only body-only ext-plist)))

(defun neo--org-haskell-read-arara-preamble ()
  "Prompt for an arara preamble used to build a notebook PDF."
  (completing-read
   (format "Arara preamble (default %s): "
           neo/org-haskell-arara-default-preamble)
   neo--org-haskell-arara-build-preambles
   nil
   t
   nil
   nil
   neo/org-haskell-arara-default-preamble))

(defun neo--org-haskell-arara-command (preamble)
  "Return the shell command used to run arara with PREAMBLE."
  (mapconcat
   #'shell-quote-argument
   (list neo/org-haskell-arara-command
         "--preamble"
         preamble
         neo--org-haskell-latex-entry-file-name)
   " "))

;;;###autoload
(defun neo/org-haskell-export-latex ()
  "Export the current notebook to its staged LaTeX entry file."
  (interactive)
  (let ((path (neo--org-haskell-export-latex-file)))
    (when (called-interactively-p 'interactive)
      (message "neo-org: exported notebook LaTeX to %s" path))
    path))

;;;###autoload
(defun neo/org-haskell-export-pdf (&optional preamble)
  "Export the current notebook to LaTeX and compile it with arara.
With prefix argument, prompt for PREAMBLE instead of using the default."
  (interactive
   (list
    (if current-prefix-arg
        (neo--org-haskell-read-arara-preamble)
      neo/org-haskell-arara-default-preamble)))
  (setq preamble (or preamble neo/org-haskell-arara-default-preamble))
  (unless (member preamble neo--org-haskell-arara-build-preambles)
    (user-error
     "neo-org: %s is not a PDF-producing arara preamble"
     preamble))
  (let* ((tex-path (neo--org-haskell-export-latex-file))
         (build-directory (file-name-directory tex-path)))
    (when (called-interactively-p 'interactive)
      (message
       "neo-org: building %s with arara preamble %s"
       tex-path
       preamble))
    (let ((default-directory build-directory))
      (compile (neo--org-haskell-arara-command preamble)))))

(neo/use-package ox-latex
  :builtin t
  :after org
  :config
  (unless (advice-member-p
           #'neo--org-haskell-around-latex-export-to-pdf
           #'org-latex-export-to-pdf)
    (advice-add
     #'org-latex-export-to-pdf
     :around
     #'neo--org-haskell-around-latex-export-to-pdf)))

(provide 'neo-org-haskell-pdf)
;;; neo-org-haskell-pdf.el ends here
