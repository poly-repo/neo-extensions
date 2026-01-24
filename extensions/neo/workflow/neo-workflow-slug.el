(defgroup neo-issue-slug nil
  "Human-readable issue → branch slug generation."
  :group 'neo-workflow)

(defcustom neo-issue-slug-max-words 5
  "Maximum number of words in the generated slug (excluding issue number)."
  :type 'integer)

(defcustom neo-issue-slug-max-chars 45
  "Maximum length of the generated slug, including issue number."
  :type 'integer)

(defcustom neo-issue-slug-stopwords
  '("a" "an" "the" "and" "or" "when" "with" "it" "to" "for" "of" "on" "by"
    "from" "is" "are" "be"
    "add" "create" "creating" "populate" "update" "implement" "make")
  "Words to drop entirely."
  :type '(repeat string))

(defcustom neo-issue-slug-drop-tail-words
  '("information" "stuff" "things" "details")
  "Words to drop when they appear at the end of the slug."
  :type '(repeat string))

(defcustom neo-issue-slug-replacements
  '(("configuration" . "config")
    ("documentation" . "docs")
    ("repository" . "repo")
    ("information" . "info"))
  "Word replacements applied after stopword filtering."
  :type '(alist :key-type string :value-type string))

(defcustom neo-issue-slug-preserve-regexp
  "\\(\\.[a-z0-9._-]+\\|[A-Z0-9]+\\)"
  "Regexp for tokens that should never be split or dropped.
Examples: filenames, acronyms."
  :type 'regexp)

(defun neo-issue-slug--normalize (title)
  "Normalize TITLE into a lowercase, space-separated string."
  (string-trim
   (replace-regexp-in-string
    "[^a-z0-9._-]+"
    " "
    (downcase title))))

(defun neo-issue-slug--tokenize (title)
  "Split normalized TITLE into tokens."
  (split-string title " +" t))

(defun neo-issue-slug--preserved-p (token)
  "Return non-nil if TOKEN should be preserved verbatim."
  (string-match-p neo-issue-slug-preserve-regexp token))

(defun neo-issue-slug--stopword-p (token)
  (member token neo-issue-slug-stopwords))

(defun neo-issue-slug--replace (token)
  (or (cdr (assoc token neo-issue-slug-replacements))
      token))

(defun neo-issue-slug--filter-tokens (tokens)
  "Remove stopwords unless preserved."
  (seq-filter
   (lambda (tok)
     (or (neo-issue-slug--preserved-p tok)
         (not (neo-issue-slug--stopword-p tok))))
   tokens))

(defun neo-issue-slug--drop-tail (tokens)
  "Drop unwanted trailing words."
  (while (and tokens
              (member (car (last tokens))
                      neo-issue-slug-drop-tail-words))
    (setq tokens (butlast tokens)))
  tokens)

(defun neo-issue-slug--assemble (tokens prefix)
  "Assemble TOKENS into a slug, respecting limits.
PREFIX is the issue number part, e.g. \"96\"."
  (let ((result '())
        (current prefix))
    (dolist (tok tokens)
      (let* ((tok (neo-issue-slug--replace tok))
             (candidate
              (concat current "-" tok)))
        (when (and (< (length result) neo-issue-slug-max-words)
                   (<= (length candidate) neo-issue-slug-max-chars))
          (push tok result)
          (setq current candidate))))
    (nreverse result)))

(defun neo-issue-title-to-slug (issue-id title)
  "Generate a human-readable slug from ISSUE-ID and TITLE.

Example:
  (neo-issue-title-to-slug
   96
   \"Add a .personal-notes file when creating a worktree\")"
  (let* ((prefix (when issue-id (format "%s" issue-id)))
         (tokens (neo-issue-slug--tokenize
                  (neo-issue-slug--normalize title)))
         (tokens (neo-issue-slug--filter-tokens tokens))
         (tokens (neo-issue-slug--drop-tail tokens))
         (words  (neo-issue-slug--assemble tokens (or prefix ""))))
    (if prefix
        (mapconcat #'identity (cons prefix words) "-")
      (mapconcat #'identity words "-"))))

(provide 'neo-workflow-slug)
