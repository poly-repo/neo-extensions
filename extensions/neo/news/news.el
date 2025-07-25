(defgroup neo/news nil
  "Neo configuration for getting news in Neo."
  :group 'neo-extensions
  :prefix "neo/news-")

(defcustom neo/news-user-mail-address "user@example.com"
  "Primary email address used for sending mail."
  :type '(string :match (lambda (val) (string-match-p "\\`[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]\\{2,\\}\\'" val)))
  :group 'neo/news)

(defcustom neo/news-openpgp-signers '("0000000000000000")
  "List of OpenPGP key IDs (long form, 16 uppercase hex digits) used for signing."
  :type '(repeat (string :match (lambda (val) (string-match-p "\\`[A-F0-9]\\{16\\}\\'" val))))
  :group 'neo/news)

(defcustom neo/news-gnus-select-method
  '(nnimap "gmail"
           (nnimap-address "imap.gmail.com")
           (nnimap-server-port 993)
           (nnimap-stream ssl))
  "IMAP server connection settings for Gnus."
  :type '(choice
          (const :tag "None" nil)
          (sexp :tag "IMAP Configuration"))
  :group 'neo/news)

(defcustom neo/news-smtpmail-smtp-server "smtp.gmail.com"
  "SMTP server used to send outgoing mail."
  :type 'string
  :group 'neo/news)

(defcustom neo/news-smtpmail-smtp-service 587
  "Port number used by the SMTP server."
  :type 'integer
  :group 'neo/news)

(defcustom neo/news-message-send-mail-function 'smtpmail-send-it
  "Function used to send email messages."
  :type 'function
  :group 'neo/news)

(defcustom neo/news-gnus-ignored-newsgroups
  "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
  "Regex for newsgroups Gnus should ignore."
  :type 'regexp
  :group 'neo/news)

(defcustom neo/news-gnus-agent nil
  "Whether the Gnus agent should be enabled.

Set to nil to avoid issues with nnimap."
  :type 'boolean
  :group 'neo/news)

(defcustom neo/news-gnus-message-archive-group nil
  "Gnus group used to archive sent messages.

Set to nil to avoid storing local unencrypted copies."
  :type '(choice (const :tag "None" nil) string)
  :group 'neo/news)

(defcustom neo/news-mml-encrypt-to-self t
  "Whether to encrypt messages to self when using OpenPGP."
  :type 'boolean
  :group 'neo/news)

;; TODO something like the following somewhere:
;; (when window-system
;;   (setq gnus-sum-thread-tree-indent "  ")
;;   (setq gnus-sum-thread-tree-root "") ;; "● ")
;;   (setq gnus-sum-thread-tree-false-root "") ;; "◯ ")
;;   (setq gnus-sum-thread-tree-single-indent "") ;; "◎ ")
;;   (setq gnus-sum-thread-tree-vertical        "│")
;;   (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
;;   (setq gnus-sum-thread-tree-single-leaf     "╰─► "))

;;; TODO can we do better? maybe if the user is signed into gmail the
;;; browser would tell us
(defun neo/guess-email-address ()
  "Attempt to guess the user's email address based on environment variables."
  (or
   (getenv "EMAIL")
   "nobody@example.com"))

;; (neo/use-package gnus
;;   :ensure nil  ;; Gnus is built into Emacs, so we don't need to
;;   ;; install it separately
;;   :custom
;;   (gnus-summary-display-arrow t)
;;   ;; most recent things to the top
;;   (gnus-thread-sort-functions '((not gnus-thread-sort-by-date) (not gnus-thread-sort-by-number)))
;;   (gnus-article-sort-functions '((not gnus-article-sort-by-date)))
;;   (gnus-summary-line-format
;;       (concat
;;        "%0{%U%R%z%}"
;;        "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
;;        "  "
;;        "%4{%-20,20f%}"               ;; name
;;        "  "
;;        "%3{│%}"
;;        " "
;;        "%1{%B%}"
;;        "%s\n"))
;;   (gnus-parameters
;;       '((".*"
;;          (gnus-thread-sort-functions
;;           '((not gnus-thread-sort-by-date)))
;;          (gnus-article-sort-functions
;;           '((not gnus-article-sort-by-date))))))
				
;;   :hook ((message-setup . mml-secure-message-encrypt)
;;          (gnus-summary-mode . (lambda ()
;; 				(local-set-key "y" #'gmail-archive)
;; 				(local-set-key "$" #'gmail-report-spam)
;; 				(gnus-summary-sort-by-date)
;; 				(gnus-summary-reverse-sort))))

;;   )

(neo/use-package gnus
  :ensure nil  ;; built-in
  :custom
  (gnus-select-method
   '(nnmaildir "Gmail"
               (directory "~/Mail/gmail")
               (get-new-mail nil)))
  (message-send-mail-function 'smtpmail-send-it)
  (smtpmail-default-smtp-server "smtp.gmail.com")
  (gnus-message-archive-method '(nnimap "imap.gmail.com"))
   (gnus-message-archive-group "[Gmail]/Sent Mail")
  (gnus-posting-styles
   '(((header "to" "address@outlook.com")
      (address "address@outlook.com"))
     ((header "to" "address@gmail.com")
      (address "address@gmail.com"))))

  ;; (nnml-directory "~/gmail")
  ;; (message-directory "~/gmail")

  ;; Automatically fetch new headers on startup
  (gnus-read-active-file t)

  ;; Don't archive sent messages
  (gnus-message-archive-group nil)

  ;; Encrypt to self (for PGP mail)
  (mml-secure-openpgp-encrypt-to-self t)

  ;; Prevent offline agent confusion for NNTP
  (gnus-agent nil)

  ;; Better threading
  (gnus-thread-sort-functions
   '((not gnus-thread-sort-by-date)
     (not gnus-thread-sort-by-number)))

  ;; Avoid filtering out real newsgroups like [Gmail]/Sent
  (gnus-ignored-newsgroups
   "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"(#')]")

  :hook
  ((gnus-summary-mode . my/gnus-summary-keys)
   (message-setup . mml-secure-message-encrypt)))


  ;; ;; Use NNTP via Gmane
  ;; (gnus-select-method
  ;;  '(nntp "news.gmane.io"))

  ;; ;; TODO allows extensions to inject newsgroups if the gnus
  ;; ;; extensions is enabled
  ;; (gnus-default-subscribed-newsgroups
  ;;  '("gmane.emacs.orgmode"
  ;;    "gmane.emacs.announce"
  ;;    "gmane.emacs.gnus.users"
  ;;    "gmane.emacs.gnus.general"))
 
  ;; ;; Automatically fetch new headers on startup
  ;; (gnus-read-active-file t)

  ;; ;; Don't archive sent messages
  ;; (gnus-message-archive-group nil)

  ;; ;; Encrypt to self (for PGP mail)
  ;; (mml-secure-openpgp-encrypt-to-self t)

  ;; ;; Prevent offline agent confusion for NNTP
  ;; (gnus-agent nil)

  ;; ;; Better threading
  ;; (gnus-thread-sort-functions
  ;;  '((not gnus-thread-sort-by-date)
  ;;    (not gnus-thread-sort-by-number)))

  ;; ;; Avoid filtering out real newsgroups like [Gmail]/Sent
  ;; (gnus-ignored-newsgroups
  ;;  "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"(#')]")

  ;; :hook
  ;; ((gnus-summary-mode . my/gnus-summary-keys)
  ;;  (message-setup . mml-secure-message-encrypt)))
  ;; ;; :config
  ;; ;; ;; Custom keybindings for Gnus
  ;; ;; (defun my/gnus-summary-keys ()
  ;; ;;   (local-set-key (kbd "y") #'gmail-archive)
  ;; ;;   (local-set-key (kbd "$") #'gmail-report-spam)))

(defun my/gnus-summary-keys ()
  (local-set-key (kbd "y") #'gmail-archive)
  (local-set-key (kbd "$") #'gmail-report-spam))

(defun gmail-archive ()
  "Archive the current or marked mails.
This moves them into the All Mail folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/All Mail"))

(defun gmail-report-spam ()
  "Report the current or marked mails as spam.
This moves them into the Spam folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/Spam"))


  ;; :custom
  ;; ;; Set gnus-init-file to dynamically load from user's Emacs directory
  ;; (setq gnus-init-file (expand-file-name "gnus.el" user-emacs-directory)))

(neo/use-package all-the-icons-gnus
  :config
  (all-the-icons-gnus-setup))
