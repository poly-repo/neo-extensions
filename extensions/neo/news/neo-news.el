;;; -*- lexical-binding: t -*-

;;; This is news, a NEO extension
;;;
;;; Mail and news in Emacs

(require 'auth-source)

(defgroup neo-news nil
  "Customization options for the news extension."
  :group 'neo-extensions)

(defcustom neo/news-imap-server-name "gmail"
  "Server name passed to `nnimap' in `gnus-select-method'."
  :type 'string
  :group 'neo-news)

(defcustom neo/news-imap-address "imap.gmail.com"
  "IMAP host used for `gnus-select-method'."
  :type 'string
  :group 'neo-news)

(defcustom neo/news-imap-port 993
  "IMAP port used for `gnus-select-method'."
  :type 'integer
  :group 'neo-news)

(defcustom neo/news-smtp-server "smtp.gmail.com"
  "SMTP host used when sending mail from Gnus."
  :type 'string
  :group 'neo-news)

(defcustom neo/news-smtp-service 587
  "SMTP port used when sending mail from Gnus."
  :type 'integer
  :group 'neo-news)

(defcustom neo/news-openpgp-auth-host "openpgp"
  "Auth-source host used to look up the OpenPGP signer key."
  :type 'string
  :group 'neo-news)

(defcustom neo/news-openpgp-auth-user nil
  "Auth-source user used to look up the OpenPGP signer key.
When nil, fall back to `user-mail-address'."
  :type '(choice (const :tag "Use user-mail-address" nil)
                 string)
  :group 'neo-news)

(defcustom neo/news-secondary-select-methods
  '((nnhackernews "")
    (nnreddit ""))
  "Secondary Gnus backends to expose alongside the primary mail backend."
  :type '(repeat sexp)
  :group 'neo-news)

(defun neo--news-openpgp-signer ()
  "Return the OpenPGP signer key ID from `auth-source'."
  (auth-source-pick-first-password
   :host neo/news-openpgp-auth-host
   :user (or neo/news-openpgp-auth-user user-mail-address)))

(defun neo--news-gmail-group (suffix)
  "Return a Gmail IMAP group name ending in SUFFIX."
  (format "nnimap+%s:[Gmail]/%s" neo/news-imap-address suffix))

(defun neo/news-archive ()
  "Archive the current or marked messages."
  (interactive)
  (gnus-summary-move-article nil (neo--news-gmail-group "All Mail")))

(defun neo/news-report-spam ()
  "Report the current or marked messages as spam."
  (interactive)
  (gnus-summary-move-article nil (neo--news-gmail-group "Spam")))

(defun neo/news-summary-keys ()
  "Install summary-mode key bindings for Gnus."
  (local-set-key (kbd "y") #'neo/news-archive)
  (local-set-key (kbd "$") #'neo/news-report-spam))

(defun neo/news-sort-summary-by-date ()
  "Sort the current Gnus summary buffer by descending date."
  (gnus-summary-sort-by-date)
  (gnus-summary-reverse-sort))

(defun neo--news-configure-openpgp ()
  "Configure OpenPGP signing for message buffers from `auth-source'."
  (let ((signer (neo--news-openpgp-signer)))
    (when signer
      (setq mml-secure-openpgp-signers (list signer)))))

(defun neo--news-configure-thread-tree ()
  "Configure a denser Gnus thread tree in graphical frames."
  (when (display-graphic-p)
    (setq gnus-sum-thread-tree-indent "  "
          gnus-sum-thread-tree-root ""
          gnus-sum-thread-tree-false-root ""
          gnus-sum-thread-tree-single-indent ""
          gnus-sum-thread-tree-vertical "│"
          gnus-sum-thread-tree-leaf-with-other "├─► "
          gnus-sum-thread-tree-single-leaf "╰─► ")))

(neo/use-package gnus
  :ensure nil ;; Gnus is built into Emacs, so we don't need to install it separately
  :custom
  (gnus-init-file (expand-file-name "gnus.el" user-emacs-directory))
  (gnus-select-method
   `(nnimap ,neo/news-imap-server-name
            (nnimap-address ,neo/news-imap-address)
            (nnimap-server-port ,neo/news-imap-port)
            (nnimap-stream ssl)))
  (smtpmail-smtp-server neo/news-smtp-server)
  (smtpmail-smtp-service neo/news-smtp-service)
  (message-send-mail-function 'smtpmail-send-it)
  (gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
  (gnus-agent nil)
  (gnus-secondary-select-methods neo/news-secondary-select-methods)
  (gnus-message-archive-group nil)
  (mml-secure-openpgp-encrypt-to-self t)
  (gnus-summary-display-arrow t)
  (gnus-summary-line-format
   (concat
    "%0{%U%R%z%}"
    "%3{│%}" "%1{%d%}" "%3{│%}"
    "  "
    "%4{%-20,20f%}"
    "  "
    "%3{│%}"
    " "
    "%1{%B%}"
    "%s\n"))
  (gnus-thread-sort-functions
   '((not gnus-thread-sort-by-date)
     (not gnus-thread-sort-by-number)))
  (gnus-article-sort-functions
   '((not gnus-article-sort-by-date)))
  (gnus-parameters
   '((".*"
      (gnus-thread-sort-functions
       '((not gnus-thread-sort-by-date)))
      (gnus-article-sort-functions
       '((not gnus-article-sort-by-date))))))
  :hook
  ((message-setup . mml-secure-message-encrypt)
   (gnus-summary-mode . neo/news-summary-keys)
   (gnus-summary-mode . neo/news-sort-summary-by-date))
  :config
  (neo--news-configure-openpgp)
  (neo--news-configure-thread-tree))

(neo/use-package all-the-icons-gnus
  :after gnus
  :config
  (all-the-icons-gnus-setup))

(neo/use-package nnhackernews
  :after gnus)

(neo/use-package nnreddit
  :after gnus)

;;; Note, no (provide 'neo-news) here, extensions are loaded not required.
