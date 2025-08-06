(defun neo--jinx-include-p (start)
  (save-excursion
    (goto-char start)
    (when (looking-at
           "include\\s-*\\([\"<][^\">]+[\">]\\)")
      (match-end 0))))

(with-eval-after-load 'jinx
  (dolist (hook '(c-mode-hook c++-mode-hook c-ts-mode-hook c++-ts-mode-hook))
    (add-hook hook
              (lambda ()
		(make-local-variable 'jinx--predicates)
                (add-to-list 'jinx--predicates #'neo--jinx-include-p)))))

;;; this is also in lsp.el should probably be here on in a bazel extension
(defun neo--bazel-clangd-path ()
  "Build `//:clangd` via Bazel and return the full path to the resulting binary."
  ;; (let* ((project-root (or (locate-dominating-file default-directory "WORKSPACE.bazel")
  ;;                          (locate-dominating-file default-directory "MODULE.bazel"))))
  ;; TODO compute project-root from project.el
  (let ((project-root "~/Projects/uno"))
    (unless project-root
      (error "Could not find WORKSPACE.bazel or MODULE.bazel"))
    (let ((default-directory project-root))
      (let* ((build-result (shell-command-to-string "bazel build //:clangd 2>&1"))
             (_ (unless (string-match "Build completed successfully" build-result)
                  (error "Failed to build //:clangd: %s" build-result)))
             (bin-dir (string-trim (shell-command-to-string "bazel info bazel-bin  2>/dev/null")))
             (clangd-path (expand-file-name "clangd" bin-dir)))
        (unless (file-executable-p clangd-path)
          (error "clangd not found at expected location: %s" clangd-path))
        clangd-path))))

(defvar neo--eglot-c++-configuration
  ;; TODO: compile-commands-dir should come from project.el
  `((c++-mode c++-ts-mode) . ,(lambda (_interactive)
                                ;; (let ((default-directory (or (project-root (project-current))
                                ;;                               default-directory)))
				  (let ((default-directory "/home/mvitale/Projects/uno"))
                                    (list (neo--bazel-clangd-path)
                                          (concat "--compile-commands-dir=" default-directory)
                                          "-j=4"
                                          "--malloc-trim"
                                          "--log=info"
                                          "--background-index"
                                          "--clang-tidy"
                                          "--completion-style=detailed"
                                          "--pch-storage=memory"
                                          "--header-insertion=iwyu"
                                          "--header-insertion-decorators=0"
                                          "--pretty")))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs neo--eglot-c++-configuration))


(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
