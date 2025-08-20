(defun neo/eglot-python-configuration ()
  '(((python-base-mode) . ("pyright-langserver" "--stdio"))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs neo/eglot-python-configuration))

