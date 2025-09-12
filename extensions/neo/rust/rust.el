(require 'eglot)

(neo/use-package rustic)

(add-to-list 'eglot-server-programs
             '((rust-ts-mode rust-mode rustic-mode) .
               ("/home/mav/.cargo/bin/rust-analyzer" :initializationOptions (:check (:command "clippy")))))
