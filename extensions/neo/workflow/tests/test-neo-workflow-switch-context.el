;;; tests/test-neo-workflow-switch-context.el --- Tests for neo/workflow-switch-context -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'neo-workflow-status)
(require 'neo-workflow-db)
(require 'neo-workflow-models)

(describe "neo/workflow-switch-context"
  (it "switches to a selected stack"
    (let ((mock-stacks '((:id 1 :name "stack1" :repository-id 10 :title "Stack 1")
                         (:id 2 :name "stack2" :repository-id 20 :title "Stack 2")))
          (mock-stack-obj (make-neo-stack :name "stack1"))
          (selected-candidate "Stack 1 (stack1)"))
      
      (cl-letf (((symbol-function 'neo-db-get-all-stacks) (lambda () mock-stacks))
                ((symbol-function 'completing-read) (lambda (prompt candidates &rest args) selected-candidate))
                ((symbol-function 'neo-load-stack) (lambda (name repo-id)
                                                     (if (and (string= name "stack1") (= repo-id 10))
                                                         mock-stack-obj
                                                       nil)))
                ((symbol-function 'neo--hack) (lambda (obj) (message "Hacking %s" (neo-stack-name obj)))))
        
        (spy-on 'neo--hack)
        (neo/workflow-switch-context)
        (expect 'neo--hack :to-have-been-called-with mock-stack-obj))))

  (it "does nothing if no stack is selected"
    (cl-letf (((symbol-function 'neo-db-get-all-stacks) (lambda () '()))
              ((symbol-function 'completing-read) (lambda (&rest args) nil)))
      (spy-on 'neo--hack)
      (neo/workflow-switch-context)
      (expect 'neo--hack :not :to-have-been-called)))

  (it "shows error if stack cannot be loaded"
    (let ((mock-stacks '((:id 1 :name "stack1" :repository-id 10 :title "Stack 1"))))
      (cl-letf (((symbol-function 'neo-db-get-all-stacks) (lambda () mock-stacks))
                ((symbol-function 'completing-read) (lambda (&rest args) "Stack 1 (stack1)"))
                ((symbol-function 'neo-load-stack) (lambda (&rest args) nil))
                ((symbol-function 'message) (lambda (&rest args) nil)))
        
        (spy-on 'message)
        (neo/workflow-switch-context)
        (expect 'message :to-have-been-called-with "Failed to load stack %s" "stack1")))))
