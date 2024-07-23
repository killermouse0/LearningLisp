(defsystem "day10"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on (:iterate :trivia)
  :components ((:module "src"
                :components
                ((:file "package")
		 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "day10/tests"))))

(defsystem "day10/tests"
  :author ""
  :license ""
  :depends-on ("day10"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for day10"
  :perform (test-op (op c) (symbol-call :rove :run c)))
