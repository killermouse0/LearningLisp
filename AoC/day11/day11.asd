(defsystem "day11"
  :version "0.1"
  :author "Stephane Kattoor"
  :mailto "stephane.kattoor@sakana.fr"
  :license ""
  :depends-on ("trivia")
  :components ((:module "src"
                :components
                ((:file "package")
		 (:file "main")
		 (:file "monkey"))))
  :description ""
  :in-order-to ((test-op (test-op "day11/tests"))))

(defsystem "day11/tests"
  :author "Stephane Kattoor"
  :license ""
  :depends-on ("day11"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for day11"
  :perform (test-op (op c) (symbol-call :rove :run c)))
