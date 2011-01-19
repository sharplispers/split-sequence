;;; -*- Lisp -*-

(defsystem :split-sequence
  :version "1.0"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :components ((:file "split-sequence"))
  :in-order-to ((asdf:test-op (asdf:load-op :split-sequence-tests)))
  :perform (asdf:test-op :after (op c)
             (funcall (intern (symbol-name '#:run!) '#:5am) :split-sequence)))

(defsystem :split-sequence-tests
  :depends-on (:split-sequence :fiveam)
  :components ((:file "tests")))
