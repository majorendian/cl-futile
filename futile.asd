
(defpackage futile-system
  (:use :common-lisp :asdf))

(in-package :futile-system)

(defsystem "futile"
  :description "futile: Functional Utility Extensions for Steel Bank Common Lisp"
  :version "0.1"
  :author "Ernest Deák <gordon.zar@gmail.com"
  :license "BSD 2-Clause License"
  :components ((:file "futile")))
