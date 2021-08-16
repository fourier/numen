#|
  This file is a part of numen project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@protonmail.com>, 2021
|#
(in-package :cl-user)
(defpackage numen-asd
  (:use :cl :asdf))
(in-package :numen-asd)

(defsystem numen
  :version "0.1"
  :author "Alexey Veretennikov"
  :license "MIT"
  :depends-on (alexandria split-sequence cl-ppcre yacc ppath usocket chanl)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "numen")
                 ;; #+lispworks(:file "ui")
                 ;; #-lispworks(:file "cmdline")
                 )))
  :description "Numen is a script server for FrostBite DragonRealms Frontend")
;;  :in-order-to ((test-op (test-op numen-test))))
