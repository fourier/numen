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
  :depends-on (alexandria
               split-sequence
               cl-ppcre
               yacc
               ppath
               usocket
               chanl
               do-urlencode)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "logger")
                 (:file "streamlistener")
;;                 (:file "apiclient")
                 (:file "numen"))))
  :description "Numen is a script server for FrostBite DragonRealms Frontend")
;;  :in-order-to ((test-op (test-op numen-test))))
