#|
  This file is a part of numen project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@protonmail.com>, 2021
|#

(in-package :cl-user)
(defpackage :numen
  (:use :cl :alexandria :split-sequence
   #+:lispworks :capi)
  ;;  (:export main #+lispworks main-ui)
  )


