#|
  This file is a part of numen project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@protonmail.com>, 2021
|#

(in-package :cl-user)
(defpackage :numen.proto
  (:use :cl))

(in-package :numen.proto)

;;----------------------------------------------------------------------------
;; Transaction class
;;----------------------------------------------------------------------------

(defclass transaction ()
  ((name :initarg :name :reader transaction-name)))

;;----------------------------------------------------------------------------
;; Protocol
;;----------------------------------------------------------------------------

(defgeneric transaction-request (trans)
  (:documentation "Encode the transaction and create the byte array"))

(defgeneric transaction-response (trans bytes)
  (:documentation "Decode the byte array and fill in the fields in the transaction"))

