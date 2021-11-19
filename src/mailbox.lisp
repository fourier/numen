#|
  This file is a part of numen project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@protonmail.com>, 2021
|#
(in-package :cl-user)

(defpackage :numen.mailbox
  (:documentation "Mailbox wrapper")
  (:use :cl)
  (:export
   mb-create
   mb-send
   mb-wait
   mb-empty-p
   mb-read))

(in-package numen.mailbox)

#+sbcl (require 'sb-concurrency)

(defun mb-create (name)
  #+sbcl (sb-concurrency:make-mailbox :name name)
  #+lispworks (mp:make-mailbox :name name))

(defun mb-send (mailbox message)
  #+sbcl (sb-concurrency:send-message mailbox message)
  #+lispworks (mp:mailbox-send mailbox message))

(defun mb-read (mailbox &optional timeout)
  ;; => object, flag
  #+sbcl (sb-concurrency:receive-message mailbox :timeout timeout)
  #+lispworks (mp:mailbox-read mailbox nil timeout))

(defun mb-empty-p (mailbox)
  #+sbcl (sb-concurrency:mailbox-empty-p mailbox)
  #+lispworks (mp:mailbox-empty-p mailbox))
