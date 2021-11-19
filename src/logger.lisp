#|
  This file is a part of numen project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@protonmail.com>, 2021
|#
(in-package :cl-user)

(defpackage :numen.logger
  (:documentation "Logger package")
  (:use :cl :alexandria :numen.mailbox)
  (:export
   logger-init
   logger-initialized-p
   logger-stop
   logger-set-level
   inf
   dbg
   err
   *logger-level*))

(in-package numen.logger)

(defparameter *logger-mailbox* nil
  "Channel for logging")

(defparameter *logger-level* 3
  "Level for logging.
0 means nothing,
1 only errors,
2 info and errors,
3 everything")

(defmacro msg(message lvl accept-lvl-num &rest args)
  (let ((module-name (package-name *package*)))
    `(if (> numen.logger::*logger-level* ,accept-lvl-num)
         (info-message ,(concatenate 'string "[" module-name "] " lvl ": " message) ,@args)
         (values))))

(defmacro inf(message &rest args)
  `(msg ,message "I" 1 ,@args))

(defmacro dbg(message &rest args)
  `(msg ,message "D" 2 ,@args))

(defmacro err(message &rest args)
  `(msg ,message "E" 0 ,@args))

(defun logger-init ()
  ;; Stop running thread
  (when *logger-mailbox*
    (mb-send *logger-mailbox* :stop))
  ;; Wait until stopped
  (when (= 1000
         (loop for counter below 1000
               while *logger-mailbox*
               do (sleep 0.01)
               finally (return counter)))
    (format t "Error: unable to stop *logger-mailbox*, forcing it"))
  ;; Create a new instance
  (setf *logger-mailbox* (mb-create "Logger mailbox"))
  ;; Run the logger loop in a thread
  (bt:make-thread #'logger-main :name "Logger thread"))

(defun logger-main ()
   (loop while *logger-mailbox* 
         for msg = (mb-read *logger-mailbox*)
         until (eq msg :stop)
         do 
         (format t "~a~%" msg))
   (setf *logger-mailbox* nil))

(defun logger-initialized-p ()
  (not (eq *logger-mailbox* nil)))
   
(defun logger-stop ()
  (mb-send *logger-mailbox* :stop))

(defun logger-set-level (level)
  (setf *logger-level* level))

(defun info-message (message &rest args)
  (let ((msg (apply #'format (append (list nil message) args))))
    (when (logger-initialized-p) (mb-send *logger-mailbox* msg)))
  (values))
