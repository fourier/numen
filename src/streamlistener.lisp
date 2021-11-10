#|
  This file is a part of numen project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@protonmail.com>, 2021
|#
(in-package :cl-user)


(defpackage :numen.streamlistener
  (:documentation "A connection to the Frostbite Client streaming service")
  (:use :cl :alexandria :numen.logger)
  (:export start stop))

(in-package :numen.streamlistener)

(defparameter *stream-server* "localhost"
  "IP Address of the machine with FrostBite frontend running")

(defparameter *stream-port* 10001
  "Port number for the Frostbite streaming server")

(defparameter *stopped* t
  "Set this parameter to T to abort the thread execution")

(defun stop ()
  (unless *stopped*
    (inf "Stopping the StreamListener")
    (setf *stopped* t)
    (sleep 1.5)
    (inf "Stopped StreamListener")))

(defun start (&optional (on-received-callback nil))
  (inf "Start StreamingListener")
  (when  *stopped*
    (inf "No previous StreamListener, starting new")
    (bt:make-thread
     (lambda () (do-start on-received-callback))
     :name "StreamListener")))

(defun do-start (on-received-callback)
  (setf *stopped* nil)
  (inf "Starting the connect loop")
  (loop while (not *stopped*)
        do
        (dbg "Trying to connect to the server on port ~a..." *stream-port*)
        (ignore-errors (connect-and-read on-received-callback))
        (sleep 1))
  (setf *stopped* t)
  (inf "Connect loop finished"))

(defun connect-and-read (on-received-callback)
  (usocket:with-client-socket (socket stream *stream-server* *stream-port* :timeout 2)
    (handler-case
        (unwind-protect
            (loop while (not *stopped*)
                  do
                  (usocket:wait-for-input socket :ready-only t :timeout 0.1 )
                  (let ((line (read-until-newline stream)))
                    (when (and line on-received-callback)
                      (funcall on-received-callback line))))
          (usocket:socket-close socket))
      (error (e) (dbg "Error: ~a" e))))
  (dbg "connect-and-read finished"))

(defun read-until-newline (&optional (stream *standard-input*))
  "Try to do nonblocking read of the stream until the newline and return a line"
  ;; TODO: with this design if the connection drops while we are still
  ;; reading from the stream, the message message received so far
  ;; will not be processed.
  ;; Consider using (read-char-no-hang stream nil :eof)
  ;; and handle :eof return value (here and in the calling function)
  (loop for c = (read-char-no-hang stream)
        while (and c (char/= c #\Newline))
        collect c into ret
        finally (return
                 (when ret (coerce ret 'string)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
