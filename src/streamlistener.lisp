(defpackage #:numen.streamlistener
  (:documentation "A connection to the Frostbite Client streaming service")
  (:use #:cl))
;;  (:export version-string))

(in-package #:numen.streamlistener)

(defparameter *stream-server* "localhost"
  "IP Address of the machine with FrostBite frontend running")

(defparameter *stream-port* 10001
  "Port number for the Frostbite streaming server")

(defparameter *interrupted* nil
  "Set this parameter to T to abort the thread execution")

(defparameter *current-thread* nil
  "Thread where the stream listener is executing")

(defparameter *current-socket* nil
  "Currently active client socket")

(defun stop ()
  (setf *interrupted* t)
  (sleep 1.5)
  (when (and *current-thread* (chanl:thread-alive-p *current-thread*))
    ;; we are waiting on a socket, so let's just kill the thread
    (numen::info "Thread not stopped, killing it")
    (chanl:kill *current-thread*)
    (setf *current-thread* nil)
    (when *current-socket*
      (numen::info "Close the socket")      
      (ignore-errors (usocket:socket-close *current-socket*))
      (setf *current-socket* nil))))

(defun start (&optional (on-received-callback nil))
  (chanl:pcall (lambda () (do-start on-received-callback))
               :name "streamlistener"))

(defun do-start (on-received-callback)
  (setf *interrupted* nil
        *current-thread* (chanl:current-thread))
  (numen::info "Starting the connect loop")
  (loop while (not *interrupted*)
        do
        (connect-and-read on-received-callback)
        (sleep 1)))  

(defun connect-and-read (on-received-callback)
  (ignore-errors
    (handler-bind ((error (lambda (e) (numen::info "Error: ~a" e))))
      (let* ((socket (usocket:socket-connect *stream-server* *stream-port*))
             (stream (usocket:socket-stream socket)))
        (setf *current-socket* socket)
        (handler-bind ((error (lambda (e)
                                (declare (ignore e))
                                (usocket:socket-close socket))))
          (loop while (not *interrupted*)
                for line = (read-line stream)
                when on-received-callback
                do (funcall on-received-callback line))
          (usocket:socket-close socket))))))
