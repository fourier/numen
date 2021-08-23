(defpackage #:numen.streamlistener
  (:documentation "A connection to the Frostbite Client streaming service")
  (:use #:cl #:alexandria))
;;  (:export version-string))

(in-package #:numen.streamlistener)

(defparameter *stream-server* "localhost"
  "IP Address of the machine with FrostBite frontend running")

(defparameter *stream-port* 10001
  "Port number for the Frostbite streaming server")

(defparameter *interrupted* t
  "Set this parameter to T to abort the thread execution")

(defun stop ()
  (unless *interrupted*
    (numen::info "Stopping the StreamListener")
    (setf *interrupted* t)
    (sleep 1.5)
    (numen::info "Stopped StreamingListener")))

(defun start (&optional (on-received-callback nil))
  (numen::info "Start StreamingListener")
  (when  *interrupted*
    (numen::info "No previous StreamListener, starting new")
    (bt:make-thread
     (lambda () (do-start on-received-callback))
     :name "StreamListener")))

(defun do-start (on-received-callback)
  (setf *interrupted* nil)
  (numen::info "Starting the connect loop")
  (loop while (not *interrupted*)
        do
        (numen::info "Trying to connect to the server on port ~a..." *stream-port*)
        (ignore-errors (connect-and-read on-received-callback))
        (sleep 1))
  (setf *interrupted* t)
  (numen::info "Connect loop finished"))

(defun connect-and-read (on-received-callback)
  (usocket:with-client-socket (socket stream *stream-server* *stream-port* :timeout 2)
    (handler-case
        (unwind-protect
            (loop while (not *interrupted*)
                  do
                  (usocket:wait-for-input socket :ready-only t :timeout 0.1 )
                  (let ((line (read-until-newline stream)))
                    (when (and line on-received-callback)
                      (funcall on-received-callback line))))
          (usocket:socket-close socket))
      (error (e) (numen::info "Error: ~a" e))))
  (numen::info "connect-and-read finished"))

(defun read-until-newline (&optional (stream *standard-input*))
  "Try to do nonblocking read of the stream until the newline and return a line"
  (loop for c = (read-char-no-hang stream)
        while (and c (char/= c #\Newline))
        collect c into ret
        finally (return
                 (when ret (coerce ret 'string)))))
