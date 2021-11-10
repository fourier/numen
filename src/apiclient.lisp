(in-package numen)

(defpackage #:numen.apiclient
  (:documentation "A connection to the Frostbite Client API")
  (:use #:cl #:alexandria #:numen.logger)
  (:export start stop))

(in-package #:numen.apiclient)

(defparameter *api-server* "localhost"
  "IP Address of the machine with FrostBite frontend running")

(defparameter *api-port* 10003
  "Port number for the Frostbite API server")

(defparameter *client-channel* nil
  "Channel for communication with API client")

(defun start ()
  (when *client-channel*
      (chanl:send *client-channel* :stop)
      (setf *client-channel* (make-instance 'chanl:channel)))
  (inf "Start ApiClient")
  (chanl:pexec (:name "ApiClient")
    (inf "Start connection loop")
    (loop while (connect-client)
          do
          (sleep 1))
    (setf *client-channel* nil)
    (inf "Stopped ApiClient")))

(defun stop ()
  (inf "Stopping ApiClient")
  (chanl:send *client-channel* :stop :blockp t))

(defun message (msg)
  (chanl:send *client-channel* msg))

(defun connect-client ()
  (dbg "Trying to connect to the API server on port ~a..." *api-port*)
  (handler-case
      (usocket:with-client-socket (socket stream *api-server* *api-port* :timeout 3)
        (dbg "Connected to API server on port ~a" *api-port*)
        (loop for msg = (chanl:recv *client-channel* :blockp nil)
              for c = (read-char-no-hang stream nil :end)
              until (or (eq msg :stop) (eq c :end))
              do
              (dbg "Waiting for the message on a queue")
              (sleep 1)
              ;; TODO implement transmission here
              finally (return (not (eq msg :stop)))))
    (error (e)
      (dbg "Connection error: ~a" e)
      ;; Return T on connection error for connection reattempt
      t)))

      ;;;                 (let ((line (read-until-slash-zero stream)))
;;;                   (when (and line on-received-callback)
;;;                     (funcall on-received-callback line))))
;;;         (error (e) (progn (dbg "Error: ~a" e))))

(defun read-until-slash-zero (&optional (stream *standard-input*))
  "Try to do nonblocking read of the stream until the '\\0' and return a line"
  (loop for c = (read-char-no-hang stream nil nil)
        and p = #\Null then c
        for i from -1
        while c
        collect c into ret
        if (and (char= p #\\) (char= c #\0))
        return (values (subseq (coerce ret 'string) 0 i) t)
        finally return (values (coerce ret 'string) nil)))
                
                   
