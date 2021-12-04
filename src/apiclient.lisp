(in-package numen)

(defpackage #:numen.apiclient
  (:documentation "A connection to the Frostbite Client API")
  (:use #:cl #:alexandria #:numen.logger #:numen.mailbox)
  (:import-from #:numen.actor start stop send)
  (:export start stop send create-api-client))

(in-package #:numen.apiclient)

(defparameter *api-server* "localhost"
  "IP Address of the machine with FrostBite frontend running")

(defparameter *api-port* 10003
  "Port number for the Frostbite API server")

(defparameter +event-delay+ 2.0
  "Timeout for an event to be received")

(defparameter *socket* nil)

(defclass api-client (numen.actor::actor)
  ((port :initarg :port :initform *api-port*
         :documentation "Frostbite API server port to connect to")
   (host :initarg :host :initform *api-server*
         :documentation "Frostbite API server hostname")
   (socket :initform nil
           :documentation "TCP client socket")))
   

(defun create-api-client ()
  (make-instance 'api-client :event-delay +event-delay+
                 :name "API Client"))
  
(defmethod numen.actor:start :before ((self api-client))
  (inf "Start ApiClient"))

(defmethod numen.actor:start :after ((self api-client))
  (numen.actor:send self :start))

(defmethod numen.actor:event-loop :after ((self api-client))
  (inf "Stopped ApiClient thread"))

(defmethod numen.actor:process-event ((self api-client) evt)
  (case evt
    (:start (connect-client self))
    (:wait (sleep +event-delay+))
    (otherwise (dbg "Received ~a~%" evt))))

(defmethod connect-client ((self api-client))
  (with-slots (socket host port) self
    (dbg "Trying to connect to the API server on port ~a..." port)
    (handler-case
        (progn
          (setf socket
                (usocket:socket-connect host port :timeout 3))
          (inf "Connected to API server on port ~a" port))
      (error (e)
        (dbg "Unable to connect" e)))))

(defmethod numen.actor:process-timer-event ((self api-client))
  (dbg "Checking the connection status...")
  (with-slots (socket) self
    (let ((should-reconnect (null socket)))
      (unless should-reconnect
        (let ((stream (usocket:socket-stream socket)))
          (handler-case 
              (let* ((c (read-char-no-hang stream nil :eof)))
                (setf should-reconnect
                      (case c
                        (:eof t)
                        ((nil) nil)
                        (otherwise 
                         (unread-char c stream) nil))))
            (error (e)
              (setf should-reconnect t)))))
      (when should-reconnect
        (inf "Connection dropped, reconnect")
        (numen.actor:send self :wait)
        (numen.actor:send self :start)))))

(defmethod numen.actor:cleanup ((self api-client))
  ;; Close and clean the socket slot
  (with-slots (socket) self
    (when socket
      (usocket:socket-close socket)
      (setf socket nil))))

(defun notify-connected()
  (inf "Connected"))

(defun notify-disconnected()
  (inf "Disconnected"))

(defun read-until-slash-zero (&optional (stream *standard-input*))
  "Try to do nonblocking read of the stream until the '\\0' and return a line"
  (loop for c = (read-char-no-hang stream nil nil)
        and p = #\Null then c
        for i from -1
        while c
        collect c into ret
        if (and (char= p #\\) (char= c #\0))
        return (values (subseq (coerce ret 'string) 0 i) t)
        finally (return (values (coerce ret 'string) nil))))
