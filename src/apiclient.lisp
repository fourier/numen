(in-package numen)

(defpackage #:numen.apiclient
  (:documentation "A connection to the Frostbite Client API")
  (:use #:cl #:alexandria #:numen.logger #:numen.mailbox)
  (:export init stop send))

(in-package #:numen.apiclient)

(defparameter *api-server* "localhost"
  "IP Address of the machine with FrostBite frontend running")

(defparameter *api-port* 10003
  "Port number for the Frostbite API server")

(defparameter +event-delay+ 2.0
  "Timeout for an event to be received")

(defparameter *input-mailbox* nil
  "Mailbox for incoming messages to be sent to the API server")

(defparameter *thread* nil
  "Thread executing client")

(defparameter *socket* nil)

(defun stop ()
  (when *thread*
    (dbg "Client is running, trying to stop...")
    (send :stop)
    ;; Wait until stopped
    (when (= 1000
             (loop for counter below 1000
                   while *thread*
                   do (sleep 0.01)
                   finally (return counter)))
      (dbg "Error: unable to stop, forcing")
      (bt:destroy-thread *thread*))))
  
(defun init ()
  (inf "Start ApiClient")
  ;; Stop running thread
  (stop)
  ;; Create new mailboxes
  (setf *input-mailbox* (mb-create "API Client Input Mailbox")
        ;; Run the event loop in a thread
        *thread* (bt:make-thread #'event-loop :name "API Client Thread")))

(defun notify-connected()
  (inf "Connected"))

(defun notify-disconnected()
  (inf "Disconnected"))

(defun event-loop ()
  (unwind-protect 
      (loop initially (send :start)
            for evt = (mb-read *input-mailbox* +event-delay+)
            until (eq evt :stop)
            do
            (process-event evt)
            finally
            (inf "Stopped event loop~%"))
    ;; cleanup after thread termination
    (when *socket*
      (usocket:socket-close *socket*))
    (setf *input-mailbox* nil
          *thread* nil
          *socket* nil)
    (inf "Stopped ApiClient thread")))

(defun process-event (evt)
  (case evt
    (:start (connect-client))
    ((nil) (process-timer-event))
    (:wait (sleep +event-delay+))
    (otherwise (dbg "Received ~a~%" evt))))

(defun send (msg)
  (dbg "Sending a message ~a" msg)
  (mb-send *input-mailbox* msg))

(defun connect-client ()
  (dbg "Trying to connect to the API server on port ~a..." *api-port*)
  (handler-case
      (progn
        (setf *socket* 
              (usocket:socket-connect *api-server* *api-port* :timeout 3))
        (inf "Connected to API server on port ~a" *api-port*))
    (error (e)
      (dbg "Unable to connect" e)
      (send :wait)
      (send :start))))

(defun process-timer-event()
  ;; Check connection status
  (dbg "Checking the connection status...")
  (let (should-reconnect 
        (stream (usocket:socket-stream *socket*)))
    (handler-case 
        (let* ((c (read-char-no-hang stream nil :eof)))
          (setf should-reconnect
                (case c
                  (:eof t)
                  ((nil) nil)
                  (otherwise 
                   (unread-char c stream) nil))))
      (error (e)
        (setf should-reconnect t)))
    (when should-reconnect
      (inf "Connection dropped, reconnect")
      (send :start))))

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
