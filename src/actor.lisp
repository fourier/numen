(in-package numen)

(defpackage #:numen.actor
  (:documentation "A work thread with a queue")
  (:use #:cl #:alexandria #:numen.logger #:numen.mailbox)
  (:export
   start
   stop
   message-loop
   send
   process-message
   cleanup
   process-timer-message))

(in-package #:numen.actor)

(defclass actor ()
  ((message-delay :initarg :message-delay :initform nil
                :documentation "If defined, the timeout for the waititng queue to get the message.
If timeout occured the timeout processing message would happen")
   (name :initarg :name :initform "Actor thread")
   (thread :initform nil
           :documentation "Actor thread")
   (mailbox :documentation
            "Actor mailbox.
The thread will wait until :stop message occures on this mailbox, using delay message-delay")
   (stop-condition :initform (bt:make-condition-variable :name "actor-cond")
                   :documentation "A condition used to determine if the thread has stopped")
   (lock :initform (bt:make-recursive-lock "actor-lock")
         :documentation "A lock for condition variable stop-condition"))
  (:documentation "An actor class. It creates a thread and a mailbox waiting on the thread.
Additionally provides a method 'send' to send messages to the mailbox."))

;;----------------------------------------------------------------------------
;; Protocol for the Actor class
;;----------------------------------------------------------------------------

(defgeneric process-timer-message (actor)
  (:documentation "Callback called when no messages in a queue after message-delay seconds.
If message-delay slot is nil, the timer message will never be called"))
   
(defgeneric process-message (actor message)
  (:documentation "Callback called in the actor thread when the SEND call places message in a queue"))

(defgeneric cleanup (actor)
  (:documentation "Called for cleaning up actor state. Derived classes should use around/before/after to add own cleanup routines"))

;;----------------------------------------------------------------------------
;; Implementation of the Actor
;;----------------------------------------------------------------------------

(defmethod start ((actor actor))
  "Start actor thread"
  ;; Stop running thread
  (stop actor)
  (with-slots (thread mailbox name) actor
    ;; Create new mailbox
    (setf mailbox (mb-create "Actor thread mailbox")
          ;; Run the message loop in a thread
          thread (bt:make-thread
                  (lambda ()
                    (unwind-protect 
                        (message-loop actor)
                      (dbg "Exit thread function")))
                  :name name))))

(defmethod stop ((actor actor))
  "Stops the actor thread"
  (with-slots (thread lock stop-condition) actor
    (when thread
      (dbg "Client is running, trying to stop...")
      ;; Wait until stopped
      (bt:with-lock-held (lock)
        (send actor :stop)
        (bt:condition-wait stop-condition lock :timeout 10))
      (when thread
        (dbg "Error: unable to stop, forcing")
        (bt:destroy-thread thread)))))

(defmethod send ((actor actor) message)
  "Send a message to an actor thread"
  (with-slots (mailbox) actor
    (when mailbox
      (dbg "Sending a message ~a" message)
      (mb-send mailbox message))))

(defmethod message-loop ((actor actor))
  "Actor thread message loop. Running in an actor thread context"
  (with-slots (mailbox thread message-delay stop-condition) actor
    (unwind-protect
        (loop for evt = (mb-read mailbox message-delay)
              until (eq evt :stop)
              if evt do
              (process-message actor evt)
              else do
              (process-timer-message actor)
              end)
      ;; cleanup after thread termination
      (setf mailbox nil
            thread nil)
      (bt:condition-notify stop-condition)
      (cleanup actor)
      (dbg "Stopped message loop"))))

;;----------------------------------------------------------------------------
;; Default implementations of generic functions
;;----------------------------------------------------------------------------

(defmethod cleanup ((actor actor))
  "Default cleanup method. Does nothing")

(defmethod process-message ((actor actor) evt)
  "Default message method. Just logs the message"
  (dbg "Ignoring message ~a" evt))

(defmethod process-timer-message ((actor actor))
  "Default timer message. Does nothing")

