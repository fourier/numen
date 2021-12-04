(in-package numen)

(defpackage #:numen.actor
  (:documentation "A work thread with a queue")
  (:use #:cl #:alexandria #:numen.logger #:numen.mailbox)
  (:export
   start
   stop
   event-loop
   send
   process-event
   cleanup
   process-timer-event))

(in-package #:numen.actor)

(defclass actor ()
  ((event-delay :initarg :event-delay :initform nil
                :documentation "If defined, the timeout for the waititng queue to get an event.
If timeout occured the timeout processing event would happen")
   (name :initarg :name :initform "Actor thread")
   (thread :initform nil
           :documentation "Actor thread")
   (mailbox :documentation
            "Actor mailbox.
The thread will wait until :stop event occures on this mailbox, using delay event-delay")
   (stop-condition :initform (bt:make-condition-variable :name "actor-cond")
                   :documentation "A condition used to determine if the thread has stopped")
   (lock :initform (bt:make-recursive-lock "actor-lock")
         :documentation "A lock for condition variable stop-condition"))
  (:documentation "An actor class. It creates a thread and a mailbox waiting on the thread.
Additionally provides a method 'send' to send messages to the mailbox."))

;;----------------------------------------------------------------------------
;; Protocol for Actor class
;;----------------------------------------------------------------------------

(defgeneric process-timer-event (actor)
  (:documentation "Callback called when no events in a queue after event-delay seconds.
If event-delay slot is nil, the timer event will never be called"))
   
(defgeneric process-event (actor event)
  (:documentation "Callback called in the actor thread when the SEND call places event in a queue"))

(defgeneric cleanup (actor)
  (:documentation "Called for cleaning up actor state. Derived classes should use around/before/after to add own cleanup routines"))

(defmethod start ((actor actor))
  "Start actor thread"
  ;; Stop running thread
  (stop actor)
  (with-slots (thread mailbox name) actor
    ;; Create new mailbox
    (setf mailbox (mb-create "Actor thread mailbox")
          ;; Run the event loop in a thread
          thread (bt:make-thread
                  (lambda ()
                    (unwind-protect 
                        (event-loop actor)
                      (dbg "Exit thread function")))
                  :name name))))

(defmethod stop ((actor actor))
  "Stops the actor thread"
  (with-slots (thread lock stop-condition) actor
    (when thread
      (dbg "Client is running, trying to stop...")
      (send actor :stop)
      ;; Wait until stopped
      (bt:with-lock-held (lock)
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

(defmethod event-loop ((actor actor))
  "Actor thread event loop. Running in an actor thread context"
  (with-slots (mailbox thread event-delay stop-condition) actor
    (unwind-protect
        (loop for evt = (mb-read mailbox event-delay)
              until (eq evt :stop)
              if evt do
              (process-event actor evt)
              else do
              (process-timer-event actor)
              end)
      ;; cleanup after thread termination
      (setf mailbox nil
            thread nil)
      (bt:condition-notify stop-condition)
      (cleanup actor)
      (dbg "Stopped event loop"))))

(defmethod cleanup ((actor actor))
  "Default cleanup method. Does nothing")

(defmethod process-event ((actor actor) evt)
  "Default event method. Just logs the event"
  (dbg "Ignoring event ~a" evt))

(defmethod process-timer-event ((actor actor))
  "Default timer event. Does nothing")

