(in-package numen)

(defpackage #:numen.worker
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

(in-package #:numen.worker)

(defclass worker ()
  ((event-delay :initarg :event-delay :initform nil
                :documentation "If defined, the timeout for the waititng queue to get an event.
If timeout occured the timeout processing event would happen")
   (name :initarg :name :initform "Worker thread")
   (thread :initform nil
           :documentation "Worker thread")
   (mailbox :documentation
            "Worker mailbox.
The thread will wait until :stop event occures on this mailbox, using delay event-delay")
   (stop-condition :initform (bt:make-condition-variable :name "worker-cond")
                   :documentation "A condition used to determine if the thread has stopped")
   (lock :initform (bt:make-recursive-lock "worker-lock")
         :documentation "A lock for condition variable stop-condition"))
  (:documentation "A worker class. It creates a thread and a mailbox waiting on the thread.
Additionally provides a method 'send' to send messages to the mailbox."))

;;----------------------------------------------------------------------------
;; Protocol for Worker class
;;----------------------------------------------------------------------------

(defgeneric process-timer-event (worker)
  (:documentation "Callback called when no events in a queue after event-delay seconds.
If event-delay slot is nil, the timer event will never be called"))
   
(defgeneric process-event (worker event)
  (:documentation "Callback called in the worker thread when the SEND call places event in a queue"))

(defgeneric cleanup (worker)
  (:documentation "Called for cleaning up worker state. Derived classes should use around/before/after to add own cleanup routines"))

(defmethod start ((worker worker))
  "Start worker thread"
  ;; Stop running thread
  (stop worker)
  (with-slots (thread mailbox name) worker
    ;; Create new mailbox
    (setf mailbox (mb-create "Worker thread mailbox")
          ;; Run the event loop in a thread
          thread (bt:make-thread
                  (lambda ()
                    (unwind-protect 
                        (event-loop worker)
                      (dbg "Exit thread function")))
                  :name name))))

(defmethod stop ((worker worker))
  "Stops the worker thread"
  (with-slots (thread lock stop-condition) worker
    (when thread
      (dbg "Client is running, trying to stop...")
      (send worker :stop)
      ;; Wait until stopped
      (bt:with-lock-held (lock)
        (bt:condition-wait stop-condition lock :timeout 10))
      (when thread
        (dbg "Error: unable to stop, forcing")
        (bt:destroy-thread thread)))))

(defmethod send ((worker worker) message)
  "Send a message to a worker thread"
  (with-slots (mailbox) worker
    (when mailbox
      (dbg "Sending a message ~a" message)
      (mb-send mailbox message))))

(defmethod event-loop ((worker worker))
  "Worker thread event loop. Running in a worker thread context"
  (with-slots (mailbox thread event-delay stop-condition) worker
    (unwind-protect
        (loop for evt = (mb-read mailbox event-delay)
              until (eq evt :stop)
              if evt do
              (process-event worker evt)
              else do
              (process-timer-event worker)
              end)
      ;; cleanup after thread termination
      (setf mailbox nil
            thread nil)
      (bt:condition-notify stop-condition)
      (cleanup worker)
      (dbg "Stopped event loop"))))

(defmethod cleanup ((worker worker))
  "Default cleanup method. Does nothing")
