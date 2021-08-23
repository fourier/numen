(in-package numen)

(defparameter *logger-channel* (make-instance 'chanl:channel)
  "Channel for logging")

(defun logger-init ()
  (chanl:pexec ()
   (loop for msg = (chanl:recv *logger-channel*)
         until (eq msg :stop)
         do 
         (format t "~a~%" msg))))


(defun logger-stop ()
  (chanl:send *logger-channel* :stop :blockp nil))

(defmacro info(message &rest args)
  (let ((module-name (package-name *package*)))
    `(info-message (concatenate 'string "[" ,module-name "] " ,message) ,@args)))

(defun info-message (message &rest args)
  (let ((msg (apply #'format (append (list nil message) args))))
    (chanl:send *logger-channel* msg :blockp nil))
  (values))
