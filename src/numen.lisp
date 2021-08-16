#|
  This file is a part of numen project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@protonmail.com>, 2021
|#
(in-package :numen)

;; Implementation
;;

(defparameter *version* "0.1"
  "The software version to be used in UI and in help")

(defparameter *float-scanner*
  (ppcre:create-scanner "-?[0-9]+([.][0-9]+([Ee][0-9]+)?)")
  "Floating point numbers scanner (including scientific format")

(defparameter *int-scanner*
  (ppcre:create-scanner "-?[0-9]+")
  "Integers scanner (including negative)")

(defparameter *hex-scanner*
  (ppcre:create-scanner "0[xX][0-9a-fA-F]+")
  "Numbers in hex format scanner")

(defparameter *quoted-string-scanner*
  (ppcre:create-scanner "\"(?:[^\"\\\\]|\\\\.)*\"")
  "String in quotes (including escaped quotation characters")

(defparameter *variable-scanner*
  (ppcre:create-scanner "[a-zA-Z_][a-zA-Z0-9]*")
  "Variable-like name, starting with letter or _ and continuing
with letters or numbers")                               

(defun read-windows-line (in)
  "Read the line from the stream.
Return the line without the trailing newline characters"
  (when-let (line (read-line in nil))
    (string-right-trim '(#\Newline #\Return) line)))

(defun parse-and-trim (line)
  "Return a list of words from the string line"
  (split-sequence:split-sequence #\Space (string-trim '(#\Tab #\Space) line)))


(macrolet ((def-is-fun (type scanner-var)
             "Create a is- function (which receives a string as an argument).
The TYPE and the regexp SCANNER-VAR used to create a function name and a
matcher correspondingly"
             (let ((fun-name (intern (string-upcase (concatenate 'string "is-" (symbol-name type))))))
               `(progn
                  (defun ,fun-name (token)
                    (multiple-value-bind (start end)
                        (ppcre:scan ,scanner-var token)
                      (and start end
                           (= (- end start) (length token)))))))))
  (def-is-fun float *float-scanner*)
  (def-is-fun int   *int-scanner*)
  (def-is-fun hex   *hex-scanner*))
  
    
(defun tokenize-string (line)
  "Tokenize the given line (without newline character at the end).
This function produces the list of tokens:
- float number
- integers
- symbols if any of them in the symbols list
- strings otherwise"
  (mapcar (lambda (token)
            (cond ((or (is-float token)
                       (is-int token))
                   (read-from-string token))
                  ((member token '("numen" "map" "version")  :test #'string=)
                   (intern (string-upcase token) :numen))
                  (t token)))
          (parse-and-trim line)))


(defun numen-list-lexer (tokens)
  "Takes the list of tokens and returns a closure lexer.
The closure will take no arguments and on each call will return 2 values (values terminal value)
Here the terminal will be the symbol and value the actual value (if applicable) or the symbol again"
  #'(lambda ()
      (let ((value (pop tokens)))
        (if (null value)
            ;; terminator
            (values nil nil)
            (let ((terminal
                   ;; first check on hex as the hex is a subset of a string
                    (cond ((and (stringp value) (is-hex value)) 'hex) 
                          ((stringp value) 'string)
                          ;; integer strings are subsets of float strinngs,
                          ;; so check them first
                          ((integerp value) 'integer)
                          ((floatp value) 'float)
                          ;; value of symbol is the symbol itself
                          ((symbolp value) value)
                          (t (error "Unexpected value ~S" value)))))
              (values terminal value))))))


(defun numen-stream-lexer (in-stream)
  "The main lexer function for Numen file format."
  (loop with result = nil
        for line = (read-windows-line in-stream)
        while line
        for tokens = (tokenize-string line)
        do
           (dolist (tok tokens)
             (push tok result))
           (push 'newline result)
        finally
           (return (nreverse result))))

