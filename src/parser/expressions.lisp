;; Copyright (C) 2014 Calvin Beck
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


(defun expression-listp (part)
  "Check for list of expressions."
  (loop for exp in (mapcar #'expressionp part) always exp))

(defun expressionp (blah)
  "Check if something looks like an expression."
  T)

(defmethod initialize-instance :after ((bin-op binary-operation) &key)
  (let* ((chunk (slot-value bin-op 'chunk))
	 (op (car chunk))
	 (arg1 (cadr chunk))
	 (arg2 (caddr chunk)))
    (if (binary-operationp NIL chunk)
	(setf (slot-value bin-op 'operation) op
	      (slot-value bin-op 'arg1) (create-expression arg1)
	      (slot-value bin-op 'arg2) (create-expression arg2))
	(error "Not a valid binary operation."))))

(defun function-callp (part)
  "Checks if a chunk of code looks like a function call."
  (and (listp part) (not (null part))))

(defclass function-call (ast-node)
  ((function-name
    :documentation "Name of the function call -- might be an operation as well."
    :accessor function-name)
   (arguments
    :documentation "Argument list for the function. Each argument is an expression."
    :accessor arguments)))

(defmethod initialize-instance :after ((fun-call function-call) &key)
  (let* ((chunk (chunk fun-call))
         (argument-chunk (cdr chunk))
         (name (car chunk)))
    (if (function-callp chunk)
        (setf (function-name fun-call) name
              (arguments fun-call) (mapcar #'create-expression argument-chunk))
        (error (format NIL "Not a valid function call: ~a" chunk)))))


(defun variable-expressionp (part)
  "Check if something looks like a variable"
  (symbolp part))

(defclass variable-expression ()
  ((variable-name
    :documentation "Identifier for the variable."
    :accessor variable-name)))

(defmethod initialize-instance :after ((var variable-expression) &key)
  (let ((chunk (chunk var)))
       (if (variable-expressionp chunk)
           (setf (variable-name var) chunk)
           (error "Invalid variable expression: ~a" chunk))))

(defun create-expression (exp)
  "Create an expression object of some form."
  (cond
    ((function-callp exp) (make-instance 'function-call :chunk exp))
    (T (error (format NIL "Not a valid expression: ~a" exp)))))

(defun expression-callp (name)
  "Check if this could be a valid function call"
  (or
   (eq '/ name)
   (eq '+ name)
   (eq '- name)
   (eq '* name)
   (eq '= name)
   (eq '!= name)
   (eq '< name)
   (eq '<= name)
   (eq '> name)
   (eq '>= name)))


;; Arithmetic expressions
(defun additionp (exp)
  "Check if this looks like an addition expression."
  (let ((operation (car exp))
        (arguments (cdr exp)))
    (and (eq operation '+) (expression-listp arguments))))

(defun subtractionp (exp)
  "Check if this looks like a subtraction expression."
  (binary-operationp '- exp))

(defun multiplicationp (exp)
  "Check if this looks like a multiplication expression."
  (let ((operation (car exp))
        (arguments (cdr exp)))
    (and (eq operation '*) (expression-listp arguments))))

(defun modulop (exp)
  "Check if this looks like a modulo expression."
  (binary-operationp 'Mod exp))

(defun divisionp (exp)
  "Check if this looks like a division expression."
  (binary-operationp '/ exp))

(defun arithmeticp (exp)
  "Check if something looks like an arithmetic expression."
  (or
   (additionp exp)
   (subtractionp exp)
   (multiplicationp exp)
   (modulop exp)
   (divisionp exp)))

;; Comparison expressions
(defun equalityp (exp)
  "Check if this looks like an equality expression."
  (binary-operationp '= exp))

(defun inequalityp (exp)
  "Check if this looks like an inequality expression."
  (binary-operationp '!= exp))

(defun less-thanp (exp)
  "Check if this looks like a less than expression."
  (binary-operationp '< exp))

(defun less-than-equalsp (exp)
  "Check if this looks like a less than or equal to expression."
  (binary-operationp '<= exp))

(defun variablep (var)
  "Check if something is a variable name"
  (stringp var))  ;; For now just check if it is a string.

(defun literalp (literal)
  "Check if something is a literal."
  (integerp literal)) ;; For now just check that it is a number.