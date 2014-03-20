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

(defun expressionp (part)
  "Check if a part of a Schimpy program is an expression. Note this only checks syntax."
  (or
    (literalp part)
    (variablep part)
    (and (listp part) (expression-callp (car part)) (expression-listp (cdr part)))))

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

(defun additionp (exp)
  "Check if this looks like an addition expression."
  (let ((operation (car exp))
        (arguments (cdr exp)))
    (and (eq operation '+) (expression-listp arguments))))

(defun subtractionp (exp)
  "Check if this looks like a subtraction expression."
  (let ((operation (car exp))
        (args (cdr exp))
        (rest (cdddr exp)))
    (if (eq operation '-)
        (cond
          ((rest) NIL) ;; Too many arguments
          (T (expression-listp args)))
        ())))

(defun expression-listp (part)
  "Check for list of expressions."
  (loop for exp in (mapcar #'expressionp part) always exp)) 

(defun variablep (var)
  "Check if something is a variable name"
  (stringp var))  ;; For now just check if it is a string.

(defun literalp (literal)
  "Check if something is a literal."
  (numberp literal)) ;; For now just check that it is a number.
