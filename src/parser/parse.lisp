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


(defun grab-sexps (file)
  "Function grabs a list of all of the sexps within a file."
  (let ((*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) :preserve)
    (let ((sexp (read file nil)))
      (if sexp (cons sexp (grab-sexps file)) nil))))

(defun funcp (part)
  "Check if a part of a Schimpy program is a function."
  (eq (car part) '|define|))

(defun func-name (func)
  "Extract the function name for a Schimpy function as a symbol."
  (caadr func))

(defun all-func-names (program)
  "Gets a list containing all of the function names."
  (let ((functions (remove-if-not #'funcp program)))
    (mapcar #'func-name functions)))

(defun nodep (part)
  "Check if a part of a Schimpy program is a node."
  (eq (car part) '|node|))

(defun node-name (node)
  "Extract the node name for a Schimpy node as a symbol"
  (cadr node))

(defun all-node-names (program)
  "Gets a list containing all of the node names in a Schimpy program."
  (let ((nodes (remove-if-not #'nodep program)))
    (mapcar #'node-name nodes)))

(defun all-global-symbols (program)
  "Fetch a list containing all of the symbols in the global scope (i.e., all of the functions and nodes)."
  (append (all-func-names program) (all-node-names program)))

(defun list-dupes (items)
  "Get a list of duplicate items in the list."
  (let ((head (car items))
        (rest (cdr items)))
    (cond
      ((null items) ())
      ((member head rest) (cons head (list-dupes (remove-if (lambda (x) (eq head x)) rest))))
      (T (list-dupes rest)))))

(defun symbol-entry (chunk)
  "Turns a chunk of code into a symbol table entry."
  (cond
    ((funcp chunk) (cons (func-name chunk) chunk))
    ((nodep chunk) (cons (node-name chunk) chunk))
    (T ())))

(defun symbol-table (program)
  "Generates a symbol table. A symbol table contains a pair of the name, and the definition of the object."
  (mapcar #'symbol-entry program))

(defun check-program (program)
  "Check program for errors. NIL if no errors are found, and an error string otherwise."
  (let ((duplicates (list-dupes (all-global-symbols program)))
	(nodes (remove-if-not #'nodep program)))
    (cond
      (duplicates (format NIL "Error -- duplicate symbols: ~a~%" duplicates))
      ((null nodes) "Error -- no node!"))))

(defun check-function (func)
  "Check a function for errors. NIL if no errors are found, and an error string otherwise."
  ())
