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


; Routine to read a single bit.
(define (ReadBit bit lock ack) (Input Input Output Bool)
  (if lock (ReadBitAck bit lock ack)
    (ReadBit bit lock ack))) ; Keep attempting to read until lock is set

(define (ReadBitAck bit lock ack) (Input Input Output Boolean)
  ; Execution order is a bit weird here. Bit must have the same value as before read_bit_unack is called.
  ; Have to be careful with function calls...
  ; With begin the last element is returned.
  (let ((Bool ourBit bit)) ; Since read_bit_unack is called first we can't read inputs after this.
    (Begin (write ack true) (read_bit_unack lock ack) ourBit)))

(define (ReadBitUnack bit lock ack) (Input Output Bool  ())
  (if lock (ReadBitUnack lock ack) ; Keep trying until unlock
    (Set ack false)))  ; Write when false


;; First byte is lowest order byte
(define (ReadByte bit lock ack left value) (Input Input Output Char Char Char)
  (let ((Char bitValue (if (ReadBit bit lock ack) 1 0))
        (Char remaining (- left 1))
        (Char newValue (+ bitValue (* 2 value))))
    (if (= 0 remaining) newValue
      (ReadByte bit lock ack left newValue))))


;; All this does is constantly read bytes
(node Reader
      ;; State variables are declared first with an initial
      ;; value. These may change after each iteration.
      (let ((Char ourByte 0)
	    (Input bit 2)  ; Initial values don't make sense for inputs.
	    (Input lock 3)
	    (Output ack 4 FALSE))
	;; Port numbers below... Final argument is the starting byte.
	(Set ourByte (ReadByte bit lock ack 8 0))))
