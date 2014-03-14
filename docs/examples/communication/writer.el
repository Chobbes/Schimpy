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

;; We need to be able to reverse a byte so we can send in the proper
;; order more easily.
(define (ByteReverse reversed left byte) (Char Char Char Char)
  (let ((leastBit (mod byte 2))
        (newReversed (+ leastBit (/ reversed 2)))
        (remaining (- left 1)))
    (if (= 0 remaining) newReversed
      (ByteReverse newReversed remaining (/ byte 2)))))


(define (WriteBit bit lock ack value) (Output Output Input Bool ())
  (begin (Set bit) (Set lock) (WriteBitUnlock lock ack)))


(define (WriteBitUnlock lock ack) (Output Input ())
  ;; Once ack is set we can unset the lock. Done transfer.
  (if ack
      (begin (Set lock FALSE) (WriteBitWait ack))
      (WriteBitUnlock lock ack)))


(define (WriteBitWait ack) (Input ())
  ;; Wait until ack is unset
  (if ack (WriteBitWait ack)
    ()))

(define (WriteByte bit lock ack byte left) (Output Output Input Char Char ())
  (let ((bitValue (= 1 (mod byte 2))))
    (if (= 0 left) ()
      (begin (WriteBit bit lock ack bitValue) (WriteByte bit lock ack (/ byte 2) (- left 1))))))
