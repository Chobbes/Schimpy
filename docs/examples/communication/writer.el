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
(: byte_reverse Byte -> Byte -> Byte -> Byte)
(define (byte_reverse reversed left byte)
  (let ((least_bit (mod byte 2))
        (new_reversed (+ least_bit (/ reversed 2)))
        (remaining (- left 1)))
    (if (eq 0 remaining) new_reversed
      (byte_reverse new_reversed remaining (/ byte 2)))))

(: write_bit Output -> Output -> Input -> Boolean -> ())
(define (write_bit bit lock ack value)
  (begin (write bit) (write lock) (write_bit_unlock lock ack)))

(: write_bit_unlock Output -> Input -> ())
(define (write_bit_unlock lock ack)
  ;; Once ack is set we can unset the lock. Done transfer.
  (if ack
      (begin (write lock false) (write_bit_wait ack))
      (write_bit_unlock lock ack)))

(: write_bit_wait Input -> ())
(define (write_bit_wait ack)
  ;; Wait until ack is unset
  (if ack (write_bit_wait ack)
    ()))

(: write_byte Output -> Output -> Input -> Byte -> Byte -> ())
(define (write_byte bit lock ack byte left)
  (let ((bit_value (eq 1 (mod byte 2))))
    (if (eq 0 left) ()
      (begin (write_bit bit lock ack bit_value) (write_byte bit lock ack (/ byte 2) (- left 1))))))
