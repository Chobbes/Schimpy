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
(: read_bit Input -> Input -> Output -> Boolean)
(define (read_bit bit lock ack)
  (if lock (read_bit_ack bit lock ack)
      (read_bit bit lock ack))) ; Keep attempting to read until lock is set

(: read_bit_ack Input -> Input -> Output -> Boolean)
(define (read_bit_ack bit lock ack)
  ; Execution order is a bit weird here. Bit must have the same value as before read_bit_unack is called.
  ; Have to be careful with function calls...
  ; With begin bit is returned.
  (let ((Bool our_bit bit))
    (begin (write ack true) (read_bit_unack lock ack) bit)))

(: read_bit_unack Input -> Output -> Boolean -> ())
(define (read_bit_unack bit lock ack)
  (if lock (read_bit_unack lock ack) ; Keep trying until unlock
    (write ack false)))  ; Write when false


;; First byte is lowest order byte
(: read_byte Input -> Input -> Output -> Byte -> Byte -> Byte)
(define (read_byte bit lock ack left value)
  (let ((bit_value (if (read_bit bit lock ack) 1 0))
        (remaining (- left 1))
        (new_value (+ bit_value (*2 value))))
    (if (eq 0 remaining) new_value
      (read_byte bit lock ack left new_value))))


;; All this does is constantly read bytes
(node reader
      ;; State variables are declared first with an initial
      ;; value. These may change after each iteration.
      (((byte our_byte) 0)
       ((input bit 2))  ; Initial values don't make sense for inputs.
       ((input lock 3))
       ((output ack 4) false))

      ;; Port numbers below... Final argument is the starting byte.
      (set our_byte (read_byte bit lock ack 8 0)))
