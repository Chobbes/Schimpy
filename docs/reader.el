; Routine to read a single bit.
(: read_bit Input -> Input -> Output -> Boolean)
(define (read_bit bit lock ack)
  (if lock (read_bit_ack bit lock ack)
      (read_bit bit lock ack)) ; Keep attempting to read until lock is set
)

(: read_bit_ack Input -> Input -> Output -> Boolean)
(define (read_bit_ack bit lock ack)
  ; Execution order is a bit weird here. Bit must have the same value as before read_bit_unack is called.
  ; Have to be careful with function calls...
  (begin0 (bit) (write ack true) (read_bit_unack lock ack)) ; read_bit_unack is in the tail position.

(: read_bit_unack Input -> Output -> Boolean)
(define (read_bit_unack bit lock ack)
  (if lock (read_bit_unack lock ack) ; Keep trying until unlock
    (write ack false)))  ; Write when false


;; First byte is lowest order byte
(: read_byte Input -> Input -> Output -> Byte -> Byte)
(define (read_byte bit lock ack left)
  (let ((bit_value (if (read_bit bit lock ack) 1 0))
        (remaining (- left 1)))
    (if (eq 0 remaining) bit_value
      (+ bit_value (* 2 (read_byte bit lock ack remaining))))))
       
;; All this does is constantly read bytes
(node reader
      ;; Port numbers below...
      (let ((byte (read_byte 2 3 4 8)))))
