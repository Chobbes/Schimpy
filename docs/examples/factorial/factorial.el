;; Attempt tail call translation...
;; Calling this should look something like:
;;
;; (fact 3 1)
;; = (fact 2 3)
;; = (fact 1 6)
;; = (fact 0 6)
;; = 6
(define (fact i n)
  (if (eq 0 i)
      n
      (fact (- i 1) (* i n))))
