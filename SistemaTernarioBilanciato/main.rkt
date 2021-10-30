#lang racket

;;; Recursive conversion from a number rapresented in the ternary balanced 3
;;; standard to natural base 10.
;;; Basically the conversion remains the same as a normal baseN -> base10
;;; conversion, in fact multiplications are used, but in this case there are no
;;; numbers only sings: '-' = -1, '.' = 0, '+' = +1.
(define balanced3->base10
  (lambda (number) (let ([len (string-length number)])
                     (cond
                       ;; Would have used unless but it returns void and I can't
                       ;; add void to a number unfortunately.
                       [(zero? len) 0]
                       [else
                         ;; This is the hot part of the code.
                         ;; Basically given idx as the current sign, the cond
                         ;; expression decides which number is to be assigned to
                         ;; it.
                         (let* ([idx (substring number 0 1)]
                                [num (cond
                                       [(string=? idx "-") -1]
                                       [(string=? idx "+")  1]
                                       [else 0])])
                           ;; Proceeding with the conversion as you would
                           ;; normally do with a base2 -> base10 conversion.
                           (+ (* num (expt 3 (sub1 len)))
                              (balanced3->base10
                                (substring number 1))))]))))

;;; Some tests
(balanced3->base10 "--+.-")
(balanced3->base10 "+.-")
