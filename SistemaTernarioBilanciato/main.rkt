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

;;; Recursive conversion from base 10 to base 3.
;;; This is usefull for converting from base 10 to balanced base 3.
(define base10->base3
  (lambda (number) (cond
                     [(zero? number) 0]
                     [else
                       (+ (remainder number 3)
                          (* (base10->base3 (quotient number 3)) 10))])))

;;; Recursive conversion from base 3 to balanced base 3. The given number must
;;; be positive (the negative version is just the opposite of all.
(define base3->balanced3
  (lambda (number) (cond
                     [(zero? number) ""]
                     [else
                       ;; Everything is rapresented using + - and \..
                       (let* ([num (remainder number 10)]
                              [sig (cond
                                    [(eq? num 1) "+"]
                                    [(zero? num) "."]
                                    [else "-"])]
                              [crr (if (> (abs num) 1) 1 0)])
                         ;; Creating the balanced number.
                         (string-append
                           (base3->balanced3 (+ (quotient number 10) crr))
                           sig))])))

(define make-negative
  (lambda (balanced) (list->string
                       (map (lambda (x)
                              (cond
                                [(char=? x #\-) #\+]
                                [(char=? x #\+) #\-]
                                [else x]))
                            (string->list balanced)))))

;;; Just a simple wrapper to simplify the use of all the procedures
(define base10->balanced3
  (lambda (number) (if (negative? number)
                     (make-negative (base3->balanced3
                                      (base10->base3 (abs number))))
                     (base3->balanced3 (base10->base3 number)))))

;;; Some tests
(base10->balanced3 1)
(base10->balanced3 2)
(base10->balanced3 3)
(base10->balanced3 4)
(base10->balanced3 5)

(base10->balanced3 (balanced3->base10 "--+.-"))
(balanced3->base10 "+.-")
(balanced3->base10 "+-")
(balanced3->base10 "-+.-")
