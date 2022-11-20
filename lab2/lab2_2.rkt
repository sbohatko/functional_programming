(; =========== zadanie 2 =============
(define (fraction a) ; процедура конечной цепной дроби
  (fraction_step a 0)
  )
(define (fraction_step a pow-of-two) ; процедура для пошагового вычисления
  (define max-pow-of-two 8) ; максимальная степень двойки в числителе
  (cond ((= pow-of-two max-pow-of-two) ; при достижении последнего элемента дроби
         (/ (expt 2 max-pow-of-two) (expt a 2))) ; вычислить его
        (else (/ (expt 2 pow-of-two) ; иначе перейти к следующему элементу
                 (+ (expt a 2) (fraction_step a (+ pow-of-two 1)))))))
(display "Лаб.2 задание 2.2:")
(newline)
(fraction 1)