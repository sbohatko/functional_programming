; =========== task 1 =============
(define (func_1 x sqrt_alt) ; определение первой функции
  (sqrt_alt (- 15 (expt x 2))))
(define (func_2 x sqrt_alt) ; определение второй функции
  (/ 1 (sqrt_alt (+ x (expt x 2)))))
(define (find_error x func_alt) ; процедура для вычисления погрешности
  (display "\t")
  (display (abs (- (func_alt x sqrt_taylor) (func_alt x sqrt)))))
(define (sqrt_taylor x) ; вычисление корня разложением в ряд Тейлора
  (define (sqrt_iter yn n)
    (cond ((= n 5) yn) ; установка точности вычисления
          ((<= x 0) 0) ; если x <= 0, вернуть 0
          (else
           (sqrt_iter (/(+ yn (/ x yn)) 2) (+ n 1))))) ; расчет следующего приближения
  (sqrt_iter 1 0))
(define (find_funcs current step end) ; процедура для рассчета функция на промежутке
  (cond ((and (>= current 1) (<= current 2)) ; если значение в диапазоне от 1 до 2
         (display "y1:\t")(display current)(display "\t")
         (display (func_1 current sqrt_taylor)) ; расчет 1-ой функ-ции используя ряд Тейлора
         (display "\t")
         (display (func_1 current sqrt)) ; расчет 1-ой функ-ции используя встроенную ф-цию
         (find_error current func_1) ; расчет погрешности для 1-ой функции
         (newline))
        ((and (>= current -1) (< current 1)) ; если значение в диапазоне от -1 до 1
         (display "y2:\t")(display current)(display "\t")
         (cond ((= 0 (sqrt_taylor (+ current (expt current 2)))) ; проверка подкорневого выражения
                (display "division by zero\t")
                (display (func_2 current sqrt)) ; расчет 2-ой функ-ции используя встроенную ф-цию
                (display "\t\t\tnot defined")
                (newline))
               (else
                (display (func_2 current sqrt_taylor)) ; расчет 2-ой функ-ции используя ряд Тейлора
                (display "\t")
                (display (func_2 current sqrt)) ; расчет 2-ой функ-ции используя встроенную ф-цию
                (find_error current func_2) ; расчет погрешности для 2-ой функции
                (newline)))))
  (cond ((< current end) ; если текущее значение меньше конечного
         (find_funcs (+ current step) step end))) ; то вычислить функции для следующего значения
  )
(display "Лаб.2 задание 2.1:")
(newline)
(display "Func\tx\tsqrt_taylor\t\tsqrt_build\t\terror")(newline)
(find_funcs -2 0.5 2)