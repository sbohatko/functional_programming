; =========== zadanie 1 =============
; заданная функция
(define (func x)
  (+ (log x) 2))
; значение точности e
(define e 0.001)
; производная от заданной функции
(define (deriv)
  (lambda (x)
         (/ 1 x)))
; вторая производная от заданной функции
(define (deriv-2)
  (lambda (x)
    (/ -1 (expt x 2))))
; вычисление слудующего приближения корня
(define (newton-next f)
  (lambda (x)
    (- x (/ (f x) ((deriv) x)))))
(define num_x 0) ; номер корня приближения
; метод Ньютона
(define (newton-method f guess)
  (let ((next-guess ((newton-next f) guess))) ; поиск следующего корня 
    ;(cond ((< (abs (/ (f guess) ((deriv) guess))) e)
    (cond ((< (abs (f guess)) e) ; если f(приближение) меньше заданной точности, значит найдено
           (display "Find! ") (display "x") (display num_x)
           (display " = ")(display guess)
           (display " func(x)= ") (display (f guess)))
          (else
           (display "x") (display num_x) (display " = ") (display guess) (newline)
           (set! num_x (+ num_x 1))
           (newton-method f next-guess))))) ; переход на след итерацию со следующим приближением
; Ньютон: проверка заданного интервала
(define (newton-interval f a b)
  (display "======Newton method======") (newline)
  (cond ((> (* (f a) ((deriv-2) a))) ; условие сходимости для начального приближения левой границы
         (display "first_approx: left_border") (newline)
         (newton-method f a)
         )
        ((> (* (f b) ((deriv-2) b))) ; условие сходимости для начального приближения правой границы
         (display "first_approx: right_border") (newline)
         (newton-method f b)
         )
        (else
         (display "error borders"))))
; Метод простых итераций
(define (iterate-method f guess)
  (let ((next (f guess)))
    (cond ((<= (abs (- guess next)) e) ; если разница между приближение и f(приближение) меньше точности
           (display "Find! ") (display "x") (display num_x) ; значит приближение найдено
           (display " = ")(display guess) (display "; f(x)=")(display (abs (- guess next))))
          (else ; иначе переход к следующей итерации со следующим приближением
           (display "x") (display num_x) (display " = ") (display guess) (newline)
           (set! num_x (+ num_x 1))
           (iterate-method f next)))))
; Метод итераций: проверка заданного интервала          
(define (iterate-interval f a b)
  (newline)
  (display "======Iterate method======") (newline)
  (cond ((< (abs((deriv) a)) 1) ; условие сходимости для начального приближения левой границы
         (display "first_approx: left_border") (newline)
         (iterate-method f a)
         )
        ((< (abs((deriv) b)) 1) ; условие сходимости для начального приближения правой границы
         (display "first_approx: right_border") (newline)
         (iterate-method f b)
         )
        (else
         (display "error borders"))))
(display "Лаб №3 задание 2.1:")
(newline)
;
(newton-interval func 0.01 1.1)
(iterate-interval func 1 2)