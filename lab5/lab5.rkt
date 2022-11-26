;Завдання 1
(newline)
(display "Завдання 1")
(newline)


(define (euclid a b) ;алгоритм евлкіда для пошуку НСД - найменшого спільного дільника чисел
  (if (> b a)
      (euclid b a) ;виконуємо якщо умова вірна
      (if (= b 0) ;новий if якщо умова не вірна
          a ;повертає а якщо умова вірна
          (euclid b (mod a b)) ;виконується якщо умова не вірна, рекурсивний виклик
          )
  )
)

;cons - позначає пару чисел вигляду (1 2)
; car - повертає перше число з пари
; cdr - повертає друге число з пари
 
(define (lcm a b) ;функція пошуку НСК - найменшого спільного кратного чисел, шукається за такою формулою (а*б)/НСД(а б)
	(/ (* a b) (euclid a b))
)

(define (sum a b c d lcm4) ;сума дробів із приведенням їх до одного знаменника
  (+ (+ (* (car a) (/ lcm4 (cdr a))) (* (car b) (/ lcm4 (cdr b)))) (+ (* (car c) (/ lcm4 (cdr c))) (* (car d) (/ lcm4 (cdr d)))))
) 

(define (lcm4num a b c d) ;пошук НСК чотирьох знаменників дробів. для цього спочатку шкаєьбся НСК перших двох чисел, потім НСК другиг двох чисел, і вже потім НСК знайдених двох чисел
  (lcm (lcm (cdr a) (cdr b)) (lcm (cdr c) (cdr d)))
)

(define (tostring a b c d lcm4) ;функція виводу чотирьох дробів у строковій формі
  (display (* (car a) (/ lcm4 (cdr a))))
  (display "/")
  (display lcm4)
  (display " + ")
  (display (* (car b) (/ lcm4 (cdr b))))
  (display "/")
  (display lcm4)
  (display " + ")
  (display (* (car c) (/ lcm4 (cdr c))))
  (display "/")
  (display lcm4)
  (display " + ")
  (display (* (car d) (/ lcm4 (cdr d))))
  (display "/")
  (display lcm4)
)

(define (main1 a b c d) ;головна функція першого завдання
  (tostring a b c d (lcm4num a b c d)) ;виводимо строку сумування чотирьох дробів
  (display " = ")  
  (display (sum a b c d (lcm4num a b c d))) ;результат сумування
  (display "/")
  (display (lcm4num a b c d))
  (newline)
  (display "Чи дорівнює дріб одиниці? - ")
  (if (= (/ (sum a b c d (lcm4num a b c d)) (lcm4num a b c d)) 0)
    (display "Yes")
    (display "No")
  )
)
 
(main1 (cons 1 3) (cons 1 5) (cons 1 7) (cons 1 9)) ;передача чотирьох дробів(у вигляді пар, де перше число чисельник дробу, друге його знаменник) до головної функції



;Завдання 2
(newline)
(newline)
(display "Завдання 2")
(newline)
(newline)


(define (r a b) ;формула знаходження r
	(sqrt (+ (expt a 2) (expt b 2)))
)
 
(define (cosf a b) ;формула знаходження cosf
	(/ a (r a b))
)
 
(define (sinf a b) ;формула знаходження sinf
	(/ b (r a b))
)
 
(define (z a b) ;формула знаходження z	
  (* (r a b) (+ (cosf a b) (* 1+1i (sinf a b))))
)
 
(define (argz a b) ;формула знаходження arg z
	(atan (/ b a))
)
 
(define (main2 listx i) ;головна функція другої програми
	(cond
		((< i (length listx))
			(newline)
			(display "Числа а і b = ")
			(display (list-ref (list-ref listx i) 0)) ;list-ref бере елемент зі списку за індексом
			(display "   ")
			(display (list-ref (list-ref listx i) 1))
			(newline)
      (display "Тригонометрична форма має вигляд r*(cos(f)+i*sin(f))")
      (newline)
			(display "Тригонометрична форма - ")
			(display (r (list-ref (list-ref listx i) 0) (list-ref (list-ref listx i) 1)))
			(display " * (")
			(display (cosf (list-ref (list-ref listx i) 0) (list-ref (list-ref listx i) 1)))
      (display " + i*")
      (display (sinf (list-ref (list-ref listx i) 0) (list-ref (list-ref listx i) 1)))
      (display ")")
			(newline)			
			(main2 listx (+ i 1))
		)
		(else
			(display "")
		)
	)
)
 
(main2 (list '(5 7) '(9 3) '(12 42) '(-9 -10) '(-6 -5) '(11 -2) '(4 4) '(5 -55) '(9 10)) 0) ;виклик головної функції другого завдання і передача списку пар чисел