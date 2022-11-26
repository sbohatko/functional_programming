;Завдання 1
(newline)
(newline)
(display "Завдання 1")
(newline)
(newline)

(define (vecmin vec min i) ;Пошук мінімального елемента вектору
  (cond
    [(= i (vector-length vec)) ;якщо індекс дорівню кількості елементів, повертаємо мінімальний елемент
      min
    ]
    [else
      (if (> (vector-ref vec min) (vector-ref vec i))
        (vecmin vec i (+ i 1)) ;передаемо новий знайдений мінімальний елемент
        (vecmin vec min (+ i 1))) ;зберігаемо старий мінімальний елемент
    ]
  )
)

(define (vecmax vec max i) ;пошук максимального елементу вектору, працює за аналогією пошуку мінімального елементу
  (cond
    [(= i (vector-length vec))
      max
    ]
    [else
      (if (< (vector-ref vec max) (vector-ref vec i))
        (vecmax vec i (+ i 1))
        (vecmax vec max (+ i 1)))
    ]
  )
)

(define (vecout vec a b) ;вивід елементів вектору що знаходяться між мінімальним та максимальним елементом
  (cond
    [(= a b)
      (display "")
    ]    
    [else
      (display (vector-ref vec a))
      (display " ")
      (vecout vec (+ a 1) b)
    ]
  )  
)

(define (minmax vec) ;повернення пари, макс та мін елементів, де менший індекс завжди буде в початку пари
  (if (> (vecmax vec 0 0) (vecmin vec 0 0))
    (cons (vecmin vec 0 0) (vecmax vec 0 0)) ;слово cons позначає пар3 двох чисел
    (cons (vecmax vec 0 0) (vecmin vec 0 0))
  )
)

(define (sum vec a b) ;сума елементів між значеннями макс та мін
  (cond ;умовна конструкція
    [(= a b)
      0
    ]    
    [else      
      (+ (vector-ref vec a) (sum vec (+ a 1) b))
    ]
  ) 
)

(define (main1 vec) ;головна функція
  (display "Вектор - ")
  (display vec)
  (newline)
  (display "Індекс мінімального елементу - ")
  (display (vecmin vec 0 0))
  (newline)
  (display "Індекс максимального елементу - ")
  (display (vecmax vec 0 0))
  (newline)
  (display "Елементи між максимальним та мінімальним елементом : ")
  (vecout vec (+ (car (minmax vec)) 1) (cdr (minmax vec))) ;car виклик першого елементу пари, cdr виклик другого елементу пари
  (newline)
  (display "Сумма елементів - ")
  (display (sum vec (+ (car (minmax vec)) 1) (cdr (minmax vec))))
)

(main1 (vector 4 6 2 9 10 4 2 15 5 6 9)) ;передача у функцію створеного вектору



;Завдання 2
(newline)
(newline)
(display "Завдання 2")
(newline)
(newline)



(define (leng q k) ;підрахунок довжини черги, її кількості елементів
  (if (pair? q) 
    (leng (cdr q) (+ k 1)) 
    k
  )
)


(define (add q1 x) ;додавання елементу в кінець черги
  (cond
    [(pair? q1)      ;якщо q1 є парою, то виконуємо цей код
      (cons (car q1) (add (cdr q1) x)) ;в перший елемент пари ставиться елемент черги, у другий, посилання на наступний елемент(пара де конструкціія продовжується)
    ]    
    [else      
      (cons x '()) ;в кінці вставляється новий елемент та пустий список що позначає кінець черги
    ]
  )
)

(define (suma q) ;сума елементів черги
  (cond
    [(pair? q)      
      (+ (car q) (suma (cdr q)))
    ]    
    [else      
      0
    ]
  )
)

(define (mult q) ;добуток елементів черги
  (cond
    [(pair? q)      
      (* (car q) (mult (cdr q)))
    ]    
    [else      
      1
    ]
  )
)

(define (average q) ;підрахунок середнього арифметичного черги
  (/ (suma q) (leng q 0))
)

(define (root number degree tolerance) ;функція пошуку н-го корню числа, параметри число, степінь корню, точність
  (define (good-enough? next guess) ;перевіряє чи менше значення за точність
    (< (abs (- next guess)) tolerance))
  (define (improve guess) ;розрахунок наступного кроку наближення до результату
    (/ (+ (* (- degree 1) guess) (/ number (expt guess (- degree 1)))) degree))
  (define (*root guess)
    (let ((next (improve guess))) ;У виразі let початкові значення обчислюються до того, як будь-яка зі змінних стане зв'язаною .
    ;next є значенням яке вираховується виразом improve. і поступово наближаеться до потрібного резальтату. а саме н-го кореню
      (if (good-enough? next guess)
          guess
          (*root next))))
  (*root 1.0)) ;виклик рекурсії із початковим значенням

(define (geoaverage q) ;підрахунок середнього географічного черги 
  (root (mult q) (leng q 0) 0.001)
)

(define (main2 q) ;головна функція котра викликає інші
  (display "Черга\n")
  (display q)
  (newline)
  (display "Середнє арифметичне - ")
  (display (average q))
  (newline)
  (display "Середнє геометричнє - ")
  (display (geoaverage q))
  (newline)
  (display "Кількість елементів - ")
  (display (leng q 0))
  (newline)
  (display "Черга із доданим значенням "); для доказу що конструкція є черго, додамо до неї число. Воно поміститься в кінець черги
  (newline)
  (display (add q 2)) ;додаємо елемент в кінець черги
)


(newline)
(main2 (cons 5 (cons 6 (cons 2 (cons 15 (cons 19 (cons 7 (cons 17 '()))))))))
; виклик головної функції та передача написаної черги. Черга складається з пар, перший елемент є елементмо черги, лругий є іншою чергою з такою ж конструкцією, тобто є посиланням на наступний елемент черги. В останній парі друге число черги є пустим списком, який позначає кінецб черги.

