(define (P) ;функція що зберігає поліном. кожен елемент поліному є парою, перше чилсло це множник змінної х, друге число степінь числа х
(list (cons 3 4) (cons 5 3))
)
 
(define (R)
(list (cons 1 2) (cons 3 1))
)
 
(define (max-step pol i max) ;пошук максимальної степені у поліномі
(cond
[(>= i (length pol))
max ;повернення результату
]
[(> (cdr (list-ref pol i)) (cdr (list-ref pol max))) ;list-ref взяття елементу зі списку за індексом. cdr - взяття другого числа пари, в даному випадку степені
(max-step pol (+ i 1) i)
]
[else
(max-step pol (+ i 1) max)
]
)
)
 
(define (firstel pol1 pol2) ;для діленнья поліномів, знаходження множника, знаходимо його через ділення елемента з найбільшою степенью першого полінома на елемент з найб. степенью другого поліному
(list (cons (/ (car (list-ref pol1 (max-step pol1 0 0))) (car (list-ref pol2 (max-step pol2 0 0)))) (floor (- (cdr (list-ref pol1 (max-step pol1 0 0))) (cdr (list-ref pol2 (max-step pol2 0 0)))))))
)
 
(define (divide pol1 pol2) ;функція ділення, робить початковий крок і виклкиає рекурсивну функцію ділення
(define (dive pol1 pol2 newpol) ;рекурсивна функція ділення
(cond
[(< (cdr (list-ref pol1 (max-step pol1 0 0))) (cdr (list-ref pol2 (max-step pol2 0 0))) ) ;якщо максимальна степінь частки менше за максимальну степінб діленого, то ділення зроблене
(newline)
(display "Результат - ")
newpol ;результатом є поліном що складається із множників
]
[else
(newline)
(display "Множник - ")
(strconv (firstel pol1 pol2) 0)
(newline)
(display "Від'ємник - ")
(strconv (multiply-polinom (firstel pol1 pol2) pol2) 0)
(newline)
(display "Остача - ")
(strconv (minus-polinom pol1 (multiply-polinom (firstel pol1 pol2) pol2)) 0)
(newline)
(dive (minus-polinom pol1 (multiply-polinom (firstel pol1 pol2) pol2)) pol2 (append newpol (firstel pol1 pol2))) ;кожен крок множемо множник на ділене і віднімаємо від попередньої частки
]
)
)
 
(newline)
(display "Множник - ")
(strconv (firstel pol1 pol2) 0)
(newline)
(display "Від'ємник - ")
(strconv (multiply-polinom (firstel pol1 pol2) pol2) 0)
(newline)
(display "Остача - ")
(strconv (minus-polinom pol1 (multiply-polinom (firstel pol1 pol2) pol2)) 0)
(newline)
(dive (minus-polinom pol1 (multiply-polinom (firstel pol1 pol2) pol2)) pol2 (firstel pol1 pol2))
)
 
(define (delete-item lx newlx i k) ;видалення елементу з поліному
(cond
[(>= i (length lx))
newlx
]
[(= i k)
(delete-item lx newlx (+ i 1) k) ;якщо індекс досягнув необхідного значення то не пишемо елемент у новий список
]
[else
(delete-item lx (append newlx (list (list-ref lx i))) (+ i 1) k)
]
)
)
 
(define (delete-zero pol newlx i) ;видаляемо нульові елементи, де множник дорівнює 0
(cond
[(>= i (length pol))
newlx
]
[(= (car (list-ref pol i)) 0)
(delete-zero pol newlx (+ i 1))
]
[else
(delete-zero pol (append newlx (list (list-ref pol i))) (+ i 1))
]
)
)
 
(define (replace-item lx newlx i k item) ;змінюємо елемент на певному індексі
(cond
[(>= i (length lx))
newlx
]
[(= i k)
(replace-item lx (append newlx (list (cons (+ item (car (list-ref lx i))) (cdr( list-ref lx i))))) (+ i 1) k item)
]
[else
(replace-item lx (append newlx (list (list-ref lx i))) (+ i 1) k item)
]
)
)
 
(define (add-polinom pol1 pol2) ;додавання поліномів
(define (add pol1 pol2 newpol i1 i2) ;рекурсивна функція додавання поліномів
(cond
[(>= i1 (length pol1))
(append newpol (append pol1 pol2))
]
[(>= i2 (length pol2))
(delete-zero (append newpol (append pol1 pol2)) '() 0)
]
[(= (cdr (list-ref pol1 i1)) (cdr (list-ref pol2 i2)) )
(add (delete-item pol1 '() 0 i1) (delete-item pol2 '() 0 i2)
(append newpol (list (cons (+ (car(list-ref pol1 i1)) (car(list-ref pol2 i2))) (cdr(list-ref pol1 i1)))))
i1 0)
]
[else
(add pol1 pol2 newpol i1 (+ i2 1))
]
)
)
(add pol1 pol2 '() 0 0)
)
 
(define (obelem pol newpol i) ;зміна знаку одинокого елемента при відніманні поліномів
(cond
[(>= i (length pol))
newpol
]
[else
(append newpol (list (cons (* -1 (car(list-ref pol i))) (cdr(list-ref pol i)) )))
]
)
)
 
(define (minus-polinom pol1 pol2) ;віднімання поліномів
(define (minu pol1 pol2 newpol i1 i2)
(cond
[(>= i1 (length pol1))
(delete-zero (append newpol (append (obelem pol1 '() 0) (obelem pol2 '() 0))) '() 0)
]
[(>= i2 (length pol2))
(minu pol1 pol2 newpol (+ i1 1) 0)
]
[(= (cdr (list-ref pol1 i1)) (cdr (list-ref pol2 i2)) )
(minu (delete-item pol1 '() 0 i1) (delete-item pol2 '() 0 i2)
(append newpol (list (cons (- (car(list-ref pol1 i1)) (car(list-ref pol2 i2))) (cdr(list-ref pol1 i1)))))
i1 0)
]
[else
(minu pol1 pol2 newpol i1 (+ i2 1))
]
)
)
(minu pol1 pol2 '() 0 0)
)
 
(define (simple pol newpol i1 i2 new) ;функція приведення поліному у скорччену форму, додавання елементів з однаковими степенями після множення поліномів
(cond
[(>= i1 (length pol))
newpol
]
[(and (>= i2 (length newpol)) (= new 0))
(simple pol (append newpol (list (list-ref pol i1))) (+ i1 1) 0 0)
]
[(and (>= i2 (length newpol)) (= new 1))
(simple pol newpol (+ i1 1) 0 0)
]
[(= (cdr (list-ref pol i1)) (cdr (list-ref newpol i2)) )
(simple pol (replace-item newpol '() 0 i2 (car (list-ref pol i1))) i1 (+ i2 1) 1)
]
[else
(simple pol newpol i1 (+ i2 1) new)
]
)
)
 
(define (multiply-polinom pol1 pol2) ;множення поліномів
(define (multiply pol1 pol2 newpol i1 i2)
(cond
[(>= i1 (length pol1))
(simple newpol '() 0 0 0)
]
[(>= i2 (length pol2))
(multiply pol1 pol2 newpol (+ i1 1) 0)
]
[else
(multiply pol1 pol2 (append newpol (list (cons (* (car (list-ref pol1 i1)) (car (list-ref pol2 i2))) (+ (cdr (list-ref pol1 i1)) (cdr (list-ref pol2 i2))) ))) i1 (+ i2 1))
]
)
)
(multiply pol1 pol2 '() 0 0)
)


 
(define (strconv pol i) ;функція конвертації поліному у строку
(cond
[(= i (- (length pol) 1)) ;виводить перше число, потім х^ , потім друге число що є степенем і далі знак наступного множника
(display (car(list-ref pol i)))
(display "x^")
(display (cdr(list-ref pol i)))
]
[(>= (car(list-ref pol (+ i 1))) 0)
(display (car(list-ref pol i)))
(display "x^")
(display (cdr(list-ref pol i)))
(display "+")
(strconv pol (+ i 1))
]
[(< (car(list-ref pol (+ i 1))) 0)
(display (car(list-ref pol i)))
(display "x^")
(display (cdr(list-ref pol i)))
(strconv pol (+ i 1))
]
)
)
 
(define (main P R) ;головна функція
(newline)
(strconv P 0)
(display " + ")
(strconv R 0)
(display " = ")
(strconv (add-polinom P R) 0)
(newline)
(newline)
(strconv P 0)
(display " * ")
(strconv R 0)
(display " = ")
(strconv (multiply-polinom P R) 0)
(newline)
(newline)
(strconv P 0)
(display " - ")
(strconv R 0)
(display " = ")
(strconv (minus-polinom P R) 0)
(newline)
(newline)
(strconv P 0)
(display " / ")
(strconv R 0)
(display " = ")
(newline)
 
(strconv (divide P R) 0)
)

 
(main (P) (R)) ;виклик головної функції