(define (fact n) ; процедура знаходження факторіалу
		  (if (= n 0) ; базова умова: якщо n=0, то повернути 1
		      1
		      (* n (fact (- n 1))))) ; виклик рекурсії
	(define (comb n k) ; процедура знаходження кількості комбінацій
	  (cond ((or (and (= n k) (> n 0)) (and (= k 0) (< n 0))) ; при виконанні цієї умови повернути 1
	         1)
	         ((and (> n 0) (> n k)) ; при виконанні цієї умови розрахувати комбінацію
	          (/ (fact n) (* (fact k) (fact (- n k))))) ; виклик рекурсії
	         (else 0)
	         )
	  )
	(comb 10 2) ;  виклик процедури