(defun evaluar (expresion &optional (ambiente nil))
	(cond	((atom expresion)
				(if (numberp expresion) 
					expresion
					(if (pertenece expresion '(t nil))
						expresion
						(buscarVariable expresion ambiente)
					)
				)
			)
			((equal (car expresion) 'quote) (cadr expresion))
			((equal (car expresion) 'and) (funcionAnd (cdr expresion) ambiente))
			((equal (car expresion) 'or) (funcionOr (cdr expresion) ambiente))
			((equal (car expresion) 'lambda) expresion)
			((equal (car expresion) 'if) 
				(if (evaluar (cadr expresion) ambiente) 
					(evaluar (nth 2 expresion) ambiente)
					(evaluar (nth 3 expresion) ambiente)
				)
			)
			(t (aplicarFuncion (car expresion) (evaluarTodosLosArg (cdr expresion) ambiente) ambiente))
	)
)

(defun pertenece (element lista)
	(if (null lista )
		nil
		(if (eq element (car lista) )
			t
			(pertenece element (cdr lista))
		)
	)
)

(defun funcionAnd (args amb)
	(cond
		((null args) t)
		((and (evaluar (car args) amb) t) (funcionAnd(cdr args) amb))
		(t nil)
	)
)

(defun funcionOr (args amb)
	(cond
		((null args) nil)
		((or (evaluar (car args) amb) nil) t)
		(t (funcionOr (cdr args) amb))
	)
)

(defun buscarVariable (var ambiente)
	(if (null ambiente) nil
		(if (eq var (car ambiente))  
			(cadr ambiente)
			(buscarVariable var (cddr ambiente))
		)
	)
)

(defun evaluarTodosLosArg (args ambiente)
	(reverse (evaluarUnArgumento args ambiente '()))
)

(defun evaluarUnArgumento (args ambiente result)
	(if (null args)
		result
		(evaluarUnArgumento 
			(cdr args) 
			ambiente 
			(cons (evaluar (car args) ambiente) result)
		)
	)
)

(defun aplicarFuncion (funcion args ambiente)
	(if (atom funcion)
		(cond 
			((equal funcion 'car) (caar args))
			((equal funcion 'cdr) (cdar args))
			((equal funcion 'cons) (cons (car args) (cadr args)))
			((equal funcion 'append) (apply 'append args ))
			((equal funcion 'list) (apply 'list args))
			((equal funcion 'mapcar) (customMapCar (car args) (cadr args) ambiente))
			((equal funcion 'atom) (atom (car args)))
			((equal funcion 'null) (null (car args)))
			((equal funcion 'numberp) (numberp (car args)))
			((equal funcion '*) (apply '* args))
			((equal funcion '+) (apply '+  args))
			((equal funcion '-) (apply '-  args))
			((equal funcion '/) (/ (car args) (cadr args)) )	
			((equal funcion 'not) (not (car args) ))
			((equal funcion '<) (< (car args) (cadr args) ))
			((equal funcion '>) (> (car args) (cadr args) ))
			((equal funcion 'eq) (eq (car args) (cadr args) ))
			; no es funcion predef -> es var / numero
			(t (aplicaNoFn funcion args ambiente))
		)
		(evaluar
			(caddr funcion)
			(asociarVars (cadr funcion) args ambiente)
		)
	)
)

(defun aplicaNoFn (x args ambiente)
	(cond 
		((pertenece x ambiente) (aplicarFuncion (evaluar x ambiente) args ambiente))
		((null args) (evaluar x ambiente))
		(t (cons (evaluar x ambiente) (list (evaluar (car args) ambiente))))
	)
)

(defun customMapCar (f l a)
	(if (null l)
		'()
		(cons (evaluar (cons f (list (car l))) a) (customMapCar f (cdr l) a))
	)
)

(defun asociarVars (vars valores ambiente)
	(cond 
		((or (null vars) (null valores)) ambiente)
		(t (cons  
				(car vars) 
				(cons 
					(car valores) 
					(asociarVars (cdr vars)(cdr valores) ambiente)
				)
			)
		)
	)
)


; con numeros
(print (evaluar '2 nil)) ; 2
; con valores booleanos  true false
(print (evaluar nil nil)) ; nil
(print (evaluar 't nil))  ; t
; asociaciones en el ambiente
(print (evaluar 'A '(A 2) ) ) ; 2
(print (evaluar 'B '(A 2 B 10))) ; 10
(print (evaluar '(quote A) nil)) ; A
(print (evaluar '(quote 1) nil)) ; 1
(print (evaluar '(quote (car a)) nil )) ; (CAR A)
(print (evaluar '(quote ((2 3) (4 5))) )) ; ((2 3) (4 5))
; funciones booleanas and y or
(print (evaluar '(and (or t nil) t) nil )) ; t
(print (evaluar '(and (or t nil) (or nil nil)) nil)) ; nil
(print (evaluar '(or (or t nil) (or nil nil )) nil)) ; t
;Función car + ambiente
(print (evaluar '(car (list a 2 3)) '(a 100) )) ; 100
;Función cdr + ambiente
(print (evaluar '(cdr (list a b c)) '(a 100 b 99 c 98) ))
;Funciones anónimas lambda
(print (evaluar '((lambda (x) (* x 2)) 2) nil ) ) ; 4
(print (evaluar '((lambda (x y) (+ (* x 2) y)) 2 4) nil)) ; 8
(print (evaluar '(lambda (x) (* x 2)) nil)) ; ....
(print (evaluar '(mapcar (lambda (x) (cons x (cdr '(3 4 5)))) '(1 2 3)) nil)) ; ..........
;Forma funcional  mapcar
(print (evaluar '(mapcar 'numberp (quote (4)))))
(print (evaluar '(mapcar 'numberp (quote (4 5 6 nil)))) )
(print (evaluar '(mapcar 'car (quote ( (2 3) (4 5 ))) )))
(print 
	(evaluar '(fact 5) '(fact (lambda(n)(if(eq n 0) 1 (* n (fact (- n 1))))) ) )
)
(print
	(evaluar '(mapcar 'fact (quote ( 2 3 4 5 ) ))
 '(fact (lambda(n)(if(eq n 0) 1 (* n (fact (- n 1)))))) )
)