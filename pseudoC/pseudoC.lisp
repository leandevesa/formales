(defun run (prog input &optional (mem ()))
	(if (null prog)
		nil
		(if (eq (caar prog) 'int)
			(run (cdr prog) 
				 input 
				 (agregarVar (cdar prog) mem)
			)
			(if (eq (caar prog) 'main)
				(ejec (cadar prog) input mem)
				'NO_HAY_PROGRAMA
			)
		)
	)
)

(defun ejec (prog input mem &optional (output nil))
	(if (null prog)
		(reverse output)
		(cond
			((eq (caar prog) 'if)
				(runIf prog input mem output)
			)
			((eq (caar prog) 'while)
				(runWhile prog input mem output)
			)
			((eq (caar prog) 'scanf) 
				(ejec 
					(cdr prog)
					(cdr input)
					(asignarVar (cadar prog) (car input) mem)
					output
				)
			)
			((eq (caar prog) 'printf)
				(ejec
					(cdr prog)
					input
					mem
					(cons (getValorDeExpr (cdar prog) mem) output)
				)
			)
			((cambiaValorVar (car prog) mem)
				(ejec 
					(cdr prog)
					input
					(convShorthand (car prog) mem)
					output
				)
			)
		)
	)
)

(defun runIf (bloque input mem output)
	(cond
		; Si cond true -> ejecuto if
		((not (equal (getValorDeExpr (cadar bloque) mem) 0))
			(ejec (append (nth 2 (car bloque)) (cdr bloque)) input mem output)
		)
		; Si cond false -> veo si tiene else y lo ejecuto
		((equal (length (car bloque)) 5)
			(ejec (append (nth 4 (car bloque)) (cdr bloque)) input mem output)
		)
		; Si no, sigo con la otra instrucc
		(t
			(ejec (cdr bloque) input mem output)
		)
	)
)

(defun runWhile (bloque input mem output)
	(cond
		((not (equal (getValorDeExpr (nth 1 (car bloque)) mem) 0))
			(ejec (append (nth 2 (car bloque) ) bloque) input mem output)
		)
		(t
			(ejec (cdr bloque) input mem output)
		)
	)
)

(defun agregarVar (var mem)
	(cond
		((null var)
			nil
		)
		((eq (length var) 1)
			(asignarVar (car var) 0 mem)
		)
		((eq (cadr var) '=)
			(asignarVar 
				(car var)
				(caddr var)
				(agregarVar (cdddr var) mem)
			)
		)
		; Multiple asignaciones inline
		(t
			(asignarVar
				(car var)
				(car (last var))
				(agregarVar (cdr var) mem)
			)
		)
	)
)

(defun asignarVar (var valor mem)
	(cond
		((null mem)
			(cons (list var valor) mem)
		)
		((equal (caar mem) var)
			(cons 
				(list var valor)
				(cdr mem)
			)
		)
		(t
			(cons
				(car mem)
				(asignarVar var valor (cdr mem))
			)
		)
	)
)

(defun asociarVars (prog mem)
	(cond
		((null prog) nil)
		((listp (car prog))
			(cons 
				(asociarVars (car prog) mem)
				(asociarVars (cdr prog) mem)
			)
		)
		((existeVar (car prog) mem) 
			(cons
				(leerVar (car prog) mem)
				(asociarVars (cdr prog) mem)
			)
		)
		(t 
			(cons
				(car prog)
				(asociarVars (cdr prog) mem)
			)
		)
	)
)

(defun esOperador (valor)
	(cond
		((null valor) nil)
		((equal valor '+) t)
		((equal valor '+=) t)
		((equal valor '++) t)
		((equal valor '-) t)
		((equal valor '-=) t)
		((equal valor '--) t)
		((equal valor '*) t)
		((equal valor '*=) t)
		((equal valor '<) t)
		((equal valor '>) t)
		((equal valor '<=) t)
		((equal valor '>=) t)
		((equal valor '==) t)
		((equal valor '=) t)
		(t nil)
	)
)

(defun todasVarExisten (prog mem)
	(cond
		((null prog) t)
		((atom (car prog))
			(cond
				((esOperador (car prog))
					(todasVarExisten (cdr prog) mem)
				)
				((numberp (car prog))
					(todasVarExisten (cdr prog) mem)
				)
				((existeVar (car prog) mem)
					(todasVarExisten (cdr prog) mem)
				)
				(t nil)
			)
		)
		(t (todasVarExisten (car prog) mem))
	)
)

(defun getExpr (expr operadores vars)
	(cond
		((null expr)
			(realizarOperaciones operadores vars)
		)
		((esOperador (car expr))
			(getExpr 
				(cdr expr)
				(cons (car expr) operadores)
				vars
			)
		)
		((atom (car expr))
			(getExpr
				(cdr expr)
				operadores
				(cons (car expr) vars)
			)
		)
		(t
			(getExpr
				(cdr expr)
				operadores
				(cons (car (getExpr (car expr) nil nil)) vars)
			)
		)
	)
)

(defun realizarOperaciones (operadores operacionesSeparadas)
	(cond
		((null operadores) 
			operacionesSeparadas
		)
		((eq (car operadores ) '==)
			(realizarOperaciones 
				(cdr operadores)
				(cons 
					(list 'eq (cadr operacionesSeparadas) (car operacionesSeparadas))
					(cddr operacionesSeparadas)
				)
			)
		)
		(t
			(realizarOperaciones 
				(cdr operadores)
				(cons 
					(list (car operadores) (cadr operacionesSeparadas) (car operacionesSeparadas))
					(cddr operacionesSeparadas)
				)
			)
		)
	)
)

(defun convShorthand (expr memoria)
	(if (existeVar (car expr) memoria) 
		(cond
			((equal (cadr expr) '=)
				(asignarVar (car expr) (getValorDeExpr (cddr expr) memoria) memoria)
			)
			((equal (cadr expr) '++)
				(transfShorthandExpr (car expr) '+ 1 memoria)
			)
			((equal (cadr expr) '+=)
				(transfShorthandExpr (car expr) '+ (getValorDeExpr (cddr expr) memoria) memoria)
			)
			((equal (cadr expr) '--)
				(transfShorthandExpr (car expr) '- 1 memoria)
			) 
			((equal (cadr expr) '-=)
				(transfShorthandExpr (car expr) '- (getValorDeExpr (cddr expr) memoria) memoria)
			)
			((equal (cadr expr) '*=)
				(transfShorthandExpr (car expr) '* (getValorDeExpr (cddr expr) memoria) memoria)
			)
		)
		(convShorthand (reverse expr) memoria)
	)
)

(defun transfShorthandExpr (var operador valor memoria)
	(convShorthand (list var '= var operador valor) memoria)
)

(defun convertirBooleanos (exprEvaluada)
	(cond
		((null exprEvaluada) 0)
		((equal exprEvaluada t) 1)
		(t exprEvaluada)
	)
)

(defun evalExpr (expr)
	(convertirBooleanos (eval (car expr)))
)

(defun getValorDeExpr (expr mem)
	(if (atom expr) 
		(if (numberp expr)
			expr
			(leerVar expr mem)
		)
		; Es una expresion compuesta -> la evaluo
		(if (todasVarExisten expr mem)
			(evalExpr (getExpr (asociarVars expr mem) nil nil))
			'ERROR_VARIABLE_NO_DECLARADA
		)
	)
)

(defun leerVar (var mem)
	(cond
		((null mem) 
			'ERROR_VARIABLE_NO_DECLARADA
		)
		((equal (caar mem) var)
			(cadar mem)
		)
		(t 
			(leerVar var (cdr mem))
		)
	)
)

(defun existeVar (var mem)
	(cond
		((null mem) nil)
		((eq (caar mem) var) t)
		(t (existeVar var (cdr mem)))
	)
)

(defun cambiaValorVar (expr memoria)
	(cond
		((existeVar (car expr) memoria) t)
		(t
			(or (equal (car expr) '++) (equal (car expr) '--))
		)
	)
)

; Factorial input
(print 
(run '(
        (int n fact = 1)
        (main (
            (scanf n)
            (if (n < 0 ) (
                (printf "no existe fact de nro negativo" )
            ) else (
                (while (n > 1) (
                    (fact = fact * n)
                    (n -- )
                )) 
                (printf fact )
            )) 
        ))
    )
    '(5)
)
)
; Simple print
(print 
(run '(
        (int a = 2 b = 3)
        (main (
            (printf a)
        ))
    )
    () 
)
)
; Var no declarada
(print 
(run '(
        (int z = 2)
        (main (
            (printf b)
        ))
    ) 
    () 
)
)
; No print
(print 
(run '(
        (int a = 6)
        (main (
        (if (a == 2) (
                (printf (a + 1))
        ))
        ))
    ) 
    () 
)
)
; Suma Simple
(print 
(run  '(
        (int a = 2)
        (main (
            (if (a == 2) (
                (printf (a + 1))
            ))
        ))
    ) 
    () 
)
)
; Suma input
(print 
(run '(
        (int a = 2 b)
        (main (
            (scanf b)
            (a = b + 3)
            (printf a)
        ))
    )
    '(5)
)
)
; Multiple prints & no-atomic ops
(print 
(run  '(
        (int a = 2 b)
        (main (
            (a = (a + 1) * 4)
            (b -= 5)
            (a += 3)
            (printf a)
            (scanf a)
            (printf a)
            (printf b)
        ))
    )
    '(6) 
)
)
; Multiple prints, no-atomics and multi-ops inside prints
(print 
(run '(
        (int x y p = 10)
        (int r)
        (main (
            (x = p + 10)
            (p ++)
            (++ x)
            (x *= p - 4)
            (if (x < p) (
                (printf x + p)
                (scanf y)
            ) else (
                (x = x * 6)
                (printf p * p)
            ))
            (while (x > p * 10) (
                (printf x + p)
                (scanf y)
                (printf y)
                (x -= y)
            ))
        ))
    )
    '(700 100)
)
)