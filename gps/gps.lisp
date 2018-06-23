(setq grafo '(
	(a (b f))
	(b (a c))
	(c (b d))
	(d (c n e))
	(e (d))
	(f (g ))
	(g (h))
	(h (i l))
	(i (m j))
	(j (k))
	(k (o))
	(l (b f)) 
	(m (l c))
 	(n (j m))
  	(o (e n))
))

(setq diccionario '(
	(a (PaseoColon Independencia))
	(b (PaseoColon Chile))
	(c (PaseoColon Mexico))
	(d (PaseoColon Venezuela))
	(e (PaseoColon Belgrano))
	(f (Independencia Balcarce))
	(g (Independencia Defensa))
	(h (Defensa Chile))
	(i (Defensa Mexico))
	(j (Defensa Venezuela))
	(k (Defensa Belgrano))
	(l (Balcarce Chile))
	(m (Balcarce Mexico))
	(n (Balcarce Venezuela))
	(o (Balcarce Belgrano))
))

(defun todosCaminos (i f grafoP &optional (result nil) (tray (list (list i))))
	(cond
		((null tray) result)
		((eq (caar tray) f)
			(todosCaminos i f grafoP (cons (reverse (car tray)) result) (cdr tray))
		)
		((null (diffListas (getNodosAlrededor (caar tray) grafoP) (car tray)))
			(todosCaminos i f grafoP result (cdr tray))
		)
		(T
			(todosCaminos 
				i 
				f 
				grafoP 
				result 
				(append 
					(mapcar 
						(lambda (x) (cons x (car tray))) 
						(diffListas (getNodosAlrededor (caar tray) grafoP) (car tray))
					) 
					(cdr tray)
				)
			)
		)
	)
)

(defun diffListas(a b)
	(cond
		((null a) nil)
		((pertenece (car a) b)
			(diffListas (cdr a) b)
		)
		(T
			(cons (car a) (diffListas (cdr a) b))
		)
	)
)

(defun getNodosAlrededor (a b)
	(cond 
		((eq a (caar b))
			(cadr (car b))
		)
		(T 
			(getNodosAlrededor a (cdr b))
		)
	)
)

(defun pertenece (elemento l)
	(cond
		((null l) nil)
		((eq elemento (car l)) T)
		(T
			(pertenece elemento (cdr l))
		)
	)
)

(defun getMenorElemento (el1 el2)
	(if (< (length el1) (length el2))
		el1
		el2
	)
)

(defun getMenorElementoFromLista (lista)
	(cond
		((null lista) nil)
		((null (cdr lista)) (car lista))
		(T 
			(getMenorElemento 
				(car lista) 
				(getMenorElementoFromLista (cdr lista))
			)
		)
	)
)

(defun caminoMasCorto (i f grafoP &optional (result nil) (tray (list (list i))))
	(getMenorElementoFromLista 
		(todosCaminos i f grafoP result tray)
	)
)

(defun getRecorridosDescripcion (recorridos dic &optional (output nil))
	(cond
		((null recorridos) output)
		(T 
			(getRecorridosDescripcion 
				(cdr recorridos) 
				dic 
				(append 
					output
					(list 
						(decorarRecorrido 
							(getRecorridoDescripcion (car recorridos) dic)
						)
					)
				)
			)
		)
	)
)

(defun getRecorridoDescripcion (recorrido dic &optional (output nil))
	(cond
		((null recorrido) output)
		((null (cdr recorrido)) output)
		(T 
			(getRecorridoDescripcion 
				(cdr recorrido) 
				dic 
				(append 
					output 
					(list 
						(getCalleEnComun 
							(getCallesFromNodo (car recorrido) dic)
							(getCallesFromNodo (cadr recorrido) dic)
						)
					)
				)
			)
		)
	)
)

(defun getCallesFromNodo (nodo &optional (dic))
	(cond
		((null dic) 'CALLE_FROM_NODO_NOT_FOUND)
		((eq nodo (caar dic)) (cadar dic))
		(T 
			(getCallesFromNodo nodo (cdr dic))
		)
	)
)

(defun getCalleEnComun (calles1 calles2)
	(cond 
		((null calles1) 'ERROR_NO_CALLE_EN_COMUN)
		((pertenece (car calles1) calles2) (car calles1))
		(T (getCalleEnComun (cdr calles1) calles2))
	)
)

; Decora el recorrido poniendo cosas como "RECORRER", cantidad de veces que se repite la calle, etc.
(defun decorarRecorrido (recorrido)
	(decorarRecorridoAux recorrido '() 0)
)

(defun decorarRecorridoAux (camino output contador &optional (ultCalle nil))
	(cond
		((null ultCalle)
			(decorarRecorridoAux camino output contador (car camino))
		)
		((equal (car camino) ultCalle)
			(decorarRecorridoAux (cdr camino) output (+ contador 1) ultCalle)
		)
		((null (cdr camino))
			(append output (list 'RECORRER contador 'CUADRAS 'POR  ultCalle 'HASTA 'LLEGAR 'A 'DESTINO))
		)
		(T
			(decorarRecorridoAux 
				(cdr camino) 
				(append output 
					(list 'RECORRER contador 'CUADRAS 'POR  ultCalle 'Y 'DOBLAR 'EN (car camino))
				)
				1 
				(car camino)
			)
		)
	)
)

(defun getGrafoFromEsquina (esquina dicc)
	(cond
		((null dicc) 'ESQUINA_NOT_FOUND)
		((equal (cadar dicc) esquina)
			(caar dicc)
		)
		(T
			(getGrafoFromEsquina esquina (cdr dicc))
		)
	)
)

(defun GPS_UNO (origen destino dicc grafo)
	(getRecorridosDescripcion
		(list 
			(caminoMasCorto 
				(getGrafoFromEsquina origen dicc) 
				(getGrafoFromEsquina destino dicc)
				grafo
			)
		)
		dicc
	)
)

(defun GPS_TODOS (origen destino dicc grafo)
	(getRecorridosDescripcion
		(todosCaminos 
			(getGrafoFromEsquina origen dicc) 
			(getGrafoFromEsquina destino dicc)
			grafo
		)
		dicc
	)
)

(print 	
	(GPS_TODOS
		'(PaseoColon Independencia)
		'(Defensa Belgrano)
		diccionario
		grafo
	)
)