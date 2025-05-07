(setq xi 250)
(setq yi 10)
(setq m 14)
(setq l '())

(defun cuadrado (m)
    (drawrel m 0)
    (drawrel 0 m)
    (drawrel (- m) 0)
    (drawrel 0 (- m))
    (cond ((> m 0) (moverel 1 1) (cuadrado (- m 1)))))

(defun dibuja-bloque (filas columnas)
  (dotimes (i filas)
    (dotimes (j columnas)
      (progn
        (move (+ xi (* j m)) (+ yi (* (- filas 1 i) m)))
        (setf l (append l '(pared)))
        (color 0 0 0)
        (cuadrado m))))
  (setq *random-state* (make-random-state t))
  (setq aleatorio 1)
  (setf l (canvia aleatorio l 'entrada))
  (guardar-laberinto l "laberinto.txt" columnas)
  (setq fi (dividir aleatorio 25))
  (setq co (resto aleatorio 25))
  (move (+ xi (* co m)) (+ yi (* (- filas 1 fi) m)))
  (color 0 255 0)
  (cuadrado m)
  (color 0 0 0))


(defun calcularcuadraro (x)

)

(defun dividir (m n)
(cond ((< m n) 0)
(t (+ 1 (dividir (- m n) n)))))

(defun resto (m n)
(cond
((< m n) m)
(t (resto (- m n) n))
)
)

(defun canvia (on l per)
    (cond 
    ((= on 0) (cons per (cdr l)))
    (t (cons (car l) (canvia (- on 1)(cdr l)per)))
    )
    )

(defun guardar-laberinto (laberinto nom columnas)
  (let ((fp (open nom :direction :output)))
    (escribir-laberinto fp laberinto columnas)
    (close fp)))

(defun escribir-laberinto (fp laberinto columnas)
   (cond 
    ((null laberinto ) nil)
    ((eq columnas 0) (write-char #\newline fp) (escribir-laberinto fp laberinto 25))
    ((eq (car laberinto) 'pared) (write-char #\# fp) (escribir-laberinto fp (cdr laberinto) (- columnas 1))) 
    ((eq (car laberinto) 'cami) (write-char #\. fp) (escribir-laberinto fp (cdr laberinto)(- columnas 1)))  
    ((eq (car laberinto) 'entrada) (write-char #\e fp) (escribir-laberinto fp (cdr laberinto)(- columnas 1))) 
    ((eq (car laberinto) 'sortida) (write-char #\s fp) (escribir-laberinto fp (cdr laberinto)(- columnas 1))) 
    (t (write-char #\? fp) (escribir-laberinto fp (cdr laberinto)))))

(defun menu ()
  (cls)
  (princ "1. JUGAR\n")
  (princ "2. SALIR\n")
  (move 150 100)
  (let ((opcion (get-key)))
    (cond
      ((= opcion 49) (dibuja-bloque 25 25))
      ((= opcion 50) (exit))
      (t (menu)))))

(menu)