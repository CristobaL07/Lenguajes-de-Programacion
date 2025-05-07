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
        (move (+ xi (* j m)) (+ yi (* i m)))
        (setf l (append l '(pared)))
        (color 255 255 255)
        (cuadrado m))))
        (setq aleatorio (random 625))
        (entrada 1 l)
        (guardar-laberinto l "laberinto.txt" columnas))

(defun canvia (on l per)
    (cond 
    ((= on 1) (setf l (cons per (cdr l))))
    (t (setf l (cons (car l) (canvia (- on 1)(cdr l)per))))
    )
    )

(defun entrada (aleatorio l)
  (canvia aleatorio l '(entrada))
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