(setq xi 250)
(setq yi 10)
(setq m 14)
(setq laberinto '())
(defparameter *random-state* (make-random-state t))

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
        (setf laberinto (append laberinto '(pared))))))
  (setq *random-state* (make-random-state t))
  (setq aleatorioE (randomE 625))
  (setf laberinto (canvia aleatorioE laberinto 'entrada))
  (setf laberinto (recursividad aleatorioE laberinto))
  (setq aleatorioS (randomS 625 laberinto))
  (setf laberinto (canvia aleatorioS laberinto 'sortida))
  (guardar-laberinto laberinto "laberinto.txt" columnas)
  (pintar laberinto filas columnas)
  (jugar laberinto (encontrarentrada laberinto) (encontrarsalida laberinto) (encontrarentrada laberinto)))

(defun pintar (laberinto filas columnas)
  (dotimes (i filas)
    (dotimes (j columnas)
      (progn
        (move (+ xi (* j m)) (+ yi (* (- filas 1 i) m)))
        (cond ((eq (nth (+ (* i columnas) j) laberinto) 'pared) (color 0 0 0) (cuadrado m))
              ((eq (nth (+ (* i columnas) j) laberinto) 'entrada) (color 0 0 255) (cuadrado m))
              ((eq (nth (+ (* i columnas) j) laberinto) 'sortida) (color 255 0 0) (cuadrado m))
              ((eq (nth (+ (* i columnas) j) laberinto) 'cami) (color 255 255 255) (cuadrado m)))))))

(defun randomE (num)
  (setq entrada (random num *random-state*))
  (cond ((and (> entrada 24) (< entrada 600) (not (eq 24 (mod entrada 25))) (not (eq 0 (mod entrada 25)))) entrada)
        (t (randomE num))))

(defun randomS (num laberinto)
  (setq salida (random num *random-state*))
  (cond ((eq (nth salida laberinto) 'cami) salida)
        (t (randomS num laberinto))))

(defun recursividad (actual laberinto)
(setq alea (random 4 *random-state*))
(setq nuevo (comprobador actual laberinto alea))
  (cond ((and (eq actual (comprobador actual laberinto 0)) (eq actual (comprobador actual laberinto 1)) (eq actual (comprobador actual laberinto 2)) (eq actual (comprobador actual laberinto 3))) laberinto)
        (t (setf laberinto (canvia nuevo laberinto 'cami)) (recursividad nuevo (recursividad nuevo laberinto)))))


(defun comprobador (pos laberinto ran)
(cond 
((eq ran 0) (cond ((> pos 49) (posible pos (- pos 25) laberinto 0)) (t pos)))  ; arriba 
((eq ran 1) (cond ((< pos 575) (posible pos (+ pos 25) laberinto 1)) (t pos)))         ;abajo
((eq ran 2) (cond ((eq 23 (mod pos 25)) pos) (t (posible pos (+ pos 1) laberinto 2)))) ;derecha
((eq ran 3) (cond ((eq 1 (mod pos 25)) pos) (t (posible pos (- pos 1) laberinto 3)))) ;izquierda
)
)

(defun posible (actual nuevo laberinto n)
  (cond ((and (eq (nth nuevo laberinto) 'pared) (eq n 0) (eq (nth (- nuevo 25) laberinto) 'pared) (eq (nth (- nuevo 24) laberinto) 'pared) (eq (nth (- nuevo 26) laberinto) 'pared) (eq (nth (+ nuevo 1) laberinto) 'pared) (eq (nth (- nuevo 1) laberinto) 'pared)) nuevo)
        ((and (eq (nth nuevo laberinto) 'pared) (eq n 1) (eq (nth (+ nuevo 25) laberinto) 'pared) (eq (nth (+ nuevo 24) laberinto) 'pared) (eq (nth (+ nuevo 26) laberinto) 'pared) (eq (nth (+ nuevo 1) laberinto) 'pared) (eq (nth (- nuevo 1) laberinto) 'pared)) nuevo)
        ((and (eq (nth nuevo laberinto) 'pared) (eq n 2) (eq (nth (+ nuevo 25) laberinto) 'pared)(eq (nth (+ nuevo 26) laberinto) 'pared)(eq (nth (- nuevo 24) laberinto) 'pared) (eq (nth (+ nuevo 1) laberinto) 'pared) (eq (nth (- nuevo 25) laberinto) 'pared)) nuevo)
        ((and (eq (nth nuevo laberinto) 'pared) (eq n 3) (eq (nth (+ nuevo 25) laberinto) 'pared) (eq (nth (+ nuevo 24) laberinto) 'pared)(eq (nth (- nuevo 26) laberinto) 'pared)(eq (nth (- nuevo 25) laberinto) 'pared) (eq (nth (- nuevo 1) laberinto) 'pared)) nuevo)
        (t actual)))

(defun dividir (m n)
(cond ((< m n) 0)
(t (+ 1 (dividir (- m n) n)))))

(defun resto (m n)
(cond
((< m n) m)
(t (resto (- m n) n))
)
)

(defun canvia (on laberinto per)
    (cond 
    ((= on 0) (cons per (cdr laberinto)))
    (t (cons (car laberinto) (canvia (- on 1)(cdr laberinto)per)))
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

(defun jugar (laberinto entrada salida actual)
(cond ((eq salida actual) (menu))
  (t (let ((opcion (get-key)))
    (cond
      ((or (= opcion 65) (= opcion 97) (= opcion 331)) (jugar laberinto entrada salida (movimiento actual 0 laberinto))) ; izquierda
      ((or (= opcion 68) (= opcion 100) (= opcion 333)) (jugar laberinto entrada salida (movimiento actual 1 laberinto))) ; derecha
      ((or (= opcion 87) (= opcion 119) (= opcion 328)) (jugar laberinto entrada salida (movimiento actual 2 laberinto))) ; arriba
      ((or (= opcion 83) (= opcion 115) (= opcion 336)) (jugar laberinto entrada salida (movimiento actual 3 laberinto))) ; abajo
      ((= opcion 27) (menu)))))))

(defun encontrarsalida (laberinto)
 (cond
  ((eq (car laberinto) 'sortida) 0)
  (t (+ 1 (encontrarsalida (cdr laberinto))))
))

(defun encontrarentrada(laberinto)
(cond
  ((eq (car laberinto) 'entrada) 0)
  (t (+ 1 (encontrarentrada (cdr laberinto))))
)
)

(defun movimiento (actual direccion laberinto)
  (cond ((eq direccion 0) (cond ((seguro laberinto (- actual 1)) (pintarjuego laberinto actual (- actual 1)) (- actual 1)) (t actual)))
        ((eq direccion 1) (cond ((seguro laberinto (+ actual 1)) (pintarjuego laberinto actual (+ actual 1)) (+ actual 1)) (t actual)))
        ((eq direccion 2) (cond ((seguro laberinto (- actual 25)) (pintarjuego laberinto actual (- actual 25)) (- actual 25)) (t actual)))
        ((eq direccion 3) (cond ((seguro laberinto (+ actual 25)) (pintarjuego laberinto actual (+ actual 25)) (+ actual 25)) (t actual)))))

(defun pintarjuego (laberinto anterior nueva)
(setq i (dividir nueva 25))
(setq j (resto nueva 25))
(move (+ xi (* j m)) (+ yi (* (- 25 1 i) m)))
(color 0 255 0)
(cuadrado m)
(setq i (dividir anterior 25))
(setq j (resto anterior 25))
(cond
((eq (nth anterior laberinto) 'entrada))
(t 
(move (+ xi (* j m)) (+ yi (* (- 25 1 i) m)))
(color 255 255 255)
(cuadrado m)
(color 0 0 0)
)
)
)        

(defun seguro (laberinto actual)
(cond 
((eq 0 actual)
(cond
((or (eq (car laberinto) 'sortida) (eq (car laberinto) 'cami)) t)
(t nil)))
(t (seguro (cdr laberinto) (- actual 1)))
)
)

(defun llegeix (nom)
 (let* ((fp (open nom))
 (contingut (llegeix-intern fp)))
 (close fp)
 contingut))
 
(defun llegeix-intern (fp)
 (let ((c (read-char fp nil nil)))
 (cond ((null c) '())
 (t (cons c (llegeix-intern fp))))))

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