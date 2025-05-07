(setq xi 10)
(setq yi 10)
(setq m 14)  ; Tamaño del bloque de cada casilla

;; Define las casillas
(setq paret 'paret)
(setq cami 'cami)
(setq entrada 'entrada)
(setq sortida 'sortida)

;; Crea una matriz de N filas y M columnas
(defun crear-laberinto (n m)
  (loop repeat n collect (loop repeat m collect paret)))

(defun imprime-laberinto (laberinto)
  "Imprime el laberinto en consola"
  (dolist (fila laberinto)
    (dolist (celda fila)
      (cond
        ((eq celda paret) (princ "#"))
        ((eq celda cami) (princ "."))
        ((eq celda entrada) (princ "e"))
        ((eq celda sortida) (princ "s")))
    (princ "\n"))))

;; Función recursiva para generar el laberinto con DFS
(defun dfs-genera (laberinto x y)
  "Genera el laberinto utilizando DFS de forma recursiva."
  (let ((direcciones '((0 . 1) (1 . 0) (0 . -1) (-1 . 0)))  ;; derecha, abajo, izquierda, arriba
        (dir (shuffle direcciones)))  ;; Mezcla las direcciones
    (setf (nth x (nth y laberinto)) cami)  ;; Marca la casilla actual como camino
    ;; Recursión en las direcciones aleatorias
    (dolist (d dir)
      (let* ((nx (+ x (car d))) (ny (+ y (cdr d))))
        (when (and (>= nx 0) (< nx (length laberinto))
                   (>= ny 0) (< ny (length (first laberinto)))
                   (eq (nth nx (nth ny laberinto)) paret))  ;; Si es pared
          ;; Cambia la casilla adyacente a camino y recursión
          (dfs-genera laberinto nx ny))))
  laberinto))

;; Función para generar el laberinto y escribirlo en un archivo
(defun genera (nom-fitxer)
  (let* ((laberinto (crear-laberinto 25 25))  ;; Laberinto 25x25
         (entrada-x 1) (entrada-y 1)  ;; Coordenadas de entrada
         (sortida-x 23) (sortida-y 23))  ;; Coordenadas de salida
    (setf (nth entrada-x (nth entrada-y laberinto)) entrada)
    (setf (nth sortida-x (nth sortida-y laberinto)) sortida)
    (dfs-genera laberinto entrada-x entrada-y)
    ;; Guarda el laberinto en un archivo
    (with-open-file (out nom-fitxer :direction :output :if-exists :overwrite)
      (dolist (fila laberinto)
        (dolist (celda fila)
          (cond
            ((eq celda paret) (write-char #\# out))
            ((eq celda cami) (write-char #\. out))
            ((eq celda entrada) (write-char #\e out))
            ((eq celda sortida) (write-char #\s out)))
        (write-char #\newline out)))))
  (princ "Laberinto generado y guardado en el archivo.\n"))

  (setq jugador-x 1)
(setq jugador-y 1)

(defun carga-laberinto (nom-fitxer)
  "Carga el laberinto desde un archivo y devuelve la matriz."
  (with-open-file (in nom-fitxer)
    (let ((laberinto '()))
      (loop for linea = (read-line in nil) while linea do
        (push (mapcar (lambda (c)
                       (cond
                         ((char= c #\#) paret)
                         ((char= c #\.) cami)
                         ((char= c #\e) entrada)
                         ((char= c #\s) sortida)))
                     linea) laberinto))
      (nreverse laberinto))))

(defun dibuja-laberinto (laberinto)
  "Dibuja el laberinto en la pantalla"
  (dotimes (i (length laberinto))
    (dotimes (j (length (first laberinto)))
      (move (* j m) (* i m))
      (cond
        ((eq (nth i (nth j laberinto)) paret) (color 255 0 0) (cuadrado m))
        ((eq (nth i (nth j laberinto)) cami) (color 255 255 255) (cuadrado m))
        ((eq (nth i (nth j laberinto)) entrada) (color 0 0 255) (cuadrado m))
        ((eq (nth i (nth j laberinto)) sortida) (color 0 255 0) (cuadrado m)))))
  ;; Dibuja la posición del jugador
  (move (* jugador-x m) (* jugador-y m))
  (color 0 255 0)
  (cuadrado m))

(defun explora (nom-fitxer)
  "Inicia la exploración del laberinto desde el archivo"
  (let ((laberinto (carga-laberinto nom-fitxer)))
    (dibuja-laberinto laberinto)
    (princ "Usa las teclas W, A, S, D para moverte y ESC para salir.\n")
    (let ((jugador-nombre (read-line)))
      (loop
        (let ((key (get-key)))
          (cond
            ((or (= key 87) (= key 119)) (setf jugador-y (max 0 (- jugador-y 1))))  ;; Arriba
            ((or (= key 65) (= key 97)) (setf jugador-x (max 0 (- jugador-x 1))))   ;; Izquierda
            ((or (= key 83) (= key 115)) (setf jugador-y (min (1- (length laberinto)) (+ jugador-y 1))))  ;; Abajo
            ((or (= key 68) (= key 100)) (setf jugador-x (min (1- (length (first laberinto))) (+ jugador-x 1))))  ;; Derecha
            ((= key 27) (return))  ;; ESC para salir
            (t nil)))  ;; Si es otra tecla, no hacer nada
      (dibuja-laberinto laberinto)
      (when (eq (nth jugador-x (nth jugador-y laberinto)) sortida)
        (princ "¡Has llegado a la salida!\n"))))))

(defun menu ()
  (cls)
  (princ "1. JUGAR\n")
  (princ "2. SALIR\n")
  (let ((opcion (get-key)))
    (cond
      ((= opcion 49) (genera "laberinto.txt") (explora "laberinto.txt"))  ;; Opción 1: Jugar
      ((= opcion 50) (princ "Saliendo...\n") (exit))  ;; Opción 2: Salir
      (t (menu)))))  ;; Si no es una opción válida, muestra el menú nuevamente

(menu)