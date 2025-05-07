(setq xi 10) (setq yi 10) (setq m 25)

(defun cuadrado (m)
      (drawrel m 0)
      (drawrel 0 m)
      (drawrel (- m) 0)
      (drawrel 0 (- m))
      (cond ((> m 0) (moverel 1 1) (cuadrado (- m 1)))))

(defun pinta (l)
      (move xi yi)
      (pintar l))

(defun pintar (l)
      (cond ((null l) t)
            ((equal (car l) 0) (color 0 0 0) (cuadrado m) (moverel 0 (- m)) (pintar (cdr l)))
            ((equal (car l) 1) (color 255 0 0) (cuadrado m) (moverel 0 (- m)) (pintar (cdr l)))
            ((equal (car l) 2) (color 0 255 0) (cuadrado m) (moverel 0 (- m)) (pintar (cdr l)))
            ((equal (car l) 3) (color 0 0 255) (cuadrado m) (moverel 0 (- m)) (pintar (cdr l)))))

(defun menu ()
    (cls)
    (pinta '(1 2 3)))

(menu)
