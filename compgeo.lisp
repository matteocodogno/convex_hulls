;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                           MEMBRI GRUPPO PROGETTO                             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                              ;
;    NOME/COG:  Matteo Codogno       (730620)                                  ;
;                                                                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                  GEOMETRY                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; create new point
(defun make-point (x y)
  (list x y))

; get x from point
(defun x (p)
  (car p))

; get y from point
(defun y (p)
  (car (cdr p)))

; triangle area2
(defun area2 (a b c)
  (- (* (- (x b) (x a)) (- (y c) (y a)))
     (* (- (y b) (y a)) (- (x c) (x a)))))

; left
(defun left (a b c)
  (if (> (area2 a b c) 0)
      (true)
    (false)))

; left-on
(defun left-on (a b c)
  (if (>= (area2 a b c) 0)
      (true)
    (false)))

; is-collinear
(defun is-collinear (a b c)
  (if (= (area2 a b c) 0)
      (true)
    (false)))

; angle from 2 points
(defun angle2d (a b)
  (atan (- (y b) (y a)) (- (x b) (x a))))

; get min point from min points list
(defun get-min-point (l &optional (m (car l)))
  (if (not (null l))
      (progn
        (if (< (y (car l)) (y m))
            (get-min-point (cdr l) (car l))
          (progn
            (if (< (y m) (y (car l)))
                (get-min-point (cdr l) m)
              (progn
                (if (< (x (car l)) (x m))
                    (get-min-point (cdr l) (car l))
                  (get-min-point (cdr l) m)))))))
    (list (x m) (y m))))
