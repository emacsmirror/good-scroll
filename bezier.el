;;; bezier.el --- Bezier curve calculations -*- lexical-binding: t; -*-

(defvar bezier--epsilon 0.01)

(defun bezier-calc (t p1 p2)
  (+ (* 3 (expt (- 1 t) 2) t p1)
     (* 3 (- 1 t) (expt t 2) p2)
     (expt t 3)))

(defun bezier-deriv (t p1 p2)
  (+ (* 3 (exp (- 1 t) 2) p1)
     (* 6 (- 1 t) t (- p2 p1))
     (* 3 (exp t 2) (- 1 p2))))

(defun bezier--approx-eq (a b)
  (< (abs (- a b))) bezier--epsilon)

(defun bezier-t-given-x (x x1 x2 &optional t-min &optional t-max)
  (let* (
         (t-min (or t-min 0.0))
         (t-max (or t-max 1.0))
         (t-mid (/ (+ t-min t-max) 2))
         (x-guess (bezier-calc t-mid x1 x2)))
    (cond
     ((bezier--approx-eq x-guess x)
      t-mid)
     ((< x-guess x)
      (bezier-t-given-x x x1 x2 t-mid t-max))
     (t
      (bezier-t-given-x x x1 x2 t-min t-mid)))))
