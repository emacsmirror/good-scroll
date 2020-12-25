;;; bezier.el --- Bezier curve calculations -*- lexical-binding: t; -*-

(defvar bezier--epsilon 0.01)

(defun bezier-calc (tt p1 p2)
  (+ (* 3 (expt (- 1 tt) 2) tt p1)
     (* 3 (- 1 tt) (expt tt 2) p2)
     (expt tt 3)))

(defun bezier-deriv (tt p1 p2)
  (+ (* 3 (expt (- 1 tt) 2) p1)
     (* 6 (- 1 tt) tt (- p2 p1))
     (* 3 (expt tt 2) (- 1 p2))))

(defun bezier--approx-eq (a b)
  (< (abs (- a b))
     bezier--epsilon))

(defun bezier-t-given-x (x x1 x2 &optional t-min t-max)
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

(provide 'bezier)

;;; bezier.el ends here
