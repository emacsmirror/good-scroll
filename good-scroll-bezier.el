;;; good-scroll-bezier.el --- Bezier scrolling algorithm -*- lexical-binding: t; -*-

(defun good-scroll-bezier--pivots (velocity position)
  (let* (
         (slope (/ (* velocity good-scroll-duration)
                   good-scroll--destination))
         (normalization (sqrt 1.0 + (expt slope 2)))
         (dt (* (/ 1.0 normalization) 0.25))
         (dxy (* (/ slope normalization) 0.25)))
    (list dt dxy 0.6 1.0)))

(defun good-scroll-bezier--velocity-at (fraction-done x1 y1 x2 y2)
  (let* (
         (t (bezier-t-given-x fraction-done x1 x2))
         (dt (bezier-deriv t x1 x2))
         (dxy (bezier-deriv t y1 y2))
         (slope (/ dxy dt))) ; TODO make sure dt != 0
    (/ (* slope good-scroll--destination)
       good-scroll-duration)))

(defun good-scroll-bezier--position-at (fraction-done x1 y1 x2 y2)
  (let* (
         (t (bezier-t-given-x fraction-done x1 x2))
         (progress (bezier-calc t y1 y2)))
    (+ (* (- 1.0 progress)))))
