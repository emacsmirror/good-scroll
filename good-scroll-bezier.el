;;; good-scroll-bezier.el --- Bezier scrolling algorithm -*- lexical-binding: t; -*-

(defvar good-scroll-bezier--x1 nil)
(defvar good-scroll-bezier--y1 nil)
(defvar good-scroll-bezier--x2 nil)
(defvar good-scroll-bezier--y2 nil)

(defun good-scroll-bezier--set-pivots (velocity position)
  (let* (
         (slope (/ (* velocity good-scroll-duration)
                   good-scroll--destination))
         (normalization (sqrt (+ 1.0 (expt slope 2))))
         (dt (* (/ 1.0 normalization) 0.25))
         (dxy (* (/ slope normalization) 0.25)))
    (setq good-scroll-bezier--x1 dt
          good-scroll-bezier--y1 dxy
          good-scroll-bezier--x2 0.6
          good-scroll-bezier--y2 1.0)))

(defun good-scroll-bezier--velocity-at (fraction-done)
  (let* (
         (tt (bezier-t-given-x fraction-done
                               good-scroll-bezier--x1
                               good-scroll-bezier--x2))
         (dt (bezier-deriv tt
                           good-scroll-bezier--x1
                           good-scroll-bezier--x2))
         (dxy (bezier-deriv tt
                            good-scroll-bezier--y1
                            good-scroll-bezier--y2))
         (slope (/ dxy dt))) ; TODO make sure dt != 0
    (/ (* slope good-scroll--destination)
       good-scroll-duration)))

(defun good-scroll-bezier--position (fraction-done)
  (let* (
         (tt (bezier-t-given-x fraction-done
                               good-scroll-bezier--x1
                               good-scroll-bezier--x2))
         (progress (bezier-calc tt
                                good-scroll-bezier--y1
                                good-scroll-bezier--y2)))
    (+ (* (- 1.0 progress) (- good-scroll--traveled))
       (* progress good-scroll--destination))))

(defun good-scroll-bezier--update (fraction-done)
  (let ((velocity (if good-scroll-bezier--x1
                      (good-scroll-bezier--velocity-at fraction-done)
                    0.0)))
    (good-scroll-bezier--set-pivots velocity 0.0)))

(defun good-scroll-bezier-position (fraction-done)
  (good-scroll-bezier--update fraction-done)
  (good-scroll-bezier--position fraction-done))

(provide 'good-scroll-bezier)

;;; good-scroll-bezier.el ends here
