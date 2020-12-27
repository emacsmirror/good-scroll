;;; good-scroll-bezier.el --- Bézier scrolling algorithm -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Benjamin Levy - MIT/X11 License
;; Author: Benjamin Levy <blevy@protonmail.com>

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This implements a scrolling algorithm for `good-scroll'
;; based on Bézier curves.
;; This is newer and feels smoother than `good-scroll-linear',
;; but is more complicated.
;; Set `good-scroll-algorithm' to `good-scroll-bezier-position' to enable.

;;; Code:



;;;; General Bézier curve calculations

(defvar good-scroll-bezier--epsilon 0.01)

(defun good-scroll-bezier--calc (tt p1 p2)
  (+ (* 3 (expt (- 1 tt) 2) tt p1)
     (* 3 (- 1 tt) (expt tt 2) p2)
     (expt tt 3)))

(defun good-scroll-bezier--deriv (tt p1 p2)
  (+ (* 3 (expt (- 1 tt) 2) p1)
     (* 6 (- 1 tt) tt (- p2 p1))
     (* 3 (expt tt 2) (- 1 p2))))

(defun good-scroll-bezier--approx-eq (a b)
  (< (abs (- a b))
     good-scroll-bezier--epsilon))

(defun good-scroll-bezier--t-given-x (x x1 x2 &optional t-min t-max)
  (let* (
         (t-min (or t-min 0.0))
         (t-max (or t-max 1.0))
         (t-mid (/ (+ t-min t-max) 2))
         (x-guess (good-scroll-bezier--calc t-mid x1 x2)))
    (cond
     ((good-scroll-bezier--approx-eq x-guess x) t-mid)
     ((< x-guess x) (good-scroll-bezier--t-given-x x x1 x2 t-mid t-max))
     (t (good-scroll-bezier--t-given-x x x1 x2 t-min t-mid)))))



;;; Integration with `good-scroll'

(defvar good-scroll-bezier--x1 nil)
(defvar good-scroll-bezier--y1 nil)
(defvar good-scroll-bezier--x2 nil)
(defvar good-scroll-bezier--y2 nil)

(defvar good-scroll-bezier--prev-time nil)

(defun good-scroll-bezier--set-pivots (velocity position)
  (let* (
         (slope (/ (* velocity good-scroll-duration)
                   good-scroll-destination))
         (normalization (sqrt (+ 1.0 (expt slope 2))))
         (dt (* (/ 1.0 normalization) 0.25))
         (dxy (* (/ slope normalization) 0.25)))
    (setq good-scroll-bezier--x1 dt
          good-scroll-bezier--y1 dxy
          good-scroll-bezier--x2 0.6
          good-scroll-bezier--y2 1.0)))

(defun good-scroll-bezier--velocity-at (fraction-done)
  (let* (
         (tt (good-scroll-bezier--t-given-x fraction-done
                                            good-scroll-bezier--x1
                                            good-scroll-bezier--x2))
         (dt (good-scroll-bezier--deriv tt
                                        good-scroll-bezier--x1
                                        good-scroll-bezier--x2))
         (dxy (good-scroll-bezier--deriv tt
                                         good-scroll-bezier--y1
                                         good-scroll-bezier--y2))
         (slope (/ dxy dt))) ; TODO make sure dt != 0
    (/ (* slope (+ good-scroll-traveled
                   good-scroll-destination))
       good-scroll-duration)))

(defun good-scroll-bezier--position (fraction-done)
  (let* (
         (tt (good-scroll-bezier--t-given-x fraction-done
                                            good-scroll-bezier--x1
                                            good-scroll-bezier--x2))
         (progress (good-scroll-bezier--calc tt
                                             good-scroll-bezier--y1
                                             good-scroll-bezier--y2)))
    (round (- (* progress (+ good-scroll-traveled
                             good-scroll-destination))
              good-scroll-traveled))))

(defun good-scroll-bezier--update (fraction-done)
  (let ((velocity (if good-scroll-bezier--x1
                      (good-scroll-bezier--velocity-at fraction-done)
                    0.0)))
    (good-scroll-bezier--set-pivots velocity 0.0)))

(defun good-scroll-bezier (fraction-done)
  (unless (equal good-scroll-bezier--prev-time
                 good-scroll-start-time)
    (good-scroll-bezier--update fraction-done)
    (setq good-scroll-bezier--prev-time
          good-scroll-start-time))
  (good-scroll-bezier--position fraction-done))



(provide 'good-scroll-bezier)

;;; good-scroll-bezier.el ends here
