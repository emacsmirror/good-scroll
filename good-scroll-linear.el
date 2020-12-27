;;; good-scroll-linear.el --- Linear scrolling algorithm -*- lexical-binding: t; -*-

(defun good-scroll-linear (fraction-done)
  (round (- (* fraction-done
               (+ good-scroll-traveled
                  good-scroll-destination))
            good-scroll-traveled)))

(provide 'good-scroll-linear)

;;; good-scroll-linear.el ends here
