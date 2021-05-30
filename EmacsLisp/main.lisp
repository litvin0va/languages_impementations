;;
;; The program for finding the root of the equation f (x) = 0
;; in a given interval ab with an accuracy of eps by
;; the method of quadratic interpolation.
;; If successful, the function displays the root and
;; the number of iterations spent looking for it.
;; Otherwise the error code.
;;
;; Run: clisp main.lisp
;;


(defun f (x)
  (- (* x x) 4))

(defun inter (x1 x2 x3 y1 y2 y3)
    (setf E 1e-15)
    (setf y11 (- (* y2 x1) (* y1 x2)))
    (if (< (abs (- x1 x2)) E)
        (return-from inter (values 0 0))
    )
    (setf y12 (/ y11 (- x1 x2)))

    (setf y21 (- (* y3 x2) (* y2 x3)))
    (if (< (abs (- x3 x2)) E)
        (return-from inter (values 0 0))
    )
    (setf y22 (/ y21 (- x2 x3)))

    (setf y31 (- (* y22 x1) (* y12 x3)))
    (if (< (abs (- x1 x3)) E)
        (return-from inter (values 0 0))
    )
    (setf y32 (/ y31 (- x1 x3)))
    (return-from inter (values 1 y32))
)

(defun solve (a b eps)
  (setf x1 a)
  (setf x2 b)
  (setf x3 (/ (+ a b) 2))
  (setf y1 (f x1))
  (setf y2 (f x2))
  (setf y3 (f x3))
  (if (< (abs y1) eps)
    (progn
        (return-from solve (values 1 x1))
    )
  )
   (if (< (abs y2) eps)
    (progn
        (return-from solve (values 1 x2))
    )
  )
   (if (< (abs y3) eps)
    (progn
        (return-from solve (values 1 x2))
    )
  )

  (dotimes (it 1234)
    (setf (values ret xn) (inter y1 y2 y3 x1 x2 x3))
    (if (< ret 1)
    (return-from solve (values -1 0))
    )
    (setf yn (f xn))

    (setf xn (coerce xn 'double-float))
    (setf yn (coerce yn 'double-float))

    (if (< (abs yn) eps)
        (return-from solve (values it xn))
    )
    (if (>= (abs yn) (abs y1))
        (if (>= (abs yn)  (abs y2))
          (if (>= (abs yn) (abs y3))
            (return-from solve (values -2 0))
            (progn
              (setf x3 xn)
              (setf y3 yn)
            )
          )
          (if (>= (abs y2) (abs y3))
            (progn
              (setf x2 xn)
              (setf y2 yn)
            )
            (progn
              (setf x3 xn)
              (setf y3 yn)
            )
          )
        )
        (if (>= (abs y1) (abs y2))
          (if (>= (abs y1) (abs y3))
            (progn
              (setf x1 xn)
              (setf y1 yn)
            )
            (progn
              (setf x3 xn)
              (setf y3 yn)
            )
          )
          (if (>= (abs y2) (abs y3))
            (progn
              (setf x2 xn)
              (setf y2 yn)
            )
            (progn
              (setf x3 xn)
              (setf y3 yn)
            )
          )
        )
      )
  )
  (return-from solve (values -3 0))
)


(defun main ()
(terpri)
(princ "Input a: ")
(setq a (read))
(princ "Input b: ")
(setq b (read))
(princ "Input eps: ")
(setq eps (read))
(setf (values ret ans) (solve a b eps))
(terpri)
(princ "Ret: ")
(write ret)
(terpri)
(princ "Ans: ")
(write (coerce ans 'double-float))
)
(main)


