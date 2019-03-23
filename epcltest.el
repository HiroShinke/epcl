

;; (require 'epcl)
(load "epcl")


(ert-deftest basic-test ()
    (should
     (equal
      (epcl-ret-success 2 "a")
      (let* ((a (epcl-regexp "a")))
	(epcl-parse-string a "a")
	)
      )
     )
    (should
     (equal
      (epcl-ret-success 4 '("a" "a" "a"))
      (let* ((a (epcl-regexp "a"))
	     (aaa (epcl-seq a a a)))
	(epcl-parse-string a "a")
	(epcl-parse-string aaa "aaa")
	)
      )
     )
    )

(ert-deftest let-test ()
  (should
   (equal
    (epcl-ret-success 4 '("a" "a"))
    (let* ((a (epcl-regexp "a"))
	   (b (epcl-regexp "b"))
	   (p (epcl-let ((x a)
			 (nil b)
			 (y a))
			(list x y))))
      (epcl-parse-string p "aba"))
    )
   )
  )

(ert-deftest token-test ()
  (should
   (equal
    (epcl-ret-success 6 "a")
    (let* ((a (epcl-token (epcl-regexp "a"))))
      (epcl-parse-string a "    a"))
    )
   )
  )

(ert-deftest bind-test ()
  (should (equal
	   (let* ((a (epcl-token (epcl-regexp "[[:digit:]]+"))))
	     (epcl-parse-string a "    10"))
	   (epcl-ret-success 7 "10")))
  (should (equal
	   (let* ((a (epcl-token (epcl-regexp "[[:digit:]]+")))
		  (i (epcl-bind a #'string-to-number)))
	     (epcl-parse-string i "    10"))
	   (epcl-ret-success 7 10)))
  )  


(ert-deftest seq-test ()
    (should (equal
	     (epcl-ret-success 11 '("a" "b" "c"))
	     (let* ((a (epcl-token (epcl-regexp "a")))
		    (b (epcl-token (epcl-regexp "b")))
		    (c (epcl-token (epcl-regexp "c")))
		    (abc (epcl-seq a b c)))
	     (epcl-parse-string abc "    a b  c"))
	     )
	    )
    )

(ert-deftest or-test ()
    (should (equal
	     (epcl-ret-success 6 "b")
	     (let* ((a (epcl-token (epcl-regexp "a")))
		    (b (epcl-token (epcl-regexp "b")))
		    (c (epcl-token (epcl-regexp "c")))
		    (abc (epcl-or (epcl-try a)
				  (epcl-try b)
				  c)))
	       (epcl-parse-string abc "    b"))
	    )
    )
  )

;; can't test macroexpand because use of interned variable
;; 
;; (ert-deftest epcl-let-test ()
;;   (skip-unless nil)
;;   (should
;;    (equal
;;     '(epcl-bind-seq
;;       (epcl-seq p1
;; 		p2
;; 		p3)
;;       (lambda
;; 	(a epcl-let b)
;; 	(list a b)))
;;     (macroexpand-1
;;      (epcl-let ((a p1)
;; 		p2
;; 		(b p3)
;; 		)
;; 	       (list a b)))
;;     )
;;    )
;;   )

(ert-deftest let-apply-test ()
  (should
   (equal
    (epcl-ret-success 4 (list "a" "c"))
    (let* ((p (epcl-regexp "a"))
	   (q (epcl-regexp "b"))
	   (r (epcl-regexp "c"))
	   (x (epcl-let ((a p)
			 q
			 (b r)
			 )
			(list a b))))
      (epcl-parse-string x "abc"))
    )
   )
  )





