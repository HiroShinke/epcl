;; -*- lexical-binding: t -*-

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
  (should
   (equal
    (epcl-ret-success 11 '("a" "b" "c"))
    (let* ((a (epcl-token (epcl-regexp "a")))
	   (b (epcl-token (epcl-regexp "b")))
	   (c (epcl-token (epcl-regexp "c")))
	   (abc (epcl-seq a b c)))
      (epcl-parse-string abc "    a b  c"))
    )
   )
  (should
   (equal
    (epcl-ret-failed 8)
    (let* ((a (epcl-token (epcl-regexp "a")))
	   (b (epcl-token (epcl-regexp "b")))
	   (c (epcl-token (epcl-regexp "c")))
	   (abc (epcl-seq a b c)))
      (epcl-parse-string abc "    a b  d"))
    )
   )
  )

(ert-deftest or-test ()
  (should
   (equal
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
  (should
   (equal
    (epcl-ret-success 6 "b")
    (let* ((a (epcl-token (epcl-regexp "a")))
	   (b (epcl-token (epcl-regexp "b")))
	   (c (epcl-token (epcl-regexp "c")))
	   (abc (epcl-or a
			 b
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


(ert-deftest many-test ()
  (should
   (equal
    (epcl-ret-success 7 '("a" "a" "a"))
    (let* ((a (epcl-token (epcl-regexp "a")))
	   (m (epcl-many a)))
      (epcl-parse-string m " a a a"))
    )
   )
  (should
   (equal
    (epcl-ret-success 1 '())
    (let* ((a (epcl-token (epcl-regexp "a")))
	   (m (epcl-many a)))
      (epcl-parse-string m " b"))
    )
   )
  )

(ert-deftest many-1-test ()
  (should
   (equal
    (epcl-ret-success 7 '("a" "a" "a"))
    (let* ((a (epcl-token (epcl-regexp "a")))
	   (m (epcl-many-1 a)))
      (epcl-parse-string m " a a a"))
    )
   )
  (should
   (equal
    (epcl-ret-failed 1)
    (let* ((a (epcl-token (epcl-regexp "a")))
	   (m (epcl-many-1 a)))
      (epcl-parse-string m "bbb"))
    )
   )
  )

(ert-deftest chainl-1-test ()
  (should
   (equal
    (epcl-ret-success 10 15)
    (let* ((a  (epcl-token (epcl-regexp "[[:digit:]]+")))
	   (d  (epcl-bind a #'string-to-number))
	   (plus (epcl-token (epcl-regexp "\\+")))
	   (op (epcl-bind  plus (lambda (n) #'+) ))
	   (c  (epcl-chainl-1 d op)))
      (epcl-parse-string c "1+2+3+4+5"))
    )
   )
  )

(ert-deftest chainr-1-test ()
  (should
   (equal
    (epcl-ret-success 10 15)
    (let* ((a  (epcl-token (epcl-regexp "[[:digit:]]+")))
	   (d  (epcl-bind a #'string-to-number))
	   (plus (epcl-token (epcl-regexp "\\+")))
	   (op (epcl-bind  plus (lambda (n) #'+) ))
	   (c  (epcl-chainr-1 d op)))
      (epcl-parse-string c "1+2+3+4+5"))
    )
   )
  )

(ert-deftest paren-test ()
  (should
   (equal
    (epcl-ret-success 6 "a")
    (let* ((po (epcl-regexp "("))
	   (p  (epcl-token (epcl-regexp "a")))
	   (pc (epcl-regexp ")"))
	   (a  (epcl-paren po p pc)))
      (epcl-parse-string a "(  a)"))
    )
   )
  )


(ert-deftest option-test ()
  (should
   (equal
    (epcl-ret-success 2 "a")
    (let* ((a  (epcl-regexp "a"))
	   (o  (epcl-option a)))
      (epcl-parse-string o "a")
      )
    )
   )
  (should
   (equal
    (epcl-ret-success 1 nil)
    (let* ((a  (epcl-regexp "a"))
	   (o  (epcl-option a)))
      (epcl-parse-string o "b")
      )
    )
   )
  )

(ert-deftest lookahead-test ()
  (should
   (equal
    (epcl-ret-success 2 '("a" "b"))
    (let* ((a (epcl-regexp "a"))
	   (b (epcl-regexp "b"))
	   (lb (epcl-lookahead b))
	   (p  (epcl-seq a lb)))
      (epcl-parse-string p "ab")
      )
    )
   )
  (should
   (equal
    (epcl-ret-failed 2)
    (let* ((a (epcl-regexp "a"))
	   (b (epcl-regexp "b"))
	   (lb (epcl-lookahead b))
	   (p  (epcl-seq a lb)))
      (epcl-parse-string p "aa")
      )
    )
   )
  )

(ert-deftest end-by-test ()
  (should
   (equal
    (epcl-ret-success 5 '("a" "a" "a"))
    (let* ((a (epcl-regexp "a"))
	   (b (epcl-regexp "b"))
	   (p (epcl-end-by a b)))
      (epcl-parse-string p "aaab")
      )
    )
   )
  (should
   (equal
    (epcl-ret-success 2 '())
    (let* ((a (epcl-regexp "a"))
	   (b (epcl-regexp "b"))
	   (p (epcl-end-by a b)))
      (epcl-parse-string p "b")
      )
    )
   )
  )

(ert-deftest end-by-1-test ()
  (should
   (equal
    (epcl-ret-success 5 '("a" "a" "a"))
    (let* ((a (epcl-regexp "a"))
	   (b (epcl-regexp "b"))
	   (p (epcl-end-by-1 a b)))
      (epcl-parse-string p "aaab")
      )
    )
   )
  (should
   (equal
    (epcl-ret-failed 1)
    (let* ((a (epcl-regexp "a"))
	   (b (epcl-regexp "b"))
	   (p (epcl-end-by-1 a b)))
      (epcl-parse-string p "b")
      )
    )
   )
  )

(ert-deftest sep-end-by-1-test ()
  (should
   (equal
    (epcl-ret-success 7 '("a" "a" "a"))
    (let* ((a (epcl-regexp "a"))
	   (b (epcl-regexp "b"))
	   (p (epcl-sep-end-by-1 a b)))
      (epcl-parse-string p "ababab")
      )
    )
   )
  (should
   (equal
    (epcl-ret-success 6 '("a" "a" "a"))
    (let* ((a (epcl-regexp "a"))
	   (b (epcl-regexp "b"))
	   (p (epcl-sep-end-by-1 a b)))
      (epcl-parse-string p "ababa")
      )
    )
   )
  (should
   (equal
    (epcl-ret-failed 1)
    (let* ((a (epcl-regexp "a"))
	   (b (epcl-regexp "b"))
	   (p (epcl-sep-end-by-1 a b)))
      (epcl-parse-string p "b")
      )
    )
   )
  )

(ert-deftest sep-end-by-test ()
  (should
   (equal
    (epcl-ret-success 7 '("a" "a" "a"))
    (let* ((a (epcl-regexp "a"))
	   (b (epcl-regexp "b"))
	   (p (epcl-sep-end-by a b)))
      (epcl-parse-string p "ababab")
      )
    )
   )
  (should
   (equal
    (epcl-ret-success 6 '("a" "a" "a"))
    (let* ((a (epcl-regexp "a"))
	   (b (epcl-regexp "b"))
	   (p (epcl-sep-end-by a b)))
      (epcl-parse-string p "ababa")
      )
    )
   )
  (should
   (equal
    (epcl-ret-success 1 '())
    (let* ((a (epcl-regexp "a"))
	   (b (epcl-regexp "b"))
	   (p (epcl-sep-end-by a b)))
      (epcl-parse-string p "b")
      )
    )
   )
  )


(ert-deftest not-followed-test ()
  (should
   (equal
    (epcl-ret-failed 2)
    (let* ((a (epcl-regexp "a"))
	   (b (epcl-regexp "b"))
	   (nb (epcl-not-followed b))
	   (p  (epcl-seq a nb)))
      (epcl-parse-string p "ab")
      )
    )
   )
  (should
   (equal
    (epcl-ret-success 2 '("a" nil))
    (let* ((a (epcl-regexp "a"))
	   (b (epcl-regexp "b"))
	   (nb (epcl-not-followed b))
	   (p  (epcl-seq a nb)))
      (epcl-parse-string p "aa")
      )
    )
   )
  )

(ert-deftest sep-by-test ()
  (should
   (equal
    (epcl-ret-success 6 '("a" "a" "a"))
    (let* ((a (epcl-regexp "a"))
	   (c (epcl-regexp "c"))
	   (p (epcl-sep-by a c)))
      (epcl-parse-string p "acaca")
      )
    )
   )
  (should
   (equal
    (epcl-ret-success 2 '("a"))
    (let* ((a (epcl-regexp "a"))
	   (c (epcl-regexp "c"))
	   (p (epcl-sep-by a c)))
      (epcl-parse-string p "ab")
      )
    )
   )
  (should
   (equal
    (epcl-ret-success 1 '())
    (let* ((a (epcl-regexp "a"))
	   (c (epcl-regexp "c"))
	   (p (epcl-sep-by a c)))
      (epcl-parse-string p "b")
      )
    )
   )
  )

(ert-deftest lazy-test ()
  (should
   (equal
    (epcl-ret-success 2 "a")
    (let* ((a (epcl-regexp "a"))
	   (p (epcl-lazy a)))
      (epcl-parse-string p "a")
      )
    )
   )
  (should
   (equal
    (epcl-ret-success 2 "a")
    (let* ((a nil)
	   (p (epcl-lazy a)))
      (setq a (epcl-regexp "a"))
      (epcl-parse-string p "a")
      )
    )
   )
  )

(ert-deftest bind-m-test ()
  (should
   (equal
    (epcl-ret-success 3 "b")
    (let* ((a (epcl-regexp "a"))
	   (b (epcl-regexp "b"))
	   (p (epcl-bind-m
	       a
	       (lambda (n) b))))
      (epcl-parse-string p "ab"))
    )
   )
  (should
   (equal
    (epcl-ret-success 3 "b")
    (let* ((a (epcl-regexp "a"))
	   (b (epcl-regexp "b"))
	   (c (epcl-regexp "c"))
	   (p (epcl-bind-m
	       a
	       (lambda (n) (if (equal n "a")
			       b c)))))
      (epcl-parse-string p "ab"))
    )
   )
  )    


(ert-deftest epcl-let*-test ()
  (should
   (equal
    (epcl-ret-success 3 "x")
    (let ((p (epcl-let* ((a (epcl-regexp "a"))
			 (b (epcl-regexp "b")))
			"x")))
      (epcl-parse-string p "ab"))
    )
   )
  (should
   (equal
    (epcl-ret-success 3 '("a" "b"))
    (let ((p (epcl-let* ((a (epcl-regexp "[abc]"))
			 (x (if (equal a "a") 
				(epcl-regexp "b") (epcl-regexp "c"))))
			(list a x))))
      (epcl-parse-string p "ab"))
    )
   )
  (should
   (equal
    (epcl-ret-success 3 '("b" "c"))
    (let ((p (epcl-let* ((a (epcl-regexp "[abc]"))
			 (x (if (equal a "a") 
				(epcl-regexp "b") (epcl-regexp "c"))))
			(list a x))))
      (epcl-parse-string p "bc"))
    )
   )
  (should
   (equal
    (epcl-ret-failed 2)
    (let ((p (epcl-let* ((a (epcl-regexp "[abc]"))
			 (x (if (equal a "a") 
				(epcl-regexp "b") (epcl-regexp "c"))))
			(list a x))))
      (epcl-parse-string p "ac"))
    )
   )
)



