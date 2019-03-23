

;; (require 'epcl)
(load "epcl")

(defvar abc nil)
(defvar abc2 nil)

(ert-deftest basic-test ()
  (should
   (equal
    (epcl--success 3 "a" "a")
    (let* ((a (epcl-regexp "a"))
	   (aa (epcl-seq a a)))
      (epcl-parse-string aa "aa")
      )
    )
   ))


(ert-deftest token-test ()
  (should (equal
	   (let* ((a (epcl-token (epcl-regexp "[[:digit:]]+")))
		  (i (epcl-bind a #'string-to-number)))
	     (epcl-parse-string i "    10"))
	   (epcl--success 7 10)))
  (should (equal
	   (let* ((a (epcl-token (epcl-regexp "[[:digit:]]+"))))
	     (epcl-parse-string a "    10"))
	   (epcl--success 7 "10")))
  )  

(ert-deftest epcl-seq-test ()
  (let* ((a (epcl-token (epcl-regexp "a")))
	 (b (epcl-token (epcl-regexp "b")))
	 (c (epcl-token (epcl-regexp "c")))
	 (abc (epcl-seq a b c)))
    (should (equal
	     (epcl-parse-string abc "    a b  c")
	     (epcl--success 11 "a" "b" "c")))
    )
  )

(ert-deftest epcl-or-test ()

  (let* ((a (epcl-token (epcl-regexp "a")))
	 (b (epcl-token (epcl-regexp "b")))
	 (c (epcl-token (epcl-regexp "c")))
	 (abc (epcl-or (epcl-try a)
		       (epcl-try b)
		       c)))
    (should (equal
	     (epcl-parse-string abc "    b")
	     (epcl--success 6 "b"))
	    )
    )
  )

(ert-deftest epcl-let-test ()
  (should
   (equal
    '(epcl-bind
      (epcl-seq p1
		(epcl-discard p2)
		p3)
      (lambda
	(a b)
	(list a b)))
    (macroexpand-1
     '(epcl-let ((a p1)
		 p2
		 (b p3)
		 )
		(list a b)))
    )
   )
  )

(ert-deftest epcl-let-apply-test ()
  (should
   (equal
    (epcl--success 4 (list "a" "c"))
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





