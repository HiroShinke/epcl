

;;(require 'epcl)

(load "epcl")

(defun token-regexp (str)
;;  (epcl-debug (format "token, str=%s" str)
  (epcl-token (epcl-regexp str))
;;  )
  )

(defun epcl-calc ()

  " expr    = term   `chainl1` addop
    term    = factor `chainl1` mulop
    factor  = parens expr <|> integer
   
    mulop   =   do{ symbol \"*\"; return (*)}
    <|> do{ symbol \"/\"; return (div) }
   
    addop   =   do{ symbol \"+\"; return (+) }
    <|> do{ symbol \"-\"; return (-) }
   "
  (let* (
	 (multop (epcl-or
		  (epcl-let ((nil (token-regexp "\\*"))) #'*)
		  (epcl-let ((nil (token-regexp "/"))) #'/)))
	 
	 (addop  (epcl-or
		  (epcl-let ((nil (token-regexp "\\+"))) #'+)
		  (epcl-let ((nil (token-regexp "-"))) #'-)))
	 
	 (digit (epcl-bind (token-regexp "[[:digit:]]+")
			   #'string-to-number))
	 (factor (epcl-or
		  (epcl-paren (token-regexp "(")
			      (epcl-lazy expr)
			      (token-regexp ")"))
		  digit))

	 (term (epcl-chainl-1 factor multop))
	 (expr (epcl-chainl-1 term addop))
	 )
    (epcl-apply expr)
    )
  )

(defun epcl-calc-print ()
  (interactive)
  (message (format "ret=%s" (epcl-ret-value (epcl-calc))))
  )

(defun epcl-calc-string (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (epcl-calc)
    ))


(ert-deftest calc1 ()
  (should (equal 100 (epcl-ret-value (epcl-calc-string "100"))))
  (should (equal 3 (epcl-ret-value (epcl-calc-string "1 + 2"))))
  (should (equal 2 (epcl-ret-value (epcl-calc-string "3 - 1"))))
  (should (equal 2 (epcl-ret-value (epcl-calc-string "4 / 2"))))
  (should (equal 8 (epcl-ret-value (epcl-calc-string "4 * 2"))))
  (should (equal 7 (epcl-ret-value (epcl-calc-string "1 + 2 * 3"))))
  (should (equal 9 (epcl-ret-value (epcl-calc-string "(1 + 2) * 3"))))
  (should (equal 8 (epcl-ret-value (epcl-calc-string " 2 * (1 + 3)"))))
  )


