

(require 'epcl)
(load "epcl")

(defun test-parser (p str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (epcl-apply p)))

(defvar abc nil)
(defvar abc2 nil)

(setq abc
      (let* ((a (epcl-token (epcl-regexp "a")))
	     (b (epcl-token (epcl-regexp "b")))
	     (c (epcl-token (epcl-regexp "c")))
	     (abc (epcl-seq a b c)))
	abc))

(setq abc2
      (let* ((a (epcl-token (epcl-regexp "a")))
	     (b (epcl-token (epcl-regexp "b")))
	     (c (epcl-token (epcl-regexp "c")))
	     (abc (epcl-or a b c)))
	abc))

(test-parser abc "   a  b c")
(test-parser abc2 "   b")


(let* ((a (epcl-token (epcl-regexp "[[:digit:]]+")))
       (i (epcl-bind a #'string-to-number)))
  (test-parser i "    10"))

(let* ((a (epcl-token (epcl-regexp "[[:digit:]]+"))))
  (test-parser a "    10"))

(let* ((a (epcl-regexp "[[:digit:]]+")))
    (test-parser a "10"))

(cl-defstruct token x y z)

(setq x (make-token :x 10 :y 20 :z 30))

(token-x x)
(token-y x)
