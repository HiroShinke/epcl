;; -*- lexical-binding: t -*-
;;; parser combinator for emacs


(defmacro save-point (&rest body)
  (let ((ret (intern "ret"))
	(point (intern "point")))
    `(let ((,point (point))
	   (,ret (progn
		   ,@body)))
       (goto-char ,point)
       ,ret
       )))

(defmacro epcl--success (point &rest vs)
  `(list :success ,point ,@vs))

(defmacro epcl--error (point)
  `(list :error ,point))

(defmacro epcl--ret (point &rest vs)
  `(list ,point ,@vs))

(defmacro epcl--ret1 (point vs)
  `(list ,point ,@vs))

(defmacro epcl--point (ret)
  `(car ,ret))

(defmacro epcl--list (ret)
  `(cdr ,ret))

(defun epcl-regexp (regexp)
  (lambda (point)
    (goto-char point)
    (if (re-search-forward (concat "\\=" regexp) nil t)
	(epcl--ret (point) (match-string 0)))))


(defun epcl-token (p)
  (let ((parser (epcl-seq
		 (epcl-regexp "\\s-*") p)
		)
	)
    (lambda (point)
      (let ((ret (funcall parser point)))
	(message (format "ret=%s" ret))
	(if ret 
	    (epcl--ret (epcl--point ret)
		       (cadr (epcl--list ret))))))))

(defun epcl-seq (&rest ps)
  (lambda (point)
    (let ((ret nil)
	  (done nil)
	  (ps2 ps))
      (while (and (not done)
		  ps2)
	(let* ((p (car ps2))
	       (r (funcall p point)))
	  (if r
	      (let ((pos (epcl--point r))
		    (vs  (epcl--list r)))
		(setq point pos)
		(setq ret (cons vs ret))
		(setq ps2 (cdr ps2)))
	    (progn
	     (setq done t)
	     (setq ret nil)))))
      (if ret
	  (epcl--ret1 point
		      (apply #'append (reverse ret)))
	)
      )
    )
  )

(defun epcl-or (&rest ps)
  (lambda (point)
    (let ((ret nil)
	  (done nil)
	  (ps2 ps))
      (while (and (not done)
		  ps2)
	(let* ((p (car ps2))
	       (r (funcall p point)))
	  (if r
	      (progn (setq ret r)
		     (setq done t))
	    (setq ps2 (cdr ps2)))))
      ret
      )
    )
  )

(defun epcl-bind (p action)
  (lambda (point)
    (let ((r (funcall p point)))
      (if r
	  (let ((pos (epcl--point r))
		(vs  (epcl--list  r)))
	    (epcl--ret
	     pos
	     (apply action vs)))))))
	    

(defun epcl-apply (p)
  (save-excursion
    (funcall p (point))
    )
  )


(provide 'epcl)
