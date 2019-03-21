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

(defun epcl--success-p (ret)
  (eq (car ret) :success))

(defmacro epcl--success (point &rest vs)
  `(list :success ,point ,@vs))

(defmacro epcl--success1 (point vs)
  `(cons :success (cons ,point ,vs)))

(defmacro epcl--failed (point)
  `(list :failed ,point))

(defmacro epcl--point (ret)
  `(cadr ,ret))

(defmacro epcl--list (ret)
  `(cddr ,ret))

(defun epcl-regexp (regexp)
  (lambda (point)
    (goto-char point)
    (if (re-search-forward (concat "\\=" regexp) nil t)
	(epcl--ret (point) (match-string 0))
      (epcl--failed point))))

(defun epcl-token (p)
  (let ((parser (epcl-seq
		 (epcl-regexp "\\s-*") p)
		)
	)
    (lambda (point)
      (let ((ret (funcall parser point)))
	(if (epcl--success-p ret)
	    (epcl--success (epcl--point ret)
			   (cadr (epcl--list ret)))
	  (epcl--failed
	   (epcl--point ret)))))))
	  

(defun epcl-seq (&rest ps)
  (lambda (point)
    (let ((pos point)
	  (ret nil)
	  (done nil)
	  (ps2 ps))
      (while (and (not done)
		  ps2)
	(let* ((p (car ps2))
	       (r (funcall p pos))
	       (pos2 (epcl--point r))
	       (vs  (epcl--list r)))
	  (if (epcl--success-p r)
	      (progn
		(setq pos pos2)
		(setq ret (cons vs ret))
		(setq ps2 (cdr ps2)))
	    (progn
	      (setq ret nil)
	      (setq pos pos2)
	      (setq done t))
	    )
	  )
	)
      (if ret
	  (epcl--success1 pos
			  (apply #'append (reverse ret)))
	(epcl--failed pos)
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
	  (if (epcl--success-p r)
	      (progn (setq ret r)
		     (setq done t))
	    (let ((pos (epcl--point r)))
	      (if (/= pos point)
		  (progn
		    (setq ret (epcl-failed pos))
		    (setq done t))))))
	(setq ps2 (cdr ps2)))
      ret
      )
    )
  )


(defun epcl-bind (p action)
  (lambda (point)
    (let ((r (funcall p point)))
      (if (epcl--success-p r)
	  (let ((pos (epcl--point r))
		(vs  (epcl--list  r)))
	    (epcl--success
	     pos
	     (apply action vs)))
	(epcl--failed point)))))
	    

(defun epcl-apply (p)
  (save-excursion
    (funcall p (point))
    )
  )


(provide 'epcl)
