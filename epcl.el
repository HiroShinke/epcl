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

(defun epcl-ret-success-p (ret)
  (eq (car ret) :success))

(defmacro epcl-ret-success (point v)
  `(list :success ,point ,v))

(defmacro epcl-ret-failed (point)
  `(list :failed ,point))

(defmacro epcl-ret-point (ret)
  `(cadr ,ret))

(defmacro epcl-ret-value (ret)
  `(caddr ,ret))

(defun epcl-regexp (regexp)
  (lambda (point)
    (goto-char point)
    (if (re-search-forward (concat "\\=" regexp) nil t)
	(epcl-ret-success (point) (match-string 0))
      (epcl-ret-failed point))))

(defun epcl-token (p)
  (epcl-try
   (epcl-let
    ((x (epcl-regexp "\\s-*"))
     (v p)
     )
    v)))


(defun epcl-const (c)
  (lambda (point)
    (epcl-ret-success point c)))

(defun epcl-fail (str)
  (lambda (point)
    (if str
	(message (format "%s at %s" str point))
      )
    (epcl-ret-failed point)))

(defun epcl-debug (label p)
  (lambda (point)
    (let ((r (funcall p point)))
      (message (format "label=%s,ret=%s" label r))
      r)))

(defun epcl-many (p)

  (lambda (point)

    (let ((pos point)
	  (success t)
	  (ret nil)
	  (done nil))
      (while (not done)
	(let* ((r (funcall p pos))
	       (pos2 (epcl-ret-point r))
	       (v    (epcl-ret-value r)))
	  (cond ((epcl-ret-success-p r)
		 (setq pos pos2)
		 (setq ret (cons v ret))
		 )
		((/= pos pos2)
		 (setq done t)
		 (setq pos pos2)
		 (setq success nil)
		 )
		(t
		 (setq done t)
		 (setq pos pos2)
		 ))
	  )
	)
      (if success
	  (epcl-ret-success pos (reverse ret))
	(epcl-ret-failed pos)))
    )
  )

(defun epcl-many1 (p)
  (epcl-let
   ((v p)
    (vs (epcl-many p)))
   (cons v vs)))

(defun epcl-chain (p op)

  (let* ((opp   (epcl-seq op p))
	 (chain (epcl-seq p (epcl-many opp))))

    (epcl-bind-seq
     chain
     (lambda (v opvs)
       (let ((vs (cons v (mapcar #'cadr opvs)))
	     (ops (mapcar #'car opvs)))
	 
	 (while ops
	   (let* ((o  (car ops))
		  (v1 (car vs))
		  (v2 (cadr vs))
		  (v  (funcall o v1 v2)))
	     (setq ops (cdr ops))
	     (setq vs (cons v (cddr vs))))
	   )
	 (car vs)
	 )
       )
     )
    )
  )

(defun epcl-paren (po p pc)
  (epcl-let
   (po
    (x p)
    pc)
   x
   )
  )

(defun epcl-option (p)
  (lambda (point)
    (let ((r (funcall p point)))
      (cond
       ((epcl-ret-success-p r)
	r)
       ((/= (epcl-ret-point r)
	    point)
	(epcl-ret-failed (epcl-ret-point r)))
       (t
	(epcl-ret-success point nil))))))

(defun epcl-lookahead (p)
  (lambda (point)
    (let ((r (funcall p point)))
      (cond
       ((epcl-ret-success-p r)
	(epcl-ret-success point nil))
       (t
	(epcl-ret-failed (epcl-ret-point r)))))))
  
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
	       (pos2 (epcl-ret-point r))
	       (vs  (epcl-ret-value r)))
	  (if (epcl-ret-success-p r)
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
	  (epcl-ret-success pos (reverse ret))
	(epcl-ret-failed pos)
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
	  (if (epcl-ret-success-p r)
	      (progn (setq ret r)
		     (setq done t))
	    (let ((pos (epcl-ret-point r)))
	      (if (/= pos point)
		  (progn
		    (setq ret (epcl-ret-failed pos))
		    (setq done t))))))
	(setq ps2 (cdr ps2)))
      ret
      )
    )
  )

(defmacro epcl-lazy (p0)
  `(epcl--lazy-helper (lambda () ,p0)))
  
(defun epcl--lazy-helper (p0)
  (let ((p nil))
    (lambda (point)      
      (if (not p) (setq p (funcall p0)))
      (funcall p point))))


(defun epcl-try (p)
  (lambda (point)
    (let ((r (funcall p point)))
      (if (epcl-ret-success-p r)
	  r
	(epcl-ret-failed point)))))

(defun epcl-bind (p action)
  (lambda (point)
    (let ((r (funcall p point)))
      (if (epcl-ret-success-p r)
	  (let ((pos (epcl-ret-point r))
		(vs  (epcl-ret-value  r)))
	    (epcl-ret-success
	     pos
	     (funcall action vs)))
	(epcl-ret-failed point)))))


(defun epcl-bind-seq (p action)
  (epcl-bind
   p
   (lambda (x)
     (apply action x))))

(defmacro epcl-let (arglist &rest body)
  (let ((vs (mapcar
	     (lambda (e)
	       (if (and (listp e)
			(car e))
		   (car e)
		 (intern "epcl-let")))
	     arglist))
	(ps (mapcar
	     (lambda (e)
	       (if (listp e)
		   (cadr e)
		 e))
	     arglist))
	)
    `(epcl-bind-seq
      (epcl-seq ,@ps)
      (lambda ,vs ,@body))
    ))

(defun epcl-apply (p)
  (save-excursion
    (funcall p (point))
    )
  )

(defun epcl-parse-string (p str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (epcl-apply p)))


(provide 'epcl)
