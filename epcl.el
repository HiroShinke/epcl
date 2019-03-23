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
  (epcl-try
   (epcl-seq
    (epcl-discard (epcl-regexp "\\s-*"))
    p)))


(defun epcl-seq (&rest ps)
  (epcl-bind1
   (epcl--seq ps)
   (lambda (vs)
     (apply #'append vs))))


(defun epcl--seq (ps)
  " ps : parser list.
    (epcl--seq ps) return each return value of parser as separate values in a list.
    don't append these as like epcl-seq do.
  "
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
	  (epcl--success
	   pos
	   (reverse ret))
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
		    (setq ret (epcl--failed pos))
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
      (if (epcl--success-p r)
	  r
	(epcl--failed point)))))

(defun epcl-discard (p)
  (lambda (point)
    (let ((r (funcall p point)))
      (if (epcl--success-p r)
	  (epcl--success (epcl--point r))
	(epcl--failed point)))))

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

(defun epcl-bind1 (p action)
  (lambda (point)
    (let ((r (funcall p point)))
      (if (epcl--success-p r)
	  (let ((pos (epcl--point r))
		(vs  (epcl--list  r)))
	    (epcl--success1
	     pos
	     (apply action vs)))
	(epcl--failed point)))))

(defmacro epcl-let (arglist &rest body)
  (let ((vs (mapcar #'car (seq-filter #'listp arglist)))
	(ps (mapcar (lambda (e)
		      (if (listp e)
			  (cadr e)
			`(epcl-discard ,e))) arglist)))
    `(epcl-bind
      (epcl-seq ,@ps)
      (lambda ,vs ,@body))))

(defmacro epcl-let (arglist &rest body)
  (let ((vs (mapcar #'car (seq-filter #'listp arglist)))
	(ps (mapcar (lambda (e)
		      (if (listp e)
			  (cadr e)
			`(epcl-discard ,e))) arglist)))
    `(epcl-bind
      (epcl-seq ,@ps)
      (lambda ,vs ,@body))))


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
