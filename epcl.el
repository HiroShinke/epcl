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
