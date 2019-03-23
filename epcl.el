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

(defmacro epcl--success (point v)
  `(list :success ,point ,v))

(defmacro epcl--failed (point)
  `(list :failed ,point))

(defmacro epcl--point (ret)
  `(cadr ,ret))

(defmacro epcl--value (ret)
  `(caddr ,ret))

(defun epcl-regexp (regexp)
  (lambda (point)
    (goto-char point)
    (if (re-search-forward (concat "\\=" regexp) nil t)
	(epcl--success (point) (match-string 0))
      (epcl--failed point))))

(defun epcl-token (p)
  (epcl-try
   (epcl-let
    ((x (epcl-regexp "\\s-*"))
     (v p)
     )
    v)))


(defun epcl-seq (&rest ps)

  (lambda (point)
    (let ((pos point)
	  (ret nil)
	  (done nil)
	  (ps2 ps))
      (while (and (not done)
		  ps2)

	(message (format "ret=%s" ret))

	(let* ((p (car ps2))
	       (r (funcall p pos))
	       (pos2 (epcl--point r))
	       (vs  (epcl--value r)))
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
	  (epcl--success pos (reverse ret))
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

(defun epcl-bind (p action)
  (lambda (point)
    (let ((r (funcall p point)))
      (if (epcl--success-p r)
	  (let ((pos (epcl--point r))
		(vs  (epcl--value  r)))
	    (epcl--success
	     pos
	     (funcall action vs)))
	(epcl--failed point)))))


(defun epcl-bind-seq (p action)
  (epcl-bind
   p
   (lambda (x)
     (apply action x))))

(defmacro epcl-let (arglist &rest body)
  (let ((vs (mapcar
	     (lambda (e)
	       (if (listp e)
		   (car e) (intern "epcl-let")))
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
