(in-package #:parse-number-range)

(defun parse (range &aux
              (from 0) from-p
              (to nil) to-p
              (by 1) by-p
              direction (limit-kind :unbounded))
  '(values from to limit-kind by direction)
  (flet ((direction (new-direction)
	   (if direction
	       (unless (eq direction new-direction)
		 (error "Conflicting directions: ~S and ~S."
                        direction new-direction))
	       (setf direction new-direction)))
         (limit-kind (new-limit-kind)
           (if (not (eq limit-kind :unbounded))
               (unless (eq limit-kind new-limit-kind)
                 (error "Conflicting limit-kinds: ~S and ~S."
                        limit-kind new-limit-kind))
               (setf limit-kind new-limit-kind)))
	 (duplicate (type)
	   (error "Duplicate ~A specification in range ~S."
		  type range)))
    (do ((tail range (cddr tail)))
        ((endp tail) (values from to limit-kind by (or direction '+)))
      (destructuring-bind (key value &rest rest) tail
        (declare (ignore rest))
        (ecase key
          ((:from :downfrom :upfrom)
           (when from-p
             (duplicate "FROM"))
           (setf from value
                 from-p t)
           (ecase key
             (:from)
             (:downfrom (direction '-))
             (:upfrom (direction '+))))
          ((:to :downto :upto :below :above)
           (when to-p
             (duplicate "TO"))
           (setf to value
                 to-p t)
           (ecase key
             (:to (limit-kind :inclusive))
             (:downto (direction '-) (limit-kind :inclusive))
             (:upto (direction '+) (limit-kind :inclusive))
             (:below (direction '+) (limit-kind :exclusive))
             (:above (direction '-) (limit-kind :exclusive))))
          (:by
           (when by-p
             (duplicate "BY"))
           (setf by value
                 by-p t)))))))

(defun %direction/limit-to-keywords (direction limit-kind)
  '(values from-keyword to-keyword)
  (cartesian-product-switch:cartesian-product-switch
      ((ecase direction
         + -)
       (ecase limit-kind
         :unbounded :inclusive :exclusive))
    ;; +
    (values :from nil)
    (values :from :to)
    (values :from :below)
    ;; -
    (values :downfrom nil)
    (values :from :downto)
    (values :from :above)))

(defun unparse (from to limit-kind by direction)
  (multiple-value-bind (from-keyword to-keyword)
      (%direction/limit-to-keywords direction limit-kind)
    ;; Backquote indented badly...
    (nconc (list from-keyword from)
           (when to-keyword
             (list to-keyword to))
           (when (and by (/= by 1))
             (list :by by)))))
