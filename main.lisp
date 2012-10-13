(in-package #:parse-number-range)

(defun parse (range &aux
              (from 0) from-p
              (to nil) to-p
              (by 1) by-p
              direction (limit-kind :unbounded))
  '(values from to limit-kind step direction)
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
