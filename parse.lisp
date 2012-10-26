(in-package #:parse-number-range)

(defun parse (range &key
              (keyword-policy :strict)
              (extrasp nil)
              (clause-kinds-p extrasp)
              (clause-keywords-p extrasp)
              (clauses-alist-p extrasp))
  '(values from to limit-kind by direction
    &key clause-kinds clause-keywords clauses-alist)
  (multiple-value-bind (process-key-value
                        finish-key-value
                        process-extras
                        finish-extras)
      (multiple-value-call #'values
        (%make-key-value-processor
         (lambda (kind)
           (error "Duplicate specification of kind ~S in range ~S."
                  kind range)))
        (%make-extras-processor clause-kinds-p
                                clause-keywords-p
                                clauses-alist-p))
    (let ((key-transform
           (ecase keyword-policy
             (:strict #'identity)
             (:loose (let ((keyword-package (find-package '#:keyword)))
                       (lambda (key)
                         (intern (symbol-name key) keyword-package)))))))
      (map-bind (%map-plist) (((key value) range))
        (setf key (funcall key-transform key))
        (let ((kind (funcall process-key-value key value)))
          (funcall process-extras kind key))))
    (multiple-value-call #'values
      (funcall finish-key-value)
      (funcall finish-extras))))

(defun unparse (from to limit-kind by direction &key clause-kinds)
  (multiple-value-bind (from-keyword to-keyword)
      (flags-to-keywords direction limit-kind)
    ;; Backquote indented badly...
    (let ((from (list from-keyword from))
          (to (when to-keyword
                (list to-keyword to)))
          (by (when (and by (/= by 1))
                (list :by by))))
      (flet ((maybe (kind list)
               (unless (member kind clause-kinds :test #'eq)
                 list)))
        (nconc (maybe :from from) (maybe :to to) (maybe :by by)
               (map-bind (mapcan) ((kind clause-kinds))
                 (ecase kind
                   (:from from)
                   (:to to)
                   (:by by))))))))

(defun canonicalize (range &key
                     (clause-kinds :preserve)
                     (keyword-policy :strict))
  (multiple-value-&bind (from to limit-kind by direction
                              &key ((:clause-kinds parsed-clause-kinds)))
      (parse range
             :clause-kinds-p (eq clause-kinds :preserve)
             :keyword-policy keyword-policy)
    (unparse from to limit-kind by direction
             :clause-kinds (if (eq clause-kinds :preserve)
                               parsed-clause-kinds
                               clause-kinds))))
