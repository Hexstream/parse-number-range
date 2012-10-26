(in-package #:parse-number-range)

(defun %make-group (kind default keyword-data-alist)
  (lambda (duplicate-function)
    '(values accumulate finish)
    (let ((value default)
          (valuep nil))
      (values
       (lambda (key val)
         (let ((data (cdr (assoc key keyword-data-alist))))
           (when data
             (when valuep
               (funcall duplicate-function kind)
               (error "duplicate-function should not return. ~S"
                      duplicate-function))
             (setf (values value valuep) (values val t))
             (values kind data))))
       (lambda ()
         ;; Second value ignored in practice.
         (values value valuep))))))

(defun %make-groups (on-duplicate)
  (flet ((make (kind default keyword-flag-values)
           (funcall (%make-group kind default keyword-flag-values)
                    on-duplicate)))
    (multiple-value-bind (update-from
                          finish-from
                          update-to
                          finish-to
                          update-by
                          finish-by)
        (multiple-value-call #'values
          (make :from 0 '((:from nil nil)
                          (:downfrom - nil)
                          (:upfrom + nil)))
          (make :to nil '((:to nil :inclusive)
                          (:downto - :inclusive)
                          (:upto + :inclusive)
                          (:below + :exclusive)
                          (:above - :exclusive)))
          (make :by 1 '((:by nil nil))))
      (values
       (lambda (key value)
         (block nil
           (let ((kind nil) (data nil))
             (macrolet ((try (update-function)
                          `(progn
                             (setf (values kind data)
                                   (funcall ,update-function key value))
                             (when kind
                               (return (values kind data))))))
               (try update-from)
               (try update-to)
               (try update-by)
               (error "Unrecognized key ~S." key)))))
       (lambda ()
         (values (funcall finish-from)
                 (funcall finish-to)
                 (funcall finish-by)))))))

(defun %make-flags ()
  (let ((direction nil)
        (limit-kind nil))
    (values (lambda (new-direction new-limit-kind)
              (when new-direction
                (if direction
                    (unless (eq direction new-direction)
                      (error "Conflicting directions: ~S and ~S."
                             direction new-direction))
                    (setf direction new-direction)))
              (when new-limit-kind
                (if limit-kind
                    (unless (eq limit-kind new-limit-kind)
                      (error "Conflicting limit-kinds: ~S and ~S."
                             limit-kind new-limit-kind))
                    (setf limit-kind new-limit-kind)))
              (values))
            (lambda ()
              (values (or direction '+) limit-kind)))))

(defun %make-key-value-processor (on-duplicate)
  (multiple-value-bind (update-groups
                        finish-groups
                        update-flags
                        finish-flags)
      (multiple-value-call #'values
        (%make-groups on-duplicate)
        (%make-flags))
    (values
     (lambda (key val)
       (multiple-value-bind (kind data) (funcall update-groups key val)
         (prog1 kind
           (apply update-flags data))))
     (lambda ()
       (multiple-value-bind (from to by) (funcall finish-groups)
         (multiple-value-bind (direction limit-kind) (funcall finish-flags)
           (values from to limit-kind by direction)))))))

(defun %make-extras-processor (clause-kinds-p clause-keywords-p clauses-alist-p)
  '(values process finish)
  (let ((clause-kinds nil)
        (clause-keywords nil)
        (clauses-alist nil))
    (values (lambda (kind key)
              (when clause-kinds-p
                (push kind clause-kinds))
              (when clause-keywords-p
                (push key clause-keywords))
              (when clauses-alist-p
                (push (cons kind key) clauses-alist))
              (values))
            (lambda ()
              (multiple-value-call #'values
                (if clause-kinds-p
                    (values :clause-kinds (nreverse clause-kinds))
                    (values))
                (if clause-keywords-p
                    (values :clause-keywords (nreverse clause-keywords))
                    (values))
                (if clauses-alist-p
                    (values :clauses-alist (nreverse clauses-alist))
                    (values)))))))

(defun %map-plist (function plist)
  (do ((tail plist (cddr tail)))
      ((endp tail))
    (destructuring-bind (key value &rest rest) tail
      (declare (ignore rest))
      (funcall function key value))))
