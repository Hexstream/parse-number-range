(in-package #:parse-number-range)

;; Stupid repetition, but I've had enough.
(defun kind (keyword &key (errorp t))
  '(values kind new-direction new-limit-kind)
  (case keyword
    ((:from :downfrom :upfrom)
     (multiple-value-call #'values
       :from
       (ecase keyword
         (:from (values nil nil))
         (:downfrom (values '- nil))
         (:upfrom (values '+ nil)))))
    ((:to :upto :downto :below :above)
     (multiple-value-call #'values
       :to
       (ecase keyword
         (:to (values nil :inclusive))
         (:downto (values '- :inclusive))
         (:upto (values '+ :inclusive))
         (:below (values '+ :exclusive))
         (:above (values '- :exclusive)))))
    (:by (values :by nil nil))
    (t (when errorp
         (error "~S is not a for-as-arithmetic keyword." keyword)))))

(defun flags (keyword &key (errorp t))
  (multiple-value-call (lambda (kind &rest flags)
                         (declare (ignore kind))
                         (values-list flags))
    (kind keyword :errorp errorp)))


(defun flags-to-keywords (direction limit-kind)
  '(values from-keyword to-keyword)
  (cartesian-product-switch:cartesian-product-switch
      ((ecase direction
         + -)
       (ecase limit-kind
         (nil) :inclusive :exclusive))
    ;; +
    (values :from nil)
    (values :from :to)
    (values :from :below)
    ;; -
    (values :downfrom nil)
    (values :from :downto)
    (values :from :above)))
