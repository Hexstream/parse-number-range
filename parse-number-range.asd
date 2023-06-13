(asdf:defsystem #:parse-number-range

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "Parses LOOP's convenient \"for-as-arithmetic\" syntax into 5 simple values: from, to, limit-kind (:inclusive, :exclusive or nil if unbounded), by (step) and direction (+ or -)). Further related utilities are provided. Intended for easy implementation of analogous functionality in other constructs."

  :depends-on (#:cartesian-product-switch
               #:enhanced-multiple-value-bind)

  :version "1.0"
  :serial cl:t
  :components ((:file "package")
               (:file "info")
               (:file "internals")
	       (:file "parse"))

  :in-order-to ((asdf:test-op (asdf:test-op #:parse-number-range_tests))))
