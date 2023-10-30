(cl:defpackage #:parse-number-range
  (:nicknames #:parse-numrange #:pnumrange)
  (:use #:cl)
  (:shadowing-import-from #:enhanced-multiple-value-bind #:multiple-value-bind)
  ;; Parse
  (:export #:parse
           #:unparse
           #:canonicalize)
  ;; Info
  (:export #:kind
           #:flags
           #:flags-to-keywords))
