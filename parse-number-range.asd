(asdf:defsystem #:parse-number-range

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "Parses LOOP's somewhat complicated number range (for-as-arithmetic) syntax into 5 simple values: from, to, by, direction (incf or decf) and bound-type (:inclusive or :exclusive). Intended for easy implementation of similar functionality in other (saner?) constructs."
  
  :version "0.1"
  :serial cl:t
  :components ((:file "package")
	       (:file "main")))
