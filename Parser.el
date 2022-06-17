;;; Parser.el --- Ecaml parser

;; Version: 1.0
;;; Commentary:
;;;
;;; Code:

(cl-defstruct input
  (position :type integer)
  (value :type string))

(cl-defstruct result/ok
  (value :type generic))

(cl-defstruct result/error
  (message :type string))

(cl-defstruct parser
  (run :type function))

(defun parser/map (f p)
  (make-parser :run `(lambda (input)
		       (cl-destructuring-bind ((consumed-input result) (parser-run p))))))

;;; Parser.el ends here
