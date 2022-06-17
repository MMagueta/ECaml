;;; test-eparsec-1.el --- Test unit for eparsec.

;; Version: 1.0
;;; Commentary:
;;;
;;; Code:

(require 'eparsec)

(ert-deftest eparsec-1 ()
  (should (equal (eparsec/hello) "Hello World!")))

;;; test-eparsec-1.el ends here
