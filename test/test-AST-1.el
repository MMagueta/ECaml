;;; test-AST-1.el --- Test unit for AST.

;; Version: 1.0
;;; Commentary:
;;;
;;; Code:

(require 'AST)

(ert-deftest ast-literal-eval ()
  (should (equal (eval-ecaml
		  (make-LiteralT :value 2))
		 (make-LiteralT :value 2))))

(ert-deftest ast-arithmetic-sum-eval ()
  (should (equal (eval-ecaml (make-ArithmeticT
			      :operation #'+
			      :parameters (list (make-LiteralT :value 2)
						(make-LiteralT :value 2)
						(make-LiteralT :value 2)
						(make-LiteralT :value 2)
						(make-LiteralT :value 2))))
		 (make-LiteralT :value 10))))

;; (ert-deftest ast-side-effect-eval ()
;;   (should (equal (eval-ecaml (make-ArithmeticT
;; 			      :operation #'princ
;; 			      :parameters (list (make-LiteralT :value "Hello!\n"))))
;; 		 (make-LiteralT :value 2))))

(ert-deftest ast-condition-eval ()
  (should (equal (eval-ecaml
		  (make-ConditionT :condition (make-LiteralT :value t)
				   :then (make-LiteralT :value 1)
				   :else (make-LiteralT :value 2)))
		 (make-LiteralT :value 1))))

(ert-deftest ast-application-with-arithmetic-eval ()
  (should (equal (eval-ecaml
		  (make-ApplicationT
		   :abstraction (make-AbstractionT
				 :param "x"
				 :body (make-ArithmeticT
					:operation #'+
					:parameters (list (make-LiteralT :value 1)
							  (make-VariableT :label "x"))))
		   :literal (make-LiteralT
			     :value 2)))
		 (make-LiteralT :value 3))))

(ert-deftest ast-nested-applications-with-arithmetic-eval ()
  (should (equal (eval-ecaml
		  (make-ApplicationT
		   :abstraction (make-ApplicationT
				 :abstraction (make-AbstractionT
					       :param "x"
					       :body (make-AbstractionT
						      :param "y"
						      :body (make-ArithmeticT
							     :operation #'+
							     :parameters (list (make-VariableT :label "y")
									       (make-VariableT :label "x")))))
				 :literal (make-LiteralT :value 2))
		   :literal (make-LiteralT :value 2)))
		 (make-LiteralT :value 4))))

(ert-deftest ast-application-variable-substitution-eval ()
  (should (equal (eval-ecaml
		  (make-ApplicationT
		   :abstraction (make-AbstractionT
				 :param "x"
				 :body (make-VariableT
					:label "x"))
		   :literal (make-LiteralT
			     :value 2)))
		 (make-LiteralT :value 2))))


;;; test-AST-1.el ends here
