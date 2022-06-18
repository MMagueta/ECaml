;;; AST.el --- Ecaml AST
;;; Version: 1.0
;;; Commentary:
;;;
;;; Code:

; (ApplicationT (AbstractionT ("x" ArithmeticT (Addition, VariableT "x", LiteralT (Integer 2)))), LiteralT 2) -> (\x.x+2)(2)

(require 'ht)

(defmacro comment (&rest body)
  nil)

(comment
 (setq test-table (ht-create))
 (ht-set test-table "abc" 123)
 (ht-find (lambda ($key _value) (string-equal $key "abc")) test-table))

(cl-deftype ExpressionT ()
  `(or AbstractionT VariableT LiteralT ApplicationT ArithmeticT))

(cl-deftype ExpressionListT ()
  `(and list (satisfies (lambda (list) (and (consp list)
					    (every #'ExpressionT-p list))))))

(cl-defstruct ArithmeticT
  (operation :type symbol)
  (parameters :type ExpressionListT))

(cl-defstruct AbstractionT
  (param :type string)
  (body :type ExpressionT))

(cl-defstruct VariableT
  (label :type string))

(cl-defstruct LiteralT
  (value :type integer))

(cl-defstruct ClosureT
  (var :type string)
  (expression :type ExpressionT)
  (environment)) ; environment = Map<string * expression>

(cl-defstruct ApplicationT
  (abstraction :type AbstractionT)
  (literal :type ExpressionT))

(cl-defstruct ConditionT
  (condition :type ExpressionT)
  (then :type ExpressionT)
  (else :type ExpressionT))

(defun true-list-p (list)
  ""
  (eval `(and ,@list)))

(defun eval-arithmetic (op env parameters)
  ""
  (let ((evaluated-parameters (mapcar #'(lambda (param) (eval-ecaml param env)) parameters)))
    (cond ((true-list-p (mapcar #'LiteralT-p evaluated-parameters))
	   (make-LiteralT :value (eval `(funcall #',op ,@(mapcar #'LiteralT-value evaluated-parameters)))))
	  (t (error "Could not reduce expression to LiteralT")))))

(defun eval-variable (name env)
  ""
  (let ((found-variable (ht-find (lambda ($key _value) (string-equal $key name)) env)))
    (unless found-variable (error (concat "Unbound variable: " name)))
    (elt found-variable 1)))

(defun eval-closure (arg body closed-env env value)
  ""
  (let ((evaluated-value (eval-ecaml value env)))
    (ht-set closed-env arg evaluated-value)
    (eval-ecaml body (ht-merge closed-env env))))

(defun eval-application (f value env)
  ""
  (let ((exp (eval-ecaml f env)))
    (cl-typecase exp
      (ClosureT (eval-closure (ClosureT-var exp) (ClosureT-expression exp) (ClosureT-environment exp) env value)))))

(defun eval-condition (control-expression env)
  ""
  (let ((evaluated-expression (eval-ecaml (ConditionT-condition control-expression) env)))
    (if (and (LiteralT-p evaluated-expression)
	     (booleanp (LiteralT-value evaluated-expression)))
	(if (LiteralT-value evaluated-expression)
	    (eval-ecaml (ConditionT-then control-expression) env)
	  (eval-ecaml (ConditionT-else control-expression) env))
      (error "ConditionT expression failed on evaluation"))))

(defun eval-ecaml (exp &optional env)
  ""
  (unless env (setq env (ht-create)))
  (cl-typecase exp
    (LiteralT exp)
    (ArithmeticT (eval-arithmetic (ArithmeticT-operation exp) env (ArithmeticT-parameters exp)))
    (VariableT (eval-variable (VariableT-label exp) env))
    (AbstractionT (make-ClosureT :var (AbstractionT-param exp) :expression (AbstractionT-body exp) :environment env))
    (ConditionT (eval-condition exp env))
    (ClosureT exp)
    (ApplicationT (eval-application (ApplicationT-abstraction exp) (ApplicationT-literal exp) env))))
  
(eval-ecaml (make-LiteralT :value 2))

(eval-ecaml (make-ArithmeticT
	     :operation #'+
	     :parameters (list (make-LiteralT :value 2)
			       (make-LiteralT :value 2)
			       (make-LiteralT :value 2)
			       (make-LiteralT :value 2)
			       (make-LiteralT :value 2))))

(eval-ecaml (make-ArithmeticT
	     :operation #'princ
	     :parameters (list (make-LiteralT :value "Hello!"))))

(eval-ecaml (make-ConditionT :condition (make-LiteralT :value t) :then (make-LiteralT :value 1) :else (make-LiteralT :value 2)))

(eval-ecaml (make-ApplicationT
 :abstraction (make-AbstractionT
	       :param "x"
	       :body (make-ArithmeticT
		      :operation #'+
		      :parameters (list (make-LiteralT :value 1)
		                        (make-VariableT :label "x"))))
 :literal (make-LiteralT
	   :value 2)))

(eval-ecaml (make-ApplicationT
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

(eval-ecaml (make-ApplicationT
 :abstraction (make-AbstractionT
	       :param "x"
	       :body (make-VariableT
		      :label "x"))
 :literal (make-LiteralT
	   :value 2)))

;; (eval-ecaml (make-ApplicationT
;; 	     :abstraction (make-AbstractionT
;; 			   :param "_"
;; 			   :body
;; 			   (make-ApplicationT
;; 			    :abstraction (make-AbstractionT
;; 					  :param "x"
;; 					  :body (make-ApplicationT :abstraction (make-VariableT :label "x") :literal (make-VariableT :label "x")))
;; 			    :literal (make-AbstractionT
;; 				      :param "x"
;; 				      :body (make-ApplicationT :abstraction (make-VariableT :label "x") :literal (make-VariableT :label "x")))))
;; 	     :literal (make-ArithmeticT
;; 		       :operation #'message
;; 		       :parameters (list (make-LiteralT :value "Hello!")))))

;;; AST.el ends here
