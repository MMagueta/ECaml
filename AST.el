;;; AST.el --- Ecaml AST.
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

(cl-defstruct ArithmeticT
  (operation :type symbol)
  (left :type ExpressionT)
  (right :type ExpressionT))

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
  (literal :type LiteralT))

(cl-defstruct ConditionT
  (condition :type ExpressionT)
  (then :type ExpressionT)
  (else :type ExpressionT))

(defun eval-arithmetic (op env left right)
  ""
  (let ((evaluated-left (eval-ecaml left env))
	(evaluated-right (eval-ecaml right env)))
    (cond ((and (LiteralT-p evaluated-left) (LiteralT-p evaluated-right))
	   (make-LiteralT :value (funcall op (LiteralT-value evaluated-left) (LiteralT-value evaluated-right))))
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
    (ArithmeticT (eval-arithmetic (ArithmeticT-operation exp) env (ArithmeticT-left exp) (ArithmeticT-right exp)))
    (VariableT (eval-variable (VariableT-label exp) env))
    (AbstractionT (make-ClosureT :var (AbstractionT-param exp) :expression (AbstractionT-body exp) :environment env))
    (ConditionT (eval-condition exp env))
    (ClosureT exp)
    (ApplicationT (eval-application (ApplicationT-abstraction exp) (ApplicationT-literal exp) env))))
  
(eval-ecaml (make-LiteralT :value 2))

(eval-ecaml (make-ArithmeticT :operation #'+ :left (make-LiteralT :value 2) :right (make-LiteralT :value 2)))

(eval-ecaml (make-ConditionT :condition (make-LiteralT :value nil) :then (make-LiteralT :value 1) :else (make-LiteralT :value 2)))

(eval-ecaml (make-ApplicationT
 :abstraction (make-AbstractionT
	       :param "x"
	       :body (make-ArithmeticT
		      :operation #'+
		      :left (make-LiteralT
				:value 1)
		      :right (make-VariableT
			    :label "x")))
 :literal (make-LiteralT
	   :value 2)))

(eval-ecaml (make-ApplicationT
 :abstraction (make-AbstractionT
	       :param "x"
	       :body (make-VariableT
		      :label "x"))
 :literal (make-LiteralT
	   :value 2)))

(eval-ecaml (make-AbstractionT
	       :param "x"
	       :body (make-ArithmeticT
		      :operation #'+
		      :left (make-LiteralT
				:value 1)
		      :right (make-VariableT
			    :label "x"))))

;;; AST.el ends here
