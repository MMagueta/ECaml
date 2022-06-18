;;; AST.el --- Ecaml AST.
;;; Version: 1.0
;;; Commentary:
;;;
;;; Code:

; (ApplicationT (AbstractionT ("x" ArithmeticT (Addition, VariableT "x", LiteralT (Integer 2)))), LiteralT 2) -> (\x.x+2)(2)

(require 'ht)

(defmacro comment (&rest body)
  nil)

(defun mapcar* (function &rest args)
  "Apply FUNCTION to successive cars of all ARGS.
Return the list of results."
  ;; If no list is exhausted,
  (if (not (memq nil args))
      ;; apply function to CARs.
      (cons (apply function (mapcar #'car args))
            (apply #'mapcar* function
                   ;; Recurse for rest of elements.
                   (mapcar #'cdr args)))))

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

(defun eval-arithmetic (op env left right)
  (let ((evaluated-left (eval-ecaml left env))
	(evaluated-right (eval-ecaml right env)))
    (cond ((and (LiteralT-p evaluated-left) (LiteralT-p evaluated-right))
	   (make-LiteralT :value (funcall op (LiteralT-value evaluated-left) (LiteralT-value evaluated-right))))
	  (t (error "Could not reduce expression to LiteralT")))))

(defun eval-variable (name env)
  (let ((found-variable (ht-find (lambda ($key _value) (string-equal $key name)) env)))
    (unless found-variable (error (concat "Unbound variable: " name)))
    (cdr found-variable)))

;; 1 -> eval the value
;; 2 -> append value and the argument to the closed-env and the env
;; 3 -> eval with environment the new env and the body
(defun eval-closure (arg body closed-env env value)
  (let ((evaluated-value (eval-ecaml value env)))
    (ht-set closed-env arg evaluated-value)
    ;; (ht-find (lambda ($key _value) (string-equal $key arg)) (ht-merge closed-env env))))
    (eval-ecaml body (ht-merge closed-env env))))

(defun eval-application (f value env)
  (let ((exp (eval-ecaml f env)))
    (cl-typecase exp
      (ClosureT (eval-closure (ClosureT-var exp) (ClosureT-expression exp) (ClosureT-environment exp) env value)))))


; expression -> Result<expression, Message>
(defun eval-ecaml (exp &optional env)
  (unless env (setf env (ht-create)))
  (cl-typecase exp
    (LiteralT exp)
    (ArithmeticT (eval-arithmetic (ArithmeticT-operation exp) env (ArithmeticT-left exp) (ArithmeticT-right exp)))
    (VariableT (eval-variable (VariableT-label exp) env))
    (AbstractionT (make-ClosureT :var (AbstractionT-param exp) :expression (AbstractionT-body exp) :environment env))
    (ClosureT exp)
    (ApplicationT (eval-application (ApplicationT-abstraction exp) (ApplicationT-literal exp) env))))
  
(eval-ecaml (make-LiteralT :value 2))

(eval-ecaml (make-ArithmeticT :operation #'+ :left (make-LiteralT :value 2) :right (make-LiteralT :value 2)))

;; (eval-ecaml (make-ApplicationT
;;  :abstraction (make-AbstractionT
;; 	       :param "x"
;; 	       :body (make-ArithmeticT
;; 		      :operation #'+
;; 		      :left (make-LiteralT
;; 				:value 1)
;; 		      :right (make-VariableT
;; 			    :label "x")))
;;  :literal (make-LiteralT
;; 	   :value 2)))

(eval-ecaml (make-ApplicationT
 :abstraction (make-AbstractionT
	       :param "x"
	       :body (make-VariableT
		      :label "x"))
 :literal (make-LiteralT
	   :value 2)))

(eval-ecaml (make-AbstractionT
	       :param "x"
	       :body (make-VariableT
		      :label "x")))

;;; AST.el ends here
