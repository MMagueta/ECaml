;;; AST.el --- Ecaml AST

;; Version: 1.0
;;; Commentary:
;;;
;;; Code:

; (ApplicationT (AbstractionT ("x" ArithmeticT (Addition, VariableT "x", LiteralT (Integer 2)))), LiteralT 2) -> (\x.x+2)(2)

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
 (gethash "key1" #s(hash-table size 30 data (key1 100 key2 300)))
 (let ((k (hash-table-keys #s(hash-table size 30 data (key1 100 key2 300))))
       (v (hash-table-values #s(hash-table size 30 data (key1 100 key2 300)))))
   (princ v)))

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
  (let ((found-variable (gethash (gensym name) env)))
    (unless found-variable (error (concat "Unbound variable: " name)))
    found-variable))

(defun eval-closure (var body closed-env env value)
  (let ((evaluated-value (eval-ecaml value env))
	(keys-values (mapcar* #'cons (hash-table-keys env) (hash-table-values env))))
    (puthash (gensym var) evaluated-value closed-env)
    (while keys-values
      (puthash (gensym (elt (car keys-values) 0)) (elt (car keys-values) 1) closed-env)
      (setq keys-values (cdr keys-values)))
    (eval-ecaml body closed-env)))

(defun eval-application (f value env)
  (let ((exp (eval-ecaml f env)))
    (cl-typecase exp
      (ClosureT (eval-closure (ClosureT-var exp) (ClosureT-expression exp) (ClosureT-environment exp) env value))
      (otherwise (error "Closure unbound")))))

; expression -> Result<expression, Message>
(defun eval-ecaml (exp &optional env)
  (unless env (setq env (make-hash-table)))
  (cl-typecase exp
    (LiteralT exp)
    (ArithmeticT (eval-arithmetic (ArithmeticT-operation exp) env (ArithmeticT-left exp) (ArithmeticT-right exp)))
    (VariableT (eval-variable (VariableT-label exp) env))
    (AbstractionT (make-ClosureT :var (AbstractionT-param exp) :expression (AbstractionT-body exp) :environment env))
    (ClosureT exp)
    (ApplicationT (eval-application (ApplicationT-abstraction exp) (ApplicationT-literal exp) env))))

(eval-ecaml (make-LiteralT :value 2))
(eval-ecaml (make-ArithmeticT :operation #'(lambda (x y) (or x y)) :left (make-LiteralT :value t) :right (make-LiteralT :value nil)))
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


;;; AST.el ends here
