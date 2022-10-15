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

(cl-defstruct NativeT
  (fun :type function))

(defun true-list-p (list)
  ""
  (eval `(and ,@list)))

(defun eval-ecaml (exp &optional env)
  ""
  
  (unless env (setq env (ht-create)))
  (catch 'result
    (while t
      (cl-typecase exp
	(LiteralT (throw 'result exp))
	(ArithmeticT
	 (throw 'result
		(let* ((op (ArithmeticT-operation exp))
		      (parameters (ArithmeticT-parameters exp))
		      (evaluated-parameters (mapcar #'(lambda (param) (eval-ecaml param env)) parameters)))
		  (cond ((true-list-p (mapcar #'LiteralT-p evaluated-parameters))
			 (make-LiteralT :value (eval `(funcall #',op ,@(mapcar #'LiteralT-value evaluated-parameters)))))
			(t (error "Could not reduce expression to LiteralT"))))))
	(VariableT
	 (throw 'result
		(let* ((name (VariableT-label exp))
		       (found-variable (ht-find (lambda ($key _value) (string-equal $key name)) env)))
		  (unless found-variable (error (concat "Unbound variable: " name)))
		  (elt found-variable 1))))
	(AbstractionT (throw 'result (make-ClosureT :var (AbstractionT-param exp) :expression (AbstractionT-body exp) :environment env)))
	(ConditionT
	 (let ((evaluated-expression (eval-ecaml (ConditionT-condition exp) env)))
	   (if (and (LiteralT-p evaluated-expression)
		    (booleanp (LiteralT-value evaluated-expression)))
	       (if (LiteralT-value evaluated-expression)
		   (setf exp (ConditionT-then exp))
		 (setf exp (ConditionT-else exp)))
	     (error "ConditionT expression failed on evaluation"))))
	(ClosureT (throw 'result exp))
	(ApplicationT
	 (let* ((f (ApplicationT-abstraction exp))
		(value (ApplicationT-literal exp))
		(new-exp (eval-ecaml f env)))
	   (cl-typecase new-exp
	     (ClosureT
	      (progn
		(setf exp (ClosureT-expression new-exp))
		(setf env (ht-merge env (ClosureT-environment new-exp)))
		(ht-set! env (ClosureT-var new-exp) value)))
	     ;; (eval-closure (ClosureT-var new-exp) (ClosureT-new-expression new-exp) (ClosureT-environment new-exp) env value))
	     (NativeT (funcall (NativeT-fun new-exp) value)))))))))
  
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

;; (setq +initial-env+ (ht-create))
;; (ht-set +initial-env+
;; 	"print_hello"
;; 	(make-NativeT :fun (lambda (x)
;; 			     (progn
;; 			       (ignore (princ "Hello!\n"))
;; 			       x))))

;; (eval-ecaml (make-ApplicationT
;; 	     :abstraction (make-AbstractionT
;; 			   :param "f"
;; 			   :body (make-ApplicationT
;; 				  :abstraction (make-VariableT :label "f")
;; 				  :literal (make-ApplicationT
;; 					    :abstraction (make-VariableT :label "print_hello")
;; 					    :literal (make-VariableT :label "f"))))
;; 	     :literal (make-AbstractionT
;; 		       :param "f"
;; 		       :body (make-ApplicationT
;; 			      :abstraction (make-VariableT :label "f")
;; 			      :literal (make-ApplicationT
;; 					:abstraction (make-VariableT :label "print_hello")
;; 					:literal (make-VariableT :label "f")))))
;; 	    +initial-env+)

(provide 'AST)

;;; AST.el ends here
