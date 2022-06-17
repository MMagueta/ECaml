;; (define-package "EParsec" "1.0.0")

(defmacro comment (&rest expr) 'nil)

(defun input.make (text &optional position)
  ":text := string\n:position := integer"
  (let ((position (or position 0)))
    (if (and (numberp position) (stringp text))
	`(:text ,text :position ,position)
      (error "Wrong parameter type provided."))))

(defun error.make (desc &optional position)
  ":desc := string\n:position := integer"
  (let ((position (or position 0)))
    (if (and (numberp position) (stringp desc))
	`(:desc ,desc :position ,position)
      (error "Wrong parameter type provided."))))

(defun record.get (keyword record)
  (eval `(plist-get record ,keyword)))

(defun parser.create (f)
  `(:run ,f))

(defun fail (e)
  (parser.create `(lambda (inp) (cons inp (plist-put '() :failure ,e)))))

(defun wrap (value)
  (parser.create `(lambda (inp) (cons inp (plist-put '() :success ,value)))))

(defun unwrap (x)
  (pcase x
    (`(:success . ,elem) elem)
    (`(:failure) nil)))

(comment
 (unwrap '(:some 123))
 (unwrap '(:nothing)))

(defun update-parser (parser-fun l ll transform)
  (let ((ll-transformed (funcall transform ll)))
    (apply (record.get :run (funcall parser-fun ll-transformed)) `(,l))))

(defun parser-map (f parser)
  (parser.create
   `(lambda (inp)
      (pcase (apply ,(record.get :run parser) `(,inp))
	(`(,l :failure ,ll) (update-parser #'fail l ll #',f))
	(`(,l :success ,ll) (update-parser #'wrap l ll #',f)) ))))


(defun test (x) (concat x "-123"))

(comment
 "Map tests"
 (apply (record.get :run (parser-map #'test (fail "ABC"))) `("DEF")))

;; (:run (lambda (inp) (pcase (apply (lambda (inp) (cons inp (plist-put 'nil :failure "ABC"))) `(,inp)) (`(,l :failure ,ll) (update-parser fail ll l)) (`(,l :success ,ll) (update-parser wrap ll l)))))

(comment
 (apply (record.get :run (parser-map nil (fail "ABC"))) `("123"))
 (apply (record.get :run (parser-map nil (wrap "ABC"))) `("123")))

(comment
 (pcase (apply (record.get :run (wrap "ABC")) `("123"))
   (`(,l :failure ,ll) "Failure")
   (`(,l :success ,ll) "Success")))

(comment
 (record.get :run (fail "ABC"))
 (record.get :failure (cdr (apply (record.get :run (fail "ABC")) `("123"))))
 (record.get :success (cdr (apply (record.get :run (wrap "ABC")) `("123")))))

(comment
 (input.get :text (input.make "abc" 123))
 (input.get :position (input.make "abc" 123)))

(comment
 (input.make "abc")
 (input.make "abc" 123)
 (input.make "abc" "123"))

(defun input-consume (start len previous-input)
  (make-input
   (substring (input.get :text previous-input) start len)
   (+ start (input.get :position previous-input))))

(comment
 (input-consume 0 3 (make-input "abc def" 0)))

(provide 'eparsec)
