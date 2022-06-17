;;; Interpreter.el -- Ecaml Interpreter
;;; Commentary:
;;; Code:

(defun eval ()
  ())

(defun repl ()
  (while t
    (progn
      (princ (concat (read-string "@> ") "\n")))))

(repl)

;;; Interpreter.el ends here
