(defun match-state (input-char state-index dfa)
  (let ((go? nil))
    (dolist (out (dfa-state-outs (aref dfa state-index)))
      (cond (t (if (eq input-char (car out))
                 (setf go? (cdr out))))))
    go?))

(defun match (input-string dfa)
  (let ((state-index 0)
        (match? t))
    (dotimes (index (string-length input-string))
      (let ((goto
              (match-state (get-char input-string index)
                           state-index dfa)))
        (if goto
          (setf state-index goto)
          (progn (setf match? nil)
                 (return)))))
    (if match?
      (dfa-state-sym-name (aref dfa state-index)))))
