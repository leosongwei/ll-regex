(defun match-state (input-char state-index dfa)
  "MATCH-STATE
   judge input and tell MATCH which state should it goto or not match.
   RETURN: NIL or Index of dest"
  (let ((go? nil))
    (dolist (out (dfa-state-outs (aref dfa state-index)))
      (if (cond ((eq 'digit (car out)) (is-digit? input-char))
                ((eq 'lowcase (car out)) (is-lowercase input-char))
                ((eq 'uppcase (car out)) (is-uppercase input-char))
                (t (eq input-char (car out))))
        (progn
          (setf go? (cdr out))
          (return))))
    go?))

(defun is-uppercase (d)
  (let ((n (char-code d)))
    (if (and (>= n 65)
             (<= n 90))
      t nil)))

(defun is-lowercase (d)
  (let ((n (char-code d)))
    (if (and (>= n 97)
             (<= n 122))
      t nil)))

(defun is-digit? (d)
  (let ((n (char-code d)))
    (if (and (>= n 48)
             (<= n 57))
      t nil)))

(defun match (input-string dfa)
  "MATCH
   jump between states of dfa according to MATCH-STATE
   RETURN: sym-name or nil"
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
