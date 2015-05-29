(defun parse-regex (regex-string)
  "PARSE-REGEX
   use box algorithm to parse regex-string.
   RETURN: parse-tree (simple list)."
  (setf parse-stack nil)
  (setf regex-string (concatenate 'string
                                  "("
                                  regex-string
                                  ")"))
  (let* ((current-char nil)
         (paren-level 0))
    (dotimes (index (string-length regex-string))
      (setf current-char (get-char regex-string index))
      (cond ((eq #\( current-char)
             (progn
               (incf paren-level)
               (setf parse-stack
                     (append parse-stack (list 'bp)))))
            ((eq #\) current-char)
             (progn
               (decf paren-level)
               (let* ((rev-stack (reverse parse-stack))
                      (to-bp (split-by-sym rev-stack nil 'bp))
                      (result-stack (reverse (car to-bp)))
                      (rest-stack (reverse (cdr to-bp))))
                 (if (is-union result-stack)
                   (setf result-stack (split-union result-stack)))
                 (if result-stack
                   (setf parse-stack
                         (append rest-stack (list result-stack)))
                   (setf parse-stack rest-stack)))))
            ((eq #\\ current-char)
             (setf parse-stack
                   (append
                     parse-stack
                     (list
                       (let ((next-char (get-char regex-string (incf index))))
                         (cond ((eq #\d next-char) 'digit)
                               (t next-char)))))))
            ((or (eq #\* current-char)
                 (eq #\+ current-char)
                 (eq #\? current-char))
             (let ((sym (cond ((eq #\* current-char) '*)
                              ((eq #\+ current-char) '+)
                              ((eq #\? current-char) 'jmp)))
                   (last-one (car (last parse-stack))))
               (if (eq 'bp last-one)
                 (error "PARSE-REGEX: invalid exp. Check repeat operators.~%")
                 (setf parse-stack
                       (append (butlast parse-stack)
                               (list
                                 (list sym last-one)))))))
            (t (setf parse-stack (append parse-stack
                                         (list current-char))))))
    (if (not (= 0 paren-level))
      (error "PARSE-REGEX: parentheses not closed!~%"))
    parse-stack))

(defun split-by-sym (stack result sym)
  "SPLIT-BY-SYM
   split a one-level list by a given symbol.
   RETURN: (cons sym-lst-before sym-lst-after)"
  (if (not (null stack))
    (if (eq sym (car stack))
      (cons result (cdr stack))
      (split-by-sym (cdr stack)
                    (append result
                            (list (car stack)))
                    sym))
    (cons result nil)))

(defun is-union (paren-lst)
  "IS-UNION
   say if a paren-lst is an union.
   RETURN: t nil"
  (if (null paren-lst)
    nil
    (if (eq #\| (car paren-lst))
      t
      (is-union (cdr paren-lst)))))

(defun split-union (paren-lst &optional result current flag)
  "SPLIT-UNION
   split union into pices, smartly :D
   RETURN: (union pice1 pice2 ...)

   for `a||b` it has: (jmp (union (a) (b)))
   for single item forms after split, it returns the singular form with jmp."
  (labels ((commit ()
             (if current
               (setf result (append result (list (reverse current))))
               (setf flag 'jmp))
             (setf current nil)))
    (if (null paren-lst)
      (progn
        (commit)
        (if result
          (if (cdr result) ; length > 1
            (progn
              (if (eq 'jmp flag)
                (list 'jmp (cons 'union result))
                (cons 'union result)))
            (cons 'jmp result))
          nil))
      (progn
        (if (eq #\| (car paren-lst))
          (commit)
          (push (car paren-lst) current))
        (split-union (cdr paren-lst) result current flag)))))
