;utils {{{
(defun string-length (string)
  (length string))

(defun add-char-to-string (char string)
  (setf string (concatenate 'string
                            string (string char)))
  string)

(defun get-char (string index)
  (char string index))
;}}}

(progn
  (defparameter trans-table (make-array 100))
  (defparameter index-trans-table 0)
  (defparameter parse-stack nil))

(array-dimension trans-table 0)

(defun print-trans-table ()
  (dotimes (index (array-dimension trans-table 0))
    (if (eq 0 (aref trans-table index))
      (return))
    (format t "~A: ~A~%" index (aref trans-table index))))

(defstruct (state (:print-function print-state))
  (attrib nil)
  (match nil)
  (out1 nil)
  (out2 nil))
(defun print-state (state stream depth)
  (format stream "#<S a:~A, c:~A, o1:~A o2:~A>"
          (state-attrib state)
          (state-match state)
          (state-out1 state)
          (state-out2 state)))

(defmacro access-state (index)
  `(aref trans-table ,index))

(defun add-out (state out)
  "ADD-OUT
   RETURN: nil"
  (cond ((null (state-out1 (access-state state)))
         (setf (state-out1 (access-state state)) out))
        (t
         (setf (state-out2 (access-state state)) out))))

(defun add-state (match attrib)
  "ADD-STATE
   make a state and add it to TRANS-TABLE.
   RETURN: new state location"
  (setf (access-state index-trans-table)
        (let ((s (make-state)))
          (setf (state-match s) match)
          (setf (state-attrib s) attrib)
          s))
  (incf index-trans-table)
  (1- index-trans-table))

(defun make-dfa (regex-string)
  "MAKE-DFA
   return a dfa.
   using GLOBAL parameters and ugly. Hence I will fix it later.
   RETURN: *TODO*"
  (progn
    (defparameter trans-table (make-array 50))
    (defparameter index-trans-table 0))
  (let* ((ret-parse (parse-regex regex-string))
         (last-one (get-ret 'end ret-parse))
         (finish (add-state nil 'fi)))
    (if (null (get-ret 'end ret-parse))
      nil
      (add-out last-one finish)))
  (print-trans-table))

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

(defun parse-regex (regex-string)
  "PARSE-REGEX
   use box algorithm to parse regex-string.
   RETURN: parse-tree (simple list)."
  (setf parse-stack nil)
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
                 (setf parse-stack
                       (append rest-stack (list result-stack))))))
            ((or (eq #\* current-char)
                 (eq #\+ current-char))
             (let ((sym (cond ((eq #\* current-char) '*)
                              ((eq #\+ current-char) '+))))
               (setf parse-stack
                     (append (butlast parse-stack)
                             (list
                               (list sym (car (last parse-stack))))))))
            (t (setf parse-stack (append parse-stack
                                         (list current-char))))))
    parse-stack))

(defun get-ret (key parse-ret)
  "GET-RET
   get value by keyword from return of PARSE-REGEX
   RETURN: numbers"
  (cond ((eq 'begin key) (nth 0 parse-ret))
        ((eq 'end   key) (nth 1 parse-ret))
        ((eq 'len key) (nth 2 parse-ret))))

(defun deal-with-paren (paren-regex last-end-index)
  "DEAL-WITH-PAREN
   RETURN:(lst begin-index end-index length+2)"
  (let ((begin-index nil)
        (end-index nil)
        (len (string-length paren-regex)))
    (if (is-union paren-regex)
      ; union paren
      (if (not (union-ugly-p paren-regex))
        (let ((ret-parse (make-union paren-regex last-end-index)))
          (setf begin-index (get-ret 'begin ret-parse))
          (setf end-index   (get-ret 'end   ret-parse))
          (list last-end-index end-index (+ 2 len)))
        (error "deal-with-paren: ugly union!"))
      ; simple paren
      (let ((ret-parse (parse-regex paren-regex last-end-index)))
        (setf begin-index (get-ret 'begin ret-parse))
        (setf end-index   (get-ret 'end   ret-parse))
        (list last-end-index end-index (+ 2 len))))))

(defun make-union (union-string last-end-index)
  "MAKE-UNION
   make union states.
   RETURN:(lst begin-index end-index length+2)"
  (let* ((begin-index (add-state nil '∈))
         (end-index   (add-state nil nil))
         (len         (string-length union-string))
         (split-cons (split-union union-string))
         (regex1 (car split-cons))
         (regex2 (cdr split-cons)))
    (add-out last-end-index begin-index)
    (let* ((ret-parse     (parse-regex regex1))
           (begin1-index  (get-ret 'begin ret-parse))
           (last1-index   (get-ret 'end   ret-parse))
           (sub-end-index (add-state nil '∈)))
      (add-out begin-index begin1-index)
      (add-out last1-index sub-end-index)
      (add-out sub-end-index end-index))
    (let* ((ret-parse     (parse-regex regex2))
           (begin1-index  (get-ret 'begin ret-parse))
           (last1-index   (get-ret 'end   ret-parse))
           (sub-end-index (add-state nil '∈)))
      (add-out begin-index begin1-index)
      (add-out last1-index sub-end-index)
      (add-out sub-end-index end-index))
    (list last-end-index end-index (+ 2 len))))

(defun get-first-paren (regex-string)
  "GET-FIRST-PAREN
   get first sub regex string from regex-string like '(sub)xxxx'
   will be triggered when caller meet an '('
   RETURN: sub regex string"
  (let ((paren-level 0)
        (string-buff "")
        (current-char nil))
    (dotimes (index (string-length regex-string))
      (setf current-char (get-char regex-string index))
      (cond ((eq #\( current-char) (incf paren-level))
            ((eq #\) current-char) (decf paren-level)))
      (setf string-buff (add-char-to-string current-char string-buff))
      (if (= 0 paren-level)
        (return))
      (if (= index (1- (string-length regex-string)))
        (error "get-first-paren: regex paren not closed")))
    (setf string-buff
          (subseq string-buff 1 (1- (string-length string-buff))))))


(defun is-union (paren-lst)
  "IS-UNION
   say if a paren-lst is an union.
   RETURN: t nil"
  (let ((split-result (split-by-sym paren-lst nil #\|)))
    (if (and (car split-result)
             (cdr split-result))
      ; check if ugly
      (if (not (cdr (split-by-sym split-result nil #\|)))
        t nil)
      nil)))

(defun union-ugly-p (regex-string)
  "UGLY-UNION-P
   say if an union is 'ugly'(start or end with |),
   avoid construction of broken union NFA.
   INPUT: MUST be a union!
   RETURN: t nil"
  (let* ((first-char (get-char regex-string 0))
         (last-char  (get-char regex-string
                               (1- (string-length regex-string)))))
    (if (or (eq #\| last-char)
            (eq #\| first-char))
      t nil)))

(defun split-union (regex-string)
  "SPLIT-UNION
   split a union regex string into 2 pices of regex string.
   don't split ugly union.
   RETURN: (cons regex1 regex2)"
  (let ((split-location nil)
        (current-char nil)
        (paren-level 0))
    (setf split-location
          (dotimes (index (string-length regex-string))
            (setf current-char (get-char regex-string index))
            (cond ((eq #\( current-char) (incf paren-level))
                  ((eq #\) current-char) (decf paren-level))
                  ((and (eq #\| current-char)
                        (= 0 paren-level))
                   (return index)))))
    (cons (subseq regex-string 0 split-location)
          (subseq regex-string (1+ split-location)))))
