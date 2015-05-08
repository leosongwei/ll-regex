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
  (defparameter index-trans-table 0))

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

(defun add-state (match attrib)
  "ADD-STATE
   make a state and add it to TRANS-TABLE.
   RETURN: new state location"
  (setf (aref trans-table index-trans-table)
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
         (finish (get-ret 'end ret-parse)))
    (setf (state-attrib (aref trans-table finish)) 'fi))
  (print-trans-table))

(defun parse-regex (regex-string &optional begin)
  "PARSE-REGEX
   parse a regex-string recursively.
   RETURN:(lst begin-index end-index length+2)
   "
  (let* ((current-char     nil)
         (initial-state    (if begin
                             begin
                             (add-state nil '∈)))
         (last-begin-index initial-state)
         (last-end-index   initial-state)
         (len (string-length regex-string)))
    (labels ((set-begin-end-f (begin end)
               (setf last-begin-index begin)
               (setf last-end-index end)))
      (dotimes (index (string-length regex-string))
        (setf current-char (get-char regex-string index))
        (cond ((eq #\( current-char)
               (let* ((rest-exp (subseq regex-string index))
                      (paren-exp (get-first-paren rest-exp))
                      (ret-parse
                        (deal-with-paren paren-exp last-end-index)))
                 (set-begin-end-f (get-ret 'begin ret-parse)
                                  (get-ret 'end   ret-parse))
                 (incf index (1- (get-ret 'len ret-parse)))))
              (t ; Simple State
               (let ((s-index (add-state nil '∈)))
                 (setf (state-out1 (aref trans-table last-end-index))
                       s-index)
                 (setf (state-attrib (aref trans-table last-end-index))
                       nil)
                 (setf (state-match (aref trans-table last-end-index))
                       current-char)
                 (set-begin-end-f last-end-index s-index)
                 ))
              ))
      (list initial-state last-end-index (+ 2 len)))))

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
          (list begin-index end-index (+ 2 len)))
        (error "deal-with-paren: ugly union!"))
      ; simple paren
      (let ((ret-parse (parse-regex paren-regex last-end-index)))
        (setf begin-index (get-ret 'begin ret-parse))
        (setf end-index   (get-ret 'end   ret-parse))
        (list begin-index end-index (+ 2 len))))))

(defun make-union (union-string last-end-index)
  "MAKE-UNION
   make union states.
   RETURN:(lst begin-index end-index length+2)"
  (let* ((begin-index last-end-index)
         (end-index   (add-state nil '∈))
         (len         (string-length union-string))
         (split-cons (split-union union-string))
         (regex1 (car split-cons))
         (regex2 (cdr split-cons)))
    (let* ((ret-parse    (parse-regex regex1))
           (begin1-index (get-ret 'begin ret-parse))
           (end1-index   (get-ret 'end   ret-parse)))
      (setf (state-out1 (aref trans-table begin-index)) begin1-index)
      (setf (state-out1 (aref trans-table end1-index)) end-index))
    (let* ((ret-parse    (parse-regex regex2))
           (begin2-index (get-ret 'begin ret-parse))
           (end2-index   (get-ret 'end   ret-parse)))
      (setf (state-out2 (aref trans-table begin-index)) begin2-index)
      (setf (state-out1 (aref trans-table end2-index)) end-index))
    (list begin-index end-index (+ 2 len))))


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


(defun is-union (regex-string)
  "IS-UNION
   say if exp is an UNION exp.
   gets ERROR when exp not support (2 |).
   RETURN: t nil"
  (let ((counter 0)
        (current-char nil)
        (paren-level 0))
    (dotimes (index (string-length regex-string))
      (setf current-char (get-char regex-string index))
      (cond ((eq #\( current-char) (incf paren-level))
            ((eq #\) current-char) (decf paren-level)))
      (if (and (eq #\| current-char)
               (= 0 paren-level))
        (incf counter)))
    (cond ((= 1 counter) t)
          ((> counter 1) (error "too many \"|\" !"))
          ((= 0 counter) nil)
          (t (error "This program has been fucked!")))))

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
