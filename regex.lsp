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

(defun parse-regex (regex-string)
  )

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
   say if an union is 'ugly'(end with |),
   avoid construction of broken union NFA.
   INPUT: MUST be a union!
   RETURN: t nil"
  (let* ((first-char (get-char regex-string 0))
         (last-char  (get-char regex-string
                               (1- (string-length regex-string)))))
    (if (or (eq #\| last-char)
            (eq #\| first-char))
      t nil)))

