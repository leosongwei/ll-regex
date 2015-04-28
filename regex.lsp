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

(defun get-top-paren (regex-string)
  "GET-TOP-PAREN
   Get first sub regex string from regex-string.
   Comment: fuck it!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   RETURN: (cons top-paren-regex-string attrib) , nil
   ATTRIB: 'o '* '+"
  (let ((paren-level 0)
        (string-buff "")
        (current-char nil)
        (end-at nil))
    (dotimes (index (string-length regex-string))
      (setf current-char (get-char regex-string index))
      (cond ((eq #\( current-char)
             (if (> paren-level 0)
               (progn
                 (setf string-buff (add-char-to-string current-char
                                                       string-buff))
                 (incf paren-level))
               (incf paren-level)))
            ((eq #\) current-char)
             (if (> paren-level 0)
               (cond ((= paren-level 1) (progn
                                          (decf paren-level)
                                          (setf end-at index)
                                          (return)))
                     (t (progn
                          (decf paren-level)
                          (setf string-buff
                                (add-char-to-string current-char
                                                    string-buff)))))))
            ((> paren-level 0)
             (setf string-buff (add-char-to-string current-char
                                                   string-buff)))))
    (cond ((= 0 (string-length string-buff)) nil)
          ((not end-at) nil)
          ((= end-at (1- (string-length regex-string)))
           (cons string-buff 'o))
          (t
           (let ((attrib-char
                   (get-char regex-string (1+ end-at))))
             (cond ((eq #\* attrib-char) (cons string-buff '*))
                   ((eq #\+ attrib-char) (cons string-buff '+))
                   (t (cons string-buff 'o))))))))

