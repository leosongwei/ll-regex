(defparameter test-table nil)
test-table

(defun push-to-test-table (tester)
  (push tester test-table))

(defmacro add-test (&body body)
  `(push-to-test-table
     (lambda ()
       ,@body)))

(defun get-doc (func)
  (documentation func 'function))

(defun run-tests ()
  (let ((all (length test-table))
        (good 0)
        (bad 0))
    (format t "autotest:~&")
    (dolist (test test-table)
      (format t "~A --- ~A~%"
              (get-doc test)
              (if (funcall test)
                (progn (incf good)
                       "yep")
                (progn (incf bad)
                       "NIL"))))
    (format t "~%~
               Good: ~A, Bad: ~A, total: ~A~%"
            good bad all)))

(progn
  (add-test
    "parse-regex: a"
    (equal (parse-regex "a") '((#\a))))

  (add-test
    "parse-regex: a+"
    (equal '(((+ #\a)))
           (parse-regex "a+")))

  (add-test
    "parse-regex: a*"
    (equal '(((* #\a)))
           (parse-regex "a*")))

  (add-test
    "parse-regex: a?"
    (equal (parse-regex "a?")
           '(((jmp #\a)))))

  (add-test
    "parse-regex: a(b)c"
    (equal (parse-regex "a(b)c")
           '((#\a (#\b) #\c))))

  (add-test
    "parse-regex: |a|"
    (equal (parse-regex "|a|")
           '((jmp (#\a)))))

  (add-test
    "parse-regex: |"
    (equal nil (parse-regex "|")))

  (add-test
    "parse-regex: null string"
    (equal nil (parse-regex "")))

  (add-test
    "parse-regex: a|b"
    (equal '((union (#\a) (#\b))) (parse-regex "a|b")))

  (add-test
    "parse-regex: a|b|c"
    (equal '((UNION (#\a) (#\b) (#\c)))
           (parse-regex "a|b|c")))

  (add-test
    "construct-dfa/match: a/ a, aa, null"
    (let ((d (construct-dfa "a")))
      (and (match "a" d)
           (not (match "aa" d))
           (not (match "" d)))))
  (add-test
    "construct-dfa/match: a*/ a, aa, aaaa, null"
    (let ((d (construct-dfa "a*")))
      (and (match "a" d)
           (match "aa" d)
           (match "aaaaa" d)
           (match "" d))))

  (add-test
    "construct-dfa/match: a+/ a, aa, aaaa, null"
    (let ((d (construct-dfa "a+")))
      (and (match "a" d)
           (match "aa" d)
           (match "aaaaa" d)
           (not (match "" d)))))

  (add-test
    "construct-dfa/match: a?/ a, aa, aaaa, null"
    (let ((d (construct-dfa "a?")))
      (and (match "a" d)
           (not (match "aa" d))
           (not (match "aaaaa" d))
           (match "" d))))

  (add-test
    "construct-dfa/match: ab/ ab, abc, a, b, null"
    (let ((d (construct-dfa "ab")))
      (and (match "ab" d)
           (not (match "abc" d))
           (not (match "a" d))
           (not (match "b" d))
           (not (match "" d)))))

  (add-test
    "construct-dfa/match: \\\\e/ a, b, z, ab, `, {, null"
    (let ((d (construct-dfa "\\e")))
      (and (match "a" d)
           (match "b" d)
           (match "z" d)
           (not (match "ab" d))
           (not (match "`" d))
           (not (match "{" d))
           (not (match "" d)))))

  (add-test
    "construct-dfa/match: \\\\E/ A, B, Z, AB, `, {, null"
    (let ((d (construct-dfa "\\E")))
      (and (match "A" d)
           (match "B" d)
           (match "Z" d)
           (not (match "AB" d))
           (not (match "@" d))
           (not (match "[" d))
           (not (match "" d)))))

  (add-test
    "construct-dfa/match: \\\\E\\\\E\\\\e\\\\e/ AAaa AAaA"
    (let ((d (construct-dfa "\\E\\E\\e\\e")))
      (and (match "AAaa" d)
           (not (match "AAaA" d)))))

  (add-test
    "construct-dfa/match: a\\\\e/ aa"
    (let ((d (construct-dfa "a\\e")))
      (match "aa" d)))
)



(run-tests)
