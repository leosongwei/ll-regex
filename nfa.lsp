(progn
  (defparameter nfa-buff-len 100)
  (defparameter trans-table (make-array nfa-buff-len :initial-element nil))
  (defparameter index-trans-table 0)
  (defparameter parse-stack nil))

(defun print-trans-table ()
  (dotimes (index (array-dimension trans-table 0))
    (if (null (aref trans-table index))
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

(defun construct-nfa (regex-string)
  "CONSTRUCT-NFA
   serving as an wrapper, dealing with dirty jobs.
   RETURN: NFA in a TRANS-TABLE"
  (progn
    (defparameter trans-table (make-array nfa-buff-len :initial-element nil))
    (defparameter index-trans-table 0)
    (defparameter parse-stack nil))
  (let* ((parse-tree (parse-regex regex-string)))
    (if parse-tree
      (let ((tail (make-nfa parse-tree)))
        (setf (state-attrib (access-state tail)) 'fi))))
  trans-table)

(defun make-union (parse-tree tail)
  (let* ((todo (cdr parse-tree))
         (last-tail (if tail
                      (progn
                        (setf (state-attrib (access-state tail)) '∈)
                        tail)
                      (add-state nil '∈)))
         (p1-head (let ((h (add-state nil nil)))
                    (setf (state-out1 (access-state last-tail)) h)
                    h))
         (p1-tail (make-nfa (car todo) p1-head))
         (p2-head (let ((h (add-state nil nil)))
                    (setf (state-out2 (access-state last-tail)) h)
                    h))
         (p2-tail (if (cddr todo)
                    (make-union (cons 'union (cdr todo)) p2-head)
                    (make-nfa (cadr todo) p2-head)))
         (new-tail (let ((l (add-state nil nil)))
                     (setf (state-attrib (access-state p1-tail)) '∈)
                     (setf (state-attrib (access-state p2-tail)) '∈)
                     (setf (state-out1 (access-state p1-tail)) l)
                     (setf (state-out1 (access-state p2-tail)) l)
                     l)))
    new-tail))

(defun make-plus (parse-tree tail)
  "MAKE-PLUS
   constructs kleene plus regex.
   RETURN: tail index"
  (let* ((last-tail (if tail
                      (progn
                        (setf (state-attrib (access-state tail)) '∈)
                        tail)
                      (add-state nil '∈)))
         (p-head (let ((h (add-state nil nil)))
                   (setf (state-out1 (access-state last-tail)) h)
                   h))
         (p-tail (make-nfa (cdr parse-tree) p-head))
         (new-tail (let ((l (add-state nil nil)))
                     (setf (state-attrib (access-state p-tail)) '∈)
                     (setf (state-out1 (access-state p-tail)) p-head)
                     (setf (state-out2 (access-state p-tail)) l))))
    new-tail))

(defun make-star (parse-tree tail)
  "MAKE-STAR
   constructs kleene star regex.
   RETURN: tail index"
  (let* ((last-tail (if tail
                      (progn
                        (setf (state-attrib (access-state tail)) '∈)
                        tail)
                      (add-state nil '∈)))
         (p-head (let ((h (add-state nil nil)))
                   (setf (state-out1 (access-state last-tail)) h)
                   h))
         (p-tail (make-nfa (cdr parse-tree) p-head))
         (new-tail (let ((l (add-state nil nil)))
                     (setf (state-attrib (access-state p-tail)) '∈)
                     (setf (state-out1 (access-state p-tail)) p-head)
                     (setf (state-out2 (access-state p-tail)) l))))
    (setf (state-out2 (access-state last-tail)) new-tail)
    new-tail))

(defun make-jmp (parse-tree tail)
  "MAKE-JMP
   constructs (a||b)
   RETURN: tail index"
  (let* ((last-tail (if tail
                      (progn
                        (setf (state-attrib (access-state tail)) '∈)
                        tail)
                      (add-state nil '∈)))
         (p-head (let ((h (add-state nil nil)))
                   (setf (state-out1 (access-state last-tail)) h)
                   h))
         (p-tail (make-nfa (cdr parse-tree) p-head))
         (new-tail (let ((l (add-state nil nil)))
                     (setf (state-attrib (access-state p-tail)) '∈)
                     (setf (state-out1 (access-state p-tail)) l))))
    (setf (state-out2 (access-state last-tail)) new-tail)
    new-tail))

(defun make-nfa (parse-tree &optional tail)
  "MAKE-NFA
   using `caller-handler` mode to construct NFA from
   parse tree, recursively. (see SICP `eval-apply` mode)

   there are 2 recurions:
   1. traverses every item in a paren.
   2. handler calls deeper MAKE-NFA

   RETURN: index of last one.
   SIDE EFFECT: constructs NFA in the TRANS-TABLE."
  (if parse-tree
    (let ((current (car parse-tree)))
      (cond ((consp current)
             (let* ((head (car current))
                    (tail
                      (cond ((eq 'union head) (make-union current tail))
                            ((eq '+ head) (make-plus current tail))
                            ((eq '* head) (make-star current tail))
                            ((eq 'jmp head) (make-jmp current tail))
                            (t (make-nfa current tail)))))
               (make-nfa (cdr parse-tree) tail)))
            (t (if (= 0 index-trans-table)
                 (let* ((head (add-state current nil))
                        (tail (add-state nil nil)))
                   (setf (state-out1 (access-state head)) tail)
                   (make-nfa (cdr parse-tree) tail))
                 (let* ((new-tail (add-state nil nil)))
                   (setf (state-match (access-state tail)) current)
                   (setf (state-out1 (access-state tail)) new-tail)
                   (make-nfa (cdr parse-tree) new-tail))))))
    tail))

