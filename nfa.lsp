(progn
  (defparameter trans-table (make-array 100))
  (defparameter index-trans-table 0)
  (defparameter parse-stack nil))

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

(defun construct-nfa (regex-string)
  "CONSTRUCT-NFA
   serving as an wrapper, dealing with dirty jobs.
   RETURN: NFA in a TRANS-TABLE"
  (progn
    (defparameter trans-table (make-array 100))
    (defparameter index-trans-table 0)
    (defparameter parse-stack nil))
  (let* ((parse-tree (parse-regex regex-string))
         (head (add-state nil nil))
         (tail (make-nfa parse-tree head)))
    (setf (state-attrib (access-state tail)) 'fi)
    (print-trans-table)
    ))

(defun make-union (parse-tree tail))

(defun make-plus (parse-tree tail)
  (format t "MAKE-PLUS: ~A, tail:~A~%" parse-tree tail)
  (let* ((head (add-state nil '∈))
        (paren-tail (make-nfa (cdr parse-tree) head))
        (new-tail (add-state nil nil)))
    (add-out tail head)
    (setf (state-attrib (access-state paren-tail)) '∈)
    (add-out paren-tail tail)))

(defun make-star (parse-tree tail))

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
                            (t (make-nfa current tail)))))
               (make-nfa (cdr parse-tree) tail)))
            (t (let ((new-s (add-state nil nil)))
                 (add-out tail new-s)
                 (setf (state-match (access-state tail)) current)
                 (make-nfa (cdr parse-tree) new-s)))))
    tail))

