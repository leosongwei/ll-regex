;∈
(defun sets-equal (A B)
  (and (not (set-difference A B))
       (not (set-difference B A))))

(defun duplicate-sets (sets A)
  (let ((dup? nil))
    (dolist (s sets)
      (if (sets-equal s A)
        (progn (setf dup? t)
               (return))
        nil))
    dup?))

(defstruct (dfa-state (:print-function print-dfa-state))
  (states nil)
  (sym-name nil)
  (outs nil))
(defun print-dfa-state (dfa-state stream depth)
  (format stream "#<D states:~A outs:~A sym:~A>"
          (dfa-state-states dfa-state)
          (dfa-state-outs dfa-state)
          (dfa-state-sym-name dfa-state)))

(defun print-dfa-table (dfa-table)
  (dotimes (index (array-dimension dfa-table 0))
    (if (null (aref dfa-table index))
      (return))
    (format t "~A: ~A~%" index (aref dfa-table index))))

(defun make-dfa (N sym-name)
  "MAKE-DFA
   make dfa, see the dragon book, 3.7
   RETURN: trans-table dfa"
  (let* ((dfa-table (make-array nfa-buff-len :initial-element nil))
         (fi-state (get-fi N))
         (index 0)
         (input-alphabet (get-input-alphabet N)))
    (labels ((add-frag (states)
               "RETURN: index of new state."
               (setf (aref dfa-table index)
                     (let ((s (make-dfa-state)))
                       (setf (dfa-state-states s) states)
                       s))
               (incf index)
               (1- index))
             (dup-state? (state-set)
               (let ((dup nil))
                 (dotimes (i index)
                   (if (sets-equal state-set
                                   (dfa-state-states
                                     (aref dfa-table i)))
                     (progn (setf dup i)
                            (return))))
                 dup))
             (apply-algorithm (dfa-state-i)
               (let ((states (dfa-state-states (aref dfa-table dfa-state-i))))
                 (dolist (c input-alphabet)
                   (let* ((result (epsilon-closure-set
                                    (move states c N) N))
                          (dup (dup-state? result)))
                     (if dup
                       (setf (dfa-state-outs (aref dfa-table dfa-state-i))
                             (append
                               (dfa-state-outs (aref dfa-table dfa-state-i))
                               (list (cons c dup))))
                       (let ((new-s (add-frag result)))
                         (setf (dfa-state-outs (aref dfa-table dfa-state-i))
                               (append
                                 (dfa-state-outs (aref dfa-table dfa-state-i))
                                 (list (cons c new-s))))
                         (apply-algorithm new-s))))))))
      (let ((s0 (add-frag (epsilon-closure 0 N))))
        (apply-algorithm 0))
      (dotimes (i nfa-buff-len)
        (if (null (aref dfa-table i))
          (return)
          (if (member fi-state (dfa-state-states (aref dfa-table i)))
            (setf (dfa-state-sym-name (aref dfa-table i)) sym-name))))
      dfa-table)))

(defun get-fi (N)
  "GET-FI
   get index of finish state of nfa.
   RETURN: index of finish state"
  (let ((result nil)
        (trans-table N))
    (dotimes (index nfa-buff-len)
      (if (eq 'fi (state-attrib (access-state index)))
        (progn (setf result index)
               (return))))
    result))

(defun get-input-alphabet (N)
  "GET-INPUT-ALPHABET
   RETURN:summation of input alphabet."
  (let ((result nil)
        (trans-table N))
    (dotimes (index nfa-buff-len)
      (if (null (access-state index))
        (return)
        (let ((match (state-match (access-state index))))
          (if match
            (if (member match result)
              nil
              (push match result))))))
    result))

(defun epsilon-closure (state-index N)
  "EPSILON-CLOSURE
   see the dragon book, 3.7
   RETURN: set of status"
  (let* ((visited (make-array nfa-buff-len :initial-element nil))
         (trans-table N)
         (result nil))
    (labels ((traverse-map (start)
               (if (null start)
                 nil
                 (if (aref visited start)
                   nil
                   (progn
                     (setf (aref visited start) t)
                     (push start result)
                     (if (eq '∈ (state-attrib (access-state start)))
                       (progn
                         (traverse-map
                           (state-out1 (access-state start)))
                         (traverse-map
                           (state-out2 (access-state start))))))))))
      (traverse-map state-index)
      result)))

(defun epsilon-closure-set (states N)
  "EPSILON-CLOSURE-SET
   see the dragon book, 3.7
   RETURN: set of status"
  (let ((result nil))
    (dolist (state states)
      (setf result (union result
                          (epsilon-closure state N))))
    (sort result #'<)))

(defun move (states match N)
  "MOVE
   see the dragon book, 3.7
   RETURN: set of status"
   (let ((result nil)
         (trans-table N))
     (dolist (index states)
       (if (eq match (state-match (access-state index)))
         (push (state-out1 (access-state index)) result)))
     result))

