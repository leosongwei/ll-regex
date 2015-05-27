;∈
(defun compare-sets (A B))

(defun make-dfa (N))

(defun get-input-alphabet (N))

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

(defun epsilon-closure-set (states))

(defun move (state-set c))

