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

;load {{{
(load "parser.lsp")
(load "nfa.lsp")
(load "dfa.lsp")
;}}}
