(defun nth-list (list-of-lists index)
  (seq-map (lambda (list) (nth index list)) list-of-lists))
