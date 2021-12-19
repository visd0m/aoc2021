(defun nth-list (list-of-lists index)
  (seq-map (lambda (list) (nth index list)) list-of-lists))

(defun update-nth-in-list (list nth new-value step)
  (if (equal step nth)
      (progn
        (setcdr list (cdr list))
        (setcar list new-value))
    (update-nth-in-list (cdr list) nth new-value (+ 1 step))))

(let* ((test-list (list 1 2 3 4)))
  (update-nth-in-list test-list 3 5 0)
  (assert (equal test-list (list 1 2 3 5))))
