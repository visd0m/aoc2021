(defun nth-list (list-of-lists index)
  "Get a list composed by all the `INDEX elements of the lists contained in `LIST-OF-LISTS."
  (seq-map (lambda (list) (nth index list)) list-of-lists))

(defun update-nth-in-list (list nth new-value &optional step)
  "Update the `NTH value of the `LIST replacing it with `NEW-VALUE."
  (let ((step (or step 0)))
    (if (equal step nth)
        (progn
          (setcdr list (cdr list))
          (setcar list new-value))
      (update-nth-in-list (cdr list) nth new-value (+ 1 step)))))

(let* ((test-list (list 1 2 3 4)))
  (update-nth-in-list test-list 3 5)
  (assert (equal test-list (list 1 2 3 5))))
