(load-file "../input.el")

(defun instruction-from-string (instruction-as-string)
  "Map a `INSTRUCTION-AS-STRING' into an a-list representing the instruction."
  (pcase (split-string instruction-as-string " ")
    (`(,l ,ll)
     (list (cons 'instruction l) (cons 'value (string-to-number ll))))))

(defun forward (current-position value use-aim?)
  "Move forward of `VALUE' `CURRENT-POSITION'."
  (setf (cdr (assoc 'horizontal current-position)) (+ (alist-get 'horizontal current-position) value))
  (if use-aim?
      (setf (cdr (assoc 'depth current-position)) (+ (alist-get 'depth current-position) (* (alist-get 'aim current-position) value))))
  current-position)

(forward (list (cons 'horizontal 0) (cons 'depth 0) (cons 'aim 1)) 1 t)

(defun down (current-position value use-aim?)
  "Move down of `VALUE' `CURRENT-POSITION'."
  (if use-aim?
      (setf (cdr (assoc 'aim current-position)) (+ (alist-get 'aim current-position) value))
    (setf (cdr (assoc 'depth current-position)) (+ (alist-get 'depth current-position) value)))
  current-position)

(defun up (current-position value use-aim?)
  "Move up of `VALUE' `CURRENT-POSITION'."
  (if use-aim?
      (setf (cdr (assoc 'aim current-position)) (- (alist-get 'aim current-position) value))
    (setf (cdr (assoc 'depth current-position)) (- (alist-get 'depth current-position) value)))
  current-position)

(defun process-instruction (current-position instruction use-aim?)
  "Given a `INSTRUCTION' and a `CURRENT-POSITION' modify the current position according to instruction."
  (let ((instruction (alist-get 'instruction instruction))
        (value (alist-get 'value instruction)))
    (pcase instruction
      (`"forward" (forward current-position value use-aim?))
      (`"down" (down current-position value use-aim?))
      (`"up" (up current-position value use-aim?)))))

(defun solution_day_2 (instructions use-aim?)
  "Given a set of movement `INSTRUCTIONS', and a behaviour modifier `USE-AIM?' compute the resulting position processing instructions."
  (let ((result (seq-reduce
                 (lambda (acc elem) (process-instruction acc elem use-aim?))
                 instructions
                 (list
                  (cons 'depth 0)
                  (cons 'horizontal 0)
                  (cons 'aim 0)))))
    (* (alist-get 'horizontal result) (alist-get 'depth result))))

(solution_day_2 (seq-map 'instruction-from-string (split-string (read-file-as-string "./day_2.in") "\n")) nil)
(solution_day_2 (seq-map 'instruction-from-string (split-string (read-file-as-string "./day_2.in") "\n")) t)
