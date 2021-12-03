;;; Advent of code 2021 problems solutions

;;;;;;;;;;;;; 
;;; Day 1 ;;;
;;;;;;;;;;;;;

(defun read-file-as-string (file-path)
  "Get a string from `FILE-PATH'."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun number-list-from-file-content (file-path)
  "Get a list of numbers from `FILE-PATH' content splitting by \n."
  (seq-map 'string-to-number (split-string (read-file-as-string file-path)) "\n"))

(defun sliding-window (list window-size)
  "Turn `LIST' into a list of lists, every sub list is created using a sliding window of size `WINDOW-SIZE'."
  (seq-map
   (lambda (index) (seq-subseq list index (+ index window-size)))
   (number-sequence 0 (- (seq-length list) window-size))))

(assert (equal
         (sliding-window (list 0 1 2 3 4 5) 3)
         (list (list 0 1 2) (list 1 2 3) (list 2 3 4) (list 3 4 5))))

(defun solution_day_1 (measuraments)
  "Count the measuraments greater than the previous one in `MEASURAMENTS'."
  (car
   (seq-reduce
    (lambda (acc window-values)
      (let* ((previuous-value (cdr acc))
             (greater-than-before-counter (car acc))
             (window-values-sum (seq-reduce '+ window-values 0))
             (counter
              (if (and
                   (/= previuous-value -1)
                   (> window-values-sum
                      previuous-value))
                  (+ greater-than-before-counter 1)
                greater-than-before-counter)))
        (cons counter window-values-sum)))
    measuraments
    (cons 0 -1))))

(solution_day_1 (sliding-window '(199 200 208 210 200 207 240 269 260 263) 1))
(solution_day_1 (sliding-window (number-list-from-file-content "./day_1.in") 1))
(solution_day_1 (sliding-window (number-list-from-file-content "./day_1.in") 3))

;;;;;;;;;;;;; 
;;; Day 2 ;;;
;;;;;;;;;;;;;

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

