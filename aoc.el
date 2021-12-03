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

(defun process-instruction (current-position instruction)
  "Given a `INSTRUCTION' and a `CURRENT-POSITION' modify the current position according to instruction."
  (let ((instruction (alist-get 'instruction instruction))
        (value (alist-get 'value instruction)))
    (pcase instruction
      (`"forward" (cons (+ (car current-position) value) (cdr current-position)))
      (`"down" (cons (car current-position) (+ (cdr current-position) value)))
      (`"up" (cons (car current-position) (- (cdr current-position) value))))))

(defun solution_day_2 (instructions)
  "Given a set of movement `INSTRUCTIONS', compute them and compute the resulting position."
  (let ((result (seq-reduce 'process-instruction instructions (cons 0 0))))
    (* (car result) (cdr result))))

(solution_day_2 (seq-map 'instruction-from-string (split-string (read-file-as-string "./day_2.in") "\n")))
(solution_day_2 (seq-map 'instruction-from-string (split-string "forward 5
down 5
forward 8
up 3
down 8
forward 2" "\n")))
