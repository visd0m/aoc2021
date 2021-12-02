;;; Advent of code 2021 problems solutions

;;; Day 1
(defun solution_1 (measuraments)
  "Count the measuraments greater than the previous one in `MEASURAMENTS'."
  (car
   (seq-reduce
    (lambda (acc window-values)
      (let* ((previuous-elem (cdr acc))
             (greater-than-before-counter (car acc))
             (window-values-sum (seq-reduce (lambda (acc elem) (+ acc elem)) window-values 0))
             (new-previous-elem window-values-sum)
             (counter
              (if (and
                   (/= previuous-elem -1)
                   (> window-values-sum
                      previuous-elem))
                  (+ greater-than-before-counter 1)
                greater-than-before-counter)))
        (cons counter new-previous-elem)))
    measuraments
    (cons 0 -1))))

(defun sliding-window (list window-size)
  "Turn `LIST' into a list of lists, every sub list is created using a sliding window of size `WINDOW-SIZE'."
  (seq-map
   (lambda (index) (seq-subseq list index (+ index window-size)))
   (number-sequence 0 (- (seq-length list) window-size))))

(assert (equal
         (sliding-window (list 0 1 2 3 4 5) 3)
         (list (list 0 1 2) (list 1 2 3) (list 2 3 4) (list 3 4 5))))


(defun number-list-from-file-content (file-path)
  "Get a list of numbers from `FILE-PATH' content splitting by \n."
  (with-temp-buffer
    (insert-file-contents file-path)
    (seq-map (lambda (s) (string-to-number s)) (split-string (buffer-string) "\n"))))

(solution_1 (sliding-window '(199 200 208 210 200 207 240 269 260 263) 1))
(solution_1 (sliding-window (number-list-from-file-content "./day_1.txt") 1))
(solution_1 (sliding-window (number-list-from-file-content "./day_1.txt") 3))
