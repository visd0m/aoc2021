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

;;;;;;;;;;;;; 
;;; Day 3 ;;;
;;;;;;;;;;;;;

(defun nth-list (list-of-lists index)
  (seq-map (lambda (list) (nth index list)) list-of-lists))

(defun most-or-least-common-bit (bits most-or-least)
  (let* ((counters (seq-reduce
                    (lambda (acc elem)
                      (if (eq elem ?0)
                          (cons (+ (car acc) 1) (cdr acc))
                        (cons (car acc) (+ (cdr acc) 1))))
                    bits
                    (cons 0 0)))
         (0-counter (car counters))
         (1-counter (cdr counters)))
    (pcase most-or-least
      ('most
       (if (> 0-counter 1-counter)
           "0"
         "1"))
      ('least
       (if (> 0-counter 1-counter)
           "1"
         "0")))))

(most-or-least-common-bit
 (nth-list
  (seq-map 'string-to-list (diagnostic-report-from-input-file "./day_3.in"))
  4)
 'least)

(defun rate (list-of-bits-lists gamma-or-epsilon)
  (let ((most-or-least (if (eq gamma-or-epsilon 'gamma)
                           'most
                         'least))
        (number-of-bits (- (length (nth 0 list-of-bits-lists)) 1)))
    (seq-reduce
     (lambda (acc elem)
       (format "%s%s" acc (most-or-least-common-bit (nth-list list-of-bits-lists elem) most-or-least)))
     (number-sequence 0 number-of-bits)
     "")))

(defun solution_day_3 (diagnostic-report)
  (let* ((list-of-byte-lists (seq-map 'string-to-list diagnostic-report))
         (gamma-rate (string-to-number (rate list-of-byte-lists 'gamma) 2))
         (epsilon-rate (string-to-number (rate list-of-byte-lists 'epsilon) 2)))
    (* gamma-rate epsilon-rate)))

(solution_day_3 (split-string (read-file-as-string "./day_3.in") "\n"))
(solution_day_3 (split-string "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010" "\n"))


(defun bit-criteria-rate (list-of-bits-lists rating-type)
  (let* ((most-or-least (if (eq rating-type 'oxygen-generator)
                            'most
                          'least))
         (number-of-bits (- (length (nth 0 list-of-bits-lists)) 1))
         (bits (first (seq-reduce (lambda (acc index)
                                    (if (equal 1 (length acc))
                                        acc
                                      (let ((filtering-bit (most-or-least-common-bit (nth-list acc index) most-or-least)))
                                        (seq-filter
                                         (lambda (elem)
                                           (equal (string-to-char filtering-bit) (nth index elem)))
                                         acc))))
                                  (number-sequence 0 number-of-bits)
                                  list-of-bits-lists))))
    (seq-reduce (lambda (acc elem) (format "%s%s" acc (string elem))) bits "")))

(bit-criteria-rate (list (list ?1) (list ?0)) 'oxygen-generator)

(defun solution_day_3_part_2 (diagnostic-report)
  (let* ((list-of-byte-lists (seq-map 'string-to-list diagnostic-report))
         (oxygen-generator-rating (string-to-number (bit-criteria-rate list-of-byte-lists 'oxygen-generator) 2))
         (co2-scrubber-rating (string-to-number (bit-criteria-rate list-of-byte-lists 'co2-scrubber) 2)))
    (* oxygen-generator-rating co2-scrubber-rating)))

(solution_day_3_part_2 (split-string "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010" "\n"))
(solution_day_3_part_2 (split-string (read-file-as-string "./day_3.in") "\n"))


;;;;;;;;;;;;; 
;;; Day 4 ;;;
;;;;;;;;;;;;;

(defun read-boards-from-input-lines (lines)
  (let* ((boards-lines (seq-subseq lines 1 (length lines)))
         (boards-lines (seq-map
                        (lambda (line-numbers) (seq-map
                                                (lambda (number-as-string)
                                                  (cons (string-to-number number-as-string) nil))
                                                (seq-remove 'string-empty-p (split-string line-numbers " "))))
                        boards-lines)))
    (seq-partition boards-lines 5)))

(read-boards-from-input-lines (split-string "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19" "\n"))

(defun update-nth-in-list (list nth new-value step)
  (if (equal step nth)
      (progn
        (setcdr list (cdr list))
        (setcar list new-value))
    (update-nth-in-list (cdr list) nth new-value (+ 1 step))))

(let* ((test-list (list 1 2 3 4)))
  (update-nth-in-list test-list 3 5 0)
  test-list)

(defun try-mark-row (row number)
  (let* ((found? (assoc number row))
         (found-as-bool (if found? t nil)))
    (if found?
        (setf (cdr found?) t))
    (cons row found-as-bool)))

(defun try-mark-number (board number)
  (car (seq-reduce
        (lambda (board-and-found row-index)
          (let ((found? (cdr board-and-found)))
            (if found?
                (cons board found?)
              (let* ((board (car board-and-found))
                     (current-line (nth row-index board))
                     (mark-number-result (try-mark-row current-line number))
                     (line-to-insert (car mark-number-result)))
                (update-nth-in-list board row-index line-to-insert 0)
                (cons board (cdr mark-number-result))))))
        (number-sequence 0 (- (length board) 1))
        (cons board nil))))

(try-mark-number (list (list (cons 1 nil) (cons 2 nil)) (list (cons 3 nil) (cons 4 nil))) 1)

(defun bingo-rows? (board)
  (seq-reduce
   (lambda (bingo? row)
     (if bingo?
         bingo?
       (seq-reduce
        (lambda (marked? number)
          (and marked? (cdr number)))
        row
        t)))
   board
   nil))

(bingo-rows? (list (list (cons 1 t) (cons 2 nil)) (list (cons 3 nil) (cons 4 t))))

(defun board-columns (board)
  (seq-reduce
   (lambda (columns index)
     (push (nth-list board index) columns))
   (number-sequence 0 (- (length (nth 0 board)) 1))
   (list)))

(board-columns (list (list (cons 1 nil) (cons 2 nil)) (list (cons 3 nil) (cons 4 nil))))

(defun bingo-columns? (board)
  (let ((columns (board-columns board)))
    (seq-reduce
     (lambda (bingo? row)
       (if bingo?
           bingo?
         (seq-reduce (lambda (marked? number) (and marked? (cdr number))) row t)))
     columns
     nil)))

(bingo-columns? (list (list (cons 1 t) (cons 2 t)) (list (cons 3 nil) (cons 4 t))))

(defun bingo? (board)
  (or (bingo-rows? board) (bingo-columns? board)))

(let* ((board (list (list (cons 1 t) (cons 2 t)) (list (cons 3 nil) (cons 4 t))))
       (bingo? (bingo? board)))
  board)

(defun winning-score (board extracted-number)
  (let* ((sum-rows (seq-map (lambda (row)
                              (seq-reduce (lambda (sum-row number)
                                            (if (cdr number)
                                                sum-row
                                              (+ sum-row (car number))))
                                          row
                                          0))
                            board))
         (board-sum (seq-reduce '+ sum-rows 0)))
    (* extracted-number board-sum)))

(defun solution_day_4 (input-as-string firt-or-last)
  (let* ((lines (seq-remove 'string-empty-p (split-string input-as-string "\n")))
         (extracted-numbers (seq-map 'string-to-number (split-string (first lines) ",")))
         (boards (read-boards-from-input-lines lines)))
    (cdr (seq-reduce
          (lambda (state number)
            (let ((winning-score (cdr state))
                  (boards (car state)))
              (if (and winning-score (equal 'first firt-or-last))
                  (cons boards winning-score)
                (let* ((boards (seq-map (lambda (board) (try-mark-number board number)) boards))
                       ;; more than one board can win for the same number 
                       (winning-boards (seq-filter 'bingo? boards))
                       ;; the first of the winning boards is considered as the FIRST winning board
                       (winning-board (first winning-boards))
                       (winning-score (if winning-board (winning-score winning-board number) winning-score)))
                  (cons
                   (seq-remove (lambda (board)
                                 (seq-find (lambda (winning-board)
                                             (equal winning-board board))
                                           winning-boards))
                               boards)
                   winning-score)))))
          extracted-numbers
          (cons boards nil)))))

(solution_day_4 "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7" 'last)
;; part 1
(solution_day_4 (read-file-as-string "./day_4.in") 'first)
;; part 2
(solution_day_4 (read-file-as-string "./day_4.in") 'last)
