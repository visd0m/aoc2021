(load-file "../input.el")

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
