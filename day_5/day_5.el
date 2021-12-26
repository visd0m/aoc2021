(load-file "../input.el")
(load-file "../list.el")

(defun parse-xy (xy-as-string)
  "Parse the string `XY-AS-STRING' in the format \"x,y\" returning a cons representing a point composed by x and uy coordinate."
  (let ((tokens (split-string xy-as-string ",")))
    (cons
     (string-to-number (nth 0 tokens))
     (string-to-number (nth 1 tokens)))))

(parse-xy "561,579")

(defun parse-line (line-as-string)
  "Parse the string `LINE-AS-STRING' in the format \"561,579 -> 965,175\" returning a cons having as car a point representing one end of the line and as cdr the other end of the line."
  (let* ((tokens (split-string line-as-string " -> "))
         (one-end (parse-xy (nth 0 tokens)))
         (the-other-end (parse-xy (nth 1 tokens))))
    (cons one-end the-other-end)))

(parse-line "561,579 -> 965,175")

(defun x (point)
  "Get x coordinate of `POINT'"
  (car point))

(defun y (point)
  "Get y coordinate of `POINT'"
  (cdr point))

(defun p1 (line)
  "Get starting point of `LINE'"
  (car line))

(defun p2 (line)
  "Get last point of `LINE'"
  (cdr line))

(defun is-valid-line? (line)
  "Return true if `LINE' is a horizontal or vertical line."
  (let* ((p1 (p1 line))
         (p2 (p2 line))
         (x1 (x p1))
         (y1 (y p1))
         (x2 (x p2))
         (y2 (y p2)))
    (or (equal x1 x2) (equal y1 y2))))

(defun range (from to)
  "Get the list of points (x, y) existing between `FROM' and `TO'."
  (let (result (list))
    (while (not (equal from to))
      (setq result (push from result)
            from (next-point from to)))
    (nreverse (push from result))))

(range (cons 0 0) (cons 0 3))

(assert (equal (list (cons 0 0) (cons 0 1) (cons 0 2) (cons 0 3)) (range (cons 0 0) (cons 0 3))))
(assert (equal (list (cons 0 0) (cons 1 0) (cons 2 0) (cons 3 0)) (range (cons 0 0) (cons 3 0))))
(assert (equal (list (cons 0 0) (cons 1 1) (cons 2 2) (cons 3 3)) (range (cons 0 0) (cons 3 3))))
(range (cons 735 73) (cons 316 73))

(defun next-point (from to)
  "Get nex point going from `FROM' to `TO'."
  (let* ((x1 (x from))
         (y1 (y from))
         (x2 (x to))
         (y2 (y to))
         (new-x (if (< x1 x2) (+ 1 x1) (if (> x1 x2) (- x1 1) x1)))
         (new-y (if (< y1 y2) (+ 1 y1) (if (> y1 y2) (- y1 1) y1))))
    (cons new-x new-y)))

(assert (equal (cons 0 4) (next-point (cons 0 5) (cons 0 1))))
(assert (equal (cons 0 2) (next-point (cons 0 1) (cons 0 5))))
(assert (equal (cons -1 4) (next-point (cons 0 4) (cons -1 4))))
(assert (equal (cons 0 4) (next-point (cons -1 4) (cons 0 4))))
(assert (equal (cons 0 5) (next-point (cons -1 4) (cons 0 5))))

(defun day_5 ()
  (let* ((input-lines (seq-map 'parse-line (split-string (read-file-as-string "./day_5.in") "\n")))
         (valid-lines (seq-filter 'is-valid-line? input-lines))
         (points (seq-map (lambda (fromto) (range (car fromto) (cdr fromto))) valid-lines))
         (points (apply 'append points))
         ;; the grouping by opertion is extremely slow
         (grouped (seq-group-by (lambda (point) (format "%s,%s" (x point) (y point))) points))
         (more-than-one-occurrence (seq-filter (lambda (entry) (>= (length (cdr entry)) 2)) grouped)))
    (length more-than-one-occurrence)))

(day_5)
