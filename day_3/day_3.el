(load-file "../input.el")
(load-file "../list.el")

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
