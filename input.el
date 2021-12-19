(defun read-file-as-string (file-path)
  "Get a string from `FILE-PATH'."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))
