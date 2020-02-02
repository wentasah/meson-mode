
(defun py2el (beg end)
  "Helper function for converting Meson data structures from
Python sources to something more similar to elisp."
  (interactive "r")
  (save-excursion
    (dolist (search-repl
	     '(("'" . "\"")
	       ("{" . "(")
	       ("}" . ")")
	       ("," . " ")))
      (goto-char beg)
      (let ((search (car search-repl))
	    (replace (cdr search-repl)))
	(while (re-search-forward search end t)
	  (replace-match replace))))))
