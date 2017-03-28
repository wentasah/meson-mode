;;; meson-mode.el --- Major mode for the Meson build system files  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Michal Sojka

;; Author: Michal Sojka <sojkam1@fel.cvut.cz>
;; Version: 0.1
;; Keywords: languages, tools
;; URL: https://github.com/wentasah/meson-mode
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a major mode for Meson build system files. Syntax
;; highlighting works reliably. Indentation works too, but there are
;; probably cases, where it breaks.

;;; Code:

(defvar meson-mode-syntax-table
  (let ((table (make-syntax-table))
	(list (list ?\# "<"
		    ?\n ">#"
		    ?\' "\"'" ; See also meson-syntax-propertize-function
		    ?\" "."
		    )))
    (while list
      (modify-syntax-entry (pop list) (pop list) table))
    table)
  "Syntax table used while in `meson-mode'.")

(defun meson--max-length (&rest args)
  (let ((lengths
	 (mapcar (lambda (x) (if (stringp x) (length x) x)) args)))
    (apply 'max lengths)))

(defconst meson-keywords
  '("true" "false" "if" "else" "elif" "endif" "and" "or" "not" "foreach" "endforeach"))

(defconst meson-keywords-regexp
  (rx symbol-start (eval `(or ,@meson-keywords)) symbol-end))

(require 'cl-lib)

(defconst meson-keywords-max-length
  (cl-reduce 'meson--max-length meson-keywords))

(defconst meson-builtin-functions-regexp
  (rx (or line-start (not (any ".")))
      symbol-start
      (group (or "add_global_arguments"
		 "add_global_link_arguments" "add_languages"
		 "add_project_arguments" "add_project_link_arguments"
		 "add_test_setup" "benchmark" "build_target"
		 "configuration_data" "configure_file" "custom_target"
		 "declare_dependency" "dependency" "error" "environment"
		 "executable" "find_program" "find_library" "files"
		 "generator" "get_option" "get_variable" "import"
		 "include_directories" "install_data" "install_headers"
		 "install_man" "install_subdir" "is_variable" "jar"
		 "join_paths" "library" "message" "project" "run_command"
		 "run_target" "set_variable" "shared_library"
		 "shared_module" "static_library" "subdir" "subproject"
		 "test" "vcs_tag"))
      symbol-end
      (zero-or-more whitespace)
      (or "(" line-end)))

(defconst meson-builtin-vars-regexp
  (rx symbol-start
      (or "meson" "build_machine" "host_machine" "target_machine")
      symbol-end))

(defconst meson-literate-tokens
  '(;;"(" ")" "[" "]" ; Let syntactic parser handle these efficiently
    "\"" "," "+=" "." "+" "-" "*"
    "%" "/" ":" "==" "!=" "=" "<=" "<" ">=" ">" "?"))

(defconst meson-literate-tokens-max-length
  (cl-reduce 'meson--max-length meson-literate-tokens))

(defconst meson-literate-tokens-regexp
  (rx (eval `(or ,@meson-literate-tokens))))

(defconst meson-multiline-string-regexp
  (rx "'''" (zero-or-more anything) "'''"))
(defconst meson-string-regexp
  (rx "'"
      (zero-or-more
       (or (not (any "'" "\\"))
	   (seq "\\" nonl)))
      "'"))
(defconst meson-comment-regexp
  (rx "#" (zero-or-more nonl)))

(defconst meson-token-spec
  `(("ignore" . ,(rx (any " " "\t")))
    ("id" . ,(rx (any "_" "a-z" "A-Z") (zero-or-more (any "_" "a-z" "A-Z" "0-9"))))
    ("number" . ,(rx (one-or-more (any digit))))
    ("eol_cont" . ,(rx "\\" "\n"))
    ("eol" . "\n")
    ("comment" . ,meson-comment-regexp)
    ("string" . ,(rx (or (eval `(regexp ,meson-multiline-string-regexp))
			 (eval `(regexp ,meson-string-regexp)))))))

(defvar meson-mode-font-lock-keywords
  `((,meson-keywords-regexp . font-lock-keyword-face)
    (,meson-builtin-functions-regexp . (1 font-lock-builtin-face))
    (,meson-builtin-vars-regexp . font-lock-variable-name-face)))

(defconst meson-syntax-propertize-function
  (syntax-propertize-rules
   ((rx (or "'''" "'")) (0 (ignore (meson-syntax-stringify))))))

(defsubst meson-syntax-count-quotes (&optional point limit)
  "Count number of quotes after point (max is 3).
POINT is the point where scan starts (defaults to current point),
and LIMIT is used to limit the scan."
  (let ((i 0)
	(p (or point (point))))
    (while (and (< i 3)
                (or (not limit) (< (+ p i) limit))
                (eq (char-after (+ p i)) ?\'))
      (setq i (1+ i)))
    i))

(defun meson-syntax-stringify ()
  "Put `syntax-table' property correctly on single/triple apostrophes."
  ;; Inspired by python-mode
  (let* ((num-quotes (length (match-string-no-properties 0)))
         (ppss (prog2
                   (backward-char num-quotes)
                   (syntax-ppss)
                 (forward-char num-quotes)))
	 (in-comment (nth 4 ppss))
         (string-start (and (not in-comment) (nth 8 ppss)))
         (quote-starting-pos (- (point) num-quotes))
         (quote-ending-pos (point))
	 (num-closing-quotes
          (and string-start
               (meson-syntax-count-quotes
                string-start quote-starting-pos))))
    (cond ((and string-start (= num-closing-quotes 0))
           ;; This set of quotes doesn't match the string starting
           ;; kind. Do nothing.
           nil)
          ((not string-start)
           ;; This set of quotes delimit the start of a string.
           (put-text-property quote-starting-pos (1+ quote-starting-pos)
                              'syntax-table (string-to-syntax "|")))
          ((= num-quotes num-closing-quotes)
           ;; This set of quotes delimit the end of a string.
           (put-text-property (1- quote-ending-pos) quote-ending-pos
                              'syntax-table (string-to-syntax "|")))
          ((> num-quotes num-closing-quotes)
           ;; This may only happen whenever a triple quote is closing
           ;; a single quoted string. Add string delimiter syntax to
           ;; all three quotes.
           (put-text-property quote-starting-pos quote-ending-pos
                              'syntax-table (string-to-syntax "|"))))))

;;; Indetation

(require 'smie)

(defun meson-smie-forward-token ()
  (let ((token 'unknown)
	(ppss (syntax-ppss)))
    (while (eq token 'unknown)
      (setq token
	    (cond
	     ((looking-at meson-keywords-regexp) (match-string-no-properties 0))
	     ((cl-some (lambda (spec) (when (looking-at (cdr spec)) (car spec)))
		       meson-token-spec))
	     ((looking-at meson-literate-tokens-regexp)
	      (match-string-no-properties 0))))
      (let ((after-token (when token (match-end 0))))
      ;; Skip certain tokens
	(when (or (equal token "comment")
		  (equal token "ignore")
		  (and (equal token "eol")    ; Skip EOL when:
		       (or (> (nth 0 ppss) 0) ; - inside parentheses
			   (looking-back      ; - after operator
			    meson-literate-tokens-regexp
			    meson-literate-tokens-max-length)
			   (smie-indent--bolp-1)))) ; - at empty line
	  (setq token 'unknown))
	(when after-token
	  (goto-char after-token))))
    token))

(defun meson-smie-backward-token ()
  (let ((token 'unknown))
    (while (eq token 'unknown)
      (let ((eopl (line-end-position 0)) ; end of previous line
	    (ppss (syntax-ppss)))
	(setq token
	      (cond
	       ;; Skip comments
	       ((nth 4 ppss)		 ; We are in a comment
		(goto-char (nth 8 ppss)) ; goto its beginning
		'unknown)
	       ;; Check for strings. Relying on syntactic parser allows us to
	       ;; find the beginning of multi-line strings efficiently.
	       ((equal (char-before) ?\')
		(let* ((ppss- (syntax-ppss (1- (point))))
		       (string-start (when (nth 3 ppss-)
				       (nth 8 ppss-))))
		  (goto-char string-start)
		  "string"))
	       ;; Regexp-based matching
	       (t (let ((tok
			 (cond
			  ((looking-back meson-keywords-regexp (- (point) meson-keywords-max-length) t)
			   (match-string-no-properties 0))
			  ((cl-some (lambda (spec) (when (looking-back (cdr spec) eopl t) (car spec)))
					  meson-token-spec))
			  ((looking-back meson-literate-tokens-regexp
					 (- (point) meson-literate-tokens-max-length) t)
			   (match-string-no-properties 0)))))
		    (when tok
		      (goto-char (match-beginning 0)))
		    tok))))
	(when (or (equal token "comment")
		  (equal token "ignore")
		  (and (equal token "eol")  ; Skip EOL when:
		       (or (> (nth 0 ppss) 0) ; - inside parentheses
			   (looking-back      ; - after operator
			    meson-literate-tokens-regexp
			    meson-literate-tokens-max-length)
			   (smie-indent--bolp-1)))) ;- at empty line
	  (setq token 'unknown))))
    token))

(defconst meson-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (codeblock (line)
		 (codeblock "eol" codeblock))
      (line (exp)
	    ("eof")
	    ("if" ifblock "endif")
	    ("if" ifblock "else" codeblock "endif")
	    ("foreach" foreachblock "endforeach"))
      (foreachblock (id ":" exp "eol" codeblock))
      (ifblock (exp "eol" codeblock)
	       (exp "eol" codeblock "elif" ifblock)
	       )
      (exp (exp "," exp)
	   (id ":" exp)
	   (exp "+=" exp)
	   (exp "=" exp)
;; 	   (exp "?" exp ":" exp)
;; 	   (exp "or" exp)
;; 	   (exp "and" exp)
;; 	   (exp "==" exp)
;; 	   (exp "!=" exp)
;; 	   (exp "<"  exp)
;; 	   (exp "<=" exp)
;; 	   (exp ">"  exp)
;; 	   (exp ">=" exp)
;; 	   (exp "+" exp)
;; 	   (exp "-" exp)
;; 	   (exp "*" exp)
;; 	   (exp "/" exp)
;; 	   (exp "%" exp)
;; 	   ("not" exp)
;; 	   ("-" exp)
;; 	   (exp "." methodcall)
;; 	   (exp "." exp)
;; 	   (exp "(" args ")")
;; 	   (exp "(" args ")" indexcall)
;; 	   ("[" array "]")
	   ("true")
	   ("false"))
;;       (args (exp)

;; 	    (id ":" exp))
;;       (array (array "," array))
;;       (methodcall (exp "(" args ")" )
;; 		  ;; (exp "(" args ")" "." methodcall)
;; 		  )
      ;;      (indexcall ( "[" exp "]"))
      )
    `((assoc "eol")
      (assoc ",")
      (assoc ":")
      (assoc "+=" "=")
;;       (assoc "or")
;;       (assoc "and")
;;       (assoc "==" "!=" "<" "<=" ">" ">=")
;;       (assoc "+" "-")
;;       (assoc "*" "/" "%")
;;       (assoc ".")
      )
    )))

(defgroup meson nil
  "Meson build system mode for Emacs."
  :group 'tools
  :prefix "meson-")

(defcustom meson-indent-basic 2
  "Indentation offset for meson.build files.")

(defun meson-smie-rules (kind token)
;;   (ignore-errors
;;     (message "Parent: %s" (smie-indent--parent)))
  (pcase (cons kind token)
    (`(:elem . basic) meson-indent-basic)
    (`(,_ . ",") (smie-rule-separator kind))
    ;(`(:after . ,(or "[" "(")) (smie-rule-s meson-indent-basic)) ; TODO
    (`(:after . "eol") (if (smie-rule-parent-p "if" "foreach" "elif" "else")
			   (smie-rule-parent meson-indent-basic)
			 (smie-rule-parent)))
    (`(:after . ,(or meson-literate-tokens)) (smie-rule-parent meson-indent-basic))
    (`(:before . "eol") (if (smie-rule-parent-p "if" "foreach" "elif" "else")
			   (smie-rule-parent meson-indent-basic)))
    (`(:before . "elif") (smie-rule-parent))
    (_ nil)))

;;; Mode definition

;; For debugging
(defvar meson-mode-map
  (let ((map (make-sparse-keymap)))
;  (let ((map global-map))
    (define-key map (kbd "<f5>") (lambda () (interactive) (message (funcall smie-backward-token-function))))
    (define-key map (kbd "<f6>") (lambda () (interactive) (message (funcall smie-forward-token-function))))
    (define-key map (kbd "<f7>") 'smie-config-show-indent)
    (define-key map (kbd "<f8>") (lambda () (interactive) (message "Indent: %d" (smie-indent-calculate))))
    map)
  "Meson mode map - helps with debugging of ‘meson-mode’ itself.")

;;;###autoload
(define-derived-mode meson-mode prog-mode "Meson"
  "Major mode for editing Meson build system files."
  :abbrev-table nil
  (setq font-lock-defaults
	'(meson-mode-font-lock-keywords
	  nil nil nil nil
	  ))

  (set (make-local-variable 'syntax-propertize-function)
       meson-syntax-propertize-function)

  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (smie-setup meson-smie-grammar #'meson-smie-rules
	      :forward-token #'meson-smie-forward-token
	      :backward-token #'meson-smie-backward-token)
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("/meson\\.build\\'" . meson-mode))

(provide 'meson-mode)
;;; meson-mode.el ends here

;;(progn (mapatoms (lambda (x) (when (string-prefix-p "meson" (symbol-name x)) (makunbound x)))) (eval-buffer))
