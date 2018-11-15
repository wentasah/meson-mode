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
;; probably cases, where it breaks. Simple completion is supported via
;; `completion-at-point'. To start completion, use either <C-M-i> or
;; install completion frameworks such as `company'. To enable
;; `company' add the following to your .emacs:
;;
;;     (add-hook 'meson-mode-hook 'company-mode)


;;; Code:

(require 'compile)

(defvar meson-mode-syntax-table
  (let ((table (make-syntax-table))
	(list (list ?\# "<"
		    ?\n ">#"
		    ?\' "\"'" ; See also meson-syntax-propertize-function
		    ?\" "."
		    ?\$ "."
		    ?\& "."
		    ?\* "."
		    ?\+ "."
		    ?\- "."
		    ?\< "."
		    ?\> "."
		    ?\= "."
		    ?\/ "."
		    ?\| "."
		    )))
    (while list
      (modify-syntax-entry (pop list) (pop list) table))
    table)
  "Syntax table used while in `meson-mode'.")

(defun meson--max-length (&rest args)
  (let ((lengths
	 (mapcar (lambda (x) (if (stringp x) (length x) x)) args)))
    (apply 'max lengths)))

(eval-and-compile
  (defconst meson-keywords
    '("true" "false" "if" "else" "elif" "endif" "and" "or" "not" "foreach" "endforeach")))

(defconst meson-keywords-regexp
  (rx symbol-start (eval `(or ,@meson-keywords)) symbol-end))

(require 'cl-lib)

(defconst meson-keywords-max-length
  (cl-reduce 'meson--max-length meson-keywords))

(eval-and-compile
  (defconst meson-builtin-functions
    '("add_global_arguments"
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
      "test" "vcs_tag")))

(defconst meson-builtin-functions-regexp
  (rx (or line-start (not (any ".")))
      symbol-start
      (group (eval `(or ,@meson-builtin-functions)))
      symbol-end
      (zero-or-more whitespace)
      (or "(" line-end)))

(eval-and-compile
  (defconst meson-builtin-vars
    '("meson" "build_machine" "host_machine" "target_machine")))

(defconst meson-builtin-vars-regexp
  (rx symbol-start
      (or (eval `(or ,@meson-builtin-vars)))
      symbol-end))

(eval-and-compile
  (defconst meson-literate-tokens
    '( ;;"(" ")" "[" "]" ; Let syntactic parser handle these efficiently
      "\"" "," "+=" "." "+" "-" "*"
      "%" "/" ":" "==" "!=" "=" "<=" "<" ">=" ">" "?")))

(defconst meson-literate-tokens-max-length
  (cl-reduce 'meson--max-length meson-literate-tokens))

(defconst meson-literate-tokens-regexp
  (rx (eval `(or ,@meson-literate-tokens))))

(defconst meson-methods
  `(("meson\\."
     . ("get_compiler"
	"is_cross_build"
	"has_exe_wrapper"
	"is_unity"
	"is_subproject"
	"current_source_dir"
	"current_build_dir"
	"source_root"
	"build_root"
	"add_install_script"
	"add_postconf_script"
	"install_dependency_manifest"
	"project_version"
	"version"
	"project_name"
	"get_cross_property"
	"backend"))
    (,(regexp-opt '("build_machine."
		    "host_machine."
		    "target_machine."))
     . ("system"
	"cpu_family"
	"cpu"
	"endian"))
    (""
     . ( ;; class TryRunResultHolder
	"returncode"
	"compiled"
	"stdout"
	"stderr"

	;; class RunProcess
	"returncode"
	"stdout"
	"stderr"

	;; class EnvironmentVariablesHolder
	"set"
	"append"
	"prepend"

	;; class ConfigurationDataHolder
	"set"
	"set10"
	"set_quoted"
	"has"
	"get"

	;; class DependencyHolder
	"found"
	"type_name"
	"version"
	"get_pkgconfig_variable"

	;; class InternalDependencyHolder
	"found"
	"version"

	;; class ExternalProgramHolder
	"found"

	;; class ExternalLibraryHolder
	"found"

	;; class GeneratorHolder
	"process"

	;; class BuildMachine
	"system"
	"cpu_family"
	"cpu"
	"endian"

	;; class CrossMachineInfo
	"system"
	"cpu"
	"cpu_family"
	"endian"

	;; class BuildTargetHolder
	"extract_objects"
	"extract_all_objects"
	"get_id"
	"outdir"
	"full_path"
	"private_dir_include"

	;; class CustomTargetHolder
	"full_path"

	;; class SubprojectHolder
	"get_variable"

	;; class CompilerHolder
	"compiles"
	"links"
	"get_id"
	"compute_int"
	"sizeof"
	"has_header"
	"has_header_symbol"
	"run"
	"has_function"
	"has_member"
	"has_members"
	"has_type"
	"alignment"
	"version"
	"cmd_array"
	"find_library"
	"has_argument"
	"has_multi_arguments"
	"first_supported_argument"
	"unittest_args"
	"symbols_have_underscore_prefix"

	;; string
	"strip"
	"format"
	"to_upper"
	"to_lower"
	"underscorify"
	"split"
	"startswith"
	"endswith"
	"contains"
	"to_int"
	"join"
	"version_compare"

	;; number
	"is_even"
	"is_odd"

	;; boolean
	"to_string"
	"to_int"

	;; array
	"length"
	"contains"
	"get"

	))))

(defconst meson-basic-kwargs
  '("install"
    "c_pch"
    "cpp_pch"
    "c_args"
    "cpp_args"
    "cs_args"
    "vala_args"
    "fortran_args"
    "d_args"
    "java_args"
    "link_args"
    "link_depends"
    "link_with"
    "include_directories"
    "dependencies"
    "install_dir"
    "main_class"
    "gui_app"
    "extra_files"
    "install_rpath"
    "resources"
    "sources"
    "objects"
    "native"
    "build_by_default"
    ))

(defconst meson-kwargs
  `(("executable"
     . ,meson-basic-kwargs)
    ("library"
     . ,(append meson-basic-kwargs
		'("version"		; Only for shared libs
		  "soversion"		; Only for shared libs
		  "name_prefix"
		  "name_suffix"
		  "vs_module_defs"	; Only for shared libs
		  "vala_header"
		  "vala_vapi"
		  "vala_gir"
		  "pic"			; Only for static libs
		  )))
    ("project"
     . ("version"
	"meson_version"
	"default_options"))
    ("run_target"
     . ("command"
	"depends"))
    ("test"
     . ("args"
	"env"
	"is_parallel"
	"should_fail"
	"valgring_args"
	"timeout"
	"workdir"))
    ("vcs_tag"
     . ("input"
	"output"
	"fallback"))
    ("install_[[:alpha:]]+"
     . ("install_dir"))
    ("add_languages"
     . ("required"))
    ("add_test_setup"
     . ("exe_wrapper"
	"gdb"
	"timeout_multiplier"
	"env"))
    ("benchmark"
     . ("args"
	"env"
	"should_fail"
	"valgring_args"
	"timeout"
	"workdir"))
    ("configure_file"
     . ("input"
	"output"
	"configuration"
	"command"
	"install_dir"))
    ("custom_target"
     . ("input"
	"output"
	"command"
	"install"
	"install_dir"
	"build_always"
	"capture"
	"depends"
	"depend_files"
	"depfile"
	"build_by_default"))
    ("declare_dependency"
     . ("include_directories"
	"link_with"
	"sources"
	"dependencies"
	"compile_args"
	"link_args"
	"version"))
     ("dependency"
      . ("modules"
	 "required"
	 "version"
	 "native"
	 "static"
	 "fallback"
	 "default_options"))
     ))


(eval-and-compile
  (defconst meson-multiline-string-regexp
    (rx "'''" (minimal-match (zero-or-more anything)) "'''"))
  (defconst meson-string-regexp
    (rx "'"
	(zero-or-more
	 (or (not (any "'" "\\"))
	     (seq "\\" nonl)))
	"'")))

(defconst meson-string-regexp
  (rx (or (eval `(regexp ,meson-multiline-string-regexp))
			 (eval `(regexp ,meson-string-regexp)))))

(defconst meson-token-spec
  `(("ignore" . ,(rx (one-or-more (any " " "\t"))))
    ("id" . ,(rx (any "_" "a-z" "A-Z") (zero-or-more (any "_" "a-z" "A-Z" "0-9"))))
    ("number" . ,(rx (one-or-more (any digit))))
    ("eol_cont" . ,(rx "\\" "\n"))
    ("eol" . "\n")))

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

;;; Completion

(defun meson-completion-at-point-function ()
  (save-excursion
    (let* ((end (progn (skip-syntax-forward "w_")
		       (point)))
	   (start (progn (skip-syntax-backward "w_")
			 (point)))
	   (ppss (syntax-ppss)))
      (cond
       ((or (nth 3 ppss)		; inside string
	    (nth 4 ppss))		; inside comment
	nil) ; nothing to complete

       ;; kwargs
       ((and (> (nth 0 ppss) 0)		; inside parentheses
	     (eq (char-after (nth 1 ppss)) ?\()) ; rounded parentheses
	(goto-char (nth 1 ppss))
	(let ((kwargs (cl-some (lambda (x)
				 (when (looking-back (concat (car x) (rx (zero-or-more (any " " "\t"))))
						     (line-beginning-position))
				   (cdr x)))
			       meson-kwargs)))
	  ;; complete mathing kwargs as well as built-in
	  ;; variables/functions
	  (list start end (append kwargs meson-builtin-vars
				  meson-builtin-functions))))

       ;; methods
       ((eq (char-before) ?.)
	(let ((methods (cl-some
			(lambda (x)
			  (when (looking-back (car x) (line-beginning-position))
			    (cdr x)))
			meson-methods)))
	  (list start end methods)))
       ;; global things
       (t
        (list start end (append meson-keywords meson-builtin-vars
				meson-builtin-functions)))))))


;;; Indetation

(require 'smie)

(defun meson--comment-bolp (&optional ppss_)
  "Return non-nil if point is at the beginning of line, ignoring
comments."
  (save-excursion
    (let ((ppss (or ppss_
		    (syntax-ppss))))
      (when (nth 4 ppss) 		; inside comment
	(goto-char (nth 8 ppss)))	; go to its beginning
      (smie-rule-bolp))))

(defun meson-smie-forward-token ()
  (let ((token 'unknown))
    (while (eq token 'unknown)
      (let ((ppss (syntax-ppss)))
	;; When inside or at start of a comment, goto end of line so
	;; that we can still return "eol" token there.
	(when (or (nth 4 ppss)
		  (and (not (nth 3 ppss)) ; not inside string
		       (looking-at "#")))
	  (end-of-line)
	  (setq ppss (syntax-ppss)))	; update ppss after move
	;; Determine token but do not move behind it
	(setq token
	      (cond
	       ;; Let syntactic parser handle parentheses (even inside
	       ;; strings - this ensures that parentheses are NOT
	       ;; indented inside strings according to meson
	       ;; indentation rules)
	       ((looking-at (rx (or (syntax open-parenthesis)
				    (syntax close-parenthesis))))
		"")
	       ;; After handling parentheses (inside strings), we can
	       ;; handle strings
	       ((or (when (nth 3 ppss)		; If inside string
		      (goto-char (nth 8 ppss))	; goto beginning
		      nil)
		    (looking-at meson-string-regexp)) ; Match the whole string
		"string")
	       ((looking-at meson-keywords-regexp) (match-string-no-properties 0))
	       ((cl-some (lambda (spec) (when (looking-at (cdr spec)) (car spec)))
			 meson-token-spec))
	       ((looking-at meson-literate-tokens-regexp)
		(match-string-no-properties 0))))
	;; Remember token end (except for parentheses)
	(let ((after-token (when (< 0 (length token)) (match-end 0))))
	  ;; Skip certain tokens
	  (when (or (equal token "ignore")
		    (equal token "eol_cont")
		    (and (equal token "eol")	; Skip EOL when:
			 (or (> (nth 0 ppss) 0) ; - inside parentheses
			     (and (looking-back	; - after operator but not inside comments
				   meson-literate-tokens-regexp
				   (- (point) meson-literate-tokens-max-length))
				  (not (nth 4 ppss)))
			     (meson--comment-bolp ppss)))) ; - at empty line
	    (setq token 'unknown))
	  (when after-token
	    (goto-char after-token)))))
    token))

(defun meson-smie-backward-token ()
  (let ((token 'unknown))
    (while (eq token 'unknown)
      (let ((eopl (max ;; end of previous line (to properly match "eol_cont" below it is actually a character before)
		   (1- (line-end-position 0))
		   (point-min)))
	    (ppss (syntax-ppss)))
	;; Skip comments
	(when (nth 4 ppss)		 ; We are in a comment
	  (goto-char (nth 8 ppss))	 ; goto its beginning
	  (setq ppss (syntax-ppss)))	 ; update ppss after move
	(setq token
	      ;; Determine token and move before it
	      (cond
	       ;; Let syntactic parser handle parentheses (even inside
	       ;; strings - this ensures that parentheses are NOT
	       ;; indented inside strings according to meson
	       ;; indentation rules)
	       ((looking-back (rx (or (syntax open-parenthesis)
				      (syntax close-parenthesis)))
			      (1- (point)))
		"")
	       ;; Check for strings. Relying on syntactic parser allows us to
	       ;; find the beginning of multi-line strings efficiently.
	       ((nth 3 ppss)		; We're inside string or
		(let ((string-start (nth 8 ppss)))
		  (when (not (equal (point) string-start))
		    (goto-char string-start)
		    "string")))
	       ((equal (char-before) ?\') ; We're just after a string
		(let* ((ppss- (syntax-ppss (1- (point)))))
		  (goto-char (nth 8 ppss-))
		  "string"))
	       ;; Regexp-based matching
	       (t (let ((tok
			 ;; Determine token but do not move before it
			 (cond
			  ((looking-back meson-keywords-regexp (- (point) meson-keywords-max-length) t)
			   (match-string-no-properties 0))
			  ((looking-back meson-literate-tokens-regexp
					 (- (point) meson-literate-tokens-max-length) t)
			   (match-string-no-properties 0))
			  ((cl-some (lambda (spec) (when (looking-back (cdr spec) eopl t) (car spec)))
				    meson-token-spec)))))
		    (when tok
		      (goto-char (match-beginning 0)) ; Go before token now
		      (setq ppss (syntax-ppss))) ; update ppss
		    tok))))
	(when (or (equal token "ignore")
		  (equal token "eol_cont")
		  (and (equal token "eol")  ; Skip EOL when:
		       (or (> (nth 0 ppss) 0) ; - inside parentheses
			   (and (looking-back ; - after operator but not inside comments
				 meson-literate-tokens-regexp
				 (- (point) meson-literate-tokens-max-length))
				(not (nth 4 ppss)))
			   (meson--comment-bolp ppss)))) ;- at empty line
	  (setq token 'unknown))))
    token))

(defconst meson-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (codeblock (line)
		 (codeblock "eol" codeblock))
      (line (exp)
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
	   (exp "or" exp)
	   (exp "and" exp)
	   (exp "==" exp)
	   (exp "!=" exp)
	   (exp "<"  exp)
	   (exp "<=" exp)
	   (exp ">"  exp)
	   (exp ">=" exp)
	   (exp "+" exp)
	   (exp "-" exp)
	   (exp "*" exp)
	   (exp "/" exp)
	   (exp "%" exp)
;; 	   ("not" exp)
;; 	   ("-" exp)
;; 	   (exp "." methodcall)
;; 	   (exp "." exp)
;; 	   (exp "(" args ")")
;; 	   (exp "(" args ")" indexcall)
;; 	   ("[" array "]")
;; 	   ("true")
;; 	   ("false")
	   )
;;       (args (exp)

;; 	    (id ":" exp))
;;       (array (array "," array))
;;       (methodcall (exp "(" args ")" )
;; 		  ;; (exp "(" args ")" "." methodcall)
;; 		  )
      ;;      (indexcall ( "[" exp "]"))
      )
    `((assoc "eol" "elif")) ; FIXME: Solving eol/elif conflict this
			    ; way may cause problems in indetation.
			    ; Revisit this if it is the case.
    `((assoc "eol")
      (assoc ",")
      (assoc ":")
      (assoc "+=" "=")
      (assoc "or")
      (assoc "and")
      (assoc "==" "!=" "<" "<=" ">" ">=")
      (assoc "+" "-")
      (assoc "*" "/" "%")
      (assoc ".")
      )
    )))

(defgroup meson nil
  "Meson build system mode for Emacs."
  :group 'tools
  :prefix "meson-")

(defcustom meson-indent-basic 2
  "Indentation offset for meson.build files."
  :type 'integer)

(defun meson-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) meson-indent-basic)
    (`(:elem . args) (- (save-excursion (beginning-of-line-text) (point)) (point)))
    (`(,_ . ",") (smie-rule-separator kind))
    (`(,(or :before :after) . "eol") (if (smie-rule-parent-p "if" "foreach" "elif" "else")
					 (smie-rule-parent meson-indent-basic)
				       (save-excursion
					 (smie-indent-forward-token)
					 (smie-backward-sexp 'halfsexp)
					 (cons 'column (current-column)))))
    (`(:list-intro . ,(or "eol" ":" "")) t) ; "" is actually "[" because that's what lexer returns
    (`(:after . ":") meson-indent-basic)
    (`(:after . ,(or "=" "+=")) meson-indent-basic)
    (`(:before . "[") (if (smie-rule-hanging-p) (smie-rule-parent)))
    (`(:before . "(") (if (smie-rule-hanging-p)
			  (save-excursion
			    (smie-backward-sexp 'halfsexp) ; goto parent
			    (beginning-of-line-text)
			    (cons 'column (current-column)))))
    (`(:after . ,(or "[" "(")) meson-indent-basic)
    (`(:before . "elif") (smie-rule-parent))
    (_ nil)))

;;; Mode definition

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
  (add-hook 'completion-at-point-functions
            #'meson-completion-at-point-function nil t)
  (smie-setup meson-smie-grammar #'meson-smie-rules
	      :forward-token #'meson-smie-forward-token
	      :backward-token #'meson-smie-backward-token)
  )

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("/meson\\(\\.build\\|_options\\.txt\\)\\'" . meson-mode))
  (eval-after-load 'compile
    '(progn
       (add-to-list 'compilation-error-regexp-alist 'meson)
       (add-to-list 'compilation-error-regexp-alist-alist
		    '(meson "^Meson encountered an error in file \\(.*\\), line \\([0-9]+\\), column \\([0-9]+\\):" 1 2 3)))))

(provide 'meson-mode)
;;; meson-mode.el ends here

;;(progn (mapatoms (lambda (x) (when (string-prefix-p "meson" (symbol-name x)) (makunbound x)))) (eval-buffer))
