;;
;; Some utilities for c-coding.
;;
;; OPS/2003-06-09 First version.
;;


(defun indent-for-comment-n (n)
  "Indent comment to column specified"
  (interactive)
  (setq tmp comment-column)
  (setq comment-column n)
  (indent-for-comment)
  (setq comment-column tmp))

(defun indent-for-comment-32 ()
  "Indent comment to column 32"
  (interactive)
  (indent-for-comment-n 32))

(defun month-name-to-int (month)
  "Converts three letter english month name to value 1 to 12."
  (cond
    ((equal month "Jan") 1)
    ((equal month "Feb") 2)
    ((equal month "Mar") 3)
    ((equal month "Apr") 4) 
    ((equal month "May") 5)
    ((equal month "Jun") 6)
    ((equal month "Jul") 7)
    ((equal month "Aug") 8)
    ((equal month "Sep") 9)
    ((equal month "Oct") 10)
    ((equal month "Nov") 11)
    ((equal month "Dec") 12)
    (t 'nil)))

(defun my-time-string ()
  "Creates time string in company format"
  (concat (format "%04d"      ;; Year
	   (string-to-int (substring (current-time-string) 20 24)))
	  (format ".%02d."    ;; Month
	   (month-name-to-int (substring (current-time-string) 4 7)))
	  (format "%02d"    ;; Day
	   (string-to-int (substring (current-time-string) 8 10)))))

(defun my-date-time-string ()
  "Creates date and time string in company format"
  (concat (format "%04d"     ;; Year
	   (string-to-int (substring (current-time-string) 20 24)))
	  (format ".%02d."   ;; Month
	   (month-name-to-int (substring (current-time-string) 4 7)))
	  (format "%02d "    ;; Day
	   (string-to-int (substring (current-time-string) 8 10)))
	  (substring (current-time-string) 11 19)))

(defun insert-my-time-string ()
  "Inserts time string in my format"
  (interactive)
  (insert (my-time-string)))

(defun string-justify-left-and-cut (str width)
  "Justifies string to left and truncates to width specified."
  (substring (concat str (make-string width ? )) 0 width))

(defun getenv-default (env-variable default-value)
  "Reads value of the einvironment variable, returns default if not found."
  (let ((env-value (getenv env-variable)))
    (if (eq env-value nil) default-value env-value)))

(defun string-char-replace (str ch1 ch2)
  "Replaces in str all occurences of ch1 to ch2"
  (setq i 0)
  (while (< i (length str))
    (if (char-equal (aref str i) ch1) (aset str i ?_))
    (setq i (1+ i)))
  str)

(defun signature()
  "Inserts signature"
  (interactive)
  (insert (concat (getenv-default "USERINITIALS" "Ano Nyymi")
		  "/"
		  (my-time-string))))

(defun h-from-c-file-name (c-file-name)
  "Makes .h file from .c filename"
  (if (> (length c-file-name) 2)
      (concat (substring c-file-name 0 (- (length c-file-name) 2)) ".h") 
    "????????.h"))

(defun my-c-file-skeleton ()
  "Insert C file skeleton."
  (interactive)
(insert "/*")(newline)
(insert (string-justify-left-and-cut 
	 (file-name-nondirectory (concat (buffer-file-name))) 35))
(insert (string-justify-left-and-cut (user-full-name) 18))
(insert-my-time-string)
(insert "/")
(insert (concat (getenv-default "USERINITIALS" "Ano Nyymi")))
(newline)
(insert "First version.")(newline)
(newline)
(insert "*/")(newline)
(newline)
(insert "/*---------------------------------------------INCLUDE FILES----*/")(newline)
(insert "#define INCLUDE_FILE_OWNER")
(newline)
(insert "#include \"")
(insert (h-from-c-file-name (file-name-nondirectory (concat (buffer-file-name)))))
(insert "\"")
(indent-to-column 32)
(insert "/* */")
(newline)
(insert "#undef INCLUDE_FILE_OWNER")
(newline)
(newline)
(insert "/* End of file */")(newline)
(newline)
(exchange-point-and-mark))



(defun my-h-file-skeleton ()
  "Insert include file skeleton."
  (interactive)
(insert "/*")(newline)
(insert (string-justify-left-and-cut 
	 (file-name-nondirectory (concat (buffer-file-name))) 35))
(insert (string-justify-left-and-cut (user-full-name) 18))
(insert "DOCUMENT HISTORY")(newline)
(newline)
(insert-my-time-string)
(insert "/")
(insert (concat (getenv-default "USERINITIALS" "Ano Nyymi")))
(newline)
(insert "First version.")(newline)
(newline)
(insert "*/")(newline)
(newline)
(insert "/*---------------------------------------------STANDARD HEADER--*/")(newline)
(insert "#ifndef ")
(insert (string-char-replace
	 (file-name-nondirectory (concat (buffer-file-name))) ?. ?_))
(newline)
(insert "#define ")
(insert (string-char-replace
	 (file-name-nondirectory (concat (buffer-file-name))) ?. ?_))
(newline)
(newline)
(insert "#ifdef __cplusplus") (newline)
(insert "extern \"C\" {") (newline)
(insert "#endif") (newline)
(newline)
(insert "#ifdef INCLUDE_FILE_OWNER")(newline)
(insert "#define iqEXT /**/")(newline)
(insert "#else")(newline)
(insert "#define iqEXT extern")(newline)
(insert "#endif")(newline)
(newline)
(insert "/*---------------------------------------------STANDARD TRAILER-*/")(newline)
(insert "#undef iqEXT")(newline)
(newline)
(insert "#ifdef __cplusplus") (newline)
(insert "}") (newline)
(insert "#endif") (newline)
(newline)
(insert "#endif /* ")
(insert (string-char-replace
	 (file-name-nondirectory (concat (buffer-file-name))) ?. ?_))
(insert " */") (newline)
(newline)
(insert "/* End of file */")(newline)
(newline)
(exchange-point-and-mark))

(defun my-c-function ()
  "Insert C function skeleton."
  (interactive)
  (setq function-name (read-from-minibuffer "Function name : "))
  (setq function-type (read-from-minibuffer "Function type : "))
(insert "/*") (newline)
(insert "********FUNCTION DESCRIPTION***************************************") (newline)
(insert "*") (newline)
(insert "*  NAME    : ") 
(insert function-name)
(newline)
(insert "*")(newline)
(insert "*  FUNCTION: ") 
(set-mark-command nil)
(newline)
(insert "*******************************************************************") (newline)
(insert "*/") (newline)
(insert function-type)
(insert " ")
(insert function-name)
(insert "(void)")
(indent-to-column 48)
(insert "/* O None */")(newline)
(insert "{")(newline)
(insert "}")(newline)
(newline)
(exchange-point-and-mark))


;;
;; For automating the use of grep inside emacs.
;; 
;;
 
;; Moved to _emacs   
;;(defvar grep-default-files "*.[cshg] *.cpp"
;;  "Default file names for grep.")

;; Copied from find-tag-default
(defun grep-find-tag-default ()
  (save-excursion
    (while (looking-at "\\sw\\|\\s_")
      (forward-char 1))
    (if (re-search-backward "\\sw\\|\\s_" nil t)
	(progn (forward-char 1)
	       (buffer-substring (point)
				 (progn (forward-sexp -1)
					(while (looking-at "\\s'")
					  (forward-char 1))
					(point))))
      nil)))

(defun grep-with-defaults (command)
  "Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to. The expression in the buffer
around or before point is used as the search expression name and default
file wildcards as file name."
  (interactive (list 
		(read-string 
		 "Run grep (with args): " 
		 (concat (grep-find-tag-default) " " grep-default-files))))
  (compile1 (concat "grep -n " 
		    command
		    " NUL")
	    "No more grep hits" "grep"))

(autoload 'grep-with-defaults "compile"
  "\
Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to."
  t)

(defun grep-with-defaults-19 (command-args)
  "Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to.

This command uses a special history list for its arguments, so you can
easily repeat a grep command."
  (interactive
   (list (read-from-minibuffer "Run grep (like this): "
			       (concat grep-command 
				       (grep-find-tag-default) " " 
				       grep-default-files) nil nil 
				       'grep-history)))
  (let ((buf (compile-internal (concat command-args " NUL")
			       "No more grep hits" "grep"
			       ;; Give it a simpler regexp to match.
			       nil grep-regexp-alist)))
    (save-excursion
      (set-buffer buf)
      (set (make-local-variable 'compilation-exit-message-function)
	   (lambda (status code msg)
	     (if (eq status 'exit)
		 (cond ((zerop code)
			'("finished (matches found)\n" . "matched"))
		       ((= code 1)
			'("finished with no matches found\n" . "no match"))
		       (t
			(cons msg code)))
	       (cons msg code)))))))


;; End of file
