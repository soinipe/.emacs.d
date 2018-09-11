;;
;; Some utilities for c-coding.
;;

(global-set-key "\C-ct" 'insert-my-signature)

(defun my-c-defs()
  (local-set-key "\C-cc" 'my-c-file)
  (local-set-key "\C-ch" 'my-h-file)
  (local-set-key "\C-cf" 'my-c-function)
  (local-set-key "\C-ck" 'my-c-function-kr)
  (if (>= (string-to-int emacs-version) 19)
      (remove-hook 'c-mode-hook 'my-c-defs)
    (if (boundp 'old-c-mode-hook)
        (progn
          (setq c-mode-hook old-c-mode-hook)
          (run-hooks 'c-mode-hook)))))

(if (>= (string-to-int emacs-version) 19)
    (add-hook 'c-mode-hook 'my-c-defs)
  (if (boundp 'c-mode-hook)
      (setq old-c-mode-hook c-mode-hook))
  (setq c-mode-hook 'my-c-defs))

;; By default the header contents aren't asked
(defvar my-read-description nil "Header descriptions asked interactively")
;; (setq my-read-description t) ;; Copy this line to ~/.emacs if
                                 ;; interactive headers wanted

;; By default no file owner structures inserted to header files
(defvar my-insert-file-owner nil "Flag for inserting file owner structures")
;; (setq my-insert-file-owner t) ;; Copy this line to ~/.emacs if
                                  ;; file owner structures wanted

(defun my-read-description-str (str def-input-line def-no-interact)
  "Read description from minibuffer"
  (interactive)
  (if my-read-description 
      (read-from-minibuffer str def-input-line) 
    def-no-interact))

(defun my-indent-for-comment (n)
  "Indent comment to column specified"
  (interactive)
  (let ((tmp comment-column))
    (setq comment-column n)
    (indent-for-comment)
    (setq comment-column tmp)))

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

(defun my-buffer-file-name ()
  "Name of file visited in current buffer, or empty string if no file."
  (if (buffer-file-name)
      buffer-file-name
    ""))

(defun my-year-string ()
  "Creates year string"
  (concat (format "%04d"      ;; Year
           (string-to-int (substring (current-time-string) 20 24)))))

(defun my-time-string ()
  "Creates time string in custom format"
  (concat (format "%04d"      ;; Year
           (string-to-int (substring (current-time-string) 20 24)))
          (format ".%02d."    ;; Month
           (month-name-to-int (substring (current-time-string) 4 7)))
          (format "%02d"    ;; Day
           (string-to-int (substring (current-time-string) 8 10)))))

(defun my-date-time-string ()
  "Creates date and time string in custom format"
  (concat (format "%04d"     ;; Year
           (string-to-int (substring (current-time-string) 20 24)))
          (format ".%02d."   ;; Month
           (month-name-to-int (substring (current-time-string) 4 7)))
          (format "%02d "    ;; Day
           (string-to-int (substring (current-time-string) 8 10)))
          (substring (current-time-string) 11 19)))

(defun my-date-string ()
  "Creates date string in custom format"
  (concat (format "%04d"     ;; Year
           (string-to-int (substring (current-time-string) 20 24)))
          (format ".%02d."   ;; Month
           (month-name-to-int (substring (current-time-string) 4 7)))
          (format "%02d"     ;; Day
           (string-to-int (substring (current-time-string) 8 10)))))

(defun my-string-justify-left-and-cut (str width)
  "Justifies string to left and truncates to width specified."
  (substring (concat str (make-string width ? )) 0 width))

(defun my-getenv-default (env-variable default-value)
  "Reads value of the einvironment variable, returns default if not found."
  (let ((env-value (getenv env-variable)))
    (if (eq env-value nil) default-value env-value)))

(defun my-string-char-replace (str ch1 ch2)
  "Replaces in str all occurences of ch1 to ch2"
  (let ((i 0))
    (while (< i (length str))
      (if (char-equal (aref str i) ch1) (aset str i ch2))
      (setq i (1+ i)))
    str))

(defun my-user-full-name ()
  "Try parse some username if user-full-name not defined"
  (interactive)
  (if (> (length user-full-name) 0)
      user-full-name 
    user-login-name))

(defun my-signature()
  "Signature"
  (interactive)
  (concat (my-date-string) " / " (my-user-full-name)))

(defun insert-my-signature()
  "Inserts signature"
  (interactive)
  (insert (my-signature)))

(defun my-h-from-c-file-name (c-file-name)
  "Makes .h file from .c filename"
  (if (> (length c-file-name) 2)
      (concat (substring c-file-name 0 (- (length c-file-name) 2)) ".h") 
    "????????.h"))

(defun my-file-owner-c (h-file-name)
  "Returns include file owner string for c-file"
  (interactive)
  (if my-insert-file-owner
      (format "%s\n%s\"%s\"             //\n%s\n"
              "#define INCLUDE_FILE_OWNER"
              "#include "
              h-file-name
              "#undef INCLUDE_FILE_OWNER")
    ""))

(defun my-file-owner-h ()
  "Returns include file owner string for h-file"
  (interactive)
  (if my-insert-file-owner
      (format "%s\n%s\n%s\n%s\n%s\n"
              "#ifdef INCLUDE_FILE_OWNER"
              "#define EXTERN /**/"
              "#else"
              "#define EXTERN extern"
              "#endif")
    ""))

(defun my-file-owner-h-2 ()
  "Returns include file owner string for h-file"
  (interactive)
  (if my-insert-file-owner
      (format "\n%s"
              "#undef EXTERN")
    ""))


(defvar my-c-file-standard
"//------------------------------------------------------------------------------
//  Copyright (C) %s
//
//    Project:   %s
//    $Author$
//  $Workfile$
//  $Revision$
//   $Modtime$
//     Status:   
//------------------------------------------------------------------------------
//Description:   %s
//------------------------------------------------------------------------------
//Version history:
//
//VERSION    :   1.0   %s
//REASON     :   
//REFERENCE  :   
//DESCRIPTION:   First version in operation.
//
//------------------------------------------------------------------------------

// -------------- IMPORT -------------------------------------------------------
%s

//----------- LOCAL TYPES ------------------------------------------------------

//----------- LOCAL CONSTANTS --------------------------------------------------

//----------- LOCAL PROTOTYPES -------------------------------------------------

//----------- LOCAL CLASSES ----------------------------------------------------

//----------- LOCAL FUNCTIONS --------------------------------------------------

//----------- EXPORTED CLASSES -------------------------------------------------

//----------- EXPORTED FUNCTIONS -----------------------------------------------



// End of file
"
"Standard template for c-file.")

(defvar my-h-file-standard
"//------------------------------------------------------------------------------
//  Copyright (C) %s
//
//    Project:   %s
//    $Author$
//  $Workfile$
//  $Revision$
//   $Modtime$
//     Status:   
//------------------------------------------------------------------------------
#if !defined(INCLUDED_%s)
#define INCLUDED_%s

#ifdef __cplusplus
extern \"C\" {
#endif

#ifdef INCLUDE_FILE_OWNER
#define EXTERN /**/
#else
#define EXTERN extern
#endif

//----------- IMPORT -----------------------------------------------------------

//----------- FORWARD DECLARATIONS ---------------------------------------------

//----------- EXPORTED TYPES ---------------------------------------------------

//----------- EXPORTED CONSTANTS -----------------------------------------------

//----------- EXPORTED CLASSES -------------------------------------------------

//----------- EXPORTED FUNCTIONS -----------------------------------------------

    
//----------- STANDARD TRAILER -------------------------------------------------
#undef EXTERN

#ifdef __cplusplus
}
#endif

#endif // include guard INCLUDED_%s

// End of file
"
"Standard for h-file.")


(defvar my-c-function-standard
"
//------------------------------------------------------------------------------
// Date/Author    : %s
// Date/Modifier  : 
// Description    : %s
//
" 
"Standard for function. Parameters in the format string for 'format'
  #1 author and date
  #2 description")

(defvar my-c-function-kr-standard
"
/**********************************************************************
 * Date/Author    : %s
 * Date/Modifier  : 
 * Description    : %s
 */
int emCreateBox (len, id)  /* O: status code or 0 on error */
int len;       /* I: the length of mailbox in bytes */
int id;        /* I: the mailbox identifier         */
" 
"Standard for function. Parameters in the format string for 'format'
  #1 author and date
  #2 description")

(defun my-c-file ()
  "Inserts standard C file header."
  (interactive)
  (let ((h-file-name
         (my-h-from-c-file-name 
          (file-name-nondirectory (my-buffer-file-name)))))

    (insert (format my-c-file-standard
                    (my-year-string)
                    my-project-name
                    (my-read-description-str "Description : "
                                              ""
                                              "")
                    (my-signature)
                    (my-file-owner-c h-file-name)))

    (beginning-of-buffer)))

(defun my-h-file ()
  "Inserts standard empty include file header."
  (interactive)
  (let ((h-file-variable 
         (my-string-char-replace 
          (file-name-nondirectory 
           (concat (my-buffer-file-name))) ?. ?_)))

    (insert (format my-h-file-standard
                    (my-year-string)
                    my-project-name
                    h-file-variable
                    h-file-variable
                    h-file-variable))
    (beginning-of-buffer)))

(defun my-c-function ()
  "Inserts C function header."
  (interactive)
  (let ((function-description "")
        (function-name "")
        (function-comment "O: ")
        (count 0)
        (function-arg "")
        (function-arg-comment ""))

    (setq function-description (my-read-description-str "Description : "
                                                         ""
                                                         ""))

    (setq function-name 
          (my-read-description-str "Function, type and name : "
                                    ""
                                    "void myfunction"))

    (if (string-match "^void" function-name)
        (setq function-comment "O: None")
      (setq function-comment 
            (my-read-description-str "Comment for return value : "
                                      function-comment
                                      "O: ")))

    (insert (format my-c-function-standard
                    (my-signature)
                    function-description))

    (insert function-name)
    (insert "(")
    ;;(newline)
    ;;(my-indent-for-comment 32)
    ;;(insert function-comment)
    ;;(end-of-line)
    ;;(c-indent-command)
    
    (while count
      (setq function-arg 
            (my-read-description-str 
             (concat "Parameter #" (int-to-string 
                                    (+ count 1)) ", type and name : ")
             ""
             (if (= count 0) "void" "")))
      
      (if (> (length function-arg) 0) 
          (progn (setq count (+ count 1))
                 (if (> count 1) (progn (insert ",")
                                        (my-indent-for-comment 32)
                                        (insert function-arg-comment)
                                        (end-of-line)
                                        (newline)))
                 (if (> count 1) (c-indent-command))
                 (insert function-arg)
                 (setq function-arg-comment "")
                 (setq function-arg-comment
                       (my-read-description-str 
                        (concat "Comment for '" function-arg "' : ")
                        function-arg-comment
                        "")))
        (progn 
          (progn (insert ")")
                 (if (> count 0) 
                     (progn (my-indent-for-comment 32)
                            (insert function-arg-comment)))
                 (end-of-line)
                 (newline)
                 (setq count nil)))))
                 
    (insert "{")
    (set-mark-command nil)
    (newline)
    (insert "}")
    (newline)
    (newline)
    (exchange-point-and-mark)))

(defun my-c-function-kr ()
  "Inserts C function header, K&R-style."
  (interactive)
  (let ((function-description ""))

    (setq function-description (my-read-description-str "Description : "
                                                         ""
                                                         ""))

    (insert (format my-c-function-kr-standard
                    (my-signature)
                    function-description))))


(defun my-untabify-tab-width-4 ()
  "Untabify buffer which have tab size of 4"
  (interactive)
  (let ((current-tab-width tab-width))
    (set-variable 'tab-width 4)
    (untabify 0 (buffer-size))
    (set-variable 'tab-width current-tab-width)))


;; End of file
