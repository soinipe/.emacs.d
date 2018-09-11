(set-variable 'compile-command "make ")
(global-set-key "\^Xc" 'compile)
(global-set-key "\e "   'set-mark-command)
(global-set-key "\^X\^b" 'buffer-menu)
(global-set-key (quote [f4]) 'next-error)
(global-set-key "\e:"   'just-one-space)
(global-set-key "\C-u" 'undo)
(global-set-key "\C-t" 'dabbrev-expand)
(global-set-key "\eg" 'goto-line)

;; enable font lock by default for modes which support it
(global-font-lock-mode t)

(defvar grep-default-files "*.cpp *.h" 
  "Default file names for grep.")

;;(defvar grep-default-files "../src/*.c ../src/*.cpp ../inc/*.h  ../Data/*.rss ../Inc/*.hrh" 
;;  "Default file names for grep.")

;; Sample project
;;(defvar grep-default-files "*.[cshg] Inc/*.h Src/*.c" "Default file names for grep.")

;;(set-variable 'grep-default-files "../src/*.c ../src/*.cpp ../inc/*.h  ../Data/*.rss ../Inc/*.hrh")

;;(load "cutil")
;;(load "myc")

(global-set-key "\^XG"  'grep-with-defaults-19)
(set-variable 'grep-command "grep -n ")
(setq my-insert-file-owner t)

;; All buffers with case-sensitive search
(setq-default case-fold-search nil)

(set-frame-size (selected-frame) 81 45)

(set-variable 'my-project-name "My Project")

(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(find-dired-find-program "unixfind")
 '(truncate-lines t)
 '(user-full-name "Petri Soininen"))

;;			 (arglist-intro . c-lineup-arglist-intro-after-paren)
;;			 (arglist-close . c-lineup-arglist)

; Set the mode to use for various extensions
(defalias 'perl-mode 'cperl-mode)

(defconst legacy-c-style
  '(
     (c-basic-offset . 4)
     (c-comment-only-line-offset . (0 . 0))
     (c-offsets-alist . ((statement-block-intro . 0)
			 (knr-argdecl-intro . 5)
			 (substatement-open . +)
			 (label . 0)
			 (statement-case-open . +)
			 (statement-cont . +)
			 (inline-open . 0)
			 (brace-list-open . +)
			 ))
     (c-special-indent-hook . c-gnu-impose-minimum)
     (c-block-comment-prefix . "")
    )
  "Legacy C/C++ Programming Style")

(defconst symbian-cpp-style
  '(
     (c-basic-offset . 4)
     (c-comment-only-line-offset . (0 . 0))
     (c-offsets-alist . ((defun-open            . +)
			 (defun-block-intro     . 0)
			 (statement-block-intro . 0)
			 (knr-argdecl-intro . 5)
			 (substatement-open . +)
			 (label . 0)
			 (statement-case-open . +)
			 (statement-cont . +)
			 (inline-open . 0)
			 (brace-list-open . +)
			 ))
     (c-special-indent-hook . c-gnu-impose-minimum)
     (c-block-comment-prefix . "")
    )
  "Symbian C++ Programming Style")

;; offset customizations not in my-c-style
(setq c-offsets-alist '((member-init-intro . ++)))

;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  ;; add my personal style and set it for the current buffer
  (c-add-style "PERSONAL" symbian-cpp-style t)
  ;; other customizations
  (setq tab-width 3
	comment-start "// "
	comment-end   ""
        ;; this will make sure spaces are used instead of tabs
        indent-tabs-mode nil)

  ;; auto-newline and hungry-delete
  ;;NO! (c-toggle-auto-hungry-state 1)
  ;; hungry delete only
  ;;NO! (c-toggle-hungry-state 1)

  ;; font lock
  (turn-on-font-lock)

  ;; key bindings for all supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map, objc-mode-map,
  ;; java-mode-map, idl-mode-map, and pike-mode-map inherit from it.
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Customizations for perl mode
(defun my-cperl-mode-hook ()
  ;; variables
  (setq cperl-indent-level 4)
  ;; font lock
  (turn-on-font-lock)
  (setq compile-command (format "perl -c %s" (buffer-file-name))))

(add-hook 'cperl-mode-hook 'my-cperl-mode-hook)

(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )

;; End of file
