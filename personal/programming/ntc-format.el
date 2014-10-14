;; Functions for re-formatting code into my personal preference.  I'm much
;; more facile with other styles now, so these aren't used as much.  Some
;; useful stuff I may rip out eventually.
;; 
;; Author:       Nathan Crapo
;; Date:         Sometime back in 2000
;; Version:      0.1

(defvar ntc-local-specifier "static")
(defvar ntc-macro-history nil)
(defvar ntc-macro-output-code-history nil)
(defvar ntc-function-history nil)
(defvar ntc-function-type-history nil)
(defvar ntc-function-args-history nil)

(defvar ntc-top-menu)
(define-key ntc-top-menu [sep-template]     '("---" . nil))
(define-key ntc-top-menu [printf-2-macro]   '(menu-item "printf => macro" ntc-dummy))
(define-key ntc-top-menu [create-dbg-macro] '(menu-item "Create debug macro" ntc-dummy))
(define-key ntc-top-menu [justify-block]    '(menu-item "Justify block" ntc-dummy))
(define-key ntc-top-menu [func-indent]      '(menu-item "Indent Function" ntc-dummy))
(define-key ntc-top-menu [func-rename-file] '(menu-item "Rename Function (file)" ntc-dummy))
(define-key ntc-top-menu [func-rename-proj] '(menu-item "Rename Function (proj)" ntc-rename-func
							 :enable t))
(define-key ntc-top-menu [func-api-add]     '(menu-item "Add API Function" ntc-add-function))
(define-key ntc-top-menu [func-local-add]   '(menu-item "Add Local Function" ntc-add-local-function))

(defun ntc-input-macro-params ()
  (list
   (read-from-minibuffer "Macro name: "  nil nil nil 'ntc-macro-history)
   (read-from-minibuffer "Output code: "  "printf x" nil nil 'ntc-macro-output-code-history)))

(defun ntc-add-debug-macro (name output)
  (interactive (ntc-input-macro-params))
  (insert (concat "#ifdef DEBUG_" name "\n"
		  "#define DBG_" name "(x) " output "\n"
		  "#else /* DEBUG_" name "*/\n"
		  "#define DBG_" name "(x) {}\n"
		  "#endif /* DEBUG_" name "*/\n")))

(defun ntc-input-function-params ()
  (list
   (read-from-minibuffer "Function name: "  nil nil nil '(ntc-function-history . 1))
   (read-from-minibuffer "Return type: "  nil nil nil '(ntc-function-type-history . 1))
   (read-from-minibuffer "Args: "  nil nil nil '(ntc-function-args-history . 1))))
  
(defun ntc-add-local-function (fname return-type args)
  (interactive (ntc-input-function-params))
  (let ((found nil))
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "/\*.*local \\(func\\|proto\\).*" nil t)
	(progn
	  (forward-line 2)
	  (insert (concat ntc-local-specifier " " return-type " " fname "(" args ");\n"))
	  (setq found t))))
  (if (not found)
      (insert (concat ntc-local-specifier " " return-type " " fname "(" args ");\n")))
  (insert (concat ntc-local-specifier " " return-type "\n" fname "(" args ")\n{\n}\n\n"))))

(defun ntc-add-function (fname return-type args)
  (interactive (ntc-input-function-params))
  (if (save-excursion
	(goto-char (point-min))
	(if (re-search-forward "/\*.*local \\(func\\|proto\\).*" nil t)
	    (progn
	      (forward-line 2)
	      (insert (concat return-type " " fname "(" args ");\n"))
	      nil)
	  t))
      (insert (concat return-type " " fname "(" args ");\n")))
  (insert (concat return-type "\n" fname "(" args ")\n{\n}\n\n")))

(defun figure-max (min max)
  (interactive "r")
  (save-excursion
    (let (num-lines (max-prefix-length 0) new-string padding)
      (goto-char min)
      (setq num-lines (count-lines min max))
      (while (not (equal 0 num-lines))
	(setq num-lines (- num-lines 1))
	(if (re-search-forward "^\\(\\(\\s-*[A-Za-z0-9_]+\\)\\(\\s-+[A-Za-z0-9_]+\\)*\\)\\s-\\([A-Za-z0-9_]+\\)")
	    (progn
	      (message "match is %s" (match-string 1))
	      (sleep-for 2)
	      (message "length is %d" (length (match-string 1)))
	      (sleep-for 2)
	      (if (> (length (match-string 1)) max-prefix-length)
		(progn 
		  (setq max-prefix-length (length (match-string 1)))
;		  (message "New length is %d" max-prefix-length)
;		  (sleep-for 2)
		  )))))

      (message "max-prefix-length is %d" max-prefix-length)
      (sit-for 1))))

(defun ntc-justify-block (min max)
  (interactive "r")
  (save-excursion
    (let (num-lines (max-prefix-length 0) new-string padding end-mark)
      (goto-char min)
      (setq num-lines (count-lines min max))
      (setq end-mark (make-marker))
      (set-marker end-mark max)
      (while (not (equal 0 num-lines))
	(setq num-lines (- num-lines 1))
	;; (re-search-forward "^\\(\\s-*[A-Za-z0-9_]+\\)\\(\\s-+[A-Za-z0-9_]+\\)*") - missing end guy
	;; (progn (re-search-forward "^\\(\\s-*[A-Za-z0-9_]+\\)\\(\\(\\s-+[A-Za-z0-9_]+\\)*\\)\\s-+[A-Za-z0-9_]+") (concat (match-string 1) (match-string 2)))
	;;(re-search-forward "^\\(\\(\\s-*[A-Za-z0-9_]+\\)\\(\\s-+[A-Za-z0-9_]+\\)*\\)\\s-+\\([A-Za-z0-9_]+\\)" end-mark t)
	;; (re-search-forward "^\\(\\(\\s-*[A-Za-z0-9_]+\\)\\(\\s-+[A-Za-z0-9_]+\\)*\\(\\s-*\\*\\s-*\\)*\\)\\s-+\\(\\(\\s-*\\*\\s-*\\)*[A-Za-z0-9_]+\\)" end-mark t)
	(if (re-search-forward "^\\(\\s-*\\)\\(\\(\\([A-Za-z0-9_]+\\)\\(\\s-+[A-Za-z0-9_]+\\)*\\)\\(\\s-*\\*\\)*\\)\\s-+\\(\\(\\s-*\\*\\s-*\\)*[A-Za-z0-9_]+\\)" end-mark t)
	    (progn
	      (if (> (length (match-string 2)) max-prefix-length)
		(progn 
		  (setq max-prefix-length (length (match-string 2)))
;		  (message "New length is %d" max-prefix-length)
;		  (sleep-for 2)
		  )))))
      ;; Adjust the spacing so it has a few extra whitespace characters
      (setq max-prefix-length (+ 4 max-prefix-length))

;      (message "max-prefix-length is %d" max-prefix-length)
;      (sit-for 1)

      ;; Modify the spacing
      (setq num-lines (count-lines min max))
      (goto-char min)
      (while (not (equal 0 num-lines))
	(setq num-lines (- num-lines 1))
	(if (re-search-forward "^\\(\\s-*\\)\\(\\(\\([A-Za-z0-9_]+\\)\\(\\s-+[A-Za-z0-9_]+\\)*\\)\\(\\s-*\\*\\)*\\)\\s-+\\(\\(\\s-*\\*\\s-*\\)*[A-Za-z0-9_]+\\)" end-mark t)
	    (let (pad-length)
	      (setq pad-length (- max-prefix-length (length (match-string 2))))
;	      (message "padding length is %d" pad-length)
;	      (sleep-for 2)
	      (setq padding (make-string pad-length ? ))
	      (setq new-string (concat
				(match-string 1)
				(match-string 2)
				padding
				(match-string 7)))
	      (replace-match new-string))))
      (set-marker end-mark nil))))

(defun ntc-indent-function ()
  (interactive)
  (let (start end)
    (save-excursion
      (beginning-of-defun)
      (setq start (point))
      (end-of-defun)
      (setq end (point))
      (indent-region start end nil))))

(defun ntc-input-func-names ()
  (let (old-func)
    (save-excursion
      ;; Check if we're on a space move forward until we see something else
      (if (equal (following-char) ? )
	  (while (and (equal (following-char) ? )
		      (< (point) (point-max)))
	    (forward-char))
	(while (and (not (equal (preceding-char) ? ))
		    (> (point) (point-min)))
	  (backward-char)))
      (setq old-func (if (re-search-forward "\\([a-zA-Z0-9_-]*\\)" nil t)
			 (read-from-minibuffer "Old Function: "
					       (match-string 1)
					       nil
					       nil
					       'ntc-input-func-history)
		       (read-from-minibuffer "Old Function: "
					     nil
					     nil
					     nil
					     'ntc-input-func-history)))
      (list old-func
	    (read-from-minibuffer "New Function: "
				  old-func
				  nil
				  nil
				  'ntc-input-func-history)))))

(defun ntc-rename-func (function new-name)
  (interactive (ntc-input-func-names))
  (message "Renaming %s to %s..." function new-name)
  (save-excursion
    (goto-char (point-min))
    (setq function (concat "\\<" function "\\>"))
    (setq new-name new-name)
    (query-replace-regexp function new-name)))

(defun ntc-rename-tag (function new-name)
  (interactive "sFunction name: \nsNew name: ")
  (message "Renaming %s to %s..." function new-name)
  (save-excursion
    (tags-query-replace function new-name)))

(defun ntc-find-func-default ()
  (save-excursion
    (while (looking-at "\\sw\\|\\s_")
      (forward-char 1))
    (if (or (re-search-backward "\\sw\\|\\s_"
				(save-excursion (beginning-of-line) (point))
				t)
	    (re-search-forward "\\(\\sw\\|\\s_\\)+"
			       (save-excursion (end-of-line) (point))
			       t))
	(progn (goto-char (match-end 0))
	       (buffer-substring (point)
				 (progn (forward-sexp -1)
					(while (looking-at "\\s'")
					  (forward-char 1))
					(point))))
      nil)))


(defun kill-from-line-start ()
  "Kill a line from the beginning.  The same as using C-a C-k."
  (interactive)
  (forward-line 0)
  (kill-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                  ;;
;;              Miscellaneous functions                             ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dbg-expand()
  "Change printf functions to a macro."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (re-search-forward "printf[ \t]*\(" nil)
      (replace-match "DPRINTF\(\(")
      (re-search-forward "\);")
      (replace-match "\)\);"))))

(defun dbg-collapse()
  "Change debug print macros to printf."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (re-search-forward "NPRINTF[ \t]*\(")
      (replace-match "printf")
      (re-search-forward "\)\);")
      (replace-match "\);"))))

;; Take a macro name at the first of the line and turn it in to a printf
(fset 'dump-macro
   [?\C-  ?\C-e ?\C-w tab ?p ?r ?i ?n ?t ?f ?( ?" ?\C-y ?: ?  ?% ?x ?\\ ?n ?" ?, ?  ?\C-y ?) ?\;])

(defun eol-to-br ()
  (interactive)
  (while (re-search-forward "\012" nil t)
    (replace-match "<br>\012" nil nil))
)

(defun dos-to-linux ()
  (interactive)
  (goto-char (point-min)) 
  (while (re-search-forward "\015" nil t)
    (replace-match "" nil nil))
 )

(defun linux-to-dos ()
  (interactive)
  (goto-char (point-min)) 
  (while (re-search-forward "\\([^\015]\\)\012" nil t)
    (replace-match "\\1\015\012" nil nil))
  (goto-char (point-min)) 
  (while (re-search-forward "^\012" nil t)
    (replace-match "\015\012" nil nil))
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                  ;;
;;              C / C++ Formating functions                         ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun full-reformat ()
  "Full C/C++ reformat"
  (interactive)
  (untabify (point-min) (point-max))
;;  (tabs-to-spaces)
  (space-operators)
  (space-commas)
  (space-parens)
  (space-conditionals)
  (format-pointers)
  
  ;; these functions tend to mess up MS C++
  ;; and aren't always needed
  (format-curly-braces)
  (format-else-statements)
  (indent-region (point-min) (point-max) nil)
 )

(defun ms-reformat ()
  (interactive)
  "Format MS C and C++"
  (untabify (point-min) (point-max))
;;  (tabs-to-spaces)
  (space-operators)
  (space-commas)
  (space-parens)
  (space-conditionals)
  (format-pointers)
  (indent-region (point-min) (point-max) nil)
 )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              C / C++ Formating sub-functions                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-else-statements ()
  ;; make sure there is a newline between end curly braces and the else
  ;; statement
  (goto-char (point-min)) 
  (while (re-search-forward "}[ \t]*else" nil t)
    (replace-match "}\nelse" nil nil))
 )

(defun format-curly-braces ()
  ;; don't allow ending and starting curly braces on the same line
  ;; in that order.  e.g.  } {
  ;; put a line feed in beetween them
  (goto-char (point-min)) 
  (while (re-search-forward "\\([^ \t\n]+\\)[ \t]*{" nil t)
    (replace-match "\\1\n{" nil nil))

  ;; if there is anything following a beginning curly brace, place
  ;; a newline between them
  (goto-char (point-min)) 
  (while (re-search-forward "{\\(.+\\)" nil t)
    (replace-match "{\n\\1" nil nil))
 
  ;; make sure an ending curly brace ends on it's own line
  (goto-char (point-min)) 
  (while (re-search-forward "\\([^ \t\n}]+\\)[ \t]*}" nil t)
    (replace-match "\\1\n}" nil nil))
 )

(defun tabs-to-spaces ()
  "tabs to spaces"
  (goto-char (point-min))
  (while (re-search-forward "\t" nil t)
    (replace-match "    " nil nil))
  )

(defun format-pointers ()
  "formatting pointers"
  ;; remove space between a variable and the pointer symbol
  ;; allow any number of stars in succession
  ;; e.g.  char *  ==>  char*
  (goto-char (point-min))
  (while (re-search-forward "\\([A-Za-z0-9]\\)[ \t]+\\([\\*]+\\)\\([^\\/]\\)" nil t)
    (replace-match "\\1\\2\\3" nil nil))

  ;; add space between a pointer symbol and a variable
  ;; allow any number of stars in succession
  ;; e.g. char*var2  ==>  char* var2
  (goto-char (point-min))
  (while (re-search-forward "\\([A-Za-z0-9]\\)\\([\\*]+\\)\\([a-zA-Z]\\)" nil t)
    (replace-match "\\1\\2 \\3" nil nil))
 )

(defun space-operators ()
  "space operators"
  ;; put space around the addition operator
  (goto-char (point-min))
  (while (re-search-forward "\\([a-zA-Z0-9]\\)\\+\\([a-zA-Z0-9]\\)" nil t)
    (replace-match "\\1 \\+ \\2" nil nil))

  ;; put spaces around the subtraction operator
  (goto-char (point-min))
  (while (re-search-forward "\\([a-zA-Z0-9]\\)\\-\\([a-zA-Z0-9]\\)" nil t)
    (replace-match "\\1 \\- \\2" nil nil))

  ;; put spaces around the multiplication operator
  (goto-char (point-min))
  (while (re-search-forward "\\([a-zA-Z0-9]\\)\\*\\([a-zA-Z0-9]\\)" nil t)
    (replace-match "\\1 \\* \\2" nil nil))

  ;; put spaces around the divide operator
  (goto-char (point-min))
  (while (re-search-forward "\\([a-zA-Z0-9]\\)\\/\\([a-zA-Z0-9]\\)" nil t)
    (replace-match "\\1 \\/ \\2" nil nil))

  ;; remove spaces around forward slashes in include paths
  (goto-char (point-min))
  (while (re-search-forward "\\(^#include.*?\\)[ \t]+\\/[ \t]+\\(.*?$\\)" nil t)
    (replace-match "\\1\\/\\2" nil nil))

  ;; put spaces around the bitwise and operator
  (goto-char (point-min))
  (while (re-search-forward "\\([a-zA-Z0-9]\\)&\\([a-zA-Z0-9]\\)" nil t)
    (replace-match "\\1 & \\2" nil nil))

  ;; put spaces around the bitwise or operator
  (goto-char (point-min))
  (while (re-search-forward "\\([a-zA-Z0-9]\\)|\\([a-zA-Z0-9]\\)" nil t)
    (replace-match "\\1 | \\2" nil nil))

  ;; put spaces around the logical and operator
  (goto-char (point-min))
  (while (re-search-forward "\\([a-zA-Z0-9]\\)&&\\([a-zA-Z0-9]\\)" nil t)
    (replace-match "\\1 && \\2" nil nil))

  ;; put spaces around the logical or operator
  (goto-char (point-min))
  (while (re-search-forward "\\([a-zA-Z0-9]\\)||\\([a-zA-Z0-9]\\)" nil t)
    (replace-match "\\1 || \\2" nil nil))

  ;; put spaces around the exclusive or operator
  (goto-char (point-min))
  (while (re-search-forward "\\([a-zA-Z0-9]\\)^\\([a-zA-Z0-9]\\)" nil t)
    (replace-match "\\1 ^ \\2" nil nil))
)  

(defun format-spacing ()
  (interactive)
  (space-parens)
  (space-conditionals))

(defun space-parens ()
  (interactive)
  "space parenthises"
  ;; remove spaces between a function name and an opening paren
  (goto-char (point-min)) 
  (while (re-search-forward "\\([A-Za-z_][A-Za-z_0-9]*\\)[ \t]+(" nil t)
    (replace-match "\\1(" nil nil)))

(defun space-conditionals ()
  "space conditionals"
  ;; put a space between "if" and the opening paren
  (goto-char (point-min)) 
  (while (search-forward "if(" nil t)
    (replace-match "if (" nil t))

  ;; put a space between "while" and the opening paren
  (goto-char (point-min)) 
  (while (search-forward "while(" nil t)
    (replace-match "while (" nil t))

  ;; put a space between "for" and the opening paren
  (goto-char (point-min)) 
  (while (search-forward "for(" nil t)
    (replace-match "for (" nil t))

  ;; put a space between "switch" and the opening paren
  (goto-char (point-min)) 
  (while (search-forward "switch(" nil t)
    (replace-match "switch (" nil t))

  ;; space return statements
  (goto-char (point-min)) 
  (while (search-forward "return(" nil t)
    (replace-match "return (" nil t))
 )

(defun space-commas ()
  "space commas"
  ;; remove space before commas
  (goto-char (point-min)) 
  (while (re-search-forward "[ \t]+," nil t)
    (replace-match "," nil nil))
 
  ;; make sure there is a single space after a comma
  (goto-char (point-min)) 
  (while (re-search-forward ",[ \t]*" nil t)
    (replace-match ", " nil nil))
 )
