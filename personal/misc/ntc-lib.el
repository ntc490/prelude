;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author          : Nathan Crapo
;; Created On      : Fri Jun  3 14:24:59 2005
;; 
;; Miscellaneous library functions written for my own use.  The "ntc"
;; in each function name is my initials.  :)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun ntc-lib-terminate-directory-name (directory-name)
  "Make sure the directory name passed in ends in a slash so that more path information can be appended"
  ;; Check if the last character is 
  (if (string-match "/$" directory-name)
      directory-name
    (setq directory-name (concat directory-name "/"))
    directory-name))




(defun ntc-lib-load-machdep-file (machdep-file)
  "Load a machine dependent lisp file if it can be found by searching the load path for the specified file.
Check both \".el\" and \".elc\" versions of the file if no extension is initially specified."
  (let (machdep-file-full-path
	(search-directory nil)
	(search-directory-list load-path)
	(file-is-found nil))

    ;; Iterate through the list of search directories
    (while (and (setq search-directory (car search-directory-list))
		(not file-is-found))
      (setq search-directory-list (cdr search-directory-list))

      ;; Find the lisp file if it exists.  Retrieve the full path to
      ;; an existing file
      (setq machdep-file-full-path (ntc-lib-find-lisp-file
				    search-directory
				    machdep-file))

      ;; If the file was found load it and stop looking
      (if machdep-file-full-path
	  (progn
	    (message "Loading machine dependent config file %s" machdep-file-full-path)
	    (load machdep-file-full-path)
	    (setq file-is-found t))))

    ;; Let the user know if the file could not be found
    (if (not file-is-found)
	(message "Could not find machdep file %s" machdep-file))))




(defun ntc-lib-file-has-lisp-extension-p (filename)
  "Test the filename to see if it ends in \".el\" or \".elc\""
  (if (or (string-match "\.el$" filename)
	  (string-match "\.elc$" filename))
      t
    nil))




(defun ntc-lib-find-lisp-file (directory filename)
  "Find a lisp file given a directory to look in and a filename.  Return the full path on success and nil upon failure."
  (let (file-with-directory)

      ;; Make sure the directory is terminated with *one* slash
      (setq directory (ntc-lib-terminate-directory-name directory))

      ;; Get the full path to the file
      (setq file-with-directory (concat directory filename))

      ;; Check if the resultant file looks like a lisp file
      (if (ntc-lib-lisp-file-exists-p file-with-directory)
	  file-with-directory
	nil)))





(defun ntc-lib-lisp-file-exists-p (filename)
  "Test to see if the specified file exists with either a \".el\" or \".elc\" extension"
  ;; If the file already ends in the right extension, merely check it
  (if (ntc-lib-file-has-lisp-extension-p filename)
      (if file-readable-p (filename)
	t
	nil)
    ;; Check for a file with both types of lisp extension
    (if (or (file-readable-p (concat filename ".elc"))
	    (file-readable-p (concat filename ".el")))
	t
      nil)))




(defun ntc-lib-running-on-windows-p ()
  "Determine if the machine we're running on is a Windows Machine or not."

  ;; It is important to note that running-on-windows is set to false
  ;; by default
  (let (str env (running-on-windows nil))
    ;; Load the env variable with this processes environment variables
    (setq env process-environment)

    ;; Iterate through the environment variables
    (while (setq str (car env))
      (setq env (cdr env))

      ;; If we can find a string matching "WINDOWS" then we are likely
      ;; running on a Windows OS.  Return true.
      (if (string-match "WINDOWS" str)
	  (progn
	    (setq env ())
	    (setq running-on-windows t))))
    ;; Return the result
    running-on-windows))

