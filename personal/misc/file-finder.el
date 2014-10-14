(defun terminate-path-with-slash (path)
  (if (not (equal (aref path (- (length path) 1)) ?/))
	(concat path "/")
    path))


(defun file-finder-visit-corresponding-file (new-project-dir)
    "Find the same file in another project so it can be easily diffed"
    (interactive "DNew Project Directory: ")
    (let (current-buffer-path new-file project-path)
      ;; grab the project specific directory and file portion only
      (setq current-buffer-path (buffer-file-name))
      (setq project-path (replace-regexp-in-string (file-truename nide-project-dir) "" current-buffer-path))

      ;; prepend the new path to the file and open
      (setq new-project-dir (terminate-path-with-slash new-project-dir))
      (setq new-file (concat new-project-dir project-path))
      (message new-file)))

(defun foo ()
  (interactive)
  (find-file (file-finder-visit-corresponding-file "~/Firmware/ENH/4.0.0_p")))

(defun bar ()
  (interactive)
  (save-excursion
    (let (comparison-file comparison-buffer current-buffer-orig)
      (setq current-buffer-orig (current-buffer))
      (setq comparison-file (file-finder-visit-corresponding-file "~/Firmware/ENH/4.0.0_p"))
      (find-file comparison-file)
      (setq comparison-buffer (get-file-buffer comparison-file))
      (ediff-buffers current-buffer-orig comparison-buffer)
      ;(kill-buffer comparison-buffer) - can't do this yet because ediff is asynchronous
      )))
    