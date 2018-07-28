;;; Personal customizations

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Beginners might consider leaving this nil
(setq inhibit-startup-message t)

(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'erase-buffer 'disabled nil)

(add-to-list 'load-path (expand-file-name "programming" prelude-personal-dir))
(prelude-require-packages '(ggtags p4 smart-tabs-mode multiple-cursors expand-region))

;;(setq p4-use-p4config-exclusively t)


;; Prelude overrides
(global-flycheck-mode -1)
(setq prelude-whitespace nil)
(global-set-key [remap move-beginning-of-line]
                'move-beginning-of-line)

;; --------------- Generic key binding ---------------

(global-set-key "\M-g"   'goto-line)
(global-set-key "\M-r"   'revert-buffer)
(global-set-key "\M-o"   'tags-search)
(global-set-key [f7]     'previous-error)
(global-set-key [f8]     'next-error)
(global-set-key [f12]    'ntc-rename-func)
(global-set-key "\C-ce"  'ediff-buffers)
(global-set-key "\C-c "  'cc-find-other-file)
(global-set-key "\C-ca"  'cc-append-to-line)
(global-set-key "\C-cc"  'cc-chomp-lines)
(global-set-key "\C-cr"  'cc-chomp-lines-regexp)
(global-set-key "\C-cf"  'cc-func-body)
(global-set-key "\C-ci"  'cc-if-clause)
(global-set-key "\C-ch"  'cc-numbers-hex)
(global-set-key "\C-cn"  'cc-numbers)
(global-set-key "\C-ct"  'cc-align-table)
(global-set-key "\C-c^"  'cc-join-lines)
(global-set-key "\M-*"   'pop-tag-mark)


(setq-default show-trailing-whitespace t)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
;;(setq backup-inhibited t)
(setq line-number-mode t)
(column-number-mode 'true)
(setq compile-command "make")

(defun my-c-mode-common-hook ()
  (c-set-style "BSD")
  (ggtags-mode 1)
  (whitespace-indent-mode-hook))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(setq minibuffer-max-depth nil)



(require 'mouse);; Additional modules
(require 'p4)
(require 'code-constructor)
(require 'mouse)
(require 'white-space)
(require 'multiple-cursors)
(require 'yasnippet)
(yas-global-mode 1)

(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (multiple-cursors smart-tabs-mode p4 ggtags company-anaconda anaconda-mode key-chord company helm-projectile helm smex ido-completing-read+ flx-ido exec-path-from-shell zop-to-char zenburn-theme which-key volatile-highlights undo-tree smartrep smartparens smart-mode-line projectile operate-on-number move-text magit imenu-anywhere hl-todo guru-mode grizzl god-mode gitignore-mode gitconfig-mode git-timemachine gist flycheck expand-region editorconfig easy-kill discover-my-major diminish diff-hl crux browse-kill-ring beacon anzu ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
