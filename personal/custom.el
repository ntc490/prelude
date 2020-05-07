;; Note: to avoid checking package signatures add the following two
;; lines to the top of init.el:
;;   (require 'package)
;;   (setq package-check-signature nil)


;; Personal customizations

(require 'prelude-helm-everywhere)
(remove-hook 'prog-mode 'flycheck-mode)
(global-flycheck-mode -1)
(flyspell-mode -1)

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
(prelude-require-packages '(ggtags p4 smart-tabs-mode multiple-cursors expand-region
			    yasnippet helm-swoop spacemacs-theme go-projectile))

(load-theme 'wombat t)

;; Prelude overrides
(setq prelude-whitespace nil)
(define-key prelude-mode-map (kbd "C-a") 'move-beginning-of-line)

(require 'ggtags)
(define-key ggtags-navigation-map (kbd "M->") 'nil)
(define-key ggtags-navigation-map (kbd "M-<") 'nil)
(define-key prelude-mode-map (kbd "\C-cr") 'nil) ;; override crux rename buffer
(rtags-enable-standard-keybindings prelude-mode-map "\C-cr")

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))


;; --------------- Generic key binding ---------------

(global-set-key "\M-g"   'goto-line)
(global-set-key "\M-r"   'revert-buffer)
(global-set-key "\M-o"   'tags-search)
(global-set-key [f7]     'previous-error)
(global-set-key [f8]     'next-error)
(global-set-key [f12]    'ntc-rename-func)
;(global-set-key "\C-s"   'helm-swoop)
(global-set-key "\C-ce"  'ediff-buffers)
(global-set-key "\C-c "  'cc-find-other-file)
(global-set-key "\C-ca"  'cc-append-to-line)
(global-set-key "\C-cc"  'cc-chomp-lines)
;;(global-set-key "\C-cr"  'cc-chomp-lines-regexp)
(global-set-key "\C-cf"  'cc-func-body)
(global-set-key "\C-ci"  'cc-if-clause)
(global-set-key "\C-ch"  'cc-numbers-hex)
(global-set-key "\C-cn"  'cc-numbers)
(global-set-key "\C-ct"  'cc-align-table)
(global-set-key "\C-c^"  'cc-join-lines)
(global-set-key "\M-*"   'rtags-location-stack-back)
(global-set-key "\M-@"   'er/expand-region)
(global-set-key "\M-#"   'mc/mark-next-like-this)
(global-set-key (kbd "M-C-c") 'mc/edit-lines)
(global-set-key "\M-n"   'next-error)
(global-set-key "\M-p"   'previous-error)

(global-set-key "\M-."   'rtags-find-symbol-at-point)
;; (global-set-key (kbd "C-.") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(setq-default show-trailing-whitespace nil)
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
;;  (ggtags-mode 1)
  (setq fill-column 80)
  (remove-dos-eol))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-c++-mode-common-hook ()
  (c-set-style "BSD")
;;  (ggtags-mode 1)
  (setq fill-column 80)
  (remove-dos-eol))
(add-hook 'c++-mode-common-hook 'my-c++-mode-common-hook)

;; go get github.com/npat-efault/godef
;; go get golang.org/x/tools/cmd/goimports
(setq company-idle-delay nil)
(global-set-key (kbd "C-c M-n") 'company-complete)
(global-set-key (kbd "C-c C-n") 'company-complete)

(require 'go-projectile)
(setq gofmt-command "goimports")
(defun my-go-mode-hook ()
  (set (make-local-variable 'company-backends) '(company-go))
  (local-set-key (kbd "C-c m") 'gofmt)
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook 'company-mode)

;; go get github.com/npat-efault/godef
(setq minibuffer-max-depth nil)
(setq ggtags-update-on-save nil)

(require 'mouse)
(require 'p4)
(require 'code-constructor)
(require 'mouse)
(require 'white-space)
(require 'multiple-cursors)
(require 'yasnippet)
(require 'helm-swoop)

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
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(package-selected-packages
   (quote
    (rtags clang-format json-mode csv-mode yaml-mode dockerfile-mode markdown-mode cmake-mode go-projectile zop-to-char yasnippet which-key volatile-highlights undo-tree smex smartrep smartparens smart-tabs-mode smart-mode-line operate-on-number multiple-cursors move-text magit key-chord imenu-anywhere ido-completing-read+ hl-todo helm-swoop helm-projectile helm-descbinds helm-ag guru-mode grizzl god-mode gitignore-mode gitconfig-mode git-timemachine gist ggtags flycheck flx-ido expand-region exec-path-from-shell editorconfig easy-kill discover-my-major diminish diff-hl company-anaconda browse-kill-ring beacon anzu ace-window))))
 '(package-selected-packages
   (quote
    (dockerfile-mode go-projectile company-go go-eldoc ag go-imports ggo-mode go-mode yafolding php-mode zop-to-char yasnippet which-key volatile-highlights undo-tree smex smartrep smartparens smart-tabs-mode smart-mode-line operate-on-number multiple-cursors move-text markdown-mode magit key-chord json-mode imenu-anywhere ido-completing-read+ hl-todo helm-swoop helm-projectile helm-descbinds helm-ag guru-mode grizzl god-mode gitignore-mode gitconfig-mode git-timemachine gist ggtags flycheck flx-ido expand-region editorconfig easy-kill discover-my-major diminish diff-hl company-anaconda cmake-mode browse-kill-ring beacon anzu ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
