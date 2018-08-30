;; Personal customizations

(load-theme 'spacemacs-dark)
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
(prelude-require-packages '(ggtags p4 smart-tabs-mode multiple-cursors expand-region))


;; Prelude overrides
(setq prelude-whitespace nil)
(define-key prelude-mode-map (kbd "C-a") 'move-beginning-of-line)

;; --------------- Generic key binding ---------------

(global-set-key "\M-g"   'goto-line)
(global-set-key "\M-r"   'revert-buffer)
(global-set-key "\M-o"   'tags-search)
(global-set-key [f7]     'previous-error)
(global-set-key [f8]     'next-error)
(global-set-key [f12]    'ntc-rename-func)
(global-set-key "\C-s"   'helm-swoop)
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
(global-set-key "\M-@"   'er/expand-region)
(global-set-key (kbd "M-C-c") 'mc/edit-lines)
(global-set-key "\M-n"   'next-error)
(global-set-key "\M-p"   'previous-error)
(global-set-key "\M-}"   'ggtags-navigation-next-file)
(global-set-key "\M-{"   'ggtags-navigation-previous-file)
;; (global-set-key (kbd "C-.") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

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
  (ggtags-mode 1))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(setq minibuffer-max-depth nil)
(setq ggtags-update-on-save nil)
(setq ggtags-enable-navigation-keys nil)

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
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   (quote
    ("3f44e2d33b9deb2da947523e2169031d3707eec0426e78c7b8a646ef773a2077" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "1e9001d2f6ffb095eafd9514b4d5974b720b275143fbc89ea046495a99c940b0" "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(fci-rule-color "#383838")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (spacemacs-theme markdown-mode helm-swoop zop-to-char zenburn-theme yasnippet which-key volatile-highlights undo-tree smex smartrep smartparens smart-tabs-mode smart-mode-line p4 operate-on-number multiple-cursors move-text magit key-chord ivy imenu-anywhere ido-completing-read+ hl-todo helm-projectile helm-descbinds helm-ag guru-mode grizzl god-mode gitignore-mode gitconfig-mode git-timemachine gist ggtags flycheck flx-ido expand-region exec-path-from-shell editorconfig easy-kill discover-my-major diminish diff-hl crux company-anaconda browse-kill-ring beacon anzu ace-window)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
