;;; Personal customizations

(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'erase-buffer 'disabled nil)

(add-to-list 'load-path (expand-file-name "programming" prelude-personal-dir))
(prelude-require-packages '(ggtags p4 smart-tabs-mode multiple-cursors expand-region))

;;(setq p4-use-p4config-exclusively t)


;; Prelude overrides
(remove-hook 'prog-mode 'flycheck-mode)
(setq prelude-whitespace nil)
(global-set-key [remap move-beginning-of-line]
                'move-beginning-of-line)

;; --------------- Generic key binding ---------------

(global-set-key "\M-g"   'goto-line)
(global-set-key "\M-r"   'revert-buffer)
(global-set-key "\M-o"   'tags-search)
(global-set-key [f7]     'previous-error)
(global-set-key [f8]     'next-error)
;; (global-set-key [f5]     'nide-build)
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


(setq-default show-trailing-whitespace t)
;;(menu-bar-mode 0)
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

;; Beginners might consider leaving this nil
(setq inhibit-startup-message t)

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
