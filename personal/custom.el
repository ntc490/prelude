;;; Personal customizations

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
