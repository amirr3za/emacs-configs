;; Don't kill emacs easily. use "C-x Really Quit"
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)


;; Easily switch between buffers. or use (C-x b) in ido-mode
(global-set-key (kbd "M-<right>") 'next-buffer)
(global-set-key (kbd "M-<left>")  'previous-buffer)


;; Move in windows
(global-set-key (kbd "<S-right>") 'windmove-right)
(global-set-key (kbd "<S-left>")  'windmove-left)
(global-set-key (kbd "<S-up>")    'windmove-up)
(global-set-key (kbd "<S-down>")  'windmove-down)


;; Undo / Redo More familiar key bindings. [undo-tree]
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-S-z"))
(global-unset-key (kbd "C-x C-z"))

(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-z")    'undo)
(global-set-key (kbd "C-S-z")  'redo)


;; When i hit TAB, just insert the TAB!
;(global-set-key (kbd "TAB") 'self-insert-command);


;; Enter act like auto-indent, like C-j
(global-set-key (kbd "RET") 'newline-and-indent)


;; Comment or uncomment lines
(global-set-key (kbd "<f8> c") 'comment-or-uncomment-region)


;; helm
(global-set-key (kbd "M-x")       'undefined)
(global-set-key (kbd "M-x")       'helm-M-x)
(global-set-key (kbd "M-y")       'helm-show-kill-ring)
(global-set-key (kbd "C-c f")     'helm-recentf)
(global-set-key (kbd "C-x C-f")   'helm-find-files)
(define-key global-map [remap list-buffers]   'helm-buffers-list)


;; Inserts date in ISO 8601 format -->
(defun insertdate ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(global-set-key [(f5)] 'insertdate)
;; <--


;;------------------------------------------------------------------------------
(provide 'config-keybindings)
