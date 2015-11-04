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


;; More familiar key bindings for undo and redo
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-Z"))
(global-unset-key (kbd "C-x C-z"))
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)


;; When i hit TAB, just insert the TAB!
;(global-set-key (kbd "TAB") 'self-insert-command);


;; Enter act like auto-indent, like C-j
(global-set-key (kbd "RET") 'newline-and-indent)


;; Comment or uncomment lines
(global-set-key (kbd "<f8> c") 'comment-or-uncomment-region)


;; Replace M-x with Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;; Inserts date in ISO 8601 format -->
(defun insertdate ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(global-set-key [(f5)] 'insertdate)
;; <--


;;------------------------------------------------------------------------------
(provide 'config~keybindings)

