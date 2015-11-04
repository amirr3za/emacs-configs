;; No autosaving, No backup files. No .save files
(setq make-backup-files        nil)
(setq auto-save-list-file-name nil)
(setq auto-save-default        nil)


;; display line numbers. Highlight current line. Emacs 23> only.
(global-linum-mode   t)
(global-hl-line-mode 1)
;(set-face-background 'hl-line "#080808")
(set-face-foreground 'highlight nil)


;; better format for line number margin
(setq linum-format  "%4d ")


;; makes the column number show up in mode-line
(setq column-number-mode t)


;; Space instead of TAB
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;(setq-default tab-always-indent t)


;; Set file encodings
(prefer-coding-system       'utf-8)
(setq locale-coding-system  'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)


;; Turn off the bell
(setq visible-bell t)


;; change yes or no prompt to y or n prompts
(fset 'yes-or-no-p 'y-or-n-p)


;; match parens
(show-paren-mode t)


;; Turn off line wrap
(setq-default truncate-lines t)


;; clean up trailing whitespaces when saving files
(add-hook 'before-save-hook 'whitespace-cleanup)


;; Always end a file with a newline
(setq require-final-newline nil)


;; Ignore this files
(setq completion-ignored-extensions
  '(".dll" ".o" ".pyc" ".bak" ".exe" ".jpg" ".jpeg" ".png" ".gif" ".pdf" ".mkv"
	".class" ".tar" ".gz" ".zip" ".jar" ".flv" ".mp3" ".rar" ".obj" ".a" ".elc"
	".webm" ".mp4"))


;; Open new emacsclient in existing window
(setq ns-pop-up-frames nil)


;; Seprate file for customs
(setq custom-file "config-customs.el")
(load custom-file 'noerror)


;; if a file have root permission, bring up the sudo.
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


;;------------------------------------------------------------------------------
(provide 'config-behavior)
