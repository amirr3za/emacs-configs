;; Initial emacs frames (windows) size
(setq default-frame-alist '((wait-for-wm . nil) (width . 120) (height . 35)))


;; Turn off GUI interfaces
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; Enable mouse in termina
(xterm-mouse-mode t)


;; Font
(set-frame-font "Source Code Pro Medium-12")


;; Theme
(load-theme 'minimal-dark t)
;(load-theme 'minimal-light t)


;; No startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)


;; Disable spell chekcer
(defconst *spell-check-support-enabled* nil)


;; change boxed cursor shape to bar/box
(setq-default cursor-type 'bar)


;; No autosaving, No backup files. No .save files
(setq make-backup-files        nil)
(setq auto-save-list-file-name nil)
(setq auto-save-default        nil)


;; display line numbers. Highlight current line. Emacs 23> only.
(global-linum-mode   t)
(global-hl-line-mode 1)


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
(setq custom-file "config~customs.el")
(load custom-file 'noerror)


;; if a file have root permission, bring up the sudo.
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


;;------------------------------------------------------------------------------
(provide 'config~behavior)

