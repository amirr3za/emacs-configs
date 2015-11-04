;; Initial emacs frames (windows) size
(setq default-frame-alist '((wait-for-wm . nil) (width . 120) (height . 35)))


;; Turn off GUI interfaces
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; Enable mouse in terminal
;(xterm-mouse-mode t)

;; Enable mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t))


;; disable bell function
(setq ring-bell-function 'ignore)


;; Set font, when running in gui and if font exist on system.
(when (display-graphic-p)
  (when (member "Fira Mono" (font-family-list))
    (set-face-attribute 'default nil :font "Fira Mono-15")))


;(when (and (display-graphic-p)
;           (member "Fira Mono" (font-family-list)))
;  (set-face-attribute 'default nil :font "Fira Mono-15"))

;(if (display-graphic-p)
;    ; Different font for arabic & persian.
;    (set-fontset-font "fontset-default"
;                      (cons (decode-char 'ucs #x0600)
;                            (decode-char 'ucs #x06ff))
;                      "DejaVu Sans Mono-14")
;  (set-frame-font "Menlo Regular-13"))



;; Theme
;(load-theme 'solarized-dark t)
(load-theme 'minimal-dark t)
;(load-theme 'monochrome t)

;; No startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)


;; Disable spell check
(defconst *spell-check-support-enabled* nil)


;; change boxed cursor shape to bar/box
(setq-default cursor-type 'bar)


;;------------------------------------------------------------------------------
(provide 'config-appearance)
