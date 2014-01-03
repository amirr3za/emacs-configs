;; Start the emacs server, only if its not running already.
(require 'server)
(unless (server-running-p) (server-start))


;; Package manager
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))
  
;; Define packages to be installed if missing
(defvar my-packages
  '(go-mode               ;; Golang
    cider                 ;; Clojure
    web-mode              ;; JavaScript, HTML, CSS, ...
    jade-mode             ;; Jade-mode and stylus-mode 
    auto-complete         ;; Auto completions
    markdown-mode         ;; Markdown
    magit                 ;; Git
    smartparens           ;; combination of autopair, textmate, wrap-region, paredit
    ido-ubiquitous        ;; ido everywhere
    smex                  ;; replacement for M-x (based on ido)
    dash                  ;; A modern list library for Emacs (need by some modes)
    s)                    ;; An Emacs string manipulation library (need by some modes))
  "A list of packages to ensure are installed at launch.")

;; Goes through the list and installs the missing packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; ido
(require 'ido)
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)


;; Smex
(smex-initialize) ;; key-binding for smex are defined in config~keybindings.el


;; Autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-modes 'go-mode)
(add-to-list 'ac-modes 'clojure-mode)
(add-to-list 'ac-modes 'web-mode)
(add-to-list 'ac-modes 'python-mode)
(add-to-list 'ac-modes 'text-mode)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/auto-complete/ac-dict")
(ac-config-default)


;; Right margin
;(require 'whitespace)
(setq whitespace-line-column 100) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)
;(global-whitespace-mode +1)


;; SmartParens
;(setq sp-highlight-pair-overlay nil)
(smartparens-global-mode t)


;; open unknown files in text mode
(setq major-mode         'text-mode)
(setq initial-major-mode 'text-mode)


;;------------------------------------------------------------------------------
;; Autoloads 
;;------------------------------------------------------------------------------

;; C family
(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "K&R")
            (setq tab-width 4)
            (setq c-basic-offset 4)))
            
            
;; Magit
(autoload 'magit-status "magit" nil t)


;; Go-mode
(autoload 'go-mode "go-mode" nil t)


;; Markdown-mode
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))


;; web-mode
;(autoload 'web-mode "web-mode" nil t)
(add-hook 'web-mode-hook (lambda () (whitespace-mode -1)))  ;;No whitespace-mode in web-mode
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css$"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl$"  . web-mode))

(setq web-mode-markup-indent-offset   2)
(setq web-mode-css-indent-offset      2)
(setq web-mode-code-indent-offset     2)
(setq web-mode-indent-style           2)
(setq web-mode-tag-auto-close-style   2)
(setq web-mode-disable-autocompletion t)


;;------------------------------------------------------------------------------
(provide 'config~modes)

