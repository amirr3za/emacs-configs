;; Configure Package manager.
(require 'package)
(setq package-user-dir "~/.emacs.d/packages/")
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; List of packages to be installed.
(defvar my-packages
  '(go-mode               ;; Golang
    erlang                ;; Erlang
    alchemist             ;; Elixir
    rust-mode             ;; Rust
    web-mode              ;; JavaScript, HTML, CSS, ...
    jade-mode             ;; Jade-mode and stylus-mode
    markdown-mode         ;; Markdown

    company               ;; Auto completions
    magit                 ;; Git
    helm                  ;; Incremental completion and selection narrowing framework
    smartparens           ;; combination of autopair, textmate, wrap-region, paredit
    solarized-theme       ;; Solarized theme
    undo-tree             ;; Better undo/redo
    guide-key)            ;; Guide the following key bindings automatically
  "A list of packages to ensure are installed at launch.")

;; Read the list and install the missing packages.
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Start the emacs server, only if its not running already.
(require 'server)
(unless (server-running-p) (server-start))


;; helm
(require 'helm-config)
(helm-mode 1)
(setq helm-mode-fuzzy-match                     t
      helm-M-x-fuzzy-match                      t
      helm-lisp-fuzzy-completion                t
      helm-buffers-fuzzy-matching               t
      helm-completion-in-region-fuzzy-match     t
      helm-boring-file-regexp-list              '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$")
      helm-move-to-line-cycle-in-source         t)
(helm-autoresize-mode 1)


;; undo-tree
(global-undo-tree-mode 1)


;; Company-mode
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay              0.3
      company-minimum-prefix-length   2
      company-show-numbers            t)


;; guide-key
;(setq guide-key/idle-delay 0.1)
;(setq guide-key/guide-key-sequence '("C-x"))
;(guide-key-mode 1)


;; Right margin
;(require 'whitespace)
(setq whitespace-line-column 100) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)
(custom-set-faces '(whitespace-line ((t (:background "gray20" :foreground nil :highlight nil)))))
;(global-whitespace-mode +1)


;; SmartParens
;(setq sp-highlight-pair-overlay nil)
(smartparens-global-mode t)


;; open unknown files in text mode
(setq major-mode         'text-mode
      initial-major-mode 'text-mode)


;; C family
(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "K&R")
            (setq tab-width 4)
            (setq c-basic-offset 4)))


;; Magit
(autoload 'magit-status "magit" t nil)


;; Go-mode
;(autoload 'go-mode "go-mode" t nil)


;; Erlang
(require 'erlang-start)

;; Elixir - alchemist
(require 'alchemist)
;(autoload 'alchemist-mode "alchemist" t nil)
;(add-to-list 'auto-mode-alist '("\\.exs" . alchemist-mode))

;; Rust-mode
;(autoload 'rust-mode "rust-mode" t nil)

;; Markdown-mode
(autoload 'markdown-mode "markdown-mode" t nil)
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))


;; web-mode
;(autoload 'web-mode "web-mode" t nil)
(add-hook 'web-mode-hook (lambda () (whitespace-mode -1)))  ;;No whitespace-mode in web-mode
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css$"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl$"  . web-mode))

(setq web-mode-markup-indent-offset    2
      web-mode-css-indent-offset       2
      web-mode-code-indent-offset      2
      web-mode-indent-style            2
      web-mode-tag-auto-close-style    2
      web-mode-disable-autocompletion  t)


;;------------------------------------------------------------------------------
(provide 'config-modes)
