;; Vendor directory
(add-to-list 'load-path  "~/.emacs.d/vendor")
(let ((default-directory "~/.emacs.d/vendor/"))
  (normal-top-level-add-subdirs-to-load-path))


;; Themes directory
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/minimal")

;;------------------------------------------------------------------------------
(provide 'config~paths)

