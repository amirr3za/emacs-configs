;; Check for emacs version
(when (< emacs-major-version 24)
    (message "Your Emacs is older than v24. Please upgrade."))

;; Check for Operation System
(defconst *is-a-linux*   (string-equal system-type "gnu/linux"))
(defconst *is-a-mac*     (string-equal system-type "darwin"))
(defconst *is-a-windows* (string-equal system-type "windows-nt"))

;; Add config files to path
(add-to-list 'load-path  "~/.emacs.d/config")

(require 'config-paths)
(require 'config-modes)
(require 'config-appearance)
(require 'config-behavior)
(require 'config-keybindings)


;(when (string-equal (getenv "emacs_on_server") "yes")
;  (message "emacs is on server"))
