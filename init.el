(require 'package)

(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
        ("org"         . "http://orgmode.org/elpa/")
        ("melpa"       . "http://melpa.org/packages/")
        ("marmalade"   . "http://marmalade-repo.org/packages/")
	))

(defvar my-packages
  '(better-defaults
    projectile
    clojure-mode
    cider
    magit
    python-mode
    php-mode
    yaml-mode))

(package-initialize)
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;(when (memq window-system '(mac ns))
;;  (exec-path-from-shell-initialize))

;; When on macOS: command -> meta, option -> super
;; This matches my normal expectation and assumes
;; that the keyboard itself hasn't be reprogrammed
(when (memq window-system '(mac ns))
  (setq ns-command-modifier 'meta)
  (setq ns-alternate-modifier 'super))

(require 'better-defaults)

;; xemacs compatible keybindings
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-xx" 'copy-to-register)
(global-set-key "\C-xg" 'insert-register)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;(require 'php-mode)
;(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
;(add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))


;;(require 'ergoemacs-mode)
;;(setq ergoemacs-theme "lvl2")
;;(setq ergoemacs-keyboard-layout "us")
;;(ergoemacs-mode 1)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (misterioso)))
 '(package-selected-packages
   (quote
    (cider clojure-mode projectile yaml-mode undo-tree python-mode php-mode persistent-soft magit exec-path-from-shell ergoemacs-mode better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
