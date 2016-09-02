(require 'package)

(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
        ("org"         . "http://orgmode.org/elpa/")
        ("melpa"       . "http://melpa.org/packages/")
        ("marmalade"   . "http://marmalade-repo.org/packages/")
	))

(defvar my-packages 
  '(better-defaults ergoemacs-mode magit python-mode php-mode yaml-mode))


(package-initialize)
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'better-defaults)

;; xemacs compatible keybindings
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-xx" 'copy-to-register)
(global-set-key "\C-xg" 'insert-register)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))


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
