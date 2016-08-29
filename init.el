(require 'package)

(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
        ("org"         . "http://orgmode.org/elpa/")
        ("melpa"       . "http://melpa.org/packages/")
        ;;("marmalade"   . "http://marmalade-repo.org/packages/")
	))

(defvar my-packages '(better-defaults ergoemacs-mode magit python-mode php-mode yaml-mode))


(package-initialize)
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'better-defaults)
(require 'ergoemacs-mode)


(setq ergoemacs-theme "lvl2")
(setq ergoemacs-keyboard-layout "us")
(ergoemacs-mode 1)

