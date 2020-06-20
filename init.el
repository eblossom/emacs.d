(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("tromey" . "https://tromey.com/elpa/") t)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; (setq package-archives
;;       '(("gnu"         . "http://elpa.gnu.org/packages/")
;;         ("org"         . "http://orgmode.org/elpa/")
;;         ("melpa"       . "http://melpa.org/packages/")
;;         ("marmalade"   . "http://marmalade-repo.org/packages/")
;; 	))


;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))


;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; evil is your friend
    evil

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ido-completing-read+

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; edit html tags like sexps
    tagedit

    ;; git integration
    magit

    python-mode
    yaml-mode
    ;; php-mode

    cmake-mode
    haskell-mode
    ggtags
    org
    julia-mode
    julia-repl
    markdown-mode
    slime
    verilog-mode
    ))

;; (defvar my-packages
;;   '(better-defaults
;;     projectile
;;     clojure-mode
;;     cider
;;     magit
;;     python-mode
;;     php-mode
;;     yaml-mode))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))


(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; When on macOS: command -> meta, option -> super
;; This matches my normal expectation and assumes
;; that the keyboard itself hasn't be reprogrammed
(when (memq window-system '(mac ns))
  (setq ns-command-modifier 'meta)
  (setq ns-alternate-modifier 'super))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")

;;(require 'better-defaults)

;; xemacs compatible keybindings
;;(global-set-key "\M-g" 'goto-line)
;;(global-set-key "\C-xx" 'copy-to-register)
;;(global-set-key "\C-xg" 'insert-register)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;(require 'php-mode)
;(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
;(add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))


(require 'cmake-mode)
(add-to-list 'auto-mode-alist '("\\CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))


;; Common Lisp support
;;(load (expand-file-name "~/quicklisp/slime-helper.el"))
;;(setq inferior-lisp-program "sbcl")
;;(require 'slime-autoloads)
;;(add-to-list 'slime-contribs 'slime-fancy)
;;(global-set-key "\C-cs" 'slime-selector)

;; use ggtags for M-. M-, C-M-.

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

;; undo ggtags override of M-< and M->
'(let ((map ggtags-navigation-map))
  (define-key map "\M->" nil)
  (define-key map "\M-<" nil))

;; Have julia mode always use julia-repl minor mode
(require 'julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(coffee-tab-width 2)
 '(custom-enabled-themes (quote (misterioso)))
 '(custom-safe-themes
   (quote
    ("934a85d32fbefd8c29bfb0a089835033866da6c01f446d86d36999b9d0eb2246" default)))
 '(package-selected-packages
   (quote
    (ido-completing-read+ verilog-mode slime evil yaml-mode tagedit smex rainbow-delimiters python-mode projectile paredit org-journal org markdown-mode magit julia-repl julia-mode json-mode ido-ubiquitous haskell-mode haskell-emacs ggtags editorconfig-custom-majormode cmake-mode clojure-mode-extra-font-locking cider auto-complete-pcmp 0blayout))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
