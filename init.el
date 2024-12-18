;;;;;;;;;;;;;;;;;;;;
;; Emacs - Config ;;
;;;;;;;;;;;;;;;;;;;;


;; Install Missing Packages
;; list the packages you want
(setq package-list '(magit ess elpy cider company pyenv-mode which-key exec-path-from-shell))

;; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
			 ("MELPA Stable" . "https://stable.melpa.org/packages/")
			 ("MELPA" . "https://melpa.org/packages/")))

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;; Custome theme
;; curl -LO https://raw.githubusercontent.com/emacs-jp/replace-colorthemes/master/billw-theme.el
(load-theme 'billw t t)
(enable-theme 'billw)

;; No splash screen
(setq inhibit-startup-screen t
      initial-scratch-message ";; Hello Hervé!\n\n")

;; Hide toolbar
(tool-bar-mode -1)

;; Avoid opening of new frame when opening from finder
(setq ns-pop-up-frames nil)

;; Avoid custom data to be writen into init
(setq custom-file (make-temp-file "custom.el"))

;; Auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; New line
(setq next-line-add-newlines t)

;; Allow hash sign
(global-set-key (kbd "M-3") #'(lambda () (interactive) (insert "#")))

;; Load path from shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Ido
(ido-mode t)

;; which - key
(require 'which-key)
(which-key-mode)

;;;;;;;
;; R ;;
;;;;;;;

;; ESS
;; (require 'ess-site)

;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

;; ELPY
;; pip install flake8
;; pip install jedi
(elpy-enable)
(setq elpy-rpc-virtualenv-path 'current)
(pyenv-mode)
;; temp fix to work around bug -> see: https://github.com/jorgenschaefer/elpy/issues/1976
(setq elpy-shell-echo-output nil) 

;;;;;;;;;;;;;
;; Clojure ;;
;;;;;;;;;;;;;

;; Cider
(require 'cider)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

;;;;;;;;;
;; End ;;
;;;;;;;;;
(put 'downcase-region 'disabled nil)
