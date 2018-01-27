;;;;;;;;;;;;;;;;;;;;
;; Emacs - Config ;;
;;;;;;;;;;;;;;;;;;;;

;; Package initialisation
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; List of packages to install
(defvar my/packages
  '(exec-path-from-shell
    magit
    ess
    elpy
    cider
    company))

;; Install missing packages
(require 'cl-lib)
(defun my/install-packages ()
  "Ensure the packages I use are installed. See `my/packages'."
  (interactive)
  (let ((missing-packages (cl-remove-if #'package-installed-p my/packages)))
    (when missing-packages
      (message "Installing %d missing package(s)" (length missing-packages))
      (package-refresh-contents)
      (mapc #'package-install missing-packages))))

(my/install-packages)

;; Custome theme
;; curl -LO https://raw.githubusercontent.com/emacs-jp/replace-colorthemes/master/billw-theme.el
(load-theme 'billw t t)
(enable-theme 'billw)

;; No splash screen
(setq inhibit-startup-screen t
      initial-scratch-message ";; Hello Hervé!\n\n")

;; Hide toolbar
(tool-bar-mode -1)

;; avoid opening of new frame when opening from finder
(setq ns-pop-up-frames nil)

;; Auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; New line
(setq next-line-add-newlines t)

;; Allow hash sign
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;; Load path from shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Ido
(ido-mode t)

;;;;;;;
;; R ;;
;;;;;;;

;; ESS
(require 'ess-site)

;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

;; ELPY
;; pip install flake8
;; pip install jedi
(elpy-enable)

;;;;;;;;;;;;;
;; Clojure ;;
;;;;;;;;;;;;;

;; Cider
(require 'cider)
(add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

;;;;;;;;;
;; End ;;
;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (magit exec-path-from-shell ess elpy cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
