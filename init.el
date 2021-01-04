;;;;;;;;;;;;;;;;;;;;
;; Emacs - Config ;;
;;;;;;;;;;;;;;;;;;;;

;; Package initialisation
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))

;; List of packages to install
(defvar my/packages
  '(exec-path-from-shell
    magit
    ess
    elpy
    cider
    company
    markdown-mode
    pyenv-mode
    ;; haskell-mode
    ;; tidal
    which-key))

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

;; Avoid opening of new frame when opening from finder
(setq ns-pop-up-frames nil)

;; Avoid custom data to be writen into init
(setq custom-file (make-temp-file "custom.el"))

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

;; which - key
(require 'which-key)

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
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

;;;;;;;;;;;;;;
;; Markdown ;;
;;;;;;;;;;;;;;

(require 'markdown-mode)

;;;;;;;;;;;;;;;;;;
;; Tydal Cycles ;;
;;;;;;;;;;;;;;;;;;

;; (require 'tidal)
;; (setq tidal-interpreter "~/.ghcup/bin/ghci")

;;;;;;;;;
;; End ;;
;;;;;;;;;
