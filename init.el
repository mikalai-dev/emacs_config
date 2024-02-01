;; Set up custom file
(setq custom-file (concat (file-name-as-directory user-emacs-directory) "custom.el"))
(load custom-file 'noerror)

;; Package initialization
(require 'package)
(setq package-archives '(("melpa-edge" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; use-package setup
(unless (package-installed-p 'use-package) (package-install 'use-package))
(require 'use-package)

;; Uncomment to update packages on start
;;(setq use-package-always-ensure t)

;; (use-package auto-package-update
;;  :custom
;;  (auto-package-update-interval 7)
;;  (auto-package-update-prompt-before-update t)
;;  (auto-package-update-hide-results t)
;;  :config
;;  (auto-package-update-maybe)
;;  (auto-package-update-at-time "09:00"))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config (counsel-mode 1))

(use-package doom-themes
  :init (load-theme 'doom-tokyo-night t))

;; Keybindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x i") 'reload-init-file)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "C-o") 'ido-find-file)
(global-set-key (kbd "C-g") 'goto-line)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-c t") 'treemacs)
(global-set-key (kbd "<C-kp-add>") 'text-scale-increase)
(global-set-key (kbd "<C-kp-subtract>") 'text-scale-decrease)

;; Custom functions
(defun reload-init-file ()
  (interactive)
  (get-buffer-create "*Messages*")
  (load-file "~/.emacs.d/init.el"))

;;;;;;;;;;;;;;;;;; go-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package go-mode
  :custom
  ;; generating completion candidates is slow
  (godoc-use-completing-read nil)
  (godoc-command "go doc -all")
  :hook (go-mode
         . (lambda ()
             (setq-local whitespace-style
                         (delq 'tabs whitespace-style)))))


;;;;;;;;;;;;;;;;; dockerfile-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dockerfile-mode
	  :mode "Dockerfile\\'")
;;;;;;;;;;;;;;;;; docker-compose-mode ;;;;;;;;;;;;;;;;;;;;;;;;s
(use-package docker-compose-mode :ensure t)

;; Final settings
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(windmove-default-keybindings 'ctrl)
(cua-mode 1)
