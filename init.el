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
;;   :custom
;;   (auto-package-update-interval 7)
;;   (auto-package-update-prompt-before-update t)
;;   (auto-package-update-hide-results t)
;;   :config
;;   (auto-package-update-maybe)
;;   (auto-package-update-at-time "09:00"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eshell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package eshell
  :init
  (defun my/eshell-buffer-name (dir)
    (format "*eshell:%s*" (expand-file-name dir)))

  (defun my/eshell-rename-buffer nil
    (rename-buffer
     (generate-new-buffer-name (my/eshell-buffer-name default-directory))))

  (defun my/eshell-here (&optional arg)
    (interactive "P")
    (let* ((dir (file-name-directory
                 (or (buffer-file-name)
                     default-directory)))
           (eshell-buffer-name (my/eshell-buffer-name dir)))
      (if arg
          ;; always create a new window with a prefix argument
          (eshell t)
        (let ((buffer (get-buffer eshell-buffer-name)))
          (if buffer
              (pop-to-buffer buffer)
            ;; create a new buffer if it doesn't already exist
            (eshell t))))))
  :bind ("C-c e" . my/eshell-here)
  :hook (eshell-directory-change . my/eshell-rename-buffer))

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

;;;;;;;;;;;;;;;;; rust-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rust-mode
  :mode "\\.rs\\'"
  :custom
  (rust-format-on-save t))

;;;;;;;;;;;;;;;;; dockerfile-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dockerfile-mode
	  :mode "Dockerfile\\'")
;;;;;;;;;;;;;;;;; docker-compose-mode ;;;;;;;;;;;;;;;;;;;;;;;;
(use-package docker-compose-mode :ensure t)

;;;;;;;;;;;;;;;;; org-bullets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-bullets :ensure t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;;;;;;;;;;;;;;;;; org-roam-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/.roam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           ;; %?"
           (file "~/.roam/templates/daily.org")
           :if-new (file+head "%<%Y>/%<%m>/%<%d-%m-%Y>.org"
                              "#+title: %<%d-%m-%Y>\n"))))
  (org-roam-db-autosync-mode))

;;;;;;;;;;;;;;;;; Final settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(windmove-default-keybindings 'ctrl)
(cua-mode 1)
