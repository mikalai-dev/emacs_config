(defvar efs/default-font-size 120)
(defvar efs/default-variable-font-size 120)

(defvar efs/frame-transparency '(90 . 90))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa-edge" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(setq lsp-pyls-server-command "/usr/bin/pylsp")
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; (use-package auto-package-update
;;  :custom
;;  (auto-package-update-interval 7)
;;  (auto-package-update-prompt-before-update t)
;;  (auto-package-update-hide-results t)
;;  :config
;;  (auto-package-update-maybe)
;;  (auto-package-update-at-time "09:00"))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

(column-number-mode)
(global-display-line-numbers-mode t)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                org-roam-mode-hook
                vterm-mode-hook
                term-mode
                shell-mode-hook
                treemacs-mode-hook
		        neotree-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "Dejavu Sans Mono" :height efs/default-font-size)

;; Set the fixed pitch face
;;(set-face-attribute 'fixed-pitch nil :font "Iosevka Extended" :height efs/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Dejavu Sans Mono" :height efs/default-variable-font-size :height efs/default-font-size :weight 'regular)
;;(set-face-attribute 'variable-pitch nil :font "Iosevka Extended" :height efs/default-variable-font-size :weight 'regular)
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package neotree)

(use-package general
  :after evil
  :config
  (general-create-definer efs/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (efs/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "fde" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/Emacs.org")))))

(setq evil-disable-insert-state-bindings t)
(use-package evil
 :init
 (setq evil-want-integration t)
 (setq evil-want-keybinding nil)
 (setq evil-want-C-u-scroll t)
 (setq evil-want-C-i-jump nil)
 :config
 (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  )

(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map
  (read-kbd-macro evil-toggle-key) 'evil-emacs-state)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package command-log-mode
  :commands command-log-mode)

(use-package doom-themes
  ;;:init (load-theme 'doom-palenight t))
  :init (load-theme 'doom-gruvbox t))
  ;;:init (load-theme 'doom-wilmersdorf t))
  ;;::init (load-theme 'doom-laserwave t))

(use-package all-the-icons)
(require 'unicode-fonts)
(unicode-fonts-setup)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))


(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))



(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  ;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))


(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Dejavu Sans Mono" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(with-eval-after-load 'org
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
      (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
 :hook (lsp-mode . lsp-ui-mode)
 :custom
 (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy
  :after lsp)

(use-package org-roam-ui
 :ensure t
 :hook (after-init . org-roam-ui-mode)
 :config
 (setq org-roam-ui-sync-theme t
       org-roam-ui-follow t
       org-roam-ui-update-on-save t
       org-roam-ui-open-on-start nil)
 (setq org-roam-ui-custom-theme
       '((bg . "#1E2029")
         (bg-alt . "#282a36")
         (fg . "#f8f8f2")
         (fg-alt . "#6272a4")
         (red . "#ff5555")
         (orange . "#f1fa8c")
         (yellow ."#ffb86c")
         (green . "#50fa7b")
         (cyan . "#8be9fd")
        (blue . "#ff79c6")
         (violet . "#8be9fd")
         (magenta . "#bd93f9"))))


(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
   (python-shell-interpreter "python3.11")
  (dap-python-executable "python3.11")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))
  (add-hook 'python-mode-hook
  (lambda ()
  (setq-default indent-tabs-mode t)
  (setq-default tab-width 4)
  (setq-default py-indent-tabs-mode t)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after magit)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


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



;; Company mode
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

(setenv "PATH"
        (concat
         "/usr/local/go/bin" path-separator
         "/usr/local/sbin" path-separator
         "/usr/local/bin" path-separator
         "/usr/sbin" path-separator
         "/usr/bin" path-separator
         "/sbin" path-separator
         "/bin/" path-separator
         "~/.go/" path-separator
     ))

(use-package go-mode
  :custom
  ;; generating completion candidates is slow
  (godoc-use-completing-read nil)
  (godoc-command "go doc -all")
  :hook (go-mode
         ;; don't highlight tabs, since go really likes them
         . (lambda ()
             (setq-local whitespace-style
                         (delq 'tabs whitespace-style)))))

(defun do-org-show-all-inline-images ()
  (interactive)
  (org-display-inline-images t t))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/RoamNotes")
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))


(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)

(defun reload-init-file ()
  (interactive)
  ;; Fails on killing the Messages buffer, workaround:
  (get-buffer-create "*Messages*")
  (load-file "~/.emacs.d/init.el")
)

;; type "y"/"n" instead of "yes"/"no"
(fset 'yes-or-no-p 'y-or-n-p)

;reload default init.el
(global-set-key (kbd "C-x i") 'reload-init-file)
;; undo
(global-set-key (kbd "C-z") 'undo)

;;Kill line
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-x b") 'ibuffer)

(global-set-key (kbd "C-o") 'ido-find-file)

(global-set-key (kbd "C-g") 'goto-line)

;;reload default init.el
(global-set-key (kbd "C-x i") 'reload-init-file)

(global-set-key (kbd "C-f") 'isearch-forward)

(global-set-key (kbd "C-x d") 'deft)
(global-set-key (kbd "C-c t") 'treemacs)

(global-set-key (kbd "<C-kp-add>") 'text-scale-increase)
(global-set-key (kbd "<C-kp-subtract>") 'text-scale-decrease)


 (org-babel-do-load-languages
   'org-babel-load-languages
  '((python . t)))


; use %cpaste to paste code into ipython in org mode
(defadvice org-babel-python-evaluate-session
  (around org-python-use-cpaste
          (session body &optional result-type result-params) activate)
  "Add a %cpaste and '--' to the body, so that ipython does the right
thing."
  (setq body (concat "%cpaste\n" body "\n--"))
  ad-do-it
  (if (stringp ad-return-value)
      (setq ad-return-value (replace-regexp-in-string "\\(^Pasting code;
enter '--' alone on the line to stop or use Ctrl-D\.[\r\n]:*\\)" ""
ad-return-value))))


(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(windmove-default-keybindings 'ctrl)
(cua-mode 1)
(setq neo-window-width 55)
(defun my-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen 'fullboth) ;this makes the frame go fullscreen
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1))

(defun my-non-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'width 82)
  (set-frame-parameter nil 'fullscreen 'fullheight)
  (menu-bar-mode -1))

(defun toggle-fullscreen ()
  (interactive)
  (if (eq (frame-parameter nil 'fullscreen) 'fullboth)
      (my-non-fullscreen)
    (my-fullscreen)))
(custom-set-variables
 '(custom-safe-themes
   '("e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "1f292969fc19ba45fbc6542ed54e58ab5ad3dbe41b70d8cb2d1f85c22d07e518" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" default))
 '(package-selected-packages
   '(elfeed-org elfeed helm helm-mode-manager lsp-mode emacsql-sqlite emacsql-sqlite-module org-roam-ui org-roam lua-mode rustic which-key vterm visual-fill-column use-package unicode-fonts typescript-mode twittering-mode rust-mode rainbow-delimiters pyvenv python-mode pdf-tools org-bullets no-littering neotree lsp-ui lsp-ivy ivy-rich ivy-prescient helpful go-mode go-errcheck general forge evil-nerd-commenter evil-collection eterm-256color eshell-git-prompt elfeed-goodies doom-themes doom-modeline dired-single dired-open dired-hide-dotfiles deft dap-mode counsel-projectile company-box command-log-mode auto-package-update all-the-icons-dired)))
(custom-set-faces
)
