;; Default settings
(defvar efs/default-font-size 130)
(defvar efs/frame-transparency '(90 . 90))
(setq gc-cons-threshold (* 50 1000 1000))
(setq inhibit-startup-message t)

;; GUI settings
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(column-number-mode)
(global-display-line-numbers-mode t)

(set-face-attribute 'default nil :font "Fira Code Nerd Font Propo" :height efs/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Fira Code Nerd Font Propo" :height efs/default-font-size)

;; Frame settings
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(docker-compose-mode dockerfile-mode docker yaml-mode ansible doom-modeline go-mode counsel doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
