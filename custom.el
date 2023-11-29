(setenv "PATH"
        (concat
         "/usr/local/go/bin" path-separator
         "/usr/local/sbin" path-separator
         "/usr/local/bin" path-separator
         "/usr/sbin" path-separator
         "/usr/bin" path-separator
         "/sbin" path-separator
         "/bin/" path-separator
     ))

(scroll-bar-mode -1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   '("570263442ce6735821600ec74a9b032bc5512ed4539faf61168f2fdf747e0668" "ddffe74bc4bf2c332c2c3f67f1b8141ee1de8fd6b7be103ade50abb97fe70f0c" "5b9a45080feaedc7820894ebbfe4f8251e13b66654ac4394cb416fef9fdca789" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "7ea883b13485f175d3075c72fceab701b5bf76b2076f024da50dff4107d0db25" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "7e068da4ba88162324d9773ec066d93c447c76e9f4ae711ddd0c5d3863489c52" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" default))
 '(package-selected-packages
   '(yaml-mode magit company org-roam-ui lsp-treemacs hydra counsel ivy all-the-icons rust-mode pdf-tools twittering-mode go-errcheck lsp-go lsp-mode unicode-fonts neotree org-roam ewal-doom-themes deft notmuch flymake-go yasnippet go-mode go org-bullets rainbow-delimiters evil-collection counsel-projectile which-key no-littering dired-open python-mode vterm lsp-ui dap-mode forge dired-hide-dotfiles visual-fill-column evil-nerd-commenter helpful eshell-git-prompt company-box doom-modeline command-log-mode dired-single general lsp-ivy ivy-rich all-the-icons-dired pyvenv doom-themes eterm-256color use-package typescript-mode ivy-prescient))
 '(tool-bar-mode nil)
 '(warning-suppress-types '((use-package) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq doom-emoji-fallback-font-families nil)
(setenv "GO111MODULE" "off")
