(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load config from org file
(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org" user-emacs-directory))

  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
;; If there is e than one, they won't work right.
  '(package-selected-packages '(smex use-package))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" default))
 '(helm-minibuffer-history-key "M-p")
 '(ido-everywhere t)
 '(package-selected-packages
   '(org-d20 pinentry yasnippet-snippets yasnippet-bundle rust-mode doom-themes doom-modeline evil-org evil-collection projectile-ripgrep ripgrep exec-path-from-shell magit cmake-ide lsp-ui company-box csharp-mode ido-completing-read+ ido-vertical-mode flx-ido helm-projectile yasnippet helm-lsp helm-xref dap-mode hydra flycheck avy which-key use-package smex projectile powerline-evil lsp-mode evil-surround evil-indent-textobject dracula-theme company)))
