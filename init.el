(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

(global-display-line-numbers-mode)

(load-theme 'dracula t)

(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config (smex-initialize))

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package csharp-mode
  :ensure t
  :init
  (add-hook 'csharp-mode-hook #'company-mode))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  (add-hook 'prog-mode-hook #'lsp)
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-idle-delay 0.1))
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

(use-package avy
  :ensure t)
(use-package flycheck
  :ensure t)
(use-package hydra
  :ensure t)
(use-package dap-mode
  :ensure t)
(use-package helm-xref
  :ensure t)
(use-package helm-projectile
  :bind (("C-S-P" . helm-projectile-switch-project)
         :map evil-normal-state-map
         ("C-p" . helm-projectile))
  :ensure t
  :config
  (evil-leader/set-key
    "ps" 'helm-projectile-ag
    "pa" 'helm-projectile-find-file-in-known-projects
  ))
(use-package helm-lsp
  :ensure t)


(use-package ido
  :ensure t
  :config
  (setq
	ido-virtual-buffers t
	ido-use-faces t
	ido-enable-flex-matching t
	ido-default-buffer-method 'selected-window
	ido-auto-merge-work-directories-length -1)
  (ido-mode)
  (ido-everywhere))

(use-package flx-ido :ensure t :requires ido :config (flx-ido-mode))
(use-package ido-completing-read+ :ensure t :requires ido
  :config
  (setq ido-ubiquitous-max-items 50000
    ido-cr+-max-items 50000)
  (ido-ubiquitous-mode +1))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init (yas-global-mode t))

(use-package company
  :ensure t
  :preface
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or
	 (not company-mode/enable-yas)
	 (and (listp backend) (member 'company-yasnippet backend)))
	backend
      (append (if (consp backend) backend (list backend))
	      '(:with company-yasnippet))))

  :init (global-company-mode t)
  :config
  (setq
   company-idle-delay 0
   company-minimum-prefix-length 1
   company-tooltip-limit 20)
  (setq company-backends
	(mapcar #'company-mode/backend-with-yas company-backends)))
(use-package company-box
  :ensure t
  :requires company
  :hook (company-mode . company-box-mode))

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (use-package evil-surround
    :ensure t
    :config (global-evil-surround-mode))
  (use-package evil-indent-textobject
    :ensure t)
  ;;  (use-package evil-org TODO
  (use-package powerline-evil
    :ensure t
    :config
    (powerline-evil-vim-color-theme))


  (use-package projectile
    :ensure t
    :config
    (projectile-global-mode)
    :bind (:map projectile-mode-map
		("s-p" . projectile-command-map)
		("C-c p" . projectile-command-map)))

  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(package-selected-packages '(dracula-theme smex use-package)))
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
 '(ido-everywhere t)
 '(package-selected-packages
   '(lsp-ui company-box csharp-mode ido-completing-read+ ido-vertical-mode flx-ido helm-projectile yasnippet helm-lsp helm-xref dap-mode hydra flycheck avy which-key use-package smex projectile powerline-evil lsp-mode evil-surround evil-indent-textobject dracula-theme company)))
