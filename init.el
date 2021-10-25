(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

(setq inhibit-startup-screen t)

(global-display-line-numbers-mode)
(electric-pair-mode)
(tool-bar-mode -1)
(global-hl-line-mode t)

(add-to-list 'default-frame-alist '(font . "FantasqueSansMono Nerd Font Mono 14"))

(setq backup-directory-alist '(("." . "~/.emacs_saves"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(use-package doom-modeline
  :ensure t
  :config (doom-modeline-mode 1))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-laserwave t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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

(use-package rust-mode
  :ensure t
  :init
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook #'company-mode)
  (add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil))))

(use-package highlight-indent-guides
  :ensure t
  :init (add-hook 'prog-mode-hook #'highlight-indent-guides-mode))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook
  ((c++-mode . lsp)
   (c-mode . lsp)
   (python-mode . lsp)
   (csharp-mode . lsp)
   (rust-mode . lsp))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-idle-delay 0.5
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet nil  ;; Not supported by company capf, which is the recommended company backend
        lsp-pyls-plugins-flake8-enabled t))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-hover t
                lsp-ui-sideline-delay 0.5
                lsp-ui-doc-delay 5
                lsp-ui-sideline-ignore-duplicates t
                lsp-ui-doc-position 'bottom
                lsp-ui-doc-alignment 'frame
                lsp-ui-doc-header nil
                lsp-ui-doc-include-signature t
                lsp-ui-doc-use-childframe t)
  :bind (:map evil-normal-state-map
              ("gd" . lsp-ui-peek-find-definitions)
              ("gr" . lsp-ui-peek-find-references)
              :map md/leader-map
              ("Ni" . lsp-ui-imenu)))

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
(use-package magit
  :ensure t
  :bind (("C-x C-g" . magit-status)
	 ("C-x g" . magit-status)))

(use-package typescript-mode
  :mode "\\.ts\\"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)
  (use-package dap-node
    :ensure t
    :config (dap-node-setup)))

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

(use-package cmake-ide
  :ensure t
  :config
  (cmake-ide-setup))

(use-package company-box
  :ensure t
  :requires company
  :hook (company-mode . company-box-mode))

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (use-package evil-collection
    :ensure t
    :requires evil
    :config
    (evil-collection-init))
  (use-package evil-surround
    :ensure t
    :config (global-evil-surround-mode))
  (use-package evil-indent-textobject
    :ensure t))

(use-package projectile
    :ensure t
    :config
    (use-package projectile-ripgrep
      :requires projectile
      :ensure t)
    (projectile-global-mode)
    :bind (("C-S-P" . projectile-switch-project))
    :bind-keymap ("C-c p" . projectile-command-map))

  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
;; If there is e than one, they won't work right.
  '(package-selected-packages '(dracula-theme smex use-package))
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
   '(rust-mode doom-themes doom-modeline evil-org evil-collection projectile-ripgrep ripgrep exec-path-from-shell magit cmake-ide lsp-ui company-box csharp-mode ido-completing-read+ ido-vertical-mode flx-ido helm-projectile yasnippet helm-lsp helm-xref dap-mode hydra flycheck avy which-key use-package smex projectile powerline-evil lsp-mode evil-surround evil-indent-textobject dracula-theme company)))
