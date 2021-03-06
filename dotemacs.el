(setq gc-cons-threshold (* 100 1024 1024) ;; C++ part
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

(use-package which-key
  :ensure t
  :init
  (which-key-mode))
(setq display-buffer-alist '(("\\`\\*e?shell" display-buffer-pop-up-window)))

(use-package sublimity
  :ensure t
  :config
  (require 'sublimity-scroll)
  (sublimity-mode 1))

(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config (smex-initialize))

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

(use-package eyebrowse
:ensure t
  :bind
  ("M-0" . eyebrowse-last-window-config)
  ("M-1" . me/eyebrowse-switch-1)
  ("M-2" . me/eyebrowse-switch-2)
  ("M-3" . me/eyebrowse-switch-3)
  ("M-4" . me/eyebrowse-switch-4)
  ("M-5" . me/eyebrowse-switch-5)
  ("M-6" . me/eyebrowse-switch-6)
  ("M-7" . me/eyebrowse-switch-7)
  ("M-8" . me/eyebrowse-switch-8)
  ("M-9" . me/eyebrowse-switch-9)
  :hook
  (after-init . eyebrowse-mode)
  :custom
  (eyebrowse-mode-line-left-delimiter "")
  (eyebrowse-mode-line-right-delimiter "")
  (eyebrowse-new-workspace t))

(defun me/eyebrowse-switch (n)
  "Switch to configuration N or to the last visited."
  (if (eq (eyebrowse--get 'current-slot) n)
      (eyebrowse-last-window-config)
    (funcall (intern (format "eyebrowse-switch-to-window-config-%s" n)))))

(dotimes (n 9)
  (let* ((n (1+ n))
         (name (intern (format "me/eyebrowse-switch-%s" n)))
         (documentation (format "Switch to configuration %s or to the last visited." n)))
    (eval `(defun ,name ()
             ,documentation
             (interactive)
             (me/eyebrowse-switch ,n)))))

(use-package hydra
  :ensure t
  :bind
  ("C-c a" . hydra-applications/body)
  ("C-c d" . hydra-dates/body)
  ("C-c e" . hydra-eyebrowse/body)
  ("C-c g" . hydra-git/body)
  ("C-c o" . hydra-org/body)
  ("C-c p" . hydra-project/body)
  ("C-c u" . hydra-ui/body)
  :custom
  (hydra-default-hint nil))

(defvar-local me/hydra-super-body nil)

(defun me/hydra-heading (&rest headings)
  "Format HEADINGS to look pretty in a hydra docstring."
  (concat "\n "
	  (mapconcat (lambda (heading)
		       (propertize (format "%-18s" heading) 'face 'shadow))
		     headings
		     nil)))

(defun me/hydra-set-super ()
  (when-let* ((suffix "-mode")
	      (position (- (length suffix)))
	      (mode (symbol-name major-mode))
	      (name (if (string= suffix (substring mode position))
			(substring mode 0 position)
		      mode))
	      (body (intern (format "hydra-%s/body" name))))
    (when (functionp body)
      (setq me/hydra-super-body body))))

(defun me/hydra-super-maybe ()
  (interactive)
  (if me/hydra-super-body
      (funcall me/hydra-super-body)
    (user-error "me/hydra-super: me/hydra-super-body is not set")))

(defhydra hydra-applications (:color teal)
  (concat (me/hydra-heading "Applications" "Launch" "Shell") "
 _q_ quit            _i_ erc             _t_ vterm           ^^
 ^^                  ^^                  _T_ eshell          ^^
")
  ("q" nil)
  ("i" me/erc)
  ("t" vterm)
  ("T" (eshell t)))

(defhydra hydra-dates (:color teal)
  (concat (me/hydra-heading "Dates" "Insert" "Insert with Time") "
 _q_ quit            _d_ short           _D_ short           ^^
 ^^                  _i_ iso             _I_ iso             ^^
 ^^                  _l_ long            _L_ long            ^^
")
  ("q" nil)
  ("d" me/date-short)
  ("D" me/date-short-with-time)
  ("i" me/date-iso)
  ("I" me/date-iso-with-time)
  ("l" me/date-long)
  ("L" me/date-long-with-time))

(defun me/date-iso ()
  "Insert the current date, ISO format, eg. 2016-12-09."
  (interactive)
  (insert (format-time-string "%F")))

(defun me/date-iso-with-time ()
  "Insert the current date, ISO format with time, eg. 2016-12-09T14:34:54+0100."
  (interactive)
  (insert (format-time-string "%FT%T%z")))

(defun me/date-long ()
  "Insert the current date, long format, eg. December 09, 2016."
  (interactive)
  (insert (format-time-string "%B %d, %Y")))

(defun me/date-long-with-time ()
  "Insert the current date, long format, eg. December 09, 2016 - 14:34."
  (interactive)
  (insert (capitalize (format-time-string "%B %d, %Y - %H:%M"))))

(defun me/date-short ()
  "Insert the current date, short format, eg. 2016.12.09."
  (interactive)
  (insert (format-time-string "%Y.%m.%d")))

(defun me/date-short-with-time ()
  "Insert the current date, short format with time, eg. 2016.12.09 14:34"
  (interactive)
  (insert (format-time-string "%Y.%m.%d %H:%M")))

(defhydra hydra-eyebrowse (:color teal)
  (concat (me/hydra-heading "Eyebrowse" "Do" "Switch") "
 _q_ quit            _c_ create          _1_-_9_ %s(eyebrowse-mode-line-indicator)
 ^^                  _k_ kill            _<_ previous        ^^
 ^^                  _r_ rename          _>_ next            ^^
 ^^                  ^^                  _e_ last            ^^
 ^^                  ^^                  _s_ switch          ^^
")
  ("q" nil)
  ("1" me/eyebrowse-switch-1)
  ("2" me/eyebrowse-switch-2)
  ("3" me/eyebrowse-switch-3)
  ("4" me/eyebrowse-switch-4)
  ("5" me/eyebrowse-switch-5)
  ("6" me/eyebrowse-switch-6)
  ("7" me/eyebrowse-switch-7)
  ("8" me/eyebrowse-switch-8)
  ("9" me/eyebrowse-switch-9)
  ("<" eyebrowse-prev-window-config :color red)
  (">" eyebrowse-next-window-config :color red)
  ("c" eyebrowse-create-window-config)
  ("e" eyebrowse-last-window-config)
  ("k" eyebrowse-close-window-config :color red)
  ("r" eyebrowse-rename-window-config)
  ("s" eyebrowse-switch-to-window-config))

(defhydra hydra-git (:color teal)
  (concat (me/hydra-heading "Git" "Do" "Gutter") "
 _q_ quit            _b_ blame           _p_ previous        ^^
 _m_ smerge...       _c_ clone           _n_ next            ^^
 ^^                  _g_ status          _r_ revert          ^^
 ^^                  _i_ init            _s_ stage           ^^
")
  ("q" nil)
  ("b" magit-blame)
  ("c" magit-clone)
  ("g" magit-status)
  ("i" magit-init)
  ("m" (progn (require 'smerge-mode) (hydra-git/smerge/body)))
  ("n" git-gutter:next-hunk :color red)
  ("p" git-gutter:previous-hunk :color red)
  ("r" git-gutter:revert-hunk)
  ("s" git-gutter:stage-hunk :color red))

(defhydra hydra-git/smerge
  (:color pink :pre (if (not smerge-mode) (smerge-mode 1)) :post (smerge-auto-leave))
  (concat (me/hydra-heading "Git / SMerge" "Move" "Keep" "Diff") "
 _q_ quit            _g_ first           _RET_ current       _<_ upper / base
 ^^                  _G_ last            _a_ all             _=_ upper / lower
 ^^                  _j_ next            _b_ base            _>_ base / lower
 ^^                  _k_ previous        _l_ lower           _E_ ediff
 ^^                  ^^                  _u_ upper           _H_ highlight
")
  ("q" nil :color blue)
  ("j" smerge-next)
  ("k" smerge-prev)
  ("<" smerge-diff-base-upper :color blue)
  ("=" smerge-diff-upper-lower :color blue)
  (">" smerge-diff-base-lower :color blue)
  ("RET" smerge-keep-current)
  ("a" smerge-keep-all)
  ("b" smerge-keep-base)
  ("E" smerge-ediff :color blue)
  ("g" (progn (goto-char (point-min)) (smerge-next)))
  ("G" (progn (goto-char (point-max)) (smerge-prev)))
  ("H" smerge-refine)
  ("l" smerge-keep-lower)
  ("u" smerge-keep-upper))

(defhydra hydra-markdown (:color pink)
  (concat (me/hydra-heading "Markdown" "Table Columns" "Table Rows") "
 _q_ quit            _c_ insert          _r_ insert          ^^
 ^^                  _C_ delete          _R_ delete          ^^
 ^^                  _M-<left>_ left     _M-<down>_ down     ^^
 ^^                  _M-<right>_ right   _M-<up>_ up         ^^
")
  ("q" nil)
  ("c" markdown-table-insert-column)
  ("C" markdown-table-delete-column)
  ("r" markdown-table-insert-row)
  ("R" markdown-table-delete-row)
  ("M-<left>" markdown-table-move-column-left)
  ("M-<right>" markdown-table-move-column-right)
  ("M-<down>" markdown-table-move-row-down)
  ("M-<up>" markdown-table-move-row-up))

(defhydra hydra-org (:color pink)
  (concat (me/hydra-heading "Org" "Links" "Outline") "
 _q_ quit            _i_ insert          _<_ previous        ^^
 ^^                  _n_ next            _>_ next            ^^
 ^^                  _p_ previous        _a_ all             ^^
 ^^                  _s_ store           _v_ overview        ^^
")
  ("q" nil)
  ("<" org-backward-element)
  (">" org-forward-element)
  ("a" outline-show-all :color blue)
  ("i" org-insert-link :color blue)
  ("n" org-next-link)
  ("p" org-previous-link)
  ("s" org-store-link)
  ("v" org-overview :color blue))

(defhydra hydra-project (:color teal)
  (concat (me/hydra-heading "Project" "Do" "Find" "Search") "
 _q_ quit            _K_ kill buffers    _b_ buffer          _r_ replace
 ^^                  ^^                  _d_ directory       _s_ ripgrep
 ^^                  ^^                  _D_ root            ^^
 ^^                  ^^                  _f_ file            ^^
 ^^                  ^^                  _p_ project         ^^
")
  ("q" nil)
  ("b" projectile-switch-to-buffer)
  ("d" projectile-find-dir)
  ("D" projectile-dired)
  ("f" projectile-find-file)
  ("K" projectile-kill-buffers)
  ("p" projectile-switch-project)
  ("r" projectile-replace-regexp)
  ("s" projectile-ripgrep)
  )

(defhydra hydra-ui (:color pink)
  (concat (me/hydra-heading "Theme" "Windows" "Zoom" "Line Numbers") "
 _t_ cycle           _b_ balance         _-_ out             _n_ mode: %s`display-line-numbers
 _T_ cycle (noexit)  _m_ maximize frame  _=_ in              _N_ absolute: %s`display-line-numbers-current-absolute
 ^^                  ^^                  _0_ reset           ^^
 ^^                  ^^                  ^^                  ^^
 ^^                  ^^                  ^^                  ^^
")
  ("q" nil)
  ("-" default-text-scale-decrease)
  ("=" default-text-scale-increase)
  ("0" default-text-scale-reset :color blue)
  ("b" balance-windows :color blue)
  ("m" toggle-frame-maximized)
  ("n" me/display-line-numbers-toggle-type)
  ("N" me/display-line-numbers-toggle-absolute)
  ("t" me/theme-cycle :color blue)
  ("T" me/theme-cycle))

(defun me/display-line-numbers-toggle-absolute ()
  "Toggle the value of `display-line-numbers-current-absolute'."
  (interactive)
  (let ((value display-line-numbers-current-absolute))
    (setq-local display-line-numbers-current-absolute (not value))))

(defun me/display-line-numbers-toggle-type ()
  "Cycle through the possible values of `display-line-numbers'.
Cycle between nil, t and 'relative."
  (interactive)
  (let* ((range '(nil t relative))
         (position (1+ (cl-position display-line-numbers range)))
         (position (if (= position (length range)) 0 position)))
    (setq-local display-line-numbers (nth position range))))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package doom-modeline
  :ensure t
  :config (doom-modeline-mode 1))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-challenger-deep t)

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

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-indent-textobject
  :ensure t
  :requires evil
  :after evil)

(use-package evil-collection
  :ensure t
  :requires evil
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :requires evil
  :config (global-evil-surround-mode))

(use-package pinentry
	    :ensure t
	    :init (setq epa-pinentry-mode 'loopback)
	    :config
	    (pinentry-start)
     (shell-command "gpgconf --launch-agent")
(shell-command "gpg-connect-agent /bye"))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package git-gutter-fringe
:ensure t
  :preface
  (defun me/git-gutter-enable ()
    (when-let* ((buffer (buffer-file-name))
                (backend (vc-backend buffer)))
      (require 'git-gutter)
      (require 'git-gutter-fringe)
      (git-gutter-mode 1)))
  :hook
  (after-change-major-mode . me/git-gutter-enable)
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [255] nil nil '(center t))
  (define-fringe-bitmap 'git-gutter-fr:deleted [255 255 255 255] nil nil 'bottom)
  (define-fringe-bitmap 'git-gutter-fr:modified [255] nil nil '(center t)))

(use-package projectile
  :ensure t
  :config
  (use-package projectile-ripgrep
    :requires projectile
    :ensure t)
  (projectile-global-mode)
  :bind (("C-S-P" . projectile-switch-project))
  :bind-keymap ("C-x p" . projectile-command-map))

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

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init (yas-global-mode t))
(use-package yasnippet-snippets
  :requires yasnippet
  :ensure t)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook
  ((c++-mode . lsp)
   (c-mode . lsp)
   (python-mode . lsp)
   (csharp-mode . lsp)
   (rust-mode . lsp)
   (typescript-mode . lsp)
   (dart-mode . lsp))
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

(use-package dap-mode
  :ensure t)

(use-package highlight-defined
:ensure t
:init
(add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode))

(use-package cmake-ide
  :ensure t
  :config
  (cmake-ide-setup))

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

(use-package lsp-dart
:ensure t
:requires lsp-mode)

(use-package dart-mode
:ensure t)

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
			 (require 'lsp-python-ms)
			 (lsp))))  ; or lsp-deferred

(use-package company-web :ensure t)

(use-package ac-html-bootstrap :ensure t)

(use-package web-mode
  :diminish t
  :ensure t
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.jsp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.jst.ejs\\'" . web-mode)
   ("\\.jst.ejs\\'" . web-mode)
   ("\\.html?\\'" . web-mode))
  :init
  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-comment-keywords t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-script-padding 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-comment-style 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (add-hook 'web-mode-hook #'company-mode))

(use-package typescript-mode
  :ensure t
  :init
  (add-hook 'typescript-mode-hook #'company-mode))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package emmet-mode
:ensure t
  :bind
  (:map emmet-mode-keymap
	("C-<return>" . nil))
  :hook
  (css-mode . emmet-mode)
  (html-mode . emmet-mode)
  (rjsx-mode . emmet-mode)
  (typescript-tsx-mode . emmet-mode)
  (web-mode . emmet-mode)
  :custom
  (emmet-insert-flash-time .1)
  (emmet-move-cursor-between-quote t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package typescript-mode		    ;;
;;   :mode "\\.ts\\"			    ;;
;;   :hook (typescript-mode . lsp-deferred) ;;
;;   :config				    ;;
;;   (setq typescript-indent-level 2)	    ;;
;;   (use-package dap-node		    ;;
;;     :ensure t			    ;;
;;     :config (dap-node-setup)))	    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun me/org-cycle-parent (argument)
  "Go to the nearest parent heading and execute `org-cycle'."
  (interactive "p")
  (if (org-at-heading-p)
      (outline-up-heading argument)
    (org-previous-visible-heading argument))
  (org-cycle))

(defun me/org-show-next-heading-tidily ()
  "Show next entry, keeping other entries closed."
  (interactive)
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (outline-show-children))
    (outline-next-heading)
    (unless (and (bolp) (org-at-heading-p))
      (org-up-heading-safe)
      (outline-hide-subtree)
      (user-error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (outline-show-children)))

(defun me/org-show-previous-heading-tidily ()
  "Show previous entry, keeping other entries closed."
  (interactive)
  (let ((pos (point)))
    (outline-previous-heading)
    (unless (and (< (point) pos) (bolp) (org-at-heading-p))
      (goto-char pos)
      (outline-hide-subtree)
      (user-error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (outline-show-children)))

(defun me/org-src-buffer-name (name &rest _)
  "Simple buffer name."
  (format "*%s*" name))

(use-package org
  :ensure nil
  :bind
  (:map org-mode-map
	("C-<return>" . nil)
	("C-<tab>" . me/org-cycle-parent))
  :hook
  (org-mode . me/hydra-set-super)
  :custom
  (org-adapt-indentation nil)
  (org-confirm-babel-evaluate nil)
  (org-cycle-separator-lines 0)
  (org-descriptive-links nil)
  (org-edit-src-content-indentation 0)
  (org-edit-src-persistent-message nil)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-return-follows-link t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'current-window)
  (org-startup-truncated nil)
  (org-support-shift-select 'always)
  :config
  (require 'ob-shell)
  (add-to-list 'org-babel-load-languages '(shell . t))
  (modify-syntax-entry ?' "'" org-mode-syntax-table)
  (advice-add 'org-src--construct-edit-buffer-name :override #'me/org-src-buffer-name)
  (with-eval-after-load 'evil
    (evil-define-key* 'motion org-mode-map
      (kbd "C-j") #'me/org-show-next-heading-tidily
      (kbd "C-k") #'me/org-show-previous-heading-tidily)))

(use-package org-d20
  :ensure t
  :requires org)
