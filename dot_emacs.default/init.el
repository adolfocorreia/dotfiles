;;; init.el -*- lexical-binding: t; -*-

;;;; General Emacs configuration ;;;;

;; Increase garbage collector threshold during startup
(setq gc-cons-threshold (* 4 1024 1024))


;; Improve user interface
(setq inhibit-startup-screen t
      initial-scratch-message nil
      visible-bell t)
(if (eq system-type 'windows-nt)
    (progn
      (menu-bar-mode -1)
      (scroll-bar-mode -1)
      (tool-bar-mode -1)))
(setq frame-title-format '(multiple-frames "%b" ("" "%b - GNU Emacs")))

(let ((fs (if (eq system-type 'windows-nt) "9" "12")))
  (set-face-font 'default
     (concat "Iosevka Term-" fs))
  (set-face-font 'fixed-pitch
     (concat "Iosevka Term-" fs))
  (set-face-font 'variable-pitch
     (concat "Iosevka Aile-" fs)))


;; Override default buffer placement actions
(setq display-buffer-base-action
      '((display-buffer-reuse-window
         display-buffer-reuse-mode-window
         display-buffer-in-previous-window)))


;; Editing settings
(column-number-mode +1)
(setq display-line-numbers-type 'relative
      scroll-margin 2)
(setq-default indent-tabs-mode nil)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(modify-syntax-entry ?_ "w")


;; Misc settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

(context-menu-mode +1)

(setq apropos-do-all t
      delete-by-moving-to-trash t
      load-prefer-newer t
      uniquify-buffer-name-style 'forward
      use-short-answers t)
(setq dired-auto-revert-buffer t
      dired-kill-when-opening-new-dired-buffer t)



;;;; Package management ;;;;

;; Initialize package manager
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t
      native-comp-async-report-warnings-errors nil)

; TODO: (use-package try)



;;; Emacs management and fixes

; Tune garbage collector
(use-package gcmh
  :config
  (gcmh-mode +1))

; Keep ~/.emacs.d clean
(use-package no-littering)

; Asynchronous tasks
(use-package async
  :config
  (async-bytecomp-package-mode +1))

; Auto compile
(use-package auto-compile
  :config
  (auto-compile-on-load-mode +1))

; Auto update packages
(use-package auto-package-update
  :defer 10
  :custom
  (auto-package-update-interval 5)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))


;; Built-in packages

(use-package autorevert
  :init
  (setq global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode +1))

(use-package ibuffer
  :init
  (setq ibuffer-expert t)
  :config
  (add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode +1)))
  (global-set-key (kbd "C-x C-b") 'ibuffer))

(use-package recentf
  :init
  (setq recentf-max-saved-items 20)
  :config
  (recentf-mode +1))

(use-package savehist
  :config
  (savehist-mode +1))

(use-package saveplace
  :config
  (save-place-mode +1))

(use-package tab-bar
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-separator "  ")
  (tab-bar-show 1)
  (tab-bar-tab-hints t)
  :custom-face
  (tab-bar-tab ((t (:foreground nil :inherit 'link))))
  :config
  (tab-bar-mode +1))


;; Community packages

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key]      . helpful-key))

(use-package minions
  :config
  (minions-mode +1))

(use-package popper
  :init
  (setq popper-group-function #'popper-group-by-directory)
  (setq popper-mode-line (propertize " POP " 'face 'mode-line-emphasis))
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*"
          help-mode
          helpful-mode
          apropos-mode
          compilation-mode))
  :config
  (popper-mode +1)
  (popper-echo-mode +1)
  (global-set-key (kbd "M-`") 'popper-cycle)
  (global-set-key (kbd "C-`") 'popper-toggle-type))
; TODO: disable popper-display-control use shackle to create custom rules for popup placement
; TODO: evaluate popwin

; Default prefix: C-c C-w
(use-package tabspaces
  :config
  (tabspaces-mode +1))

; Default prefix: C-x w
(use-package winum
  :config
  (winum-mode +1))


;;; Evil-mode

; References:
; - https://evil.readthedocs.io
; - https://github.com/noctuid/evil-guide

(use-package evil
  :init
  (setq evil-search-module 'isearch
        evil-split-window-right t
        evil-vsplit-window-below t
        evil-undo-system 'undo-redo
        evil-want-C-u-scroll t
        evil-want-C-w-in-emacs-state t
        evil-want-Y-yank-to-eol t
        evil-want-fine-undo t
        evil-want-integration t
        evil-want-keybinding nil)
  :config
  (evil-mode +1)

  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ; Use readline-like bindings in insert state
  (define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-insert-state-map (kbd "M-n") 'next-line)
  (define-key evil-insert-state-map (kbd "M-p") 'previous-line)
  (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
  (define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)
  (define-key evil-insert-state-map (kbd "C-y") 'yank)

  ; C-w extra bindings
  (define-key evil-window-map "`" 'evil-switch-to-windows-last-buffer)
  (define-key evil-window-map "1" 'winum-select-window-1)
  (define-key evil-window-map "2" 'winum-select-window-2)
  (define-key evil-window-map "3" 'winum-select-window-3)
  (define-key evil-window-map "4" 'winum-select-window-4)
  (define-key evil-window-map "5" 'winum-select-window-5)

  ; Center screen after n/N
  (defun my/center-line (&rest _)
    (evil-scroll-line-to-center nil))
  (advice-add 'evil-search-next     :after #'my/center-line)
  (advice-add 'evil-search-previous :after #'my/center-line)

  ; Select new window after evil split)
  (defun my/other-window (&rest _)
    (other-window 1))
  (advice-add 'evil-window-split  :after #'my/other-window)
  (advice-add 'evil-window-vsplit :after #'my/other-window))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :bind
  (:map evil-normal-state-map
        ("gc" . evil-commentary)))

(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-use-diff-faces)
  (evil-goggles-mode +1))

(use-package evil-numbers
  :after evil
  :config
  (evil-define-key '(normal visual) 'global (kbd "C-c C-a") 'evil-numbers/inc-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "C-c C-x") 'evil-numbers/dec-at-pt))

(use-package evil-lion
  :after evil
  :bind
  (:map evil-normal-state-map
        ("g l " . evil-lion-left)
        ("g L " . evil-lion-right)
   :map evil-visual-state-map
        ("g l " . evil-lion-left)
        ("g L " . evil-lion-right)))

(use-package evil-snipe
  :after evil
  :init
  (setq evil-snipe-scope 'whole-visible
        evil-snipe-repeat-scope 'whole-visible)
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-surround
  :after evil
  :commands
  (evil-surround-edit
   evil-Surround-edit
   evil-surround-region
   evil-Surround-region)
  :init
  (evil-define-key 'operator global-map "s"  'evil-surround-edit)
  (evil-define-key 'operator global-map "S"  'evil-Surround-edit)
  (evil-define-key 'visual global-map   "S"  'evil-surround-region)
  (evil-define-key 'visual global-map   "gS" 'evil-Surround-region))

(use-package evil-visualstar
  :after evil
  :bind
  (:map evil-visual-state-map
        ("*" . evil-visualstar/begin-search-forward)
        ("#" . evil-visualstar/begin-search-backward)))

(use-package ace-window
  :after evil
  :config
  (define-key evil-window-map "a" 'ace-window))

(use-package avy
  :after evil
  :custom
  (avy-single-candidate-jump nil)
  :bind
  (:map evil-normal-state-map
        ("g s " . 'evil-avy-goto-char-timer)))

(use-package winner
  :after evil
  :init
  (setq winner-dont-bind-my-keys t)
  :config
  (winner-mode +1)
  (define-key evil-window-map "u" 'winner-undo)
  (define-key evil-window-map "U" 'winner-redo))

; evil-exchange (http://evgeni.io/posts/quick-start-evil-mode)
; evil-replace-with-register
; evil-ediff
; evil-vimish-fold
; evil-multiedit
; text-objects (targets, indent, entire)
; evil-textobj-tree-sitter
; evil-owl


;;; Editing helps

; multicursor
; autopairs
; snippets



;;; Language support (major-modes)

;; LSP
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)


;; Emacs Lisp
(use-package parinfer-rust-mode
  :hook emacs-lisp-mode
  :init
  (setq parinfer-rust-auto-download t))


;; Python
; TODO: use hook to auto activate venv on project switch (auto-virtualenv?)
(use-package pyvenv
  :hook (python-mode . pyvenv-mode))
; TODO: add poetry
; TODO: use ipython as REPL



;; Julia
(use-package julia-mode
  :mode "\\.jl\\'"
  :interpreter "julia")

; Julia REPL usage: C-c C-z (raise REPL), C-c C-a (activate project),
;   C-c C-b (send buffer), C-c C-c (send region or line), C-c C-d (invoke @doc)
(use-package julia-repl
  :after julia-mode
  :hook (julia-mode . julia-repl-mode))


;; ESS
(use-package ess
  :mode
  (("\\.[rR]\\'" . R-mode)
   ("\\.[rR]nw\\'" . Rnw-mode))
  :init
  (setq ess-use-flymake nil
        ess-use-toolbar nil)
  :config
  (require 'ess-site))

(use-package poly-R
  :mode
  ("\\.[rR]md\\'" . poly-markdown+R-mode))

; tree sitter
; dap-mode
; python
; lua
; haskell



;;; Org-mode



;;; Git

;; Magit
(use-package magit
  :commands magit-status
  :bind
  (("C-x g" . magit-status)))

; git gutter



;;; Fuzzy search & completion

;; Vertico
(use-package vertico
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode +1))

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode +1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

(use-package consult
  :bind
  (("C-x b"    . consult-buffer)
   ("C-x p b"  . consult-project-buffer)
   ; TODO: avoid loading major-modes when previewing recent files
   ("C-x C-r"  . consult-recent-file)
   ("<help> a" . consult-apropos))
  :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package embark
  :bind
  (("C-;"   . embark-act)
   ("C-x B" . embark-bindings)))

(use-package embark-consult
  :after (embark consult))

; prescient?
; company?
; tabnine?



;;; Terminal and file management support

; eshell
; vterm
; repl
; dired/ranger
; quickrun



;;; Windows, interface elements, visual editing helpers and themes

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :custom
  (doom-modeline-height 30)
  (doom-modeline-minor-modes t)
  :config
  (doom-modeline-mode +1))

(use-package doom-themes
  :config
  (load-theme 'doom-tokyo-night t))

(use-package which-key
  :config
  (which-key-mode +1))

; hydra



;;;; Keybindings ;;;;

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Kill current buffer
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

