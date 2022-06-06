;; init.el -*- lexical-binding: t; -*-

;; Custom settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

; TODO: replace :init with :custom in use-package declarations


;;;; Package management ;;;;

(require 'package)
(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
                         ("org"    . "https://orgmode.org/elpa/")
                         ("elpa"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t
      use-package-compute-statistics t)

; TODO: (use-package try)



;;;; General Emacs configuration ;;;;

;; Improve user interface ;;

(use-package emacs
  :ensure nil
  :init
  (setq initial-scratch-message nil
        visible-bell t)
  :config
  (context-menu-mode +1))


;; Editing settings ;;

(use-package emacs
  :ensure nil
  :init
  (setq-default indent-tabs-mode nil)
  (setq display-line-numbers-type 'relative
        scroll-margin 2)
  :config
  (column-number-mode +1)
  (modify-syntax-entry ?_ "w")
  (dolist
      (mode-hook
       '(conf-mode-hook
         prog-mode-hook
         text-mode-hook))
    (progn
      (add-hook mode-hook 'display-line-numbers-mode)
      (add-hook mode-hook 'hl-line-mode))))


;; Misc settings ;;

(use-package emacs
  :ensure nil
  :init
  (setq delete-by-moving-to-trash t
        load-prefer-newer t
        use-short-answers t))


;; System dependent settings ;;

; Linux
(use-package emacs
  :ensure nil
  :if (eq system-type 'gnu/linux)
  :custom-face
  (default        ((t :family "Iosevka Term" :height 120)))
  (fixed-pitch    ((t :family "Iosevka Term" :height 120)))
  (variable-pitch ((t :family "Iosevka Aile" :height 120))))

; Windows
(use-package emacs
  :ensure nil
  :if (eq system-type 'windows-nt)
  :custom-face
  (default     ((t :family "Source Code Pro" :height 90)))
  (fixed-pitch ((t :family "Source Code Pro" :height 90)))
  :config
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))
 


;;;; Emacs management and fixes ;;;;

;; Early setup ;;

(use-package gcmh
  :config
  (gcmh-mode +1))

(use-package no-littering)

(use-package auto-compile
  :config
  (auto-compile-on-load-mode +1))

(use-package auto-package-update
  :defer 10
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  (auto-package-update-interval 5)
  (auto-package-update-prompt-before-update t)
  :config
  (auto-package-update-maybe))


;; Built-in packages (:ensure nil) ;;

(use-package apropos
  :ensure nil
  :init
  (setq apropos-do-all t))

(use-package autorevert
  :ensure nil
  :init
  (setq global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode +1))

(use-package dired
  :ensure nil
  :hook
  (dired-mode . hl-line-mode)
  :init
  (setq dired-auto-revert-buffer t))

(use-package ibuffer
  :ensure nil
  :commands ibuffer
  :bind
  (("C-x C-b" . ibuffer))
  :init
  (setq ibuffer-expert t)
  :config
  (add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
  (add-hook 'ibuffer-mode-hook #'hl-line-mode))

(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-saved-items 20)
  :config
  (recentf-mode +1))

(use-package savehist
  :ensure nil
  :config
  (savehist-mode +1))

(use-package saveplace
  :ensure nil
  :config
  (save-place-mode +1))

(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-separator "  ")
  (tab-bar-show 1)
  (tab-bar-tab-hints t)
  :custom-face
  (tab-bar-tab ((t :foreground nil :inherit 'link)))
  :config
  (tab-bar-mode +1))
; TODO: add ibuffer tab/project filters
; TODO: add easier bindings
; TODO; evaluate desktop-save-mode

(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'forward))


;; Community packages ;;

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
          "\\*Warnings\\*"
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

; TODO: disable popper-display-control and use shackle to create custom rules for popup placement
; TODO: add this to shackle
;; Override default buffer placement actions
(setq display-buffer-base-action
      '((display-buffer-reuse-window
         display-buffer-reuse-mode-window
         display-buffer-in-previous-window)))
; TODO: evaluate popwin

;; Default prefix: C-c C-w
(use-package tabspaces
  :defer 1
  :config
  (tabspaces-mode +1))

;; Default prefix: C-x w
(use-package winum
  :config
  (winum-mode +1))


;;;; Evil-mode ;;;;

; References:
; - https://evil.readthedocs.io
; - https://github.com/noctuid/evil-guide

(use-package evil
  :defer 1
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
  :custom
  ; TODO: evaluate this variable
  (evil-collection-want-find-usages-bindings nil)  ; Conflicts with 'gr' binding from evil-extra-operator
  :config
  (evil-collection-init))
; TODO: add evil-dired hydra/transient
; TODO: add evil-ibuffer hydra/transient

(use-package evil-anzu
  :after evil
  :config
  (global-anzu-mode +1))

(use-package evil-args
  :after evil
  :config
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

(use-package evil-commentary
  :after evil
  :bind
  (:map evil-normal-state-map
        ("gc" . evil-commentary)))

(use-package evil-extra-operator
  :after evil
  :bind
  (:map evil-motion-state-map
        ("gr" . evil-operator-eval)))

(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-use-diff-faces)
  (evil-goggles-mode +1))

(use-package evil-numbers
  :after evil)

(use-package evil-lion
  :after evil
  :bind
  (:map evil-normal-state-map
        ("gl" . evil-lion-left)
        ("gL" . evil-lion-right)
   :map evil-visual-state-map
        ("gl" . evil-lion-left)
        ("gL" . evil-lion-right)))

(use-package evil-quickscope
  :after evil
  :hook
  (conf-mode . turn-on-evil-quickscope-always-mode)
  (prog-mode . turn-on-evil-quickscope-always-mode)
  (text-mode . turn-on-evil-quickscope-always-mode))

(use-package evil-snipe
  :after evil
  :init
  (setq evil-snipe-scope 'whole-visible
        evil-snipe-repeat-scope 'whole-visible)
  :config
  ; Only using evil-snipe for f/F/t/T motions
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
  (avy-all-windows nil)
  (avy-keys '(?a ?s ?d ?h ?j ?k ?l))
  (avy-single-candidate-jump t)
  :init
  (defun my/goto-char-timer ()
    (interactive)
    ; Calling avy functions with an argument negates the current setting of 'avy-all-windows'
    (avy-goto-char-timer t))
  :bind
  (:map evil-normal-state-map
        ("s"      . 'evil-avy-goto-char-2)
        ("S"      . 'evil-avy-goto-char-2)
        ("gsl"    . 'evil-avy-goto-line)
        ("gsw"    . 'evil-avy-goto-word-1)
        ("gss"    . 'my/goto-char-timer)
        ("gs SPC" . 'my/goto-char-timer))) 

(use-package winner
  :after evil
  :init
  (setq winner-dont-bind-my-keys t)
  :config
  (winner-mode +1)
  (define-key evil-window-map "u" 'winner-undo)
  (define-key evil-window-map "U" 'winner-redo))
; TODO: evaluate tab-bar-history-mode

; evil-exchange (http://evgeni.io/posts/quick-start-evil-mode)
; evil-leader
; evil-mc/evil-mc-extras
; evil-multiedit
; evil-owl
; evil-replace-with-register
; evil-string-inflection
; evil-textobj-tree-sitter
; evil-vimish-fold
; text-objects (targets, indent, entire)


;;;; Editing helps ;;;;

(use-package company) 
; company-prescient

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package flymake
  :defer t
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))
; TODO: evaluate flycheck

; autopairs
; hl-todo
; multicursor
; smartparens/evil-smartparens
; snippets
; which-func
; whitespace-mode



;;;; Language support (major-modes) ;;;;

;; LSP
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-enable-snippet nil)
  (lsp-keymap-prefix "C-c l")
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

; References:
; - https://wikemacs.org/wiki/Python
; - https://www.emacswiki.org/emacs/PythonProgrammingInEmacs
; - https://github.com/doomemacs/doomemacs/blob/master/modules/lang/python
; - https://develop.spacemacs.org/layers/+lang/python/README.html
; - https://elpa.gnu.org/packages/python.html

(use-package python
  :ensure nil
  :mode
  ("\\.py\\'" . python-mode)
  :config
  (add-hook 'isend-mode-hook #'isend-default-ipython-setup))

(use-package anaconda-mode
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode))

(use-package pyvenv
  :hook (python-mode . pyvenv-mode))

(use-package lsp-pyright
  :after (python-mode lsp-mode)
  :init
  (advice-add 'lsp :before (lambda () (require 'lsp-pyright))))

; TODO: add poetry
; TODO: add format-all/black
; TODO: evaluate ein (emacs-ipython-notebook) and emacs-jupyter
; TODO: evaluate lpy
; TODO: evaluate python-x


;; Julia
(use-package julia-mode
  :mode "\\.jl\\'"
  :interpreter "julia")

;; Julia REPL usage: C-c C-z (raise REPL), C-c C-a (activate project),
;; C-c C-b (send buffer), C-c C-c (send region or line), C-c C-d (invoke @doc))
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
; latex


;;;; Org-mode ;;;;

; evil-org-mode



;;;; Git ;;;;

;; Magit
(use-package magit
  :commands magit-status
  :bind
  (("C-x g" . magit-status)))

; diff-hl
; git gutter



;;;; Fuzzy search & completion ;;;;

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



;;;; Terminal and file management support ;;;;

(use-package isend-mode
  :commands isend
  :config
  (defun my/send-region-ipython-paste (begin end)
    (interactive (list (region-beginning) (region-end)))
    (isend--check)
    (clipboard-kill-ring-save begin end)
    (isend--send-dest "%paste" isend--command-buffer)
    (deactivate-mark))

  (add-to-list 'evil-extra-operator-eval-modes-alist
               '(python-mode my/send-region-ipython-paste)))

(use-package vterm
  :unless (eq system-type 'windows-nt)
  :commands vterm)

; eshell
; dired/diredfl/ranger/dirvish
; eval-in-repl
; quickrun



;;;; Windows, interface elements, visual editing helpers and themes ;;;;

(use-package all-the-icons
  :if (display-graphic-p))

(use-package dashboard
  :custom
  (dashboard-center-content t)
  (dashboard-projects-backend 'project-el)
  (dashboard-startup-banner 'logo)
  (dashboard-items '((recents   . 10)
                     (projects  .  5)
                     (bookmarks .  5)
                     (registers .  5)))
  :init
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config
  (dashboard-setup-startup-hook))

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

