;;; init.el -*- lexical-binding: t; -*-

;;;; General Emacs configuration ;;;;

;; Increase garbage collector threshold during startup
(setq gc-cons-threshold (* 4 1024 1024))


;; Improve user interface
(setq inhibit-startup-screen t
      initial-scratch-message nil
      visible-bell t)
(if (eq system-type 'windows-nt)
    (progn (tool-bar-mode -1)
           (scroll-bar-mode -1)))
(setq frame-title-format '(multiple-frames "%b" ("" "%b - GNU Emacs")))

; TODO: add Iosevka Aile as non-fixed font
(if (eq system-type 'windows-nt)
    (set-frame-font "Iosevka Term-10" nil t)
    (set-frame-font "Iosevka Term-12" nil t))


;; Editing settings
(column-number-mode +1)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)


;; Misc settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)



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
(setq use-package-always-ensure t)

(use-package auto-package-update
  :defer 10
  :custom
  (auto-package-update-interval 5)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  :config
  (auto-package-update-maybe))

;(use-package try)



;;; Emacs management and fixes

; Tune garbage collector
(use-package gcmh
  :config (gcmh-mode +1))

; Keep ~/.emacs.d clean
(use-package no-littering)


;; Built-in packages

(use-package autorevert
  :init
  (setq global-auto-revert-non-file-buffers t)
  :config (global-auto-revert-mode +1))

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
  (recentf-mode +1)
  (global-set-key (kbd "C-x C-r") 'recentf-open-files))

(use-package savehist
  :config (savehist-mode +1))

(use-package saveplace
  :config (save-place-mode +1))

; TODO: winner-mode


;; Community packages

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") 'helpful-callable)
  (global-set-key (kbd "C-h v") 'helpful-variable)
  (global-set-key (kbd "C-h k") 'helpful-key)
  (global-set-key (kbd "C-h C") 'helpful-command))

(use-package minions
  :config (minions-mode +1))

(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


;;; Useful (evil) keybingings

(use-package evil
  :init
  (setq evil-search-module 'isearch
        evil-split-window-right t
        evil-vsplit-window-below t
        evil-undo-system 'undo-redo
        evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
        evil-want-integration t
        evil-want-keybinding nil)
  :config
  (evil-mode +1))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-surround
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

(use-package evil-commentary
  :bind (:map evil-normal-state-map
              ("gc" . evil-commentary)))

(use-package evil-snipe
  :after evil
  :init
  (setq evil-snipe-scope 'whole-visible
        evil-snipe-repeat-scope 'whole-visible)
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-visualstar
  :bind (:map evil-visual-state-map
              ("*" . evil-visualstar/begin-search-forward)
              ("#" . evil-visualstar/begin-search-backward)))

(use-package evil-lion
  :bind (:map evil-normal-state-map
              ("g l " . evil-lion-left)
              ("g L " . evil-lion-right)
         :map evil-visual-state-map
              ("g l " . evil-lion-left)
              ("g L " . evil-lion-right)))

(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-use-diff-faces)
  (evil-goggles-mode +1))

; evil-exchange (http://evgeni.io/posts/quick-start-evil-mode)
; evil-replace-with-register
; evil-ediff
; text-objects (targets, indent, entire)


;;; Editing helps

; multicursor
; autopairs
; avy
; snippets



;;; Language support (major-modes)

;; Emacs Lisp
(use-package parinfer-rust-mode
  :hook emacs-lisp-mode
  :init (setq parinfer-rust-auto-download t))


;; Julia
(use-package julia-mode
  :mode "\\.jl\\'"
  :interpreter "julia")

; Julia REPL usage: C-c C-z (raise REPL), C-c C-a (activate project),
;   C-c C-b (send buffer), C-c C-c (send region or line), C-c C-d (invoke @doc)
(use-package julia-repl
  :after julia-mode
  :hook (julia-mode . julia-repl-mode))

; lsp-mode
; lsp-ui
; tree sitter
; dap-mode
; python
; lua
; haskell
; ess



;;; Org-mode



;;; Git

;; Magit
(use-package magit
  :commands magit-status
  :bind (("C-x g" . magit-status)))

; git gutter



;;; Fuzzy search & completion

;; Vertico
(use-package vertico
  :custom (vertico-cycle t)
  :config (vertico-mode +1))

(use-package marginalia
  :config (marginalia-mode +1))

; tabnine?



;;; Terminal and file management support

; eshell
; vterm
; repl
; dired



;;; Windows, interface elements, visual editing helpers and themes

;; which-key
(use-package which-key
  :config (which-key-mode +1))

;; Nord theme
; TODO: remove ensure statement below
; TODO: load theme earlier to avoid blank screen at startup
(use-package nord-theme
  :ensure t
  :load-path "themes"
  :config (load-theme 'nord t))

; modeline
; hydra



;;;; Keybindings ;;;;

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

; TODO: add readline bindings in evil insert mode
