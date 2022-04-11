;;; init.el -*- lexical-binding: t; -*-

;;;; General Emacs configuration ;;;;

;; Increase garbage collector threshold during startup
(setq gc-cons-threshold (* 4 1024 1024))


;; Improve user interface
(setq inhibit-startup-screen t
      initial-scratch-message nil
      visible-bell t)

(set-frame-font "Iosevka-13" nil t)


;; Split vertically by default
(setq split-height-threshold nil
      split-width-threshold 80)


;; Editing settings
(column-number-mode +1)
(global-display-line-numbers-mode +1)
; TODO: add this to a hook
(setq display-line-numbers 'relative)


;; Misc settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))



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

(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-interval 1)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  :config
  (auto-package-update-maybe))



;;; Emacs management and fixes

; Tune garbage collector
(use-package gcmh
  :ensure t
  :config (gcmh-mode +1))

; Keep ~/.emacs.d clean
(use-package no-littering
  :ensure t)

;; projectile


;;; Useful (evil) keybingings

(use-package evil
  :ensure t
  :init
  (setq evil-split-window-right t
        evil-vsplit-window-below t
        evil-want-C-u-scroll t
        evil-want-keybinding nil)
  :config
  (evil-mode +1))

(use-package evil-collection
  :ensure t
  :after evil
  :config (evil-collection-init))

;; evil-lion
;; evil-commentary
;; evil-exchange
;; evil-replace-with-register
;; evil-visualstar
;; evil-expat
;; evil-goggles
;; evil-surround
;; unimpaired
;; sneak
;; text-objects (targets, indent


;;; Editing helps

;; multicursor
;; autopairs
;; avy
;; snippets



;;; Language support (major-modes)

;; lsp
;; python
;; lua
;; haskell
;; ess



;;; Org-mode



;;; Git



;;; Fuzzy search & completion

;; vertico
;; tabnine?



;;; Terminal and file management support

;; eshell
;; vterm
;; repl
;; dired



;;; Windows, interface elements, visual editing helpers and themes

;; which-key
(use-package which-key
  :ensure t
  :config (which-key-mode +1))

;; Nord theme
(use-package nord-theme
  :ensure t
  :load-path "themes"
  :config (load-theme 'nord t))

;; modeline



;;;; Keybindings ;;;;

;; C-z to enable Emacs bindings

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

