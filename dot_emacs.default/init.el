;;; init.el -*- lexical-binding: t; -*-

;; Check the system used
(defconst ON-LINUX   (eq system-type 'gnu/linux))
(defconst ON-MAC     (eq system-type 'darwin))
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;; Custom settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

; TODO: evaluate (setq debug-on-error t)
; TODO: replace :init with :custom in use-package declarations
; TODO: open emacs as server



;;;; Package management ;;;;

;; package and use-package ;;

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


;; Early setup packages ;;

(use-package auto-compile
  :config
  (auto-compile-on-load-mode +1))

(use-package no-littering)



;;;; General Emacs configuration ;;;;

;; Improve user interface ;;

(use-package emacs
  :ensure nil
  :custom
  (initial-scratch-message nil)
  (visible-bell t)
  :init
  (setq-default cursor-in-non-selected-windows nil)
  :config
  (context-menu-mode +1))


;; Editing settings ;;

(use-package emacs
  :ensure nil
  :init
  (setq-default indent-tabs-mode nil
                truncate-lines t)
  :custom
  (completion-cycle-threshold 3)
  (display-line-numbers-type 'relative)
  (scroll-margin 2)
  (tab-always-indent 'complete)
  :config
  (column-number-mode +1)
  (defun my/enable-show-trailing-whitespace ()
    (setq show-trailing-whitespace t))
  (dolist
      (mode-hook
       '(conf-mode-hook
         prog-mode-hook
         text-mode-hook))
    (progn
      (add-hook mode-hook #'display-line-numbers-mode)
      (add-hook mode-hook #'hl-line-mode)
      (add-hook mode-hook #'my/enable-show-trailing-whitespace))))


;; Misc settings ;;

(use-package emacs
  :ensure nil
  :custom
  (delete-by-moving-to-trash t)
  (native-comp-deferred-compilation t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (use-short-answers t))


;; System dependent settings ;;

; Linux
(use-package emacs
  :ensure nil
  :if ON-LINUX
  :custom-face
  (default ((t :family "Hack" :height 105)))
  :config
  (menu-bar-mode +1)
  (scroll-bar-mode +1)
  (tool-bar-mode +1))

; Windows
(use-package emacs
  :ensure nil
  :if ON-WINDOWS
  :custom-face
  (default ((t :family "Hack" :height 90)))
  :config
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

; Terminal
(use-package emacs
  :unless (display-graphic-p)
  :config
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))


;; Window placement rules ;;
; References:
; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Displaying-Buffers.html
; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Side-Windows.html

(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
    '(;; no window
      ("\\`\\*Async Shell Command\\*\\'"
       (display-buffer-no-window))
      ;; top side window
      ("\\*\\(Messages\\|package update results\\)\\*"
       (display-buffer-in-side-window) (side . top) (slot . -1) (window-height . 0.2))
      ("\\*\\(Warnings\\|Compile-Log\\)\\*"
       (display-buffer-in-side-window) (side . top) (slot . +1) (window-height . 0.2))
      ;; right side window
      ("\\*\\(Help\\|helpful\\|Apropos\\|info\\)"
       (display-buffer-in-side-window) (side . right) (slot . 0) (window-width . 0.3))))
  (display-buffer-base-action
   '((display-buffer-reuse-window
      display-buffer-reuse-mode-window
      display-buffer-in-previous-window))))

; TODO: evaluate removing popper (e.g. window-toggle-side-windows)
(use-package popper
  :defer 1
  :custom
  (popper-display-control nil)
  (popper-group-function #'popper-group-by-directory)
  (popper-mode-line (propertize " POP " 'face 'mode-line-emphasis))
  (popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "Output\\*"
          "\\*package update results\\*"
          help-mode
          helpful-mode
          apropos-mode
          compilation-mode))
  :config
  (popper-mode +1)
  (popper-echo-mode +1)
  (bind-key "C-`"   #'popper-cycle)
  (bind-key "M-`"   #'popper-toggle-latest)
  (bind-key "C-M-`" #'popper-toggle-type))

; TODO: evaluate golden-ration


;;;; Emacs management and fixes ;;;;

;; Built-in packages (:ensure nil) ;;

(use-package apropos
  :ensure nil
  :defer t
  :custom
  (apropos-do-all t))

(use-package autorevert
  :ensure nil
  :defer t
  :custom
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode +1))

(use-package comint
  :ensure nil
  :defer t
  :custom
  (comint-scroll-to-bottom-on-input t))

(use-package dabbrev
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"
                                    "\\`\\*"
                                    "\\*\\'"))
  :bind
  ("M-/" . dabbrev-completion)
  ("C-M-/" . dabbrev-expand))

(use-package dired
  :ensure nil
  :hook
  (dired-mode . display-line-numbers-mode)
  (dired-mode . hl-line-mode)
  :custom
  (dired-auto-revert-buffer t))

(use-package eshell
  :ensure nil
  :defer t
  :custom
  (eshell-scroll-to-bottom-on-input t))

(use-package eww
  :ensure nil
  :defer t
  :custom
  (browse-url-browser-function 'eww-browse-url))

(use-package ibuffer
  :ensure nil
  :commands ibuffer
  :bind
  ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-expert t)
  :config
  (add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
  (add-hook 'ibuffer-mode-hook #'hl-line-mode))

(use-package recentf
  :ensure nil
  :defer 1
  :custom
  (recentf-max-saved-items 20)
  :config
  (recentf-mode +1))

(use-package savehist
  :ensure nil
  :defer 1
  :config
  (savehist-mode +1))

(use-package saveplace
  :ensure nil
  :defer 1
  :config
  (save-place-mode +1))

(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-format '(tab-bar-format-align-right tab-bar-format-tabs-groups tab-bar-separator))
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-choice "*dashboard*")
  (tab-bar-separator "   ")
  (tab-bar-show +1)
  (tab-bar-tab-hints t)
  :config
  (tab-bar-mode +1)
  (tab-bar-rename-tab "main" 1))
; TODO: add ibuffer tab/project filters
; TODO: add easier bindings (e.g. general)
; TODO; evaluate desktop-save-mode

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward))


;; Community packages ;;

(use-package auto-package-update
  :defer 5
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results nil)
  (auto-package-update-interval 5)
  (auto-package-update-prompt-before-update t)
  :config
  (auto-package-update-maybe))

; TODO: make evil bindings work with bufler
(use-package bufler
  :commands bufler)

(use-package gcmh
  :defer 2
  :config
  (gcmh-mode +1))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key]      . helpful-key)
  :config
  (add-hook 'helpful-mode-hook #'display-line-numbers-mode))

(use-package minions
  :config
  (minions-mode +1))

(use-package page-break-lines
  :defer 1
  :config
  (global-page-break-lines-mode +1))

(use-package project-tab-groups
  :config
  (project-tab-groups-mode +1))

(use-package tab-bar-echo-area
  :config
  (tab-bar-echo-area-mode +1))

; TODO: add C-w key bindings
(use-package transpose-frame
  :commands (transpose-frame flip-frame flop-frame rotate-frame))

(use-package try
  :commands try)

(use-package unkillable-scratch
  :defer 1
  :config
  (unkillable-scratch t))

(use-package vlf
  :commands vlf
  :config
  (require 'vlf-setup))

;; Default prefix: C-x w
(use-package winum
  :defer 1
  :config
  (winum-mode +1))



;;;; Evil-mode ;;;;

; References:
; - https://evil.readthedocs.io
; - https://github.com/noctuid/evil-guide

(use-package evil
  :custom
  (evil-respect-visual-line-mode t)
  (evil-search-module 'isearch)
  (evil-split-window-right t) (evil-vsplit-window-below t)
  (evil-symbol-word-search t)
  (evil-undo-system 'undo-redo)
  (evil-want-C-u-scroll t)
  (evil-want-C-w-in-emacs-state t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-fine-undo t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  ;; evil-want-o/O-to-continue-comments t ; TODO: check this in Doom Emacs
  :config
  (modify-syntax-entry ?_ "w")

  (evil-mode +1)

  (evil-define-key nil evil-insert-state-map (kbd "C-g") #'evil-normal-state)

  ; Use readline-like bindings in insert state
  (evil-define-key nil evil-insert-state-map
    (kbd "C-a") #'move-beginning-of-line
    (kbd "C-e") #'move-end-of-line
    (kbd "M-n") #'next-line
    (kbd "M-p") #'previous-line
    (kbd "C-d") #'delete-char
    (kbd "C-h") #'delete-backward-char
    (kbd "C-y") #'yank)

  ; C-w extra bindings
  (evil-define-key nil evil-window-map
    "`" #'evil-switch-to-windows-last-buffer
    "1" #'winum-select-window-1
    "2" #'winum-select-window-2
    "3" #'winum-select-window-3
    "4" #'winum-select-window-4
    "5" #'winum-select-window-5)

  ; Open Dired buffer
  (evil-define-key nil evil-normal-state-map "-" #'dired-jump)

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
  (evil-collection-init)

  ; comint mode
  (evil-collection-define-key 'insert 'comint-mode-map
      (kbd "C-p") #'comint-previous-input
      (kbd "C-n") #'comint-next-input)

  ; dashboard mode
  (evil-collection-define-key 'normal 'dashboard-mode-map
      (kbd "C-p") #'dashboard-previous-line
      (kbd "C-n") #'dashboard-next-line))

; TODO: add evil-dired transient
; TODO: add evil-ibuffer transient
; TODO: 'gt' binding for magit conflicts with evil binding for tabs

(use-package evil-anzu
  :after evil
  :config
  (global-anzu-mode +1))

(use-package evil-args
  :after evil
  :config
  (evil-define-key nil evil-inner-text-objects-map "a" #'evil-inner-arg)
  (evil-define-key nil evil-outer-text-objects-map "a" #'evil-outer-arg))

; TODO: evaluate evil-nerd-commenter
(use-package evil-commentary
  :after evil
  :config
  (evil-define-key nil evil-normal-state-map "gc" #'evil-commentary))

(use-package evil-extra-operator
  :after evil
  :config
  (evil-define-key nil evil-motion-state-map "gr" #'evil-operator-eval))

(use-package evil-goggles
  :after evil
  :defer 1
  :config
  ; Also pulse on evil-operator-eval
  (add-to-list 'evil-goggles--commands
               '(evil-operator-eval :face evil-goggles-default-face :switch t :advice evil-goggles--generic-async-advice))
  (evil-goggles-use-diff-faces)
  (evil-goggles-mode +1))

(use-package evil-indent-plus
  :after evil
  :config
  (evil-define-key nil evil-inner-text-objects-map
    "i" #'evil-indent-plus-i-indent
    "I" #'evil-indent-plus-i-indent-up
    "J" #'evil-indent-plus-a-indent-up-down)
  (evil-define-key nil evil-outer-text-objects-map
    "i" #'evil-indent-plus-a-indent
    "I" #'evil-indent-plus-i-indent-up
    "J" #'evil-indent-plus-a-indent-up-down))

; Usage: gl MOTION CHAR or gl MOTION / REGEX
(use-package evil-lion
  :after evil
  :config
  (evil-define-key nil evil-normal-state-map
    "gl" #'evil-lion-left
    "gL" #'evil-lion-right)
  (evil-define-key nil evil-visual-state-map
    "gl" #'evil-lion-left
    "gL" #'evil-lion-right))

; TODO: bind to g-/g=/g+ (same as Doom Emacs)
(use-package evil-numbers
  :after evil)

(use-package evil-org
  :after (evil org)
  :custom
  (evil-org-key-theme '(navigation insert textobjects additional return))
  (evil-org-retain-visual-state-on-shift t)
  (evil-org-use-additional-insert t)
  :hook
  (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme))

; Use C-f and C-b to scroll the evil-owl buffer
(use-package evil-owl
  :after evil
  :custom
  (evil-owl-display-method 'window)
  (evil-owl-idle-delay 0.25)
  :config
  (add-to-list 'display-buffer-alist
               '("*evil-owl*"
                 (display-buffer-in-side-window) (side . bottom) (slot . 0) (window-height . 0.3)))
  (evil-owl-mode +1))

(use-package evil-quickscope
  :after evil
  :hook
  (conf-mode . turn-on-evil-quickscope-always-mode)
  (prog-mode . turn-on-evil-quickscope-always-mode)
  (text-mode . turn-on-evil-quickscope-always-mode))

(use-package evil-snipe
  :after evil
  :custom
  (evil-snipe-scope 'whole-visible)
  (evil-snipe-repeat-scope 'whole-visible)
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
  (evil-define-key 'operator global-map
    "s" #'evil-surround-edit
    "S" #'evil-Surround-edit)
  (evil-define-key 'visual global-map
    "S"  #'evil-surround-region
    "gS" #'evil-Surround-region))

(use-package evil-textobj-anyblock
  :after evil
  :config
  (evil-define-key nil evil-inner-text-objects-map "b" #'evil-textobj-anyblock-inner-block)
  (evil-define-key nil evil-outer-text-objects-map "b" #'evil-textobj-anyblock-a-block))

(use-package evil-textobj-entire
  :after evil
  :config
  (evil-define-key nil evil-inner-text-objects-map "e" #'evil-entire-entire-buffer)
  (evil-define-key nil evil-outer-text-objects-map "e" #'evil-entire-entire-buffer))

(use-package evil-visualstar
  :after evil
  :config
  (evil-define-key nil evil-visual-state-map
    "*" #'evil-visualstar/begin-search-forward
    "#" #'evil-visualstar/begin-search-backward))

(use-package ace-window
  :after evil
  :defer 1
  :config
  (evil-define-key nil evil-window-map "a" #'ace-window))

(use-package avy
  :after evil
  :defer 1
  :custom
  (avy-all-windows nil)
  (avy-keys '(?a ?s ?d ?h ?j ?k ?l))
  (avy-single-candidate-jump t)
  :init
  (defun my/goto-char-timer ()
    (interactive)
    ; Calling avy functions with an argument negates the current setting of 'avy-all-windows'
    (avy-goto-char-timer t))
  :config
  (evil-define-key nil evil-normal-state-map
    "gsl"    #'evil-avy-goto-line
    "gsw"    #'evil-avy-goto-word-1
    "gss"    #'evil-avy-goto-char-2
    "gs SPC" #'my/goto-char-timer))

(use-package winner
  :after evil
  :defer 1
  :custom
  (winner-dont-bind-my-keys t)
  :config
  (winner-mode +1)
  (evil-define-key nil evil-window-map
    "u" #'winner-undo
    "U" #'winner-redo))
; TODO: evaluate tab-bar-history-mode

; evil-cleverparens
; evil-embrace
; evil-exchange (http://evgeni.io/posts/quick-start-evil-mode)
; evil-markdown
; evil-mc/evil-mc-extras
; evil-multiedit
; evil-quick-diff
; evil-replace-with-register
; evil-string-inflection
; evil-textobj-tree-sitter
; evil-vimish-fold
; exato
; text-objects (targets/things)



;;;; Editing helps ;;;;

(use-package apheleia
  :hook
  (prog-mode . apheleia-mode))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package flymake
  :defer t
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))
; TODO: evaluate flycheck


;; LSP
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-completion-provider :none)  ; avoid using company
  (lsp-enable-snippet nil)
  (lsp-keymap-prefix "C-c l")
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

; autopairs
; hl-todo
; multicursor
; smartparens/evil-smartparens
; yasnippets
; tempel
; which-func
; whitespace-mode



;;;; Language support ;;;;

;; Emacs Lisp
(use-package parinfer-rust-mode
  :hook emacs-lisp-mode
  :custom
  (parinfer-rust-auto-download t))


;; Python

; References:
; - https://wikemacs.org/wiki/Python
; - https://www.emacswiki.org/emacs/PythonProgrammingInEmacs
; - https://github.com/doomemacs/doomemacs/blob/master/modules/lang/python
; - https://develop.spacemacs.org/layers/+lang/python/README.html
; - https://elpa.gnu.org/packages/python.html

(use-package python
  :ensure nil
  :custom
  (python-indent-offset 4)
  (python-indent-guess-indent-offset nil)
  (python-shell-font-lock-enable nil)
  :mode
  ("\\.py\\'" . python-mode))

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
; TODO: evaluate ein (emacs-ipython-notebook) and emacs-jupyter
; TODO: evaluate lpy
; TODO: evaluate python-x
; TODO: evaluate python-mls


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
  ("\\.[rR]\\'" . R-mode)
  ("\\.[rR]nw\\'" . Rnw-mode)
  :custom
  (ess-use-flymake nil)
  (ess-use-toolbar nil)
  :config
  (require 'ess-site))

(use-package poly-R
  :mode
  ("\\.[rR]md\\'" . poly-markdown+R-mode))


; LaTeX
;; (use-package tex
;;   :ensure auctex)


; tree sitter
; dap-mode
; lua
; haskell



;;;; Org-mode ;;;;



;;;; Git ;;;;

;; Magit
(use-package magit
  :commands magit-status
  :bind
  ("C-x g" . magit-status)
  :config
  (add-to-list 'display-buffer-alist
               '("magit:"
                 (display-buffer-in-tab) (tab-name . "magit"))))

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
  :after vertico
  :custom
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (completion-styles '(orderless basic partial-completion)))

(use-package consult
  :bind
  ("C-x b"    . consult-buffer)
  ("C-x p b"  . consult-project-buffer)
  ; TODO: avoid loading major-modes when previewing recent files (e.g. force fundamental-mode)
  ("C-x C-r"  . consult-recent-file)
  ("<help> a" . consult-apropos)
  :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package embark
  :bind
  ("C-;"   . embark-act)
  ("C-x B" . embark-bindings))

(use-package embark-consult
  :after (embark consult))

; TODO: evaluate corfu vs company
; TODO: evaluate how to use corfu (or vertico) in evil-ex minibuffer
(use-package corfu
  :after vertico
  :custom
  (corfu-auto nil)
  (corfu-cycle t)
  (corfu-quit-no-match 'separator)
  :config
  (global-corfu-mode +1))

(use-package kind-icon
  :after corfu
  :defer 1
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

; tabnine?



;;;; Terminal and file management support ;;;;

(use-package vterm
  :unless ON-WINDOWS
  :commands vterm)

; dired/diredfl/ranger/dirvish
; eshell
; eval-in-repl
; isend-mode
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
  (setq initial-buffer-choice (lambda () (if (buffer-file-name) (current-buffer) (get-buffer "*dashboard*"))))
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



;;;; Keybindings ;;;;

; References:
; - https://www.masteringemacs.org/article/mastering-key-bindings-emacs

; - M-x describe-bindings
; - M-x describe-mode
; - M-x describe-personal-keybidings
; - PREFIX C-h / F1

;; Make ESC quit prompts
(bind-key "<escape>" #'keyboard-escape-quit)

;; Kill current buffer
(bind-key "C-x C-k" #'kill-this-buffer)

