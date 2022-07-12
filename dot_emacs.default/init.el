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
  (scroll-conservatively 2)
  (scroll-margin 2)
  (scroll-preserve-screen-position t)
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



;;;; Keybindings ;;;;

; References:
; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Key-Bindings.html
; - https://www.masteringemacs.org/article/mastering-key-bindings-emacs
; - https://github.com/noctuid/evil-guide#keybindings-and-states

; - M-x describe-bindings
; - M-x describe-mode
; - M-x describe-personal-keybindings
; - M-x general-describe-keybindings
; - PREFIX C-h / F1

; TODO: evaluate replacing bind-key for general
(use-package bind-key
  :config
  ;; Make ESC quit prompts
  (bind-key "<escape>" #'keyboard-escape-quit)

  ;; Kill current buffer
  (bind-key "C-x C-k" #'kill-this-buffer))


(use-package general
  :config

  (general-def
    :prefix "C-c"

    "C-u" #'universal-argument
    "h"   #'major-mode-hydra

    ; TODO: set "C-c I" to open init file (e.g. crux-find-user-init-file)

    ; TODO: lsp

    ; TODO: improve project bindings
    "p" '(:keymap project-prefix-map :which-key "project"))

  ; Prefix renaming (which-key)
  (general-def
    :prefix "C-x"
    "RET" '(:ignore t :which-key "coding-system")
    "4"   '(:ignore t :which-key "other-window")
    "5"   '(:ignore t :which-key "other-frame")
    "6"   '(:ignore t :which-key "two-column")
    "8"   '(:ignore t :which-key "unicode")
    "X"   '(:ignore t :which-key "edebug")
    "a"   '(:ignore t :which-key "abbrev")
    "ai"  '(:ignore t :which-key "inverse")
    "n"   '(:ignore t :which-key "narrow")
    "p"   '(:ignore t :which-key "project")
    "r"   '(:ignore t :which-key "register/bookmark")
    "t"   '(:ignore t :which-key "tab")
    "v"   '(:ignore t :which-key "vc")
    "vM"  '(:ignore t :which-key "merge")
    "w"   '(:ignore t :which-key "winum")
    "x"   '(:ignore t :which-key "buffer")
    "C-a" '(:ignore t :which-key "edebug"))

  (dolist (prefix '("C-h" "<f1>"))
    (general-def
      :prefix prefix
      "4" '(:ignore t :which-key "other-window"))))



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
                     (bookmarks .  5)))
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

(use-package major-mode-hydra
  :custom
  (major-mode-hydra-invisible-quit-key "q")
  (major-mode-hydra-separator "-"))

(use-package which-key
  :config
  (which-key-mode +1))

; Window placement rules
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
       (display-buffer-in-side-window) (side . top) (slot . -1) (window-height . 0.3))
      ("\\*\\(Warnings\\|Compile-Log\\)\\*"
       (display-buffer-in-side-window) (side . top) (slot . +1) (window-height . 0.3))
      ;; right side window
      ("\\*\\(Help\\|helpful\\|Apropos\\|info\\)"
       (display-buffer-in-side-window) (side . right) (slot . 0) (window-width . 0.35))))
  (display-buffer-base-action
   '((display-buffer-reuse-window
      display-buffer-reuse-mode-window
      display-buffer-in-previous-window))))

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
  ; TODO: evaluate HippieExpand
  ("M-/" . dabbrev-completion)
  ("C-M-/" . dabbrev-expand))

(use-package dired
  :ensure nil
  :hook
  (dired-mode . display-line-numbers-mode)
  (dired-mode . hl-line-mode)
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  :mode-hydra
  (dired-mode (:title "Dired")
              ("Mark"
               (("m" dired-mark             :exit nil)
                ("u" dired-unmark           :exit nil)
                ("U" dired-unmark-all-marks :exit nil)
                ("t" dired-toggle-marks     :exit nil))
               "Operations"
               (("C" dired-do-copy)
                ("D" dired-delete)
                ("R" dired-rename)
                ("Z" dired-do-compress)
                ("+" dired-create-directory))
               "Shell"
               (("!" dired-do-shell-command)
                ("&" dired-do-async-shell-command)))))

(use-package ediff
  :ensure nil
  :defer t
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

; TODO: evaluate this better
(use-package elec-pair
  :ensure nil
  :hook
  (conf-mode . electric-pair-mode)
  (text-mode . electric-pair-mode))

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

(use-package help
  :ensure nil
  :defer t
  :config
  (add-hook 'help-mode-hook #'display-line-numbers-mode)
  (add-hook 'help-mode-hook #'hl-line-mode))

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

(use-package info
  :ensure nil
  :mode-hydra
  (Info-mode (:title "Info")
             ("Navigation"
              (("u"   Info-up            :exit nil)
               ("gj"  Info-next          :exit nil)
               ("gk"  Info-prev          :exit nil)
               ("C-j" Info-forward-node  :exit nil)
               ("C-k" Info-backward-node :exit nil))
              "History"
              (("C-o" Info-history-back    :exit nil)
               ("C-i" Info-history-forward :exit nil)
               ("gL"  Info-history))
              "Goto"
              (("d"  Info-directory)
               ("gt" Info-top-node)
               ("gm" Info-menu)
               ("gG" Info-goto-node)
               ("gT" Info-toc))
              "Search"
              (("s"  Info-search)
               ("S"  Info-search-case-insensitive)
               ("i"  Info-index)
               ("g," Info-index-next)
               ("I"  Info-virtual-index)
               ("a"  Info-apropos)))))

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
; TODO: evaluate desktop-save-mode

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

; TODO: evaluate crux and better-defaults
(use-package crux
  :bind
  ([remap move-beginning-of-line] . crux-move-beginning-of-line))

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
  (add-hook 'helpful-mode-hook #'display-line-numbers-mode)
  (add-hook 'helpful-mode-hook #'hl-line-mode))

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

(use-package restart-emacs
  :commands restart-emacs)

(use-package tab-bar-echo-area
  :config
  (tab-bar-echo-area-mode +1))

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
  :hook
  (after-init . evil-mode)
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

  (define-key evil-insert-state-map (kbd "C-g") #'evil-normal-state)

  ; Use readline-like bindings in insert state
  (define-key evil-insert-state-map (kbd "M-n") #'next-line)
  (define-key evil-insert-state-map (kbd "M-p") #'previous-line)
  (define-key evil-insert-state-map (kbd "C-a") #'move-beginning-of-line)  ; original: evil-paste-last-insertion
  (define-key evil-insert-state-map (kbd "C-e") #'move-end-of-line)        ; original: evil-copy-from-below
  ;; (define-key evil-insert-state-map (kbd "C-d") #'delete-char)             ; original: evil-shift-left
  ;; (define-key evil-insert-state-map (kbd "C-h") #'delete-backward-char)    ; original: help prefix
  ;; (define-key evil-insert-state-map (kbd "C-y") #'yank)                    ; original: evil-copy-from-above

  ; C-w extra bindings
  (define-key evil-window-map "`" #'evil-switch-to-windows-last-buffer)
  (define-key evil-window-map "1" #'winum-select-window-1)
  (define-key evil-window-map "2" #'winum-select-window-2)
  (define-key evil-window-map "3" #'winum-select-window-3)
  (define-key evil-window-map "4" #'winum-select-window-4)
  (define-key evil-window-map "5" #'winum-select-window-5)

  (general-def
    :prefix "C-w"
    :prefix-map 'evil-window-map
    "TAB"     '(:ignore t :which-key "tabs")
    "TAB RET" #'tab-switch
    "TAB `"   #'tab-bar-switch-to-last-tab
    "TAB n"   #'tab-next
    "TAB p"   #'tab-previous
    "TAB r"   #'tab-rename
    "TAB u"   #'tab-undo
    "TAB c"   #'tab-close
    "TAB D"   #'tab-duplicate
    "TAB N"   #'tab-new
    "TAB b"   #'switch-to-buffer-other-tab
    "TAB d"   #'dired-other-tab
    "TAB f"   #'find-file-other-tab
    "TAB P"   #'project-other-tab-command
    "TAB 1"   '((lambda () (interactive) (tab-bar-select-tab 1)) :which-key "goto-tab-1")
    "TAB 2"   '((lambda () (interactive) (tab-bar-select-tab 2)) :which-key "goto-tab-2")
    "TAB 3"   '((lambda () (interactive) (tab-bar-select-tab 3)) :which-key "goto-tab-3")
    "TAB 4"   '((lambda () (interactive) (tab-bar-select-tab 4)) :which-key "goto-tab-4")
    "TAB 5"   '((lambda () (interactive) (tab-bar-select-tab 5)) :which-key "goto-tab-5"))

  ; Open Dired buffer
  (define-key evil-normal-state-map "-" #'dired-jump)

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

(use-package evil-anzu
  :after evil
  :config
  (global-anzu-mode +1))

(use-package evil-args
  :after evil
  :config
  (define-key evil-inner-text-objects-map "a" #'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" #'evil-outer-arg))

(use-package evil-commentary
  :after evil
  :config
  (define-key evil-normal-state-map "gc" #'evil-commentary))

(use-package evil-extra-operator
  :after evil
  :config
  (define-key evil-motion-state-map "gr" #'evil-operator-eval))

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
  (define-key evil-inner-text-objects-map "i" #'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" #'evil-indent-plus-a-indent)
  (define-key evil-inner-text-objects-map "I" #'evil-indent-plus-i-indent-up)
  (define-key evil-outer-text-objects-map "I" #'evil-indent-plus-i-indent-up)
  (define-key evil-inner-text-objects-map "J" #'evil-indent-plus-a-indent-up-down)
  (define-key evil-outer-text-objects-map "J" #'evil-indent-plus-a-indent-up-down))

; Usage: gl MOTION CHAR or gl MOTION / REGEX
(use-package evil-lion
  :after evil
  :config
  (define-key evil-normal-state-map "gl" #'evil-lion-left)
  (define-key evil-normal-state-map "gL" #'evil-lion-right)
  (define-key evil-visual-state-map "gl" #'evil-lion-left)
  (define-key evil-visual-state-map "gL" #'evil-lion-right))

; TODO: evaluate evil-mc (https://github.com/doomemacs/doomemacs/blob/master/modules/editor/multiple-cursors/config.el)
(use-package evil-multiedit
  :after evil
  :custom
  (evil-multiedit-follow-matches t)
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-numbers
  :after evil
  :config
  (define-key evil-normal-state-map "g-" #'evil-numbers/dec-at-pt)
  (define-key evil-normal-state-map "g=" #'evil-numbers/inc-at-pt))

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
  :defer 1
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
  (evil-define-text-object my/evil-textobj-anyblock-inner-quote (count &optional beg end type)
    "Select the closest outer quote."
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "'")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count nil)))

  (evil-define-text-object my/evil-textobj-anyblock-a-quote (count &optional beg end type)
    "Select the closest outer quote."
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "'")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count t)))

  (define-key evil-inner-text-objects-map "b" #'evil-textobj-anyblock-inner-block)
  (define-key evil-outer-text-objects-map "b" #'evil-textobj-anyblock-a-block)
  (define-key evil-inner-text-objects-map "q" #'my/evil-textobj-anyblock-inner-quote)
  (define-key evil-outer-text-objects-map "q" #'my/evil-textobj-anyblock-a-quote))

(use-package evil-textobj-entire
  :after evil
  :config
  (define-key evil-inner-text-objects-map "e" #'evil-entire-entire-buffer)
  (define-key evil-outer-text-objects-map "e" #'evil-entire-entire-buffer))

(use-package evil-visualstar
  :after evil
  :config
  (define-key evil-visual-state-map "*" #'evil-visualstar/begin-search-forward)
  (define-key evil-visual-state-map "#" #'evil-visualstar/begin-search-backward))

(use-package ace-window
  :after evil
  :defer 1
  :config
  (define-key evil-window-map "a" #'ace-window))

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
  (general-def
    :prefix "g"
    "s" '(:ignore t :which-key "goto"))
  (define-key evil-normal-state-map "gsl" #'evil-avy-goto-line)
  (define-key evil-normal-state-map "gsw" #'evil-avy-goto-word-1)
  (define-key evil-normal-state-map "gss" #'evil-avy-goto-char-2)
  (define-key evil-normal-state-map (kbd "g s SPC") #'my/goto-char-timer))

(use-package transpose-frame
  :after evil
  :defer 1
  :config
  (define-key evil-window-map "T" #'transpose-frame))

(use-package winner
  :after evil
  :defer 1
  :custom
  (winner-dont-bind-my-keys t)
  :config
  (winner-mode +1)
  (define-key evil-window-map "u" #'winner-undo)
  (define-key evil-window-map "U" #'winner-redo))
; TODO: evaluate tab-bar-history-mode

; evil-cleverparens
; evil-embrace
; evil-exchange (http://evgeni.io/posts/quick-start-evil-mode)
; evil-markdown
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

(use-package ws-butler
  :hook
  (conf-mode . ws-butler-mode)
  (prog-mode . ws-butler-mode)
  (text-mode . ws-butler-mode))


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
  (parinfer-rust-auto-download t)
  :config
  (general-def :prefix "C-c" "C-p" '(:ignore t :which-key "parinfer")))


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
  ("\\.py\\'" . python-mode)
  :config
  (general-def :prefix "C-c" "C-t" '(:ignore t :which-key "python-skeleton")))

(use-package anaconda-mode
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode))

(use-package pyvenv
  :hook (python-mode . pyvenv-mode)
  :bind
  (:map python-mode-map
        ("C-c a" . pyvenv-activate)
        ("C-c d" . pyvenv-deactivate)
        ("C-c r" . pyvenv-restart-python)))

; TODO: evaluate python-pytest (beware of projectile dependency)
; TODO: add --color argument
(use-package pytest
  :after python
  :custom
  (pytest-cmd-flags "--exitfirst --capture=no")
  (pytest-project-root-files '(".venv" "setup.py" ".git"))
  :commands (pytest-one pytest-all)
  :bind
  (:map python-mode-map
        ("C-c t" . pytest-one)
        ("C-c T" . pytest-all))
  :config
  (add-to-list 'display-buffer-alist
               '("*pytest*"
                 (display-buffer-in-side-window) (side . right) (slot . 1) (window-width . 0.35))))


(use-package lsp-pyright
  :after (python-mode lsp-mode)
  :init
  (advice-add 'lsp :before (lambda () (require 'lsp-pyright))))

; TODO: create python-mode hydra and/or major-mode specific bindings

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

; TODO: evaluate julia-snail


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

; pdf-tools


; tree sitter
; dap-mode
; lua
; haskell



;;;; Org-mode ;;;;

(use-package org
  :defer 5)



;;;; Git ;;;;

;; Magit
(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-c g" . magit-file-dispatch)
  :commands magit-status
  :custom
  (magit-define-global-key-bindings nil)
  :config
  (add-to-list 'display-buffer-alist
               '("magit:"
                 (display-buffer-in-tab) (tab-name . "magit"))))

; TODO: evaluate git-gutter-fringe
(use-package diff-hl
  :after magit
  :hook
  (conf-mode . diff-hl-mode)
  (prog-mode . diff-hl-mode)
  (dired-mode . diff-hl-dired-mode)
  :config
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))



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
  :defer 1
  :bind
  ("C-x C-r"  . consult-recent-file)
  ([remap apropos-command] . consult-apropos)
  ([remap project-switch-to-buffer] . consult-project-buffer)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  (general-def
    :prefix "C-c"
    "SPC" '(:ignore t :which-key "consult"))
  (general-def
    :prefix "C-c SPC"
    "SPC" #'consult-buffer
    "b"   #'consult-buffer
    "o"   #'consult-outline
    "i"   #'consult-imenu
    "l"   #'consult-line
    "r"   #'consult-ripgrep
    "g"   #'consult-git-grep
    "f"   #'consult-find
    "y"   #'consult-flymake))

(use-package embark
  :defer 1
  :bind
  ("C-;"      . embark-act)
  ("<help> B" . embark-bindings))

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



