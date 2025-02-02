;;; init.el -*- lexical-binding: t; -*-

;; Comments best practice: same line with code (;), comment-only line (;;), headlines in outline mode (;;;)

;; Suppress annoying messages
(defun suppress-messages (func &rest args)
  "Suppress message output from (FUNC ARGS)."
  ;; Reference: https://superuser.com/questions/669701/emacs-disable-some-minibuffer-messages
  (cl-flet ((silence (&rest _) (ignore)))
    (advice-add 'message :around #'silence)
    (unwind-protect
        (apply func args)
      (advice-remove 'message #'silence))))

(defadvice load (before quiet-load (file &optional noerror nomessage nosuffix must-suffix) activate)
  "Suppress file loading messages."
  ;; Reference: https://stackoverflow.com/questions/11498108/elisp-silence-loading-messages-in-batch-mode
  (setq nomessage t))


;; Custom settings
(setq custom-file (make-temp-file "custom_" nil ".el"))


;; TODO: evaluate (setq debug-on-error t)
;; TODO: evaluate bug-hunter (ELPA)
;; TODO: set use-package-always-defer t with :demand t
;; TODO: replace :init with :custom in use-package declarations
;; TODO: define and set order for :mode :custom :bind :commands :interpreter keywords
;; TODO: check if :hook declarations are only being used to load the package at hand, since :hook implies :defer t (e.g. :hook (some-other-mode . this-package-mode))
;; TODO: open Emacs as server
;; TODO: evaluate all packages for lazy loading and :defer declarations (e.g. evil extension packages)
;; TODO: check if :after declarations are necessary/useful
;; TODO: evaluate project-rootfile
;; TODO: review new features in Emacs 29 and later (e.g. treesitter modes, completion)



;;; Package management

;; package and use-package

(require 'package)
(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
                         ("org"    . "https://orgmode.org/elpa/")
                         ("elpa"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(eval-when-compile
  (require 'use-package))
(use-package use-package
  :custom
  (use-package-always-defer t)
  (use-package-always-ensure t)
  (use-package-compute-statistics t))

;; use-package keywords that imply :defer t
;; - :bind, :bind*
;; - :bind-keymap, :bind-keymap*
;; - :commands
;; - :hook
;; - :interpreter
;; - :mode


;; Early setup packages

(use-package benchmark-init
  :demand t
  :config
  (add-hook 'after-init-hook #'benchmark-init/deactivate))

(use-package auto-compile
  :demand t
  :config
  (auto-compile-on-load-mode +1))

(use-package no-littering
  :demand t)

;; TODO: evaluate this better
;; (use-package quelpa-use-package)



;;; General Emacs configuration

;; Improve user interface

(use-package emacs
  :ensure nil
  :demand t
  :custom
  (initial-major-mode 'text-mode)
  (initial-scratch-message nil)
  (visible-bell t)
  :init
  (setq-default cursor-in-non-selected-windows nil)
  :config
  (context-menu-mode +1))


;; Editing settings

(use-package emacs
  :ensure nil
  :demand t
  :init
  (setq-default indent-tabs-mode nil
                truncate-lines t)
  :custom
  (completion-auto-help 'visible)
  (completion-auto-select 'second-tab)
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


;; File backup settings
(use-package emacs
  :ensure nil
  :demand t
  :custom
  (backup-by-copying t)
  (backup-directory-alist (list (cons "." (expand-file-name "var/backups/" user-emacs-directory))))
  (delete-old-versions t)
  (kept-new-versions 10)
  (kept-old-versions 5)
  (make-backup-files t)
  (version-control t))


;; Misc settings

(use-package emacs
  :ensure nil
  :demand t
  :custom
  (bookmark-save-flag 1)
  (delete-by-moving-to-trash t)
  (find-file-visit-truename t)
  (native-comp-deferred-compilation t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (use-short-answers t))


;; System dependent settings

;; Linux
(use-package emacs
  :ensure nil
  :demand ON-LINUX
  :if ON-LINUX
  :custom-face
  (default ((t :family "Iosevka Fixed" :height 120)))
  (variable-pitch ((t :family "Iosevka Aile" :height 115)))
  :config
  (menu-bar-mode -1)
  (scroll-bar-mode +1)
  (tool-bar-mode -1))

;; macOS
(use-package emacs
  :ensure nil
  :demand ON-MAC
  :if ON-MAC
  :custom-face
  (default ((t :family "Iosevka Fixed" :height 160)))
  (variable-pitch ((t :family "Iosevka Aile" :height 155)))
  :config
  (menu-bar-mode +1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

;; Windows
(use-package emacs
  :ensure nil
  :demand ON-WINDOWS
  :if ON-WINDOWS
  :custom-face
  (default ((t :family "Iosevka Fixed" :height 100)))
  (variable-pitch ((t :family "Iosevka Aile" :height 95)))
  :config
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

;; Terminal
(use-package emacs
  :ensure nil
  :demand (not (display-graphic-p))
  :unless (display-graphic-p)
  :config
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))



;;; Keybindings

;; References:
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Key-Bindings.html
;; - https://www.masteringemacs.org/article/mastering-key-bindings-emacs
;; - https://github.com/noctuid/evil-guide#keybindings-and-states

;; - M-x describe-bindings
;; - M-x describe-mode
;; - M-x describe-personal-keybindings
;; - PREFIX C-h / F1

(use-package bind-key
  :demand t
  :config
  ;; Make ESC quit prompts
  (bind-key "<escape>" #'keyboard-escape-quit)
  ;; Avoid delete-other-windows call when pressing ESC
  ;; Reference: https://stackoverflow.com/questions/557282/in-emacs-whats-the-best-way-for-keyboard-escape-quit-not-destroy-other-windows
  (defadvice keyboard-escape-quit (around my/keyboard-escape-quit-advice activate)
    (let (orig-one-window-p)
      (fset 'orig-one-window-p (symbol-function 'one-window-p))
      (fset 'one-window-p (lambda (&optional nomini all-frames) t))
      (unwind-protect
          ad-do-it
        (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

  ;; Alternative binding for universal-argument
  (bind-key "C-c u" #'universal-argument)

  ;; Kill current buffer
  (bind-key "C-x C-k" #'kill-this-buffer)

  ;; Open dashboard buffer
  (bind-key "C-c D" #'dashboard-refresh-buffer)

  ;; Open Emacs init file
  (defun my/open-init-file () (interactive) (find-file user-init-file))
  (bind-key "C-c I" #'my/open-init-file)

  ;; Open Messages buffer
  (defun my/open-messages-buffer () (interactive) (switch-to-buffer "*Messages*"))
  (bind-key "C-c M" #'my/open-messages-buffer)

  ;; Open scratch buffers
  (defun my/open-scratch-buffer () (interactive) (switch-to-buffer "*scratch*"))
  (bind-key "C-c S" #'my/open-scratch-buffer)
  (bind-key "C-c s" #'scratch)

  ;; Toggle frame decoration
  (defun my/toggle-frame-decoration ()
    (interactive)
    (set-frame-parameter nil 'undecorated (not (frame-parameter nil 'undecorated))))
  (bind-key "M-<f11>" #'my/toggle-frame-decoration))


;; TODO: evaluate replacing general for something else
;; TODO: find best way to replace +prefix with appropriate name for all minor and major modes (e.g. pytest, pyenv, parinfer)
(use-package general
  :demand t
  :config
  ;; Prefix renaming (which-key)
  ;; TODO: evaluate using which-key-add-keymap-based-replacements for prefix renaming
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
    "C-a" '(:ignore t :which-key "gud"))

  (dolist (prefix '("C-h" "<f1>"))
    (general-def
      :prefix prefix
      "4" '(:ignore t :which-key "other-window"))))



;;; Windows, interface elements, visual editing helpers and themes

(use-package dashboard
  :demand t
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
  :demand t
  :after (doom-themes nerd-icons)
  :custom
  (doom-modeline-height 30)
  (doom-modeline-minor-modes t)
  :config
  (doom-modeline-mode +1))

(use-package doom-themes
  :demand t)

(use-package catppuccin-theme
  :demand t
  :custom
  (catppuccin-flavor 'macchiato)
  :config
  (load-theme 'catppuccin t))

(use-package major-mode-hydra
  :demand t
  :custom
  (major-mode-hydra-invisible-quit-key "q")
  (major-mode-hydra-separator "-")
  :config
  (bind-key "C-c H" #'major-mode-hydra))

(use-package nerd-icons
  :demand t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :hook
  (ibuffer-mode . nerd-icons-ibuffer-mode))

;; TODO: evaluate removing popper (e.g. window-toggle-side-windows)
(use-package popper
  :demand t
  :custom
  (popper-display-control nil)
  (popper-group-function #'popper-group-by-directory)
  (popper-mode-line (propertize " POP " 'face 'mode-line-emphasis))
  (popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "\\*Backtrace\\*"
          "\\*Compile-Log\\*"
          "Output\\*"
          "\\*package update results\\*"
          "\\*Anaconda\\*"
          help-mode
          helpful-mode
          apropos-mode
          Man-mode
          woman-mode
          devdocs-mode
          dictionary-mode
          compilation-mode
          emacs-lisp-compilation-mode
          flymake-diagnostics-buffer-mode
          occur-mode
          xref--xref-buffer-mode))
  :config
  (popper-mode +1)
  (popper-echo-mode +1)
  (bind-key "C-`"   #'popper-cycle)
  (bind-key "M-`"   #'popper-toggle)
  (bind-key "C-M-`" #'popper-toggle-type))

(use-package which-key
  :demand t
  :config
  (which-key-mode +1))

;; Window placement rules
;; References:
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Displaying-Buffers.html
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Side-Windows.html
;; TODO: organize display-buffer-alist usage
(use-package window
  :ensure nil
  :demand t
  :custom
  (display-buffer-alist
   '(
     ;; no window
     ("\\`\\*Async Shell Command\\*\\'"
      (display-buffer-no-window))
     ;; top side window
     ("\\*\\(Messages\\|package update results\\)\\*"
      (display-buffer-in-side-window) (side . top) (slot . -1) (window-height . 0.3))
     ("\\*\\(Warnings\\|Compile-Log\\)\\*"
      (display-buffer-in-side-window) (side . top) (slot . +1) (window-height . 0.3))
     ;; right side window
     ("\\*\\(Help\\|helpful\\|Apropos\\|info\\|Man\\|WoMan\\|Dictionary\\)"
      (display-buffer-in-side-window) (side . right) (slot . 0) (window-width . 0.35))
     ;; bottom side window
     ("\\*Process List\\*"
      (display-buffer-in-side-window) (side . bottom) (slot . 0) (window-height . 0.2))))
  (display-buffer-base-action
   '((display-buffer-reuse-window
      display-buffer-reuse-mode-window
      display-buffer-in-previous-window))))

;; TODO: evaluate golden-ration



;;; Emacs management and fixes

;; Built-in packages (:ensure nil)

(use-package apropos
  :ensure nil
  :custom
  (apropos-do-all t))

(use-package autorevert
  :ensure nil
  :demand t
  :custom
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode +1))

(use-package comint
  :ensure nil
  :custom
  (comint-scroll-to-bottom-on-input t))

(use-package dabbrev
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"
                                    "\\`\\*"
                                    "\\*\\'"))
  :bind
  ;; TODO: evaluate HippieExpand
  ("M-/" . dabbrev-completion)
  ("C-M-/" . dabbrev-expand))

;; TODO: HTTP proxy configuration
(use-package dictionary
  :ensure nil
  :custom
  (dictionary-use-single-buffer t)
  (dictionary-server "dict.org")
  (dictionary-proxy-server "localhost")
  (dictionary-proxy-port 3128))

(use-package dired
  :ensure nil
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (add-hook 'dired-mode-hook #'display-line-numbers-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)
  :mode-hydra
  (dired-mode (:title "Dired")
              ("Mark"
               (("m" dired-mark             :exit nil)
                ("u" dired-unmark           :exit nil)
                ("U" dired-unmark-all-marks :exit nil)
                ("t" dired-toggle-marks     :exit nil))
               "Operations"
               (("C" dired-do-copy)
                ("D" dired-do-delete)
                ("R" dired-do-rename)
                ("Z" dired-do-compress)
                ("+" dired-create-directory))
               "Shell"
               (("!" dired-do-shell-command)
                ("&" dired-do-async-shell-command)))))

(use-package ediff
  :ensure nil
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package eshell
  :ensure nil
  :custom
  (eshell-scroll-to-bottom-on-input t))

;; TODO: add hydra (https://readingworldmagazine.com/emacs/2022-01-24-how-to-use-eww-browser-in-emacs)
(use-package eww
  :ensure nil
  :custom
  (browse-url-browser-function 'eww-browse-url))

;; TODO: evaluate grammarly, languagetool and proselint
(use-package flymake
  :ensure nil
  :after popper
  :hook
  (conf-mode . flymake-mode)
  (prog-mode . flymake-mode)
  (text-mode . flymake-mode)
  :config
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)
  (add-to-list 'display-buffer-alist
               '("\\*Flymake diagnostics"
                 (display-buffer-in-side-window) (side . bottom) (slot . 0) (window-height . 0.3))))

(use-package help
  :ensure nil
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
              "Go to"
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

(use-package outline
  :ensure nil
  :config
  (general-def
    :prefix "C-c"
    "@" '(:ignore t :which-key "outline")))

(use-package project
  :ensure nil
  :custom
  (project-vc-extra-root-markers '(".project")))

(use-package recentf
  :ensure nil
  :demand t
  :custom
  (recentf-max-saved-items 20)
  :config
  (advice-add 'recentf-cleanup :around #'suppress-messages)
  (recentf-mode +1)
  (advice-remove 'recentf-cleanup #'suppress-messages))

(use-package replace
  :ensure nil
  :commands occur
  :config
  (add-hook 'occur-hook #'occur-rename-buffer)
  (add-to-list 'display-buffer-alist
               '("\\*Occur\\*"
                 (display-buffer-below-selected) (inhibit-same-window . t) (window-height . 0.25))))

(use-package savehist
  :ensure nil
  :demand t
  :config
  (savehist-mode +1))

(use-package saveplace
  :ensure nil
  :demand t
  :config
  (save-place-mode +1))

(use-package tab-bar
  :ensure nil
  :demand t
  :custom-face
  (tab-bar-tab ((t :underline t)))
  :custom
  (tab-bar-format '(my/tab-bar-format-menu-bar tab-bar-format-align-right tab-bar-format-tabs-groups tab-bar-separator))
  (tab-bar-auto-width nil)
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-separator "   ")
  (tab-bar-tab-hints t)
  :init
  (defun my/tab-bar-format-menu-bar ()
    `((menu-bar menu-item (propertize " Menu " 'face 'tab-bar-tab-inactive) tab-bar-menu-bar :help "Menu Bar")))
  :config
  (tab-bar-mode +1)
  (tab-bar-rename-tab "main" 1))
;; TODO: evaluate desktop-save-mode / tab-bar-history-mode

(use-package tab-line
  :ensure nil
  :demand t
  :custom
  (tab-line-exclude-modes '(completion-list-mode dashboard-mode))
  (tab-line-new-button-show nil)
  (tab-line-tabs-function #'tab-line-tabs-mode-buffers)
  :config
  (global-tab-line-mode +1))

(use-package uniquify
  :ensure nil
  :demand t
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package xref
  :ensure nil
  :demand t
  :custom
  (xref-search-program 'ripgrep)
  :config
  (add-to-list 'display-buffer-alist
          '("\\*xref\\*"
            (display-buffer-in-side-window) (side . right) (slot . 1) (window-width . 0.35))))


;; Community packages

;; TODO: evaluate purcell/envrc

(use-package auto-package-update
  :defer 5
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results nil)
  (auto-package-update-interval 5)
  (auto-package-update-prompt-before-update t)
  :config
  (auto-package-update-maybe))

;; TODO: learn how to use calc-mode
(use-package casual
  :after calc
  :hook
  (calc-mode . casual-calc-tmenu)
  :config
  (evil-collection-define-key 'normal 'calc-mode-map (kbd "C-o") 'casual-calc-tmenu))

;; TODO: evaluate crux and better-defaults
(use-package crux
  :bind
  ([remap move-beginning-of-line] . crux-move-beginning-of-line))

(use-package elfeed
  :defer 10
  :custom
  (elfeed-feeds '(
                  ("https://hnrss.org/frontpage" hn)
                  ("https://planet.emacslife.com/atom.xml" emacs)
                  ("https://rss.slashdot.org/Slashdot/slashdotMain" slashdot)
                  ("https://this-week-in-neovim.org/rss" vim)
                  ("https://reddit.com/r/emacs/.rss" emacs reddit)
                  ("https://reddit.com/r/neovim/.rss" vim reddit)
                  ("https://reddit.com/r/vim/.rss" vim reddit)
                  ("https://cyberciti.com/atom/atom.xml" linux)
                  ("https://itsfoss.com/rss" linux)
                  ("https://linux.com/feed" linux)
                  ("https://linuxjournal.com/node/feed" linux)
                  ("https://linuxtoday.com/feed" linux)
                  ("https://lwn.net/headlines/rss" linux)
                  ("https://phoronix.com/rss.php" linux)
                  ("https://tecmint.com/feed" linux)))
  (elfeed-search-filter "@6-months-ago +unread ")
  :config
  (elfeed-goodies/setup)
  (add-hook 'elfeed-search-mode-hook #'elfeed-update)
  (run-at-time t (* 60 60) #'elfeed-update)
  ;; TODO: open elfeed in exclusive frame
  ;; Display elfeed in new tab
  (advice-add #'elfeed :before (lambda (&rest args) (display-buffer-in-tab (elfeed-search-buffer) '((tab-name . "elfeed")))))
  ;; Avoid flicker when closing elfeed-show buffer
  (advice-add #'elfeed-kill-buffer :after (lambda (&rest args) (evil-window-delete))))

(use-package elfeed-goodies
  :after elfeed
  :custom
  (elfeed-goodies/feed-source-column-width 40)
  (elfeed-goodies/powerline-default-separator 'alternate))

(use-package ellama
  :commands (ellama-chat)
  :config
  (add-hook 'org-ctrl-c-ctrl-c-hook #'ellama-chat-send-last-message))

(use-package gcmh
  :defer 2
  :config
  (gcmh-mode +1))

;; TODO: evaluate native equivalent features (e.g. bookmarks)
(use-package harpoon
  :demand t
  :custom
  (harpoon-project-package 'project)
  :init
  (general-def
    :prefix "C-c"
    "h" '(:ignore t :which-key "harpoon"))
  (bind-keys
    :prefix "C-c h"
    :prefix-map my/harpoon-prefix-map
    ("<return>" . harpoon-add-file)
    ("h"        . harpoon-toggle-quick-menu)
    ("1"        . harpoon-go-to-1)
    ("2"        . harpoon-go-to-2)
    ("3"        . harpoon-go-to-3)
    ("4"        . harpoon-go-to-4)
    ("5"        . harpoon-go-to-5)
    ("F"        . harpoon-toggle-file)
    ("C"        . harpoon-clear)))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key]      . helpful-key)
  :config
  (add-hook 'helpful-mode-hook #'display-line-numbers-mode)
  (add-hook 'helpful-mode-hook #'hl-line-mode))

(use-package ibuffer-project
  :init
  (defun my/ibuffer-project ()
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative)))
  :hook
  (ibuffer-mode . my/ibuffer-project))

;; TODO: evaluate mode-minder

(use-package minions
  :demand t
  :config
  (minions-mode +1))

(use-package page-break-lines
  :demand t
  :config
  (global-page-break-lines-mode +1))

(use-package persistent-scratch
  :demand t
  :config
  (persistent-scratch-setup-default))

(use-package pcmpl-args
  :after eshell)

(use-package project-tab-groups
  :demand t
  :config
  (project-tab-groups-mode +1))

;; TODO: evaluate dired-hacks
(use-package ranger
  :commands (deer ranger))

(use-package rg
  :commands (rg rg-menu rg-project))

(use-package scratch
  :commands scratch)

(use-package tab-bar-echo-area
  :demand t
  :config
  (tab-bar-echo-area-mode +1))

(use-package tabspaces
  :demand t
  :custom
  (tabspaces-exclude-buffers '("*dashboard*"))
  (tabspaces-include-buffers '("*scratch*" "*Messages*"))
  (tabspaces-use-filtered-buffers-as-default t)
  :config
  (tabspaces-mode +1))

(use-package tmr
  :commands tmr)

(use-package try
  :commands try)

(use-package vlf
  :commands vlf
  :config
  (require 'vlf-setup))

(unless ON-WINDOWS
  (use-package vterm
    :custom
    (vterm-kill-buffer-on-exit nil)
    :commands vterm
    :config
    ;; Reference: https://github.com/akermu/emacs-libvterm/issues/313
    ;; Fix cursor shape in vterm with evil-mode
    (advice-add #'vterm--redraw :around (lambda (fun &rest args) (let ((cursor-type cursor-type)) (apply fun args))))
    ;; Fix cursor position after append
    (advice-add #'vterm-send-key :before (lambda (&rest args) (vterm-goto-char (point))))))

(use-package windresize
  :commands windresize)

;; Default prefix: C-x w
(use-package winum
  :demand t
  :config
  (winum-mode +1))



;;; Evil-mode

;; References:
;; - https://evil.readthedocs.io
;; - https://github.com/noctuid/evil-guide

(use-package evil
  :demand t
  :custom
  (evil-respect-visual-line-mode t)
  (evil-symbol-word-search t)
  (evil-undo-system 'undo-redo)
  (evil-want-C-h-delete t)
  (evil-want-C-u-delete t)
  (evil-want-C-u-scroll t)
  (evil-want-C-w-in-emacs-state t)
  (evil-want-fine-undo t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  ;; evil-want-o/O-to-continue-comments t ; TODO: check this in Doom Emacs

  :init
  ;; Reference: https://evil.readthedocs.io/en/latest/faq.html#underscore-is-not-a-word-character
  (defun my/set-underscore-as-word ()
    (interactive)
    (modify-syntax-entry ?_ "w"))

  :config
  ;; For some reason, some evil customizations do not work with :custom
  ;; Reference: https://github.com/emacs-evil/evil/issues/1571
  (customize-set-variable 'evil-search-module 'evil-search)
  (customize-set-variable 'evil-split-window-right t)
  (customize-set-variable 'evil-vsplit-window-below t)
  (customize-set-variable 'evil-want-Y-yank-to-eol t)

  (add-hook 'conf-mode-hook #'my/set-underscore-as-word)
  (add-hook 'prog-mode-hook #'my/set-underscore-as-word)
  (add-hook 'text-mode-hook #'my/set-underscore-as-word)

  (evil-mode +1)

  (define-key evil-insert-state-map (kbd "C-g") #'evil-normal-state)

  ;; Use readline-like bindings in insert state
  (define-key evil-insert-state-map (kbd "M-n") #'next-line)
  (define-key evil-insert-state-map (kbd "M-p") #'previous-line)
  (define-key evil-insert-state-map (kbd "C-a") #'move-beginning-of-line)  ; original: evil-paste-last-insertion
  (define-key evil-insert-state-map (kbd "C-e") #'move-end-of-line)        ; original: evil-copy-from-below
  (define-key evil-insert-state-map (kbd "C-d") #'delete-char)             ; original: evil-shift-left-line
  (define-key evil-insert-state-map (kbd "M-h") #'backward-kill-word)      ; original: mark-paragraph

  ;; Since C-d is not available in insert mode (see above), remap evil-shift-line commands
  (define-key evil-insert-state-map (kbd "C-,") #'evil-shift-left-line)
  (define-key evil-insert-state-map (kbd "C-.") #'evil-shift-right-line)

  ;; Also use readline-like binding in minibuffer
  (define-key minibuffer-local-map (kbd "C-h") #'delete-backward-char)
  (define-key minibuffer-local-map (kbd "M-h") #'backward-kill-word)

  ;; C-w extra bindings
  (define-key evil-window-map "`" #'evil-switch-to-windows-last-buffer)
  (define-key evil-window-map "C" #'kill-buffer-and-window)
  (define-key evil-window-map "T" #'tab-window-detach)
  (define-key evil-window-map "1" #'winum-select-window-1)
  (define-key evil-window-map "2" #'winum-select-window-2)
  (define-key evil-window-map "3" #'winum-select-window-3)
  (define-key evil-window-map "4" #'winum-select-window-4)
  (define-key evil-window-map "5" #'winum-select-window-5)
  (define-key evil-window-map (kbd "C-h") #'evil-window-left)
  (define-key evil-window-map (kbd "C-l") #'evil-window-right)
  (define-key evil-window-map (kbd "C-j") #'evil-window-down)
  (define-key evil-window-map (kbd "C-k") #'evil-window-up)

  ;; C-w TAB bindings
  (defun my/tab-goto-1 () (interactive) (tab-select 1))
  (defun my/tab-goto-2 () (interactive) (tab-select 2))
  (defun my/tab-goto-3 () (interactive) (tab-select 3))
  (defun my/tab-goto-4 () (interactive) (tab-select 4))
  (defun my/tab-goto-5 () (interactive) (tab-select 5))
  (general-def
    :prefix "C-w"
    "TAB" '(:ignore t :which-key "tabs")
    "g"   '(:ignore t :which-key "tab-bar-switch"))
  (bind-keys
    :map evil-window-map
    ("TAB TAB" . tab-bar-switch-to-recent-tab)
    ("TAB RET" . tab-switch)
    ("TAB `"   . tab-bar-switch-to-last-tab)
    ("TAB n"   . tab-next)
    ("TAB p"   . tab-previous)
    ("TAB r"   . tab-rename)
    ("TAB u"   . tab-undo)
    ("TAB c"   . tab-close)
    ("TAB D"   . tab-duplicate)
    ("TAB b"   . switch-to-buffer-other-tab)
    ("TAB d"   . dired-other-tab)
    ("TAB w"   . tab-window-detach)
    ("TAB f"   . find-file-other-tab)
    ("TAB P"   . project-other-tab-command)
    ("TAB 1"   . my/tab-goto-1)
    ("TAB 2"   . my/tab-goto-2)
    ("TAB 3"   . my/tab-goto-3)
    ("TAB 4"   . my/tab-goto-4)
    ("TAB 5"   . my/tab-goto-5))

  ;; Open Dired buffer with backspace
  (define-key evil-normal-state-map (kbd "DEL") #'dired-jump)

  ;; Select new window after evil split)
  (defun my/other-window (&rest _)
    (other-window 1))
  (advice-add 'evil-window-split  :after #'my/other-window)
  (advice-add 'evil-window-vsplit :after #'my/other-window)

  ;; Use @p to paste with a space before the inserted text (let @p="a \<Esc>p")
  (evil-set-register ?p [?a ?\s escape ?p]))

(use-package evil-collection
  :after evil
  :demand t
  :config
  (evil-collection-init)

  ;; unimpaired-like bindings
  (evil-collection-define-key 'normal 'evil-collection-unimpaired-mode-map
    "[h" #'diff-hl-previous-hunk
    "]h" #'diff-hl-next-hunk
    "[d" #'flymake-goto-prev-error
    "]d" #'flymake-goto-next-error)

  ;; comint mode
  (evil-collection-define-key 'insert 'comint-mode-map
    (kbd "C-p") #'comint-previous-input
    (kbd "C-n") #'comint-next-input)

  ;; corfu mode
  (evil-collection-define-key 'insert 'corfu-map
    (kbd "C-y") #'corfu-insert
    (kbd "C-e") #'evil-collection-corfu-quit-and-escape)

  ;; custom mode
  (evil-collection-define-key 'normal 'custom-mode-map
    (kbd "C-p") #'evil-previous-line
    (kbd "C-n") #'evil-next-line
    (kbd "<mouse-1>") #'widget-button-click)

  ;; dashboard mode
  (evil-collection-define-key 'normal 'dashboard-mode-map
    (kbd "C-p") #'dashboard-previous-line
    (kbd "C-n") #'dashboard-next-line)

  ;; dired mode
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "C-p") #'dired-previous-line
    (kbd "C-n") #'dired-next-line)

  ;; elfeed modes
  (evil-collection-define-key 'normal 'elfeed-search-mode-map
    (kbd "C-p") #'evil-previous-line
    (kbd "C-n") #'evil-next-line)
  (evil-collection-define-key 'normal 'elfeed-show-mode-map
    (kbd "C-p") #'evil-previous-line
    (kbd "C-n") #'evil-next-line)

  ;; ibuffer mode
  (evil-collection-define-key 'normal 'ibuffer-mode-map
    (kbd "C-p") #'evil-previous-line
    (kbd "C-n") #'evil-next-line))

(use-package evil-anzu
  :after evil
  :demand t
  :config
  (global-anzu-mode +1))

(use-package evil-args
  :after evil
  :demand t
  :config
  (define-key evil-inner-text-objects-map "a" #'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" #'evil-outer-arg))

(use-package evil-commentary
  :after evil
  :demand t
  :config
  (define-key evil-normal-state-map "gc" #'evil-commentary))

;; Usage: gx MOTION (twice or . to repeat), gxx to select line, gX to cancel
(use-package evil-exchange
  :after evil
  :demand t
  :config
  (evil-exchange-install))

;; TODO: evaluate replacement (e.g. quickrun.el)
;; TODO: make this work with julia-repl
(use-package evil-extra-operator
  :after evil
  :demand t
  :config
  (define-key evil-motion-state-map "gy" #'evil-operator-eval))

(use-package evil-goggles
  :after evil
  :demand t
  :config
  ;; Also pulse on evil-operator-eval
  (add-to-list 'evil-goggles--commands
               '(evil-operator-eval :face evil-goggles-default-face :switch t :advice evil-goggles--generic-async-advice))
  (evil-goggles-use-diff-faces)
  (evil-goggles-mode +1))

(use-package evil-indent-plus
  :after evil
  :demand t
  :config
  (define-key evil-inner-text-objects-map "i" #'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" #'evil-indent-plus-a-indent)
  (define-key evil-inner-text-objects-map "I" #'evil-indent-plus-i-indent-up)
  (define-key evil-outer-text-objects-map "I" #'evil-indent-plus-i-indent-up)
  (define-key evil-inner-text-objects-map "J" #'evil-indent-plus-a-indent-up-down)
  (define-key evil-outer-text-objects-map "J" #'evil-indent-plus-a-indent-up-down))

;; Usage: ga MOTION CHAR or ga MOTION / REGEX
(use-package evil-lion
  :after evil
  :demand t
  :config
  (define-key evil-normal-state-map "ga" #'evil-lion-left)
  (define-key evil-normal-state-map "gA" #'evil-lion-right)
  (define-key evil-visual-state-map "ga" #'evil-lion-left)
  (define-key evil-visual-state-map "gA" #'evil-lion-right))

;; TODO: make evil-variable-segment package
;; https://github.com/Julian/vim-textobj-variable-segment/blob/main/autoload/textobj/variable_segment.vim
(use-package emacs
  :after evil
  :demand t
  :config
  (setq evil-variable-segment-left-inner-regex
        (rx (or
             (seq (one-or-more "_") (group alnum))
             (group word-start)
             (seq lower (group upper))
             (seq alpha (group digit))
             (seq digit (group alpha)))))
  (setq evil-variable-segment-right-inner-regex
        (rx (or
             (seq (group not-newline) (one-or-more "_"))
             (seq (group lower) upper)
             (seq (group alpha) digit)
             (seq (group digit) alpha)
             (group (any alnum "_") word-end))))
  (setq evil-variable-segment-left-outer-regex
        (rx (or
             (group (one-or-more "_") alnum)
             (group word-start)
             (seq lower (group upper))
             (seq alpha (group digit))
             (seq digit (group alpha)))))
  (setq evil-variable-segment-right-outer-regex
        (rx (or
             (group (one-or-more "_"))
             (seq (group lower) upper)
             (seq (group alpha) digit)
             (seq (group digit) alpha)
             (group (any alnum "_") word-end))))

  (defun evil-variable-segment--get-first-matched-group ()
    "Return list with the positions (beginning and end) of the first matched group from match-data."
    (let ((matched-groups (cdr (cdr (match-data t))))
          (beg nil) (end nil))
      (while (and (not beg) (not end))
        (setq beg (pop matched-groups))
        (setq end (pop matched-groups)))
      (list beg end)))

  (defun evil-variable-segment--find-range (left-re right-re)
    "Return [beg, end) range defined by text object."
    (save-match-data
      (let ((case-fold-search nil) (beg nil) (end nil))
         (save-excursion
           (forward-char)
           (re-search-backward left-re)
           (setq beg (car (evil-variable-segment--get-first-matched-group)))
           (goto-char (+ beg 1))
           (re-search-forward right-re)
           (setq end (car (cdr (evil-variable-segment--get-first-matched-group))))
           ;; If there are '_' characters on both sides of the range, select only the right one
           (if (and (eq (char-after beg) ?_) (eq (char-before end) ?_))
               (setq beg (+ beg 1))))
         (list beg end))))

  (evil-define-text-object evil-variable-segment-inner (count &optional beg end type)
    (evil-variable-segment--find-range evil-variable-segment-left-inner-regex evil-variable-segment-right-inner-regex))
  (evil-define-text-object evil-variable-segment-outer (count &optional beg end type)
    (evil-variable-segment--find-range evil-variable-segment-left-outer-regex evil-variable-segment-right-outer-regex))

  (define-key evil-inner-text-objects-map "v" #'evil-variable-segment-inner)
  (define-key evil-outer-text-objects-map "v" #'evil-variable-segment-outer))

(use-package evil-matchit
  :after evil
  :demand t
  :config
  (global-evil-matchit-mode +1))

(use-package evil-mc
  :after evil
  :commands evil-mc-mode
  :init
  ;; evil-mc will be loaded after the first usage of "g.", which will then be remapped
  (define-key evil-normal-state-map "g." #'evil-mc-mode)
  :config
  (define-key evil-mc-cursors-map (kbd "<mouse-1>") #'evil-mc-toggle-cursor-on-click)
  (general-def
    :prefix "g"
    "." '(:ignore t :which-key "evil-mc")))

(use-package evil-multiedit
  :after evil
  :demand t
  :custom
  (evil-multiedit-follow-matches t)
  (iedit-toggle-key-default nil)
  :config
  (evil-multiedit-default-keybinds)
  ;; Use readline-like bindings in insert state
  (define-key evil-insert-state-map (kbd "M-d") #'kill-word))

;; Only works in Emacs state (C-z)
(use-package multiple-cursors
  :after evil
  :demand t
  :custom
  (mc/always-run-for-all t)
  :config
  ;; Create cursors on each line of active region
  (define-key evil-emacs-state-map (kbd "C-S-c C-S-c") #'mc/edit-lines)
  ;; Create cursor on next/previous line or active region
  (define-key evil-emacs-state-map (kbd "C->") #'mc/mark-next-like-this)
  (define-key evil-emacs-state-map (kbd "C-<") #'mc/mark-previous-like-this)
  ;; Create cursor on mouse click
  (define-key evil-emacs-state-map (kbd "C-S-<mouse-1>") #'mc/add-cursor-on-click)
  ;; Delete all cursors when exiting Emacs state
  (add-hook 'evil-emacs-state-exit-hook #'mc/keyboard-quit))

(use-package evil-numbers
  :after evil
  :demand t
  :config
  (define-key evil-normal-state-map "g-" #'evil-numbers/dec-at-pt)
  (define-key evil-normal-state-map "g=" #'evil-numbers/inc-at-pt))

(use-package evil-org
  :after (evil org)
  :hook
  (org-mode . evil-org-mode)
  :custom
  (evil-org-key-theme '(navigation insert textobjects additional return))
  (evil-org-retain-visual-state-on-shift t)
  (evil-org-use-additional-insert t)
  :config
  (evil-org-set-key-theme))

;; Use C-f and C-b to scroll the evil-owl buffer
(use-package evil-owl
  :after evil
  :demand t
  :custom
  (evil-owl-display-method 'window)
  (evil-owl-idle-delay 0.25)
  :config
  (add-to-list 'display-buffer-alist
               '("\\*evil-owl\\*"
                 (display-buffer-in-side-window) (side . bottom) (slot . -1) (window-height . 0.3)))
  (evil-owl-mode +1))

(use-package evil-quickscope
  :after evil
  :hook
  (conf-mode . turn-on-evil-quickscope-always-mode)
  (prog-mode . turn-on-evil-quickscope-always-mode)
  (text-mode . turn-on-evil-quickscope-always-mode))

(use-package evil-snipe
  :after evil
  :demand t
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
  :demand t
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
  :demand t
  :config
  (define-key evil-inner-text-objects-map "e" #'evil-entire-entire-buffer)
  (define-key evil-outer-text-objects-map "e" #'evil-entire-entire-buffer))

;; TODO: evaluate Doom Emacs' tree-sitter config: https://github.com/doomemacs/doomemacs/blob/master/modules/tools/tree-sitter/config.el
(use-package evil-textobj-tree-sitter
  :after (evil tree-sitter)
  :demand t
  :config
  ;; This wrapper function prevents eager expansion
  ;; Reference: https://github.com/doomemacs/doomemacs/commit/84d47016d0eb26f5eae37c1de13c16717dc0f090
  (defun my/evil-textobj-tree-sitter-get-textobj (group &optional query)
    (eval `(evil-textobj-tree-sitter-get-textobj ,group ,query)))

  ;; TODO: evaluate creating text objects for loops, parameters and comments
  ;; FIXME: text objects are not working
  (define-key evil-inner-text-objects-map "f" (my/evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "f" (my/evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "c" (my/evil-textobj-tree-sitter-get-textobj "class.inner"))
  (define-key evil-outer-text-objects-map "c" (my/evil-textobj-tree-sitter-get-textobj "class.outer"))

  ;; Goto next start
  (define-key evil-normal-state-map (kbd "]]") (lambda () (interactive)
                                                 (if tree-sitter-mode
                                                     (evil-textobj-tree-sitter-goto-textobj "function.outer" nil nil)
                                                   (evil-forward-section-begin))))
  (define-key evil-normal-state-map (kbd "]c") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" nil nil)))
  ;; Goto next end
  (define-key evil-normal-state-map (kbd "][") (lambda () (interactive)
                                                 (if tree-sitter-mode
                                                     (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)
                                                   (evil-forward-section-end))))
  (define-key evil-normal-state-map (kbd "]C") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" nil t)))
  ;; Got previous start
  (define-key evil-normal-state-map (kbd "[[") (lambda () (interactive)
                                                 (if tree-sitter-mode
                                                     (evil-textobj-tree-sitter-goto-textobj "function.outer" t nil)
                                                   (evil-backward-section-begin))))
  (define-key evil-normal-state-map (kbd "[c") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" t nil)))
  ;; Goto previous end
  (define-key evil-normal-state-map (kbd "[]") (lambda () (interactive)
                                                 (if tree-sitter-mode (evil-textobj-tree-sitter-goto-textobj "function.outer" t t)
                                                   (evil-backward-section-end))))
  (define-key evil-normal-state-map (kbd "[C") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" t t))))

(use-package evil-visualstar
  :after evil
  :demand t
  :config
  (define-key evil-visual-state-map "*" #'evil-visualstar/begin-search-forward)
  (define-key evil-visual-state-map "#" #'evil-visualstar/begin-search-backward))

(use-package ace-window
  :after evil
  :demand t
  :config
  (define-key evil-window-map "a" #'ace-window))

(use-package avy
  :after evil
  :demand t
  :custom
  (avy-all-windows nil)
  (avy-keys '(?a ?s ?d ?h ?j ?k ?l))
  (avy-single-candidate-jump t)
  :init
  (defun my/evil-avy-goto-char-timer ()
    (interactive)
    ;; Calling avy functions with an argument negates the current setting of 'avy-all-windows'
    (avy-goto-char-timer t))
  (general-def
    :prefix "g"
    "s" '(:ignore t :which-key "avy-goto"))
  :config
  (define-key evil-normal-state-map "gsl" #'evil-avy-goto-line)
  (define-key evil-normal-state-map "gsw" #'evil-avy-goto-word-1)
  (define-key evil-normal-state-map "gss" #'my/evil-avy-goto-char-timer))

(use-package transpose-frame
  :commands (transpose-frame flip-frame flop-frame))

(use-package winner
  :after evil
  :demand t
  :custom
  (winner-dont-bind-my-keys t)
  :config
  (winner-mode +1)
  (define-key evil-window-map "u" #'winner-undo)
  (define-key evil-window-map "U" #'winner-redo))
;; TODO: evaluate tab-bar-history-mode

;; TODO: evaluate packages
;; evil-cleverparens
;; evil-embrace
;; evil-quick-diff
;; evil-replace-with-register
;; evil-string-inflection
;; evil-vimish-fold
;; exato
;; text-objects (targets/things)



;;; Editing helps

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package string-inflection
  :commands (string-inflection-camelcase-function
             string-inflection-kebabcase-function
             string-inflection-pascal-case-function
             string-inflection-underscore-function
             string-inflection-upcase-function))

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode)
  :config
  (general-def
    :prefix "C-c"
    "&" '(:ignore t :which-key "yas")))

(use-package yasnippet-snippets
  :after yasnippet
  :demand t)

(use-package ws-butler
  :hook
  (conf-mode . ws-butler-mode)
  (prog-mode . ws-butler-mode)
  (text-mode . ws-butler-mode))

;; TODO: evaluate packages
;; autopairs
;; hl-todo
;; smartparens/evil-smartparens
;; puni
;; vundo
;; tempel
;; which-func
;; whitespace-mode
;; olivetti



;;; Language support

;; LSP
;; TODO: add LSP evil-mode mappings (e.g. from lsp-zero.nvim)
(use-package eglot
  :ensure nil
  :commands eglot
  :init
  ;; Reference: https://github.com/noctuid/evil-guide/blob/master/README.org#why-dont-keys-defined-with-evil-define-key-work-immediately
  (add-hook 'eglot-managed-mode-hook #'evil-normalize-keymaps)
  :config
  (general-def
    :prefix "C-c"
    "l" '(:ignore t :which-key "eglot"))
  (bind-keys
    :prefix "C-c l"
    :prefix-map eglot-mode-map
    ("a" . eglot-code-actions)
    ("d" . xref-find-definitions)
    ("D" . xref-find-definitions-other-window)
    ("f" . eglot-format)
    ("r" . eglot-rename)
    ("R" . xref-find-references)
    ("s" . consult-eglot-symbols)
    ("S" . eglot-shutdown)))

(use-package consult-eglot
  :after (eglot consult)
  :commands consult-eglot-symbols)


;; tree-sitter
;; TODO: prevent writing on Messages buffer during initialization
;; TODO: switch language modes to tree-sitter alternatives
(use-package tree-sitter
  :hook
  (haskell-mode . tree-sitter-mode)
  ;; TODO: try new grammar @ https://github.com/tree-sitter/tree-sitter-julia
  ;; (julia-mode . tree-sitter-mode)
  (python-mode . tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)


;; Language documentation
;; TODO: set devdocs-current-docs variable using dir-local variables
(use-package devdocs
  :after popper
  :bind
  ("<help> D" . devdocs-lookup)
  :commands (devdocs-lookup devdocs-peruse devdocs-install devdocs-update-all)
  :config
  (add-to-list 'display-buffer-alist
               '("\\*devdocs\\*"
                 (display-buffer-in-side-window) (side . right) (slot . 0) (window-width . 0.35))))


;; CLI Debugger
;; Emacs bindings:
;; - C-x C-q  realgud-short-key-mode
;; - F10      step over (next)
;; - F11      step into
;; - S-F11    step out
;; - F5       continue execution
;; - S-F5     quit
;; - F9       set breakpoint
;; - M-up     older history
;; - M-down   newer history
;; Evil bindings:
;; - n       step over (Next)
;; - s,i     Step Into
;; - f,o     step Out (Finish)
;; - c       Continue
;; - q       Quit debugger
;; - Q       kill debugger
;; - r,R,gr  Restart execution
;; - b       set Breakpoint
;; - X       clear breakpoint (at current line)
;; - D       Delete breakpoint (by number)
;; - +,-     enable/disable breakpoint
;; - U       continue Until a posterior line is reached
;; - H       continue until Here
;; - J       Jump here
;; - <,>     move to older/newer frame
;; - u,d     move to older/newer frame
;; - 1-9     go to position #
;; - e,E     Evaluate expression (dwim/at-point)
;; - T       show current call stack (Trace)
;; - g?      show debugger information
;; - gR      Recenter window around arrow
(use-package realgud
  :after evil-collection
  :custom
  (realgud-window-split-orientation 'horizontal)
  :commands realgud:pdb
  :init
  ;; For some reason, the evil-collection realgud bindings are not working on normal mode.
  ;; As a workaround, use Emacs mode instead (C-z).
  ;; TODO: fix this
  (advice-add #'realgud-short-key-mode-setup :before
              (lambda (modeon) (if modeon (evil-emacs-state) (evil-normal-state))))
  ;; Display realgud in new tab
  (advice-add #'realgud:run-debugger :before
              (lambda (debugger-name &rest args)
                (let ((tab-index (tab-bar--tab-index-by-name debugger-name)))
                  (if tab-index
                      (tab-bar-select-tab (1+ tab-index))
                    (tab-bar-new-tab)
                    (tab-bar-rename-tab debugger-name)))))
  :config
  ;; TODO: "o" not working fix this
  (evil-collection-define-key 'emacs 'realgud:shortkey-mode-map "o" 'realgud:cmd-finish))
;; TODO: evaluate realgud-ipdb, realgud-lldb, trepan3k, ipdb, pdbpp


;; Emacs Lisp
(when ON-MAC
  ;; Compile parinfer manually and copy it to Emacs home:
  ;;   git clone https://github.com/eraserhd/parinfer-rust.git
  ;;   cd parinfer-rust
  ;;   cargo build --release --features emacs
  ;;   cp ./target/release/libparinfer_rust.dylib ~/.emacs.d/parinfer-rust
  ;; Reference: https://github.com/justinbarclay/parinfer-rust-mode/issues/41#issuecomment-1496907286
  ;; TODO: automate this
  (setq parinfer-rust-library "~/.emacs.d/parinfer-rust/libparinfer_rust.dylib"))
(use-package parinfer-rust-mode
  :hook
  (emacs-lisp-mode . parinfer-rust-mode)
  (lisp-mode . parinfer-rust-mode)
  :custom
  (parinfer-rust-auto-download t)
  (parinfer-rust-prefered-mode 'smart))


;; Python

;; References:
;; - https://wikemacs.org/wiki/Python
;; - https://www.emacswiki.org/emacs/PythonProgrammingInEmacs
;; - https://github.com/doomemacs/doomemacs/blob/master/modules/lang/python
;; - https://develop.spacemacs.org/layers/+lang/python/README.html
;; - https://elpa.gnu.org/packages/python.html

;; TODO: improve documentation display when coding (e.g. explain function arguments)
;; TODO: evaluate comint-mime
(use-package python
  :ensure nil
  :custom
  (python-check-command "epylint")
  (python-indent-offset 4)
  (python-shell-font-lock-enable nil)
  (python-shell-interpreter "python")  ; On Windows, some virtual environments don't come with the "python3" binary
  :mode
  ("\\.py\\'" . python-mode)
  :config
  (if ON-WINDOWS (setenv "PYTHONIOENCODING" "UTF-8"))
  (general-def
    :prefix "C-c"
    :major-modes 'python-mode
    "C-t" '(:ignore t :which-key "python-skeleton"))
  (add-to-list 'display-buffer-alist
               '("\\*Python\\*"
                 (display-buffer-reuse-window display-buffer-same-window))))

(use-package anaconda-mode
  :after python
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode)
  :config
  (add-to-list 'display-buffer-alist
          '("\\*Anaconda\\*"
            (display-buffer-in-side-window) (side . right) (slot . 0) (window-width . 0.35))))

(use-package blacken
  :after python
  :commands (blacken-buffer blacken-mode))

(use-package python-isort
  :after python
  :custom
  (python-isort-arguments '("--stdout" "--atomic" "--profile black" "-"))
  :commands (python-isort-buffer python-isort-on-save-mode))

;; TODO: evaluate alternative for virtual environment management (https://mclare.blog/posts/using-uv-in-emacs)
;; TODO: evaluate purcell/envrc
(unless ON-WINDOWS
  (use-package poetry
    :after python
    :hook
    (python-mode . poetry-tracking-mode)
    :custom
    (poetry-tracking-strategy 'switch-buffer)
    :bind
    (:map python-mode-map
     ("C-c P" . poetry))))

(use-package pytest
  :after python
  :bind
  (:map python-mode-map
    ("C-c t t" . pytest-again)
    ("C-c t 1" . pytest-one)
    ("C-c t p" . pytest-pdb-one)
    ("C-c t a" . pytest-all)
    ("C-c t A" . pytest-pdb-all)
    ("C-c t m" . pytest-module)
    ("C-c t M" . pytest-pdb-module)
    ("C-c t d" . pytest-directory)
    ("C-c t D" . pytest-pdb-directory)
    ("C-c t f" . pytest-last-failed)
    ("C-c t F" . pytest-pdb-last-failed))
  :custom
  (pytest-cmd-flags "--exitfirst --capture=no --durations=10")
  (pytest-project-root-files '(".venv" "setup.py" ".git"))
  :init
  (general-def
    :prefix "C-c"
    :major-modes 'python-mode
    "t" '(:ignore t :which-key "pytest"))
  :config
  (add-to-list 'display-buffer-alist
               '("\\*pytest\\*"
                 (display-buffer-in-side-window) (side . right) (slot . 1) (window-width . 0.35))))

(use-package rst
  :ensure nil
  :mode
  ("\\.rst\\'" . rst-mode)
  :config
  (add-hook 'rst-mode-hook #'my/set-underscore-as-word)
  (add-hook 'rst-mode-hook #'turn-on-auto-fill)  ; Also use M-q (fill-paragraph) to reset paragraph shape
  (add-hook 'rst-mode-hook #'ws-butler-mode)
  (general-def
    :prefix "C-c"
    :major-modes 'rst-mode
    "C-a" '(:ignore t :which-key "adjust")
    "C-c" '(:ignore t :which-key "compile")
    "C-l" '(:ignore t :which-key "list")
    "C-r" '(:ignore t :which-key "region")
    "C-t" '(:ignore t :which-key "toc")))

;; TODO: add mypy backend for flymake?
;; TODO: evaluate ein (emacs-ipython-notebook) and emacs-jupyter
;; TODO: evaluate lpy
;; TODO: evaluate python-x
;; TODO: evaluate python-mls
;; TODO: evaluate https://github.com/andcarnivorous/pyconf


;; Julia
(use-package julia-mode
  :mode
  ("\\.jl\\'" . julia-mode)
  :interpreter "julia")

(use-package eglot-jl
  :after julia-mode
  :hook
  (julia-mode . eglot-jl-init)
  :init
  (defun my/eglot-jl-format-buffer ()
    (if (eglot-managed-p) (eglot-format-buffer)))
  :config
  (add-hook 'before-save-hook #'my/eglot-jl-format-buffer))

;; Julia REPL usage: C-c C-z (raise REPL), C-c C-a (activate project),
;; C-c C-b (send buffer), C-c C-c (send region or line), C-c C-d (invoke @doc))
;; TODO: use same bindings as python/ess
(unless ON-WINDOWS
  (use-package julia-repl
    :after julia-mode
    :hook
    (julia-mode . julia-repl-mode)
    :config
    (add-to-list 'display-buffer-alist
                 '("\\*julia\\*"
                   (display-buffer-reuse-window display-buffer-same-window)))
    (julia-repl-set-terminal-backend 'vterm)))

;; TODO: evaluate julia-snail


;; ESS
(use-package ess
  :mode
  ("\\.[rR]\\'" . R-mode)
  ("\\.[rR]nw\\'" . Rnw-mode)
  :custom
  (ess-use-toolbar nil)
  :config
  (require 'ess-site))

(use-package poly-R
  :mode
  ("\\.[rR]md\\'" . poly-markdown+r-mode))


;; Lua
(use-package lua-mode
  :mode
  ("\\.lua\\'" . lua-mode))


;; Vimrc
(use-package vimrc-mode
  :mode
  ("\\.vim\\(rc\\)?\\'" . vimrc-mode))


;; Fish
(use-package fish-mode
  :mode
  ("\\.fish\\'" . fish-mode))


;; JavaScript
(use-package js2-mode
  :mode
  ("\\.js\\'" . js2-mode))


;; Haskell
(unless ON-WINDOWS
  (use-package haskell-mode
    :custom
    (haskell-process-suggest-remove-import-lines t)
    :mode
    ("\\.hs\\'" . haskell-mode)))


;; Markdown
(use-package markdown-mode
  :mode
  ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :custom
  (markdown-command "pandoc")
  :config
  (general-def
    :prefix "C-c"
    :major-modes 'markdown-mode
    "C-a" '(:ignore t :which-key "links")
    "C-c" '(:ignore t :which-key "commands")
    "C-s" '(:ignore t :which-key "styling-headings")
    "C-t" '(:ignore t :which-key "insert-header")
    "C-x" '(:ignore t :which-key "toggle")))


;; LaTeX
;; Useful functions: (repunctuate-sentences)
(when ON-LINUX
  ;; Documentation: https://www.gnu.org/software/auctex/manual/auctex.index.html
  (use-package tex
    :ensure auctex
    :mode
    ("\\.tex\\'" . LaTeX-mode)
    :custom
    (TeX-PDF-mode t)
    (TeX-check-TeX t)
    (TeX-check-engine t)
    ;; Parse on save and load
    (TeX-auto-save t)
    (TeX-parse-self t)
    ;; Use hidden directories for AUCTeX files
    (TeX-auto-local ".auctex-auto")
    (TeX-style-local ".auctex-style")
    ;; Pause on errors
    (TeX-interactive-mode t)
    ;; Correlate tex and pdf files
    (TeX-source-correlate-mode t)
    (TeX-source-correlate-start-server nil)
    ;; Do not ask before saving file
    (TeX-save-query nil)
    ;; TeX electricity
    (TeX-electric-math (cons "$" "$"))
    (TeX-electric-sub-and-superscript t)
    (LaTeX-electric-left-right-brace t)
    :config
    (general-def
      :prefix "C-c"
      :major-modes 'latex-mode
      "C-o"     '(:ignore t :which-key "TeX-fold")
      "C-p"     '(:ignore t :which-key "preview")
      "C-p C-c" '(:ignore t :which-key "preview-clearout")
      "C-q"     '(:ignore t :which-key "LaTeX-fill")
      "C-t"     '(:ignore t :which-key "LaTeX-toggle"))
    ;; Set Emacs' PDF Tools as default
    (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
    ;; Enable corfu completion
    (add-hook 'LaTeX-mode-hook #'corfu-mode)
    ;; Misc configuration
    (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))  ; Reference: https://pdftools.wiki/24b671c6

  ;; Documentation: https://www.gnu.org/software/auctex/manual/preview-latex.index.html
  (use-package preview
    :ensure auctex
    :after tex
    :hook
    (LaTeX-mode . LaTeX-preview-setup)
    :custom
    (preview-auto-cache-preamble t)
    (preview-scale-function 1.1))

  ;; Documentation: https://www.gnu.org/software/auctex/manual/reftex.index.html
  (use-package reftex
    :ensure auctex
    :after tex
    :hook
    (LaTeX-mode . turn-on-reftex)
    :custom
    (reftex-plug-into-AUCTeX t))

  (use-package evil-tex
    :after (evil tex)
    :hook
    (LaTeX-mode . evil-tex-mode))

  (use-package pdf-tools
    :mode
    ("\\.pdf\\'" . pdf-view-mode)
    :init
    (setq-default pdf-view-display-size 'fit-page)
    :config
    ;; On Windows, build in mingw64 with command build/server/autobuild
    (pdf-tools-install)
    (add-hook 'pdf-tools-enabled-hook #'pdf-view-themed-minor-mode)
    (general-def
      :prefix "C-c"
      :major-modes 'pdf-view-mode
      "C-a" '(:ignore t :which-key "pdf-annot")
      "C-r" '(:ignore t :which-key "pdf-view"))
    ;; Fix blinkg cursor arounf PDF in evil-mode
    ;; Reference: https://github.com/doomemacs/doomemacs/pull/1107
    (add-hook 'pdf-view-mode-hook (lambda () (set (make-local-variable 'evil-normal-state-cursor) (list nil))))
    :mode-hydra
    (pdf-view-mode (:title "PDF View")
                   ("Zoom"
                    (("=" pdf-view-enlarge              :exit nil)
                     ("-" pdf-view-shrink               :exit nil)
                     ("0" pdf-view-scale-reset          :exit nil)
                     ("H" pdf-view-fit-height-to-window :exit nil)
                     ("P" pdf-view-fit-page-to-window   :exit nil)
                     ("W" pdf-view-fit-width-to-window  :exit nil))
                    "Color modes"
                    (("d" pdf-view-dark-minor-mode      :exit nil)
                     ("m" pdf-view-midnight-minor-mode  :exit nil)
                     ("p" pdf-view-printer-minor-mode   :exit nil)
                     ("t" pdf-view-themed-minor-mode    :exit nil))))))


;; TODO: evaluate latex related packages
;; reftex/bibtex (https://www.gnu.org/software/auctex/manual/reftex.index.html)
;; latex-lsp/texlab
;; company-auctex/company-reftex/company-math
;; latex + flymake
;; magic-latex-buffer
;; adaptive-wrap
;; prettify-symbols-mode for custom commands (e.g. \R)


;; XML
(use-package xml-format
  :after nxml-mode
  :commands (xml-format-buffer xml-format-region xml-format-on-save-mode))


;; CSV
(use-package csv-mode
  :mode
  ("\\.csv\\'" . csv-mode)
  :custom
  (csv-separators '("," ";" "\t"))
  :config
  (add-hook 'csv-mode-hook #'csv-guess-set-separator))


;; SQL

;; The sql-product variable needs to be set (using local variable or sql-set-product function)
;; TODO: auto-select interactive buffer
(use-package sql
  :ensure nil
  :custom
  (sql-sqlite-options '("-interactive"))
  :config
  (modify-syntax-entry ?_ "w" sql-mode-syntax-table)
  (add-to-list 'display-buffer-alist
               '("\\*SQL: "
                 (display-buffer-reuse-window display-buffer-same-window)))
  (general-def
    :prefix "C-c"
    :major-modes 'sql-mode
    "C-l" '(:ignore t :which-key "list")))

(use-package sql-indent
  :hook
  (sql-mode . sqlind-minor-mode)
  :init
  (setq-default sqlind-basic-offset 4))

(use-package sqlup-mode
  :hook
  (sql-mode . sqlup-mode))

(use-package sqlformat
  :custom
  (sqlformat-command 'sqlfluff)
  :commands (sqlformat sqlformat-buffer))

;; TODO: evaluate ejc-sql (JDBC client)



;;; Org-mode

(use-package org
  :defer t
  :config
  (general-def
    :prefix "C-c"
    :major-modes 'org-mode
    "\""  '(:ignore t :which-key "plot")
    "C-v" '(:ignore t :which-key "babel")
    "C-x" '(:ignore t :which-key "org")))

(when ON-LINUX
  (use-package org-inline-pdf
    :hook
    (org-mode . org-inline-pdf-mode)))

;; TODO: evaluate packages
;; denote



;;; Obsidian

(unless ON-WINDOWS
  (use-package obsidian
    :demand t
    :bind
    (:map obsidian-mode-map
      ("C-c C-o" . obsidian-follow-link-at-point)
      ("C-c C-b" . obsidian-backlink-jump)
      ("C-c C-l" . obsidian-insert-wikilink))
    :custom
    (obsidian-directory (file-truename "~/Obsidian"))
    (obsidian-inbox-directory "Inbox")
    :config
    (global-obsidian-mode +1)))



;;; Chezmoi

;; TODO: add ediff and magit extensions
(use-package chezmoi
  :commands (chezmoi-find chezmoi-open-other))



;;; Git

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
                 (display-buffer-in-tab) (tab-name . "magit")))
  (add-hook 'after-save-hook #'magit-after-save-refresh-status)
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (magit-wip-mode +1))

;; TODO: evaluate git-gutter-fringe
(use-package diff-hl
  :hook
  (conf-mode . diff-hl-mode)
  (prog-mode . diff-hl-mode)
  (text-mode . diff-hl-mode)
  (dired-mode . diff-hl-dired-mode))



;;; Fuzzy search & completion

(use-package vertico
  :demand t
  :custom
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (vertico-count 20)
  (vertico-cycle t)
  :config
  (vertico-mode +1))

(use-package marginalia
  :after vertico
  :demand t
  :config
  (marginalia-mode +1))

(use-package orderless
  :after vertico
  :demand t
  :custom
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (completion-styles '(orderless basic partial-completion)))

(use-package nerd-icons-completion
  :after (vertico marginalia)
  :demand t
  :config
  (nerd-icons-completion-mode +1))

;; Use M-n to insert symbol or thing at point into the input
(use-package consult
  :demand t
  :bind
  ("C-x C-r" . consult-recent-file)
  ([remap bookmark-jump] . consult-bookmark)
  ([remap project-switch-to-buffer] . consult-project-buffer)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (consult-narrow-key "<")
  (consult-widen-key ">")
  :init
  (general-def
    :prefix "C-c"
    "c" '(:ignore t :which-key "consult"))
  (bind-keys
    :prefix "C-c c"
    :prefix-map my/consult-prefix-map
    ("c" . consult-buffer)
    ("b" . consult-buffer)
    ("f" . consult-find)                ; find file by name
    ("g" . consult-ripgrep)             ; search for regexes in files
    ("G" . consult-git-grep)            ; search for regexes in git tracked files
    ("o" . consult-outline)
    ("i" . consult-imenu)
    ("l" . consult-line)                ; search for lines in current buffer
    ("L" . consult-line-multi)          ; search for lines in all buffer
    ("m" . consult-mark)                ; search for mark
    ("y" . consult-yank-from-kill-ring) ; search for previous yanks
    ("F" . consult-flymake)
    ("C" . consult-complex-command)     ; find commands in command-history
    ("H" . consult-history)             ; find commands in current buffer history (e.g. eshell or comint)
    ("I" . consult-info)))              ; search info pages (full text search)

(use-package embark
  :demand t
  :bind
  ("C-;"      . embark-act)
  ("C-:"      . embark-dwim)
  ("<help> B" . embark-bindings)
  :config
  (add-to-list 'display-buffer-alist
               '("\\*Embark Actions\\*"
                 (display-buffer-in-side-window) (side . left) (slot . 1) (window-width . 0.4))))

(use-package embark-consult
  :after (embark consult)
  :demand t)

;; TODO: evaluate corfu vs company
;; TODO: disable corfu when using multicursors
;; TODO: evaluate how to use corfu (or vertico) in evil-ex minibuffer
;; TODO: enable corfu in comint case by case (e.g. eshell, inferior python, inferior elisp)
(use-package corfu
  :after vertico
  :hook
  (prog-mode . corfu-mode)
  (conf-mode . corfu-mode)
  ;; comint-modes
  (eshell-mode . corfu-mode)
  (inferior-elisp-mode . corfu-mode)
  (inferior-ess-mode . corfu-mode)
  (inferior-python-mode . corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-count 15)
  (corfu-cycle t)
  (corfu-popupinfo-max-height 15)
  (corfu-min-width 30)
  (corfu-quit-no-match 'separator)
  :config
  (add-hook 'corfu-mode-hook #'corfu-history-mode)
  (add-hook 'corfu-mode-hook #'corfu-popupinfo-mode))

(use-package kind-icon
  :after corfu
  :demand t
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; TODO: restrict to prog-mode
(use-package cape
  :after corfu
  :demand t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (general-def
    :prefix "C-c"
    "p" '(:ignore t :which-key "cape"))
  (bind-keys
    :prefix "C-c p"
    :prefix-map my/cape-prefix-map
    ("p" . completion-at-point)
    ("d" . cape-dabbrev)
    ("h" . cape-history)
    ("f" . cape-file)
    ("k" . cape-keyword)
    ("s" . cape-symbol)
    ("i" . cape-ispell)
    ("l" . cape-line)
    ("t" . cape-tex)
    ("&" . cape-sgml)
    ("r" . cape-rfc1345)))

;; TODO: evaluate packages
;; eval-in-repl
;; isend-mode
;; quickrun
