;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;;;;;;;;;;;;;;;;;;;;;;
;; General settings ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Enable menu bar
(menu-bar-mode +1)

;; Set the number of lines of margin at the top and bottom of windows.
(setq scroll-margin 5)

;; Uses visible buffer as search scope and highlight matches
(setq evil-snipe-scope 'whole-visible)

;; Enable 80th column indicator for specific modes
(add-hook 'emacs-lisp-mode-hook (lambda () (display-fill-column-indicator-mode +1)))
(add-hook 'julia-mode-hook      (lambda () (display-fill-column-indicator-mode +1)))
(add-hook 'text-mode-hook       (lambda () (display-fill-column-indicator-mode +1)))


;;;;;;;;;;;;;;;;;;;;
;; Julia settings ;;
;;;;;;;;;;;;;;;;;;;;

;; Set number of threads used by Julia
(setenv "JULIA_NUM_THREADS" "15")

;; Workaround for "no method matching LanguageServer.FoldingRangeCapabilities"
;; error.
;; References:
;; - https://github.com/non-Jedi/lsp-julia/issues/23
;; - https://github.com/non-Jedi/lsp-julia/issues/35
(setq lsp-enable-folding t)
(setq lsp-folding-range-limit 100)

;; For some odd reason, julia cannot instantiate the LSP environment when
;; Project.toml is a symbolic link. As a workaround, rename the link and create
;; a new Project.toml file with the same contents in the environment path.
;; The default lsp-julia LanguagerServer.jl installation is located at:
;; ~/.emacs.doom/.local/straight/build-27.2/lsp-julia/languageserver

;; Use vterm with julia-repl
(after! julia-repl (julia-repl-set-terminal-backend 'vterm))
