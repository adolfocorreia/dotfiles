;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(setq! doom-font (font-spec :family "Iosevka Term" :size 14)
       doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq! doom-theme 'doom-tokyo-night)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq! display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq! org-directory "~/org")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.



;;;;;;;;;;;;;;;;;;;;;;
;; General settings ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Load environment variables.
(doom-load-envvars-file "~/.doom.d/doom_env")

;; Enable menu bar.
(menu-bar-mode +1)

;; Set the number of lines of margin at the top and bottom of windows.
(setq! scroll-margin 5)

;; Consider _ as part of a word (for specific modes).
(add-hook 'prog-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

;; Enable 80th column indicator (for specific modes).
(add-hook 'prog-mode-hook (lambda () (display-fill-column-indicator-mode +1)))
(add-hook 'text-mode-hook (lambda () (display-fill-column-indicator-mode +1)))

;; Enable eww as default browser.
(setq! browse-url-browser-function 'eww-browse-url)


;;;;;;;;;;;;;;;;;;;
;; Evil settings ;;
;;;;;;;;;;;;;;;;;;;

;; Switch to new window after splitting.
(setq! evil-split-window-below t
       evil-vsplit-window-right t)

;; Use visible buffer as search scope and highlight matches.
(setq! evil-snipe-scope 'whole-visible)

;; Prevent o/O keys from continuing comments.
(setq! +evil-want-o/O-to-continue-comments nil)

;; Provide paste above ([p) and below (]p) unimpaired mappings in evil-mode.
;; Reference: evil-unimpaired.el / Spacemacs
(defun evil-unimpaired-paste-above ()
  (interactive)
  (evil-insert-newline-above)
  (evil-paste-after 1 evil-this-register))
(defun evil-unimpaired-paste-below ()
  (interactive)
  (evil-insert-newline-below)
  (evil-paste-after 1 evil-this-register))
(after! evil
        (map!
         :n "[p" #'evil-unimpaired-paste-above
         :n "]p" #'evil-unimpaired-paste-below))


;;;;;;;;;;;;;;;;;;;;;;
;; Package settings ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Always invalidate projectile cache when switching projects.
(add-hook 'projectile-after-switch-project-hook
          (lambda () (projectile-invalidate-cache nil)))


;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode settings ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Hide emphasis characters (e.g. *, /, =).
(setq! org-hide-emphasis-markers nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python mode settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Notes:
;; - Doom Emacs uses the Emacs 'lsp-python-ms' package as a client for both
;;   pylsp (Spyder IDE's) and mspyls (Microsoft's) LSP servers.
;; - Microsoft's LSP server implementation is written in C# and is deprecated
;;   in favor of Pyright/Pylance.
;; References:
;; - https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/python/README.org
;; - https://develop.spacemacs.org/layers/+lang/python/README.html
;; - https://pypi.org/project/python-lsp-server
;; - https://github.com/python-lsp/python-lsp-server
;; - https://emacs-lsp.github.io/lsp-python-ms
;; - https://github.com/emacs-lsp/lsp-python-ms


;;;;;;;;;;;;;;;;;;;;
;; Julia settings ;;
;;;;;;;;;;;;;;;;;;;;

;; Workaround for "no method matching LanguageServer.FoldingRangeCapabilities" error.
;; References:
;; - https://github.com/non-Jedi/lsp-julia/issues/23
;; - https://github.com/non-Jedi/lsp-julia/issues/35
;; (setq-hook! 'julia-mode-hook
;;         lsp-enable-folding t
;;         lsp-folding-range-limit 100)

;; It is necessary to add LanguageServer.jl and SymbolServer.jl to the default
;; Julia environment (e.g. @v1.6) for LSP to work.


;;;;;;;;;;;;;;;;
;; R settings ;;
;;;;;;;;;;;;;;;;

;; It is necessary to have the 'styler' package available (globally installed)
;; in order for code formatting to take place when saving R buffers. The 'gQ'
;; evil keybing can also be used to format a region.
