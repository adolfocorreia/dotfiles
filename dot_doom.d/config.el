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
(setq doom-font (font-spec :family "Iosevka" :size 14)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(when IS-LINUX
  (setq doom-theme 'doom-gruvbox))
(when IS-WINDOWS
  (setq doom-theme 'doom-tomorrow-night))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org")

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

; Load environment variables.
(doom-load-envvars-file "~/.doom.d/doom_env")

;; Enable menu bar.
(menu-bar-mode +1)

;; Copy mouse selections to clipboard.
(setq mouse-drag-copy-region t)

;; Flash modeline on errors.
(doom-themes-visual-bell-config)

;; Set the number of lines of margin at the top and bottom of windows.
(setq scroll-margin 5)

;; Make Emacs ask about loading unsafe local variables (dir-locals).
;; Doom Emacs configuration changes this variable from its default setting
;; to silently ignore unsafe local variables.
(setq enable-local-variables t)

;; Use system's trash can.
(setq delete-by-moving-to-trash t)

;; Set idle delay (in seconds) until completion starts.
(setq company-idle-delay 1.0)

;; Consider _ as part of a word (for specific modes).
(add-hook 'prog-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

;; Enable 80th column indicator (for specific modes).
(add-hook 'prog-mode-hook (lambda () (display-fill-column-indicator-mode +1)))
(add-hook 'text-mode-hook (lambda () (display-fill-column-indicator-mode +1)))

;; Enable eww as default browser.
(setq browse-url-browser-function 'eww-browse-url)


;;;;;;;;;;;;;;;;;;;
;; Evil settings ;;
;;;;;;;;;;;;;;;;;;;

;; Switch to new window after splitting.
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Make hints (ophints module) pulse.
(setq evil-goggles-pulse t)

;; Use visible buffer as search scope and highlight matches.
(setq evil-snipe-scope 'whole-visible)

;; Prevent o/O keys from continuing comments.
(setq +evil-want-o/O-to-continue-comments nil)

;; evil-escape with both "jk" and "kj".
(setq evil-escape-unordered-key-sequence t)

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

;; Use evil operator (g~) to cycle text objects through camelCase, kebab-case,
;; snake_case and UPPER_CASE.
(use-package! evil-string-inflection
  :after evil)


;;;;;;;;;;;;;;;;;;;;;;
;; Package settings ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Doom Emacs vterm module configuration adds a hook to hide the modeline in
;; every vterm buffer. The command below reverts this setting. Note that
;; popup-rules may still inhibit the modeline from appearing.
(after! vterm
  (remove-hook 'vterm-mode-hook #'hide-mode-line-mode))

;; Enable midnight mode by default in pdf buffers.
(add-hook 'pdf-tools-enabled-hook (lambda () (pdf-view-midnight-minor-mode +1)))

;; Always invalidate projectile cache when switching projects.
(add-hook 'projectile-after-switch-project-hook
          (lambda () (projectile-invalidate-cache nil)))

;; Show event and command history on a dedicated buffer.
(use-package! command-log-mode
  :commands (command-log-mode global-command-log-mode))

;; Directory diff tool.
(use-package! ztree
  :commands (ztree-diff ztree-dir))

;; vimrc-mode.
(use-package! vimrc-mode
  :mode ("\\.vim\\'" "\\.vimrc\\'"))


;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode settings ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Hide emphasis characters (e.g. *, /, =).
(setq org-hide-emphasis-markers nil)

;; Set org-roam directory.
(setq org-roam-directory "~/org/roam")

;; Increase the scale of LaTeX fragments.
(after! org
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 2.5)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python mode settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set default Python interpreter.
(setq-hook! 'python-mode-hook
  python-shell-interpreter "python3")

;; Enable UTF-8 mode for Python in Windows.
;; Reference: https://www.python.org/dev/peps/pep-0540
(if (eq system-type 'windows-nt)
    (setenv "PYTHONUTF8" "1"))

;; Popup rule for Python REPL buffer.
(after! python
  (set-popup-rule! "^\\*Python\\*$"
    :actions '(display-buffer-reuse-window
               display-buffer-in-previous-window
               display-buffer-same-window)
    :quit nil
    :select t
    :modeline t))


;;;;;;;;;;;;;;;;;;;;
;; Julia settings ;;
;;;;;;;;;;;;;;;;;;;;

;; Use vterm with julia-repl.
(after! julia-repl (when IS-LINUX
                     (julia-repl-set-terminal-backend 'vterm)))

;; Popup rule for Julia REPL buffer.
(after! julia-repl
  (set-popup-rule! "^\\*julia:.*"  ; e.g. "*julia:workspace*"
    :actions '(display-buffer-reuse-window
               display-buffer-in-previous-window
               display-buffer-same-window)
    :quit nil
    :select t
    :modeline t))

;; Workaround for "no method matching LanguageServer.FoldingRangeCapabilities" error.
;; References:
;; - https://github.com/non-Jedi/lsp-julia/issues/23
;; - https://github.com/non-Jedi/lsp-julia/issues/35
(setq-hook! 'julia-mode-hook
        lsp-enable-folding t
        lsp-folding-range-limit 100)

;; It is necessary to add LanguageServer.jl and SymbolServer.jl to the default
;; Julia environment (e.g. @v1.6) for LSP to work.
