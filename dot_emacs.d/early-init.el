;;; early-init.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; Check the system used
(defconst ON-LINUX   (eq system-type 'gnu/linux))
(defconst ON-MAC     (eq system-type 'darwin))
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;; Increase garbage collector threshold during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Prefer newest version when loading elisp files
(setq load-prefer-newer t)

;; Change location of the native compilation cache (Emacs 29+)
(when (native-comp-available-p)
  (startup-redirect-eln-cache (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory))))

;; Set title template for visible frames
(setq-default frame-title-format '(multiple-frames "%b" ("" "%b - GNU Emacs")))

;; Start frames maximized and with no window decorations (including title bar)
(when ON-LINUX
  (add-to-list 'default-frame-alist '(undecorated . t)))
(when ON-MAC
  (add-to-list 'default-frame-alist '(height . 70))
  (add-to-list 'default-frame-alist '(width . 240)))
(when ON-WINDOWS
  (add-to-list 'default-frame-alist '(height . 50))
  (add-to-list 'default-frame-alist '(width . 200)))
(setq frame-inhibit-implied-resize t)

;; Avoid black screen at startup (bg/fg colors taken from doom-tokyo-night theme)
(add-to-list 'default-frame-alist '(background-color . "#1a1b26"))
(add-to-list 'default-frame-alist '(foreground-color . "#a9b1d6"))

;; Remove UI elements during startup
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

;; Disable scroll-bar on minibuffer window
;; Reference: https://www.gnu.org/software/emacs/manual/html_node/elisp/Scroll-Bars.html
(add-hook 'after-make-frame-functions
          (lambda (frame) (set-window-scroll-bars (minibuffer-window frame) 0 nil 0 nil t)))

;; Make UTF-8 the default coding system
(set-language-environment "UTF-8")
(setq default-input-method nil)
