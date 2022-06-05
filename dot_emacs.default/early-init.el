;; Increase garbage collector threshold during startup
(setq gc-cons-threshold most-positive-fixnum)

; Set title template for visible frames
(setq-default frame-title-format '(multiple-frames "%b" ("" "%b - GNU Emacs")))

; Start frame at maximum width and height and with no title bar
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

; Avoid black screen at startup (bg/fg colors taken from doom-tokyo-night theme)
(add-to-list 'default-frame-alist '(background-color . "#1a1b26"))
(add-to-list 'default-frame-alist '(foreground-color . "#a9b1d6"))

; Disable scroll-bar on minibuffer window
; Reference: https://www.gnu.org/software/emacs/manual/html_node/elisp/Scroll-Bars.html
(add-hook 'after-make-frame-functions
	  (lambda (frame) (set-window-scroll-bars (minibuffer-window frame) 0 nil 0 nil t)))

; Make UTF-8 the default coding system
(set-language-environment "UTF-8")
(setq default-input-method nil)
