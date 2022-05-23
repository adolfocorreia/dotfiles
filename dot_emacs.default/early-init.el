; Disable scroll-bar on minibuffer window
; Reference: https://www.gnu.org/software/emacs/manual/html_node/elisp/Scroll-Bars.html
(add-hook 'after-make-frame-functions
	  (lambda (frame) (set-window-scroll-bars (minibuffer-window frame) 0 nil 0 nil t)))

; Avoid black screen at startup (bg/fg colors taken from doom-tokyo-night theme)
(add-to-list 'default-frame-alist '(background-color . "#1a1b26"))
(add-to-list 'default-frame-alist '(foreground-color . "#a9b1d6"))

; Start frame at maximum width and height and with no title bar
(add-to-list 'default-frame-alist '(fullscreen . fullboth))
