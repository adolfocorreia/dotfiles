; Disable scroll-bar on minibuffer window
; Reference: https://www.gnu.org/software/emacs/manual/html_node/elisp/Scroll-Bars.html
(add-hook 'after-make-frame-functions
	  (lambda (frame)
	    (set-window-scroll-bars (minibuffer-window frame) 0 nil 0 nil t)))
