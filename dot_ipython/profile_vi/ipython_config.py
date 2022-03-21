# pylint: disable=C0111
c = c  # noqa: F821 pylint: disable=E0602,C0103,W0127

## Configuration reference:
## - https://ipython.readthedocs.io/en/stable/config/options/terminal.html

## Autoindent IPython code entered interactively.
#  Default: True
c.InteractiveShell.autoindent = False

## Shortcut style to use at the prompt. 'vi' or 'emacs'.
#  Default: 'emacs'
c.TerminalInteractiveShell.editing_mode = 'vi'

## Use 24bit colors instead of 256 colors in prompt highlighting.
# Default: False
c.TerminalInteractiveShell.true_color = True
