; Makes CapsLock become Ctrl. Press both left and right Shift keys to turn CapsLock on/off.
; Reference: https://www.autohotkey.com/docs/misc/Remap.htm
LShift & RShift::CapsLock
RShift & LShift::CapsLock
CapsLock::Ctrl


; Maps Right Control key using scan code.
; References:
; - https://www.autohotkey.com/docs/KeyList.htm#SpecialKeys
; - http://www.quadibloc.com/comp/scan.htm
SC073::RCtrl


; Keypirinha key bindings.
; Reference: https://github.com/Keypirinha/Keypirinha/issues/477
#if WinActive("ahk_class keypirinha_wndcls_run")
^j::Send {Down}
^k::Send {Up}
^n::Send {Down}
^p::Send {Up}
#if
