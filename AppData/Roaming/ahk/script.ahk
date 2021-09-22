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


; Workspacer key bindings.
; Workaround for https://github.com/workspacer/workspacer/issues/110
#j::SendInput #!j
#k::SendInput #!k
#m::SendInput #!m
#t::SendInput #!t
;#Enter::SendInput #!{Enter}
;#Backspace::SendInput #!{Backspace}
#0::SendInput #!0
#1::SendInput #!1
#2::SendInput #!2
#3::SendInput #!3
#4::SendInput #!4
#5::SendInput #!5
#6::SendInput #!6
#7::SendInput #!7
#8::SendInput #!8
#9::SendInput #!9

