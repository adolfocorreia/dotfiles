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
  ^j::SendInput {Down}
  ^k::SendInput {Up}
  ^n::SendInput {Down}
  ^p::SendInput {Up}
#if


; Disable Alt+<number> Notes key bindings.
; Reference: https://stackoverflow.com/questions/5954682/create-lotus-notes-hotkey-shortcut-key-for-reply-to-all-with-history
#if WinActive("ahk_exe notes2.exe")
  !1::
  !2::
  !3::
  !4::
  !5::
  !6::
  !7::
  !8::
  !9::
#if


; Workspacer key bindings.
; Workaround for https://github.com/workspacer/workspacer/issues/110
#j::SendInput #!j
#k::SendInput #!k
#m::SendInput #!m
#t::SendInput #!t
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

; Disable problematic Windows bindings.
; Reference: https://support.microsoft.com/en-us/windows/keyboard-shortcuts-in-windows-dcc61a57-8ff0-cffe-9796-cb9706c75eec
#Tab::
#Up::
#Down::
#Left::
#Right::
#+Left::
#+Right::

