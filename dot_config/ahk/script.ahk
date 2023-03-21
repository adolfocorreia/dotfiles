; Makes CapsLock become Ctrl. Press both left and right Shift keys to turn CapsLock on/off.
; Reference: https://www.autohotkey.com/docs/misc/Remap.htm
;LShift & RShift::CapsLock
;RShift & LShift::CapsLock
;CapsLock::Ctrl


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


; Remap Alt+D to Ctrl+L in Firefox.
#if WinActive("ahk_exe firefox.exe")
  !d::SendInput ^l
#if


; Disable problematic Windows bindings.
; Reference: https://support.microsoft.com/en-us/windows/keyboard-shortcuts-in-windows-dcc61a57-8ff0-cffe-9796-cb9706c75eec
#Tab::
#Up::
#Down::
#Left::
#Right::
#+Left::
#+Right::
#^d::


; Workspacer bindings.
#0::#!0
#1::#!1
#2::#!2
#3::#!3
#4::#!4
#5::#!5
#6::#!6
#7::#!7
#8::#!8
#9::#!9
#g::#!g
#n::#!n
#z::#!z
#q::#!q


; Multimedia keys remapping.
; Browser_Home
; Volume_Down
; Volume_Up
; Volume_Mute
; Media_Play_Pause
; Media_Stop
; Media_Prev
; Media_Next
; Launch_Media
; Launch_Mail
; Ctrl+Shift+Esc
Launch_App2::Run speedcrunch.exe

