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
#HotIf WinActive("ahk_class keypirinha_wndcls_run")
  ^j::SendInput "{Blind^}{Down}"
  ^k::SendInput "{Blind^}{Up}"
  ^n::SendInput "{Blind^}{Down}"
  ^p::SendInput "{Blind^}{Up}"
#HotIf


; Remap Alt+D to Ctrl+L in Firefox.
#HotIf WinActive("ahk_exe firefox.exe")
  !d::SendInput "^l"
#HotIf


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
; Launch_App2

