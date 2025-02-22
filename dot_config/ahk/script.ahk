; Makes CapsLock become Ctrl. Press both left and right Shift keys to turn CapsLock on/off.
; Reference: https://www.autohotkey.com/docs/misc/Remap.htm
LShift & RShift::CapsLock
RShift & LShift::CapsLock
CapsLock::Ctrl


; Maps Right Control key using scan code.
; References:
; - https://www.autohotkey.com/docs/KeyList.htm#SpecialKeys
; - http://www.quadibloc.com/comp/scan.htm
;SC073::RCtrl


; PowerLauncher key bindings.
#HotIf WinActive("ahk_exe PowerToys.PowerLauncher.exe")
  ^j::SendInput "{Blind^}{Down}"
  ^k::SendInput "{Blind^}{Up}"
  ^n::SendInput "{Blind^}{Down}"
  ^p::SendInput "{Blind^}{Up}"
  ^m::SendInput "{Enter}"
#HotIf


; Swallow Insert on Outlook.
#HotIf WinActive("ahk_exe olk.exe")
  Insert::return
#HotIf
#HotIf WinActive("ahk_exe OUTLOOK.EXE")
  Insert::return
#HotIf


; JupyterLab remaps (override Chrome/Edge default bindings).
; - Ctrl-W to Ctrl-K (vim)
; - Ctrl-P to Up (readline)
#HotIf WinActive("JupyterLab ahk_exe chrome.exe")
  ^w::SendInput "^k"
  ^p::SendInput "{Blind^}{Up}"
#HotIf
#HotIf WinActive("JupyterLab ahk_exe msedge.exe")
  ^w::SendInput "^k"
  ^p::SendInput "{Blind^}{Up}"
#HotIf


; Remap Alt+D to Ctrl+L on Firefox.
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


#Include "fancywm.ahk"
