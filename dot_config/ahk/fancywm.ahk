; FancyWM Script for AutoHotkey
; References:
; - https://github.com/FancyWM/fancywm/wiki/Scripting
; - https://www.autohotkey.com/docs/v2/Hotkeys.htm
; - https://www.autohotkey.com/docs/v2/KeyList.htm
; - https://www.ishaat.ca/posts/2023/02/11/my-ultimate-windows-10-window-management-setup
; - https://github.com/dankrusi/WindowsVirtualDesktopHelper
; - https://github.com/Ciantic/VirtualDesktopAccessor

; Switch to the selected virtual desktop (Alt + #).
!1::Run "fancywm.exe --action SwitchToDesktop1"
!2::Run "fancywm.exe --action SwitchToDesktop2"
!3::Run "fancywm.exe --action SwitchToDesktop3"
!4::Run "fancywm.exe --action SwitchToDesktop4"
!5::Run "fancywm.exe --action SwitchToDesktop5"
!6::Run "fancywm.exe --action SwitchToDesktop6"
!7::Run "fancywm.exe --action SwitchToDesktop7"
!8::Run "fancywm.exe --action SwitchToDesktop8"
!9::Run "fancywm.exe --action SwitchToDesktop9"
!Backspace:: Run "fancywm.exe --action SwitchToPreviousDesktop"

; Move the focused window to the selected virtual desktop (Alt + Shift + #).
!+1::Run "fancywm.exe --action MoveToDesktop1"
!+2::Run "fancywm.exe --action MoveToDesktop2"
!+3::Run "fancywm.exe --action MoveToDesktop3"
!+4::Run "fancywm.exe --action MoveToDesktop4"
!+5::Run "fancywm.exe --action MoveToDesktop5"
!+6::Run "fancywm.exe --action MoveToDesktop6"
!+7::Run "fancywm.exe --action MoveToDesktop7"
!+8::Run "fancywm.exe --action MoveToDesktop8"
!+9::Run "fancywm.exe --action MoveToDesktop9"
!+Backspace:: Run "fancywm.exe --action MoveToPreviousDesktop"

; Move the focus to an adjacent window (Alt + hjkl).
!h::Run "fancywm.exe --action MoveFocusLeft"
!j::Run "fancywm.exe --action MoveFocusDown"
!k::Run "fancywm.exe --action MoveFocusUp"
!l::Run "fancywm.exe --action MoveFocusRight"

; Swap the focused window (Alt + Shift + hjkl).
!+h::Run "fancywm.exe --action SwapLeft"
!+j::Run "fancywm.exe --action SwapDown"
!+k::Run "fancywm.exe --action SwapUp"
!+l::Run "fancywm.exe --action SwapRight"

; Move the focused window (Alt + Control + hjkl).
!^h::Run "fancywm.exe --action MoveLeft"
!^j::Run "fancywm.exe --action MoveDown"
!^k::Run "fancywm.exe --action MoveUp"
!^l::Run "fancywm.exe --action MoveRight"

; Switch to monitor (Alt + wer).
!w::Run "fancywm.exe --action SwitchToDisplay2"
!e::Run "fancywm.exe --action SwitchToDisplay3"
!r::Run "fancywm.exe --action SwitchToDisplay1"

; Move the focused window to monitor (Alt + Shift + wer).
!+w::Run "fancywm.exe --action MoveToDisplay2"
!+e::Run "fancywm.exe --action MoveToDisplay3"
!+r::Run "fancywm.exe --action MoveToDisplay1"

; Close windows.
!+c::SendInput "!{F4}"

