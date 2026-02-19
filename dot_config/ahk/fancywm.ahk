; FancyWM Script for AutoHotkey
; References:
; - https://github.com/FancyWM/fancywm/wiki/Scripting
; - https://www.autohotkey.com/docs/v2/Hotkeys.htm
; - https://www.autohotkey.com/docs/v2/KeyList.htm
; - https://www.ishaat.ca/posts/2023/02/11/my-ultimate-windows-10-window-management-setup
; - https://github.com/dankrusi/WindowsVirtualDesktopHelper
; - https://github.com/Ciantic/VirtualDesktopAccessor
; - https://github.com/MScholtes/PSVirtualDesktop

#Requires AutoHotkey v2.0

#SingleInstance Force

; Enable "Snap windows" setting (see https://github.com/FancyWM/fancywm/commit/f03b2445286e049bd76590062de63d8fcbeb93d8)
EnableSnap() {
    SPI_SETWINARRANGING := 0x0083
    SPIF_SENDCHANGE := 2
    NULL := 0
    DllCall("SystemParametersInfo",
        "UInt", SPI_SETWINARRANGING,
        "UInt", 1,
        "UInt", NULL,
        "UInt", SPIF_SENDCHANGE
    )
}

FancyWM(action) {
    RunWait(format("fancywm.exe --action {}", action), , "Hide")
    EnableSnap()
}


; Switch to the selected virtual desktop (Alt + #).
!1::FancyWM("SwitchToDesktop1")
!2::FancyWM("SwitchToDesktop2")
!3::FancyWM("SwitchToDesktop3")
!4::FancyWM("SwitchToDesktop4")
!5::FancyWM("SwitchToDesktop5")
!6::FancyWM("SwitchToDesktop6")
!7::FancyWM("SwitchToDesktop7")
!8::FancyWM("SwitchToDesktop8")
!9::FancyWM("SwitchToDesktop9")
!Backspace::FancyWM("SwitchToPreviousDesktop")

; Move the focused window to the selected virtual desktop (Alt + Shift + #).
!+1::FancyWM("MoveToDesktop1")
!+2::FancyWM("MoveToDesktop2")
!+3::FancyWM("MoveToDesktop3")
!+4::FancyWM("MoveToDesktop4")
!+5::FancyWM("MoveToDesktop5")
!+6::FancyWM("MoveToDesktop6")
!+7::FancyWM("MoveToDesktop7")
!+8::FancyWM("MoveToDesktop8")
!+9::FancyWM("MoveToDesktop9")
!+Backspace:: FancyWM("MoveToPreviousDesktop")

; Move the focus to an adjacent window (Alt + hjkl).
!h::FancyWM("MoveFocusLeft")
!j::FancyWM("MoveFocusDown")
!k::FancyWM("MoveFocusUp")
!l::FancyWM("MoveFocusRight")

; Swap the focused window (Alt + Shift + hjkl).
!+h::FancyWM("SwapLeft")
!+j::FancyWM("SwapDown")
!+k::FancyWM("SwapUp")
!+l::FancyWM("SwapRight")

; Move the focused window (Alt + Control + hjkl).
!^h::FancyWM("MoveLeft")
!^j::FancyWM("MoveDown")
!^k::FancyWM("MoveUp")
!^l::FancyWM("MoveRight")

; Switch to monitor (Alt + wer).
; Move the focused window to monitor (Alt + Shift + wer).
#Include "monitors.ahk"

; Close windows.
!+q::SendInput "!{F4}"
!+c::SendInput "!{F4}"


; Available commands:
; FancyWM("ToggleManager")            ; Toggle FancyWM's window management functionality
; FancyWM("RefreshWorkspace")         ; Force a change-detection cycle of the whole workspace
; FancyWM("Cancel")                   ; Cancel the action
; FancyWM("MoveFocusLeft")            ; Focus the window to the left of the current window
; FancyWM("MoveFocusUp")              ; Focus the window above the current window
; FancyWM("MoveFocusRight")           ; Focus the window to the right of the current window
; FancyWM("MoveFocusDown")            ; Focus the window below the current window
; FancyWM("ShowDesktop")              ; Show/hide the desktop
; FancyWM("CreateHorizontalPanel")    ; Create a new horizontal panel around the focused window
; FancyWM("CreateVerticalPanel")      ; Create a new vertical panel around the focused window
; FancyWM("CreateStackPanel")         ; Create a new stack panel around the focused window
; FancyWM("PullWindowUp")             ; Move the window out of its containing panel
; FancyWM("ToggleFloatingMode")       ; Toggle floating mode for the focused window
; FancyWM("MoveLeft")                 ; Move the window next to the window on the left
; FancyWM("MoveUp")                   ; Move the window next to the window above
; FancyWM("MoveRight")                ; Move the window next to the window on the right
; FancyWM("MoveDown")                 ; Move the window next to the window below
; FancyWM("SwapLeft")                 ; Swap the current window with the window on its left
; FancyWM("SwapUp")                   ; Swap the current window with the window above it
; FancyWM("SwapRight")                ; Swap the current window with the window on its right
; FancyWM("SwapDown")                 ; Swap the current window with the window below it
; FancyWM("IncreaseWidth")            ; Increase the width of the focused window
; FancyWM("IncreaseHeight")           ; Increase the height of the focused window
; FancyWM("DecreaseWidth")            ; Decrease the width of the focused window
; FancyWM("DecreaseHeight")           ; Decrease the height of the focused window
; FancyWM("SwitchToPreviousDesktop")  ; Switch from the current virtual desktop to the previous virtual desktop
; FancyWM("SwitchToLeftDesktop")      ; Switch from the current virtual desktop to the virtual desktop on the left
; FancyWM("SwitchToRightDesktop")     ; Switch from the current virtual desktop to the virtual desktop on the right
; FancyWM("SwitchToDesktop1")         ; Switch from the current virtual desktop to virtual desktop 1
; FancyWM("SwitchToDesktop2")         ; Switch from the current virtual desktop to virtual desktop 2
; FancyWM("SwitchToDesktop3")         ; Switch from the current virtual desktop to virtual desktop 3
; FancyWM("SwitchToDesktop4")         ; Switch from the current virtual desktop to virtual desktop 4
; FancyWM("SwitchToDesktop5")         ; Switch from the current virtual desktop to virtual desktop 5
; FancyWM("SwitchToDesktop6")         ; Switch from the current virtual desktop to virtual desktop 6
; FancyWM("SwitchToDesktop7")         ; Switch from the current virtual desktop to virtual desktop 7
; FancyWM("SwitchToDesktop8")         ; Switch from the current virtual desktop to virtual desktop 8
; FancyWM("SwitchToDesktop9")         ; Switch from the current virtual desktop to virtual desktop 9
; FancyWM("MoveToPreviousDesktop")    ; Move the focused window from the current virtual desktop to the previous virtual desktop
; FancyWM("MoveToLeftDesktop")        ; Move the focused window from the current virtual desktop to virtual desktop on the left
; FancyWM("MoveToRightDesktop")       ; Move the focused window from the current virtual desktop to virtual desktop on the right
; FancyWM("MoveToDesktop1")           ; Move the focused window from the current virtual desktop to virtual desktop 1
; FancyWM("MoveToDesktop2")           ; Move the focused window from the current virtual desktop to virtual desktop 2
; FancyWM("MoveToDesktop3")           ; Move the focused window from the current virtual desktop to virtual desktop 3
; FancyWM("MoveToDesktop4")           ; Move the focused window from the current virtual desktop to virtual desktop 4
; FancyWM("MoveToDesktop5")           ; Move the focused window from the current virtual desktop to virtual desktop 5
; FancyWM("MoveToDesktop6")           ; Move the focused window from the current virtual desktop to virtual desktop 6
; FancyWM("MoveToDesktop7")           ; Move the focused window from the current virtual desktop to virtual desktop 7
; FancyWM("MoveToDesktop8")           ; Move the focused window from the current virtual desktop to virtual desktop 8
; FancyWM("MoveToDesktop9")           ; Move the focused window from the current virtual desktop to virtual desktop 9
; FancyWM("SwitchToPreviousDisplay")  ; Switch from the current display to the previous display
; FancyWM("SwitchToDisplay1")         ; Switch from the current display to display 1
; FancyWM("SwitchToDisplay2")         ; Switch from the current display to display 2
; FancyWM("SwitchToDisplay3")         ; Switch from the current display to display 3
; FancyWM("SwitchToDisplay4")         ; Switch from the current display to display 4
; FancyWM("SwitchToDisplay5")         ; Switch from the current display to display 5
; FancyWM("SwitchToDisplay6")         ; Switch from the current display to display 6
; FancyWM("SwitchToDisplay7")         ; Switch from the current display to display 7
; FancyWM("SwitchToDisplay8")         ; Switch from the current display to display 8
; FancyWM("SwitchToDisplay9")         ; Switch from the current display to display 9
; FancyWM("MoveToPreviousDisplay")    ; Move the focused window from the current display to the previous display
; FancyWM("MoveToDisplay1")           ; Move the focused window from the current display to display 1
; FancyWM("MoveToDisplay2")           ; Move the focused window from the current display to display 2
; FancyWM("MoveToDisplay3")           ; Move the focused window from the current display to display 3
; FancyWM("MoveToDisplay4")           ; Move the focused window from the current display to display 4
; FancyWM("MoveToDisplay5")           ; Move the focused window from the current display to display 5
; FancyWM("MoveToDisplay6")           ; Move the focused window from the current display to display 6
; FancyWM("MoveToDisplay7")           ; Move the focused window from the current display to display 7
; FancyWM("MoveToDisplay8")           ; Move the focused window from the current display to display 8
; FancyWM("MoveToDisplay9")           ; Move the focused window from the current display to display 9
