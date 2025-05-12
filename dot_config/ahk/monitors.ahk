; Log function
WriteLog(text) {
    logFile := EnvGet("TEMP") . "\monitors.log"
    FileAppend(A_Now . ": " . text . "`n", logFile, "`n")
}

; Get logical displays and their physical order from x-coordinate (left to right)
; The equivalent PowerShell command to get monitor data is:
;   > Add-Type -AssemblyName System.Windows.Forms ; [System.Windows.Forms.Screen]::AllScreens
; See also: mstsc /l)
monitors := Map()
monitorCount := MonitorGetCount()
WriteLog("Monitor count: " . monitorCount)
Loop monitorCount {
    MonitorGet(A_Index, &left, &top, &right, &bottom)
    monitorName := MonitorGetName(A_Index)  ; e.g., \\.\DISPLAY1
    fancyWmID := SubStr(monitorName, -1, 1)
    monitor := {AhkID: A_Index, FancyWmID: fancyWmID, X: left, Y: top, Name: monitorName}
    monitors[monitor.X] := monitor
    WriteLog("Monitor info: ID: #" . fancyWmID . ", Name: '" . monitorName . "', Left: " . left . ", Top: " . top)
}

orderedMonitors := Array()
for x, monitor in monitors {
  orderedMonitors.Push(monitor)
}


; Return function (closure) to be bound to hotkeys
; (see https://www.autohotkey.com/docs/v2/Functions.htm#closures)
FancyFunction(action) {
    runAction(key) {
        Run("fancywm.exe --action " . action)
    }
    return runAction
}

; Bind w, e and r monitor shortcuts
keys := ["w", "e", "r"]
Loop monitorCount {
    key := keys[A_Index]
    fancyId := orderedMonitors[A_Index].FancyWmID
    Hotkey("!" . key, FancyFunction("SwitchToDisplay" . fancyId))
    Hotkey("!+" . key, FancyFunction("MoveToDisplay" . fancyId))
}

; TODO: create some mechanism to handle monitor connection/disconnection events
