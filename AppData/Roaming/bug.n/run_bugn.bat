@echo off

set CONFIG_PATH=%USERPROFILE%\AppData\Roaming\bug.n

del /Q %CONFIG_PATH%\data\*.ini
copy %CONFIG_PATH%\Layout.ini %CONFIG_PATH%\data\_Layout.ini

start /B C:\apps\bug.n\bugn.exe
