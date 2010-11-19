@echo off
setlocal
for /f "usebackq tokens=*" %%i in (`cd`) do @set PWD=%%i
if "%1" equ "" goto doit
cd /D "%1"
:doit
vim -c SimpleTapRunDir
cd %PWD%
endlocal
