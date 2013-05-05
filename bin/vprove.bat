@echo off

set VPROVE_TESTING=1

cd /D "%1"
if %ERRORLEVEL% neq 0 goto err
vim -c SimpleTapRunDir
cd /D %~dp0
:err
