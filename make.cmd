@echo off
set LAZDIR=D:\lazarus
set PATHOPT=-Fusrc\test -Fusrc\test\base -Fusrc\test\neuron -Fusrc\main -Fusrc\main\base -Fusrc\main\neuron
set OPT32=-MDelphi -B -Scghi -CX -O3 -XX -l -vewnhibq -Filib\i386-win32 %PATHOPT% -Fu%LAZDIR%\components\fpcunit\lib\i386-win32\win32 -Fu%LAZDIR%\components\synedit\units\i386-win32\win32 -Fu%LAZDIR%\components\lazcontrols\lib\i386-win32\win32 -Fu%LAZDIR%\lcl\units\i386-win32\win32 -Fu%LAZDIR%\lcl\units\i386-win32 -Fulib\i386-win32 -Fu%LAZDIR%\components\lazutils\lib\i386-win32 -Fu%LAZDIR%\packager\units\i386-win32 -Fu. -FUlib\i386-win32 -FElib\i386-win32\ -dLCL -dLCLwin32
set OPT64=-B -Twin64 -Px86_64 -MDelphi -Scghi -CX -O3 -XX -l -vewnhibq -Filib\x86_64-win64 %PATHOPT% -Fu%LAZDIR%\components\fpcunit\lib\x86_64-win64\win32 -Fu%LAZDIR%\components\synedit\units\x86_64-win64\win32 -Fu%LAZDIR%\components\lazcontrols\lib\x86_64-win64\win32 -Fu%LAZDIR%\lcl\units\x86_64-win64\win32 -Fu%LAZDIR%\lcl\units\x86_64-win64 -Fulib\x86_64-win64 -Fu%LAZDIR%\components\lazutils\lib\x86_64-win64 -Fu%LAZDIR%\packager\units\x86_64-win64 -Fu. -FUlib\x86_64-win64 -FElib\x86_64-win64\ -dLCL -dLCLwin32
if "%1"=="-64" (set OPT=%OPT64%) else set OPT=%OPT32%
set FPC_PATH=%LAZDIR%\fpc\3.0.0\bin\i386-win32\
%FPC_PATH%fpc %OPT% dmbasepackage.pas
if %ERRORLEVEL% NEQ 0 goto ERROR
%FPC_PATH%fpc %OPT% DmBaseIntfPackage.pas
if %ERRORLEVEL% NEQ 0 goto ERROR
%FPC_PATH%fpc %OPT% dmbasennetwork.pas
if %ERRORLEVEL% NEQ 0 goto ERROR
%FPC_PATH%fpc %OPT% fpcunitproject1.lpr
goto :FINISH
:ERROR
echo Compilation Error
pause
goto :EOF
:FINISH
echo ALL DONE