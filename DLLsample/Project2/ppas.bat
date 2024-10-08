@echo off
SET THEFILE=C:\Users\catik\source\repos\bellyoshi\Lazarus-learning\DLLsample\Project2\project1.exe
echo Linking %THEFILE%
C:\lazarus\fpc\3.2.2\bin\i386-win32\ld.exe -b pei-i386 -m i386pe  --gc-sections    --entry=_mainCRTStartup    -o C:\Users\catik\source\repos\bellyoshi\Lazarus-learning\DLLsample\Project2\project1.exe C:\Users\catik\source\repos\bellyoshi\Lazarus-learning\DLLsample\Project2\link8240.res
if errorlevel 1 goto linkend
C:\lazarus\fpc\3.2.2\bin\i386-win32\postw32.exe --subsystem console --input C:\Users\catik\source\repos\bellyoshi\Lazarus-learning\DLLsample\Project2\project1.exe --stack 16777216
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occurred while assembling %THEFILE%
goto end
:linkend
echo An error occurred while linking %THEFILE%
:end
