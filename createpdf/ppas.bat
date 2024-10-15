@echo off
SET THEFILE=C:\Users\catik\source\repos\bellyoshi\Lazarus-learning\createpdf\project1.exe
echo Linking %THEFILE%
C:\lazarus\fpc\3.2.2\bin\x86_64-win64\ld.exe -b pei-x86-64  --gc-sections    --entry=_mainCRTStartup    -o C:\Users\catik\source\repos\bellyoshi\Lazarus-learning\createpdf\project1.exe C:\Users\catik\source\repos\bellyoshi\Lazarus-learning\createpdf\link2648.res
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occurred while assembling %THEFILE%
goto end
:linkend
echo An error occurred while linking %THEFILE%
:end
