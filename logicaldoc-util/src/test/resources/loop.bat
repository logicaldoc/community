@echo off
echo %TS10loopcount%
set loopcount=%TS10loopcount%

:loop
echo Hello World!
set /a loopcount=loopcount-1
if %loopcount%==0 goto exitloop
goto loop

:exitloop
echo Activity completed!