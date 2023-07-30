@ECHO OFF
REM Batch file for building VTuberVoice on Windows.
REM See the following for help in BAT scripts.
REM https://en.wikibooks.org/wiki/Windows_Batch_Scripting

REM Check for a command line argument.
REM If none just run the build.
IF -%1-==-- GOTO runbuild

REM Figure out what we were given on the command line.
SET command=%1
if /i "%command%"=="build" GOTO runbuild
if /i "%command%"=="clean" GOTO runclean
if /i "%command%"=="test"  GOTO runtests
if /i "%command%"=="tests" GOTO runtests
if /i "%command%"=="help"  GOTO showhelp
if /i "%command%"=="/?"    GOTO showhelp
if /i "%command%"=="-h"    GOTO showhelp
GOTO exitscript

REM Run the actual build.
:runbuild
    ECHO Building VTuberVoice...
    IF NOT EXIST "Makefile" (
        fpcmake
    )
    make
GOTO exitscript

REM Cleanup build artifacts, including the Makefile.
:runclean
    ECHO Cleaning VTuberVoice...
    fpcmake
    make distclean
    DEL Makefile
GOTO exitscript

REM Show some help text.
:showhelp
    ECHO VTuberVoice Windows Build Script
    ECHO Performs build tasks for VTuberVoice.
    ECHO.
    ECHO Commands:
    ECHO build - Builds the VTuberVoice project. (Default)
    ECHO clean - Runs the make command 'distclean'.
    ECHO help  - Display this help text.
    ECHO test  - Runs the VTuberVoice tests.
GOTO exitscript

REM Run the tests.
:runtests
    IF NOT EXIST "rununit.exe" (
        echo First building VTuberVoice...
        fpcmake
        make
        ECHO.
    )
    ECHO Running VTuberVoice Unit tests...
    rununit
GOTO exitscript

REM Exit the script.
:exitscript