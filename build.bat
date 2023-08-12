@ECHO OFF
REM Batch file for building VTuberVoice on Windows.
REM See the following for help in BAT scripts.
REM https://en.wikibooks.org/wiki/Windows_Batch_Scripting

REM Check for a command line argument.
REM If none just run the build.
IF -%1-==-- GOTO runbuild

REM Figure out what we were given on the command line.
:processargs
    if /i "%1"=="build" CALL :runbuild
    if /i "%1"=="clean" CALL :runclean
    if /i "%1"=="test"  CALL :runtests
    if /i "%1"=="tests" CALL :runtests
    if /i "%1"=="help"  CALL :showhelp
    if /i "%1"=="/?"    CALL :showhelp
    if /i "%1"=="-h"    CALL :showhelp
    shift
    if not -%1-==-- goto processargs
GOTO exitscript

REM Run the actual build.
:runbuild
    ECHO Building VTuberVoice...
    IF NOT EXIST "Makefile" (
        fpcmake
    )
    make
    EXIT /b

REM Cleanup build artifacts, including the Makefile.
:runclean
    ECHO Cleaning VTuberVoice...
    fpcmake
    make distclean
    DEL Makefile
    EXIT /b

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
        CALL :runbuild
    )
    ECHO Running VTuberVoice Unit tests...
    rununit
    EXIT /b

REM Exit the script.
:exitscript