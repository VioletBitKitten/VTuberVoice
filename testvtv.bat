@ECHO OFF
REM Run some tests of the VTuberVoice application.
REM I keep missing bugs because I don't fully test the program.

REM ----------========== Setup ==========----------

REM Variables for testing.
SET APPNAME="VTuberVoice"
SET OUTPUTFILE=output.txt
SET ERRORCOUNT=0

ECHO Running tests of %APPNAME%.
ECHO Writing output to the file: %OUTPUTFILE%
ECHO.

REM Make sure the application has been built.
IF NOT EXIST "vtv.exe" (
    ECHO Building the %APPNAME% application.
    CALL build > %OUTPUTFILE% 2>&1
    IF %ERRORLEVEL% neq 0 (
        ECHO The build failed.
        ECHO See the file %OUTPUTFILE% for errors.
        GOTO :EOF
    )
    ECHO.
)

REM If the output file exists delete it.
IF EXIST "%OUTPUTFILE%" (
    DEL "%OUTPUTFILE%"
)

REM ----------========== Run the tests ==========----------

CALL :RunTest ^
    "Generating help text to check %APPNAME% starts successfully..." ^
    "vtv -h >> %OUTPUTFILE% 2>&1"

CALL :RunTest ^
    "Checking %APPNAME% interactive mode starts successfully..." ^
    "ECHO /q | vtv >> %OUTPUTFILE% 2>&1"

CALL :RunTest ^
    "Runnin %APPNAME% with no configuration file..." ^
    "vtv -h >> %OUTPUTFILE% 2>&1"

CALL :RunTest ^
    "Listing available Audio Output Devices..." ^
    "vtv -V >> %OUTPUTFILE% 2>&1"

CALL :RunTest ^
    "Listing available Audio Output Devices in interactive mode..." ^
    "vtv < testfiles/interactiveoutputs.txt >> %OUTPUTFILE% 2>&1"

CALL :RunTest ^
    "Listing available Voices..." ^
    "vtv -V >> %OUTPUTFILE% 2>&1"

CALL :RunTest ^
    "Listing available Voices in interactive mode..." ^
    "vtv < testfiles/interactivevoices.txt >> %OUTPUTFILE% 2>&1"

CALL :RunTest ^
    "Speaking text from the command line..." ^
    "vtv Testing >> %OUTPUTFILE% 2>&1"

CALL :RunTest ^
    "Speaking text from a file from the command line..." ^
    "vtv -f testfiles/commandlinespeech.txt >> %OUTPUTFILE% 2>&1"

CALL :RunTest ^
    "Speaking text in interactive mode..." ^
    "vtv.exe < testfiles/interactivespeech.txt >> %OUTPUTFILE% 2>&1"

REM Finished with tests. Check if there were any failures.
CALL :OutputString
if %ERRORCOUNT% NEQ 0 (
    CALL :OutputString "%ERRORCOUNT% tests have failed."
    CALL :OutputString "See the file %OUTPUTFILE% for possible errors."
) else (
    CALL :OutputString "Testing successful."
)
GOTO :EOF

REM ----------========== Subroutines ==========----------

REM Run a test.
REM %1 = The string to print.
REM %2 = The test to run.
:RunTest
    CALL :OutputStringNONL "%~1%"
    %~2
    if %ERRORLEVEL% EQU 0 (
        CALL :OutputString "SUCCEEEDED"
    ) else (
        CALL :OutputString "FAILED"
        SET /A ERRORCOUNT = %ERRORCOUNT% + 1
    )
    EXIT /b

REM Write a string to STDOUT and the log file.
:OutputString
    IF -%1-==-- (
        ECHO.
        ECHO.>> %OUTPUTFILE%
    ) else (
        ECHO %~1
        ECHO %~1>> %OUTPUTFILE%
    )
    EXIT /b

REM Write a string to STDOUT and the log file. Special case.
REM No newline is output to STDOUT and a space is added to the end.
:OutputStringNONL
    IF -%1-==-- (
        ECHO.
        ECHO.>> %OUTPUTFILE%
    ) else (
        set <NUL /p="%~1 "
        ECHO %~1>> %OUTPUTFILE%
    )
    EXIT /b
