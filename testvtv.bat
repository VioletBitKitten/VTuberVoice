@ECHO OFF
REM Run some tests of the VTuberVoice application.
REM I keep missing bugs because I don't fully test the program.

REM Variables for testing.
SET APPNAME="VTuberVoice"
SET OUTPUTFILE=output.txt

ECHO Running tests of %APPNAME%.
ECHO Testing will abort on the first failure.
ECHO Writing output to the file: %OUTPUTFILE%
ECHO.

REM Make sure the application has been built.
IF NOT EXIST "vtv.exe" (
    ECHO Building the %APPNAME% application.
    CALL build > NUL
    IF %ERRORLEVEL% neq 0 (
        ECHO The build failed. Please run the build again and check for errors.
        GOTO exitscript
    )
    ECHO.
)

REM If the output file exists delete it.
IF EXIST "%OUTPUTFILE%" (
    DEL "%OUTPUTFILE%"
)

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

REM Finished with tests.
CALL :OutputString "Testing successful."
GOTO :exitscript

REM Run a test.
REM %1 = The string to print.
REM %2 = The test to run.
:RunTest
    CALL :OutputString "%~1%"
    %~2
    if %ERRORLEVEL% equ 0 (
        CALL :OutputString "SUCCEEEDED"
    ) else (
        CALL :OutputString "FAILED"
        GOTO exitscript
    )
    CALL :OutputString
    EXIT /b

REM Write a string to STDOUT and the log file.
:OutputString
    IF -%1-==-- (
        ECHO.
        ECHO. >> %OUTPUTFILE%
    ) else (
        ECHO %~1
        ECHO %~1 >> %OUTPUTFILE%
    )
    EXIT /b

REM Exit the script.
:exitscript