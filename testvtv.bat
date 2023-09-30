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

REM Print the help text.
CALL :OutputString "Generating help text to check %APPNAME% starts successfully..."
vtv -h >> %OUTPUTFILE% 2>&1
if %ERRORLEVEL% equ 0 (
    CALL :OutputString "SUCCEEEDED"
) else (
    CALL :OutputString "Failed to run the program."
    GOTO exitscript
)
CALL :OutputString

REM Run interactive mode.
CALL :OutputString "Checking %APPNAME% interactive mode starts successfully..."
ECHO /q | vtv >> %OUTPUTFILE% 2>&1
if %ERRORLEVEL% equ 0 (
    CALL :OutputString "SUCCEEEDED"
) else (
    CALL :OutputString "Failed to start %APPNAME% interactive mdoe."
    GOTO exitscript
)
CALL :OutputString

REM Run without a configuration file..
CALL :OutputString "Runnin %APPNAME% with no configuration file..."
vtv -h >> %OUTPUTFILE% 2>&1
if %ERRORLEVEL% equ 0 (
    CALL :OutputString "SUCCEEEDED"
) else (
    CALL :OutputString "Failed to run without configuration."
    GOTO exitscript
)
CALL :OutputString


REM List available Audio Outputs.
CALL :OutputString "Listing available Audio Output Devices..."
vtv -V >> %OUTPUTFILE% 2>&1
if %ERRORLEVEL% equ 0 (
    CALL :OutputString "SUCCEEEDED"
) else (
    CALL :OutputString "Failed to list Audio Output Devices."
    GOTO exitscript
)
CALL :OutputString

REM List available Audio Outputs interactively.
CALL :OutputString "Listing available Audio Output Devices in interactive mode..."
vtv < testfiles/interactiveoutputs.txt >> %OUTPUTFILE% 2>&1
if %ERRORLEVEL% equ 0 (
    CALL :OutputString "SUCCEEEDED"
) else (
    CALL :OutputString "Failed to list Audio Output Devices."
    GOTO exitscript
)
CALL :OutputString

REM List available voices.
CALL :OutputString "Listing available Voices..."
vtv -V >> %OUTPUTFILE% 2>&1
if %ERRORLEVEL% equ 0 (
    CALL :OutputString "SUCCEEEDED"
) else (
    CALL :OutputString "Failed to list voices."
    GOTO exitscript
)
CALL :OutputString

REM List available Vices interactively.
CALL :OutputString "Listing available Voices in interactive mode..."
vtv < testfiles/interactivevoices.txt >> %OUTPUTFILE% 2>&1
if %ERRORLEVEL% equ 0 (
    CALL :OutputString "SUCCEEEDED"
) else (
    CALL :OutputString "Failed to list Voices."
    GOTO exitscript
)
CALL :OutputString

REM Speak some text from the command line.
CALL :OutputString "Speaking text from the command line..."
CALL :OutputString "You may not hear anything depending on your settings."
vtv "Speaking text from the command line." >> %OUTPUTFILE% 2>&1
if %ERRORLEVEL% equ 0 (
    CALL :OutputString "SUCCEEEDED"
) else (
    CALL :OutputString "Failed to speak text."
    GOTO exitscript
)
CALL :OutputString

REM Speak text in interactive mode.
CALL :OutputString "Speaking text in interactive mode..."
vtv.exe < testfiles/interactivespeech.txt >> %OUTPUTFILE% 2>&1
if %ERRORLEVEL% equ 0 (
    CALL :OutputString "SUCCEEEDED"
) else (
    CALL :OutputString "Failed to speak text in interactive mode."
    GOTO exitscript
)
CALL :OutputString

REM Finished with tests.
CALL :OutputString "Testing successful."
GOTO :exitscript

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