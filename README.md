# VTuberVoice

TTS Software for VTubers who don't or can't speak.

## WARNING

This software is in development. The software is not complete and there is no documentation yet. Use with caution!

## Introduction

Provides a simple command line TTS interface with customization. Currently only works on Windows.

This software was created because I do not like to talk, and at times I am unable to speak.

## Building

To build this project use the script `build.bat`.
For help in the build scripts run the command `build.bat help`.

To Manually build this project:

* Run the command `fpcmake` to create the file `Makefile`.
* Run the command `make` to build the project.

## TODO

* Create Unit tests.
* Implement customization.
  * FreePascal has an INI interface.\
    <https://www.freepascal.org/docs-html/fcl/inifiles/tinifile.html>
  * The INI file could live with the app or be placed in a config directory.\
    <https://www.freepascal.org/docs-html/rtl/sysutils/getappconfigdir.html>
  * Automatically save changes made in-program back to the INI file.
  * Backup the INI files. On start / change / timer.
  * Change command line parameters for setting options.
* Expand the interactive user interface.
  * Add aliases for frequently spoken phrases.
  * Add support for "speaking" WAV files.
  * Add a way to cancel the text being typed.
  * Add options for how to exit the application.
* Expand support for writing spoken text to a file.
  * More options for how to write to the file.
  * Options to clear the file or write extra lines after a specified time.\
    <https://wiki.freepascal.org/Multithreaded_Application_Tutorial>
* Port this to other platforms? A Mac and Linux port is possible, though both platforms have different speech interfaces.

## Reference

* FreePascal SAPI library used to interface with with Microsoft SAPI speech library.\
  <https://github.com/VioletBitKitten/SAPI>

## Copyright and License

Copyright (c) 2023 Violet Bit Kitten

Distributed under the MIT license. Please see the file LICENSE.
