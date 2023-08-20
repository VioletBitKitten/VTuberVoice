# VTuberVoice

TTS Software for VTubers who don't or can't speak.

## WARNING

This software is in development. The software is not complete and there is no documentation yet. Use with caution!

## Introduction

Provides a simple command line TTS interface with customization. Currently only works on Windows.

This software was created because I do not like to talk, and at times I am unable to speak.

**Please note** Sometimes Windows is odd and will pick an Audio Output that is not the default. If there is no audio when speaking, try setting an Audio Output using the options `-O` to list Audio Outputs and `-o` to set an Audio Output.

## Building

To build this project use the script `build.bat`.
For help in the build scripts run the command `build.bat help`.

To Manually build this project:

* Run the command `fpcmake` to create the file `Makefile`.
* Run the command `make` to build the project.

## Completed

* Implement customization.
  * INI Files for configuration implemented.
    * INI file can be in the local directory or in the users configuration directory.
    * Backups of the configuration file.

## TODO

* Create Unit tests.
* Find a way to handle Control-C.
* For some reason the default output device is not used. Find a way to identify the default audio output device. Backport the change to the SAPI library.
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
