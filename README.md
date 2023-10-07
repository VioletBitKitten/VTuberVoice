# VTuberVoice

[![FreePascal](https://img.shields.io/badge/FreePascal-3.2.2-blue?logo=lazarus)](https://www.freepascal.org/)
[![Github: VioletBitKitten - VTuberVoice](https://img.shields.io/github/license/VioletBitKitten/VTuberVoice)](https://github.com/VioletBitKitten/VTuberVoice/blob/main/LICENSE)
[![Github: VioletBitKitten - VTuberVoice](https://img.shields.io/github/last-commit/VioletBitKitten/VTuberVoice/main)](https://github.com/VioletBitKitten/VTuberVoice/commits/main)
[![Github: VioletBitKitten - VTuberVoice](https://img.shields.io/github/issues/VioletBitKitten/VTuberVoice)](https://github.com/VioletBitKitten/VTuberVoice/issues)
[![Github: VioletBitKitten - VTuberVoice](https://img.shields.io/github/stars/VioletBitKitten/VTuberVoice)](https://github.com/VioletBitKitten/VTuberVoice)
[![Discord](https://img.shields.io/discord/1144984263347929098?label=Discord)](https://discord.gg/4ZQuQFEYht)

[![Twitter: VioletBitKitten](https://img.shields.io/twitter/follow/violetbitkitten?style=social)](https://twitter.com/violetbitkitten)

TTS Software for VTubers who don't or can't speak.

## WARNING

VTuberVoice is in development. The software is not complete and there is no documentation yet. Use with caution!

## Introduction

VTuberVoice provides a simple command line TTS (Text To Speech) interface with customization. Currently only works on Windows.

VTuberVoice was created because I do not like to talk, and at times I am unable to speak. I was unable to find a good existing solution to TTS that is easy to use while streaming. VTuberVoice provides a simple text interface that can be run in command window.

**Please note** VTuberVoice attempts to pick the default Audio Output Device. If there is no audio when speaking, try setting an Audio Output Device using the options `-O` to list Audio Output DEvices and `-o` to set an Audio Output Device.

## Building

The best way to build VTuberVoice is using the `build.bat` file:

* To build VTuberVoice use the script: `build.bat`
* For help in the build scripts run the command: `build.bat help`
* The unit tests can be run using the command: `build.bat help`
* Application test can be run using the command: `testvtv.bat`

To Manually build VTuberVoice:

* To create the `Makefile` use the command: `fpcmake`
* To build VTuberVoice use the command: `make`
* To run the unit tests use the command: `runtests.exe`

If you are using [Visual Studio Code](https://code.visualstudio.com/) there are build tasks that can be used. The `fpc:` tasks will be faster since FPC compiler is being called directly. The `build.bat` tasks will be slower since they use `fpmake` and `make`.

* `Build VTuberVoice` - Build VTuberVoice using the `build.bat` file.
* `Clean VTuberVoice` - Clean the compiled binaries using the `build.bat` file.
* `Test VTuberVoice` - Build and run the Unit tests using the `build.bat` file.
  This does a clean, a full build then runs the `runtests.exe` program.
* `fpc: Build Debug` - Build VTuberVoice directly with the FPC compiler with debug options.
* `fpc: Build Release` - Build VTuberVoice directly with the FPC compiler with release options.
* `fpc: Syntax Check` - Build VTuberVoice directly with the FPC compiler but do not generate `vtv.exe`.
 Builds the current file. Just for checking syntax.
* `fpc: Build Unit Tests` - Build the VTuberVoice tests directly with the FPC compiler.
* `fpc: Execute Binary` - Run the `vtv.exe` program.
  First runs the build task `fpc: Build Release`.
* `fpc: Create Build Folder` - Create the `out/` directory for binaries.
  Called by other `fpc:` build commands.
* `fpc: Remove Build Files` - Delete the `/out` directory, intermediate build files along with the `vtv.exe` and `runtests.exe` programs.
* `fpc: Execute Test` - Execute the tests by running the `runtests.exe` program.
  First runs the build task `fpc: Build Unit Tests`.

## Reference

* FreePascal SAPI library used to interface with with Microsoft SAPI speech library. \
  <https://github.com/VioletBitKitten/SAPI>

* VTuberVoice loosely follows Dephi Style Guide. \
  <https://wiki.delphi-jedi.org/wiki/Project_JEDI_Delphi_Language_Style_Guide>

## Copyright and License

Copyright (c) 2023 Violet Bit Kitten

Distributed under the MIT license. Please see the file LICENSE.
