# VTuberVoice

TTS Software for VTubers who don't or can't speak.

## WARNING

This software is in development. The software is not complete and there is no documentation yet. Use with caution!

## Introduction

Provides a simple command line TTS interface with customization. Currently only works on Windows.

## Building

To build this project use the script `build.bat` or `build.sh`.

For help in the build scripts run the command `build.bat help` or `./build.sh help`.

To Manually build this project:

* Run the command `fpcmake` to create the file `Makefile`.
* Run the command `make` to build the project.

## TODO

* Create Unit tests.
* Implement customization. FreePascal has an INI interface that would work.

## Reference

* FreePascal SAPI library used to interface with with Microsoft SAPI speech library.\
  <https://github.com/VioletBitKitten/SAPI>

## Copyright and License

Copyright (c) 2023 Violet Bit Kitten

Distributed under the MIT license. Please see the file LICENSE.
