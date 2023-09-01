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

If you are using [Visual Studio Code](https://code.visualstudio.com/) there are build tasks that can be used. The `fpc:` tasks will be faster since FPC compiler is being called directly. The `build.bat` tasks will be slower since they use `fpmake` and `make`.

* `Build VTuberVoice` - Build VTuberVoice using the `build.bat` file.
* `Clean VTuberVoice` - Clean the compiled binaries using the `build.bat` file.
* `Test VTuberVoice` - Build and run the Unit tests using the `build.bat` file.
* `fpc: Build Debug` - Build VTuberVoice directly with the FPC compiler with debug options.
* `fpc: Build Release` - Build VTuberVoice directly with the FPC compiler with release options.
* `fpc: Syntax Check` - Build VTuberVoice directly with the FPC compiler but do not generate `vtv.exe`. Just for checking syntax.
* `fpc: Build Unit Tests` - Build the VTuberVoice tests directly with the FPC compiler.
* `fpc: Execute Binary` - Run the `vtv.exe` program.
* `fpc: Create Build Folder` - Create the `out/`` directory for binaries. Called by build commands.
* `fpc: Remove Build Files` - Delete the `/out` directory and the `vtv.exe` program.
* `fpc: Execute Test` - Execute the tests.

## TODO

* Create Unit tests. `In Progress.`
  * Tests for the vtvapp class.
  * Created tests for the filelist Unit.
  * Created tests for the vtvsettings class. This could use more work.
* Find a way to handle Control-C. This is proving difficult. As far as I can tell I would have to use the Crt and Keyboard Units, which pose other issues.
* For some reason the default output device is not used. Find a way to identify the default audio output device. Backport the change to the SAPI library.
* Expand the interactive user interface.
  * Add aliases for frequently spoken phrases. `In Progress.`
    * Simple alias support. Can make it better.
    * Add ability to create/delete/edit aliases from VTV.
    * Added Abbreviation support. Rather inefficient.
  * Add support for "speaking" WAV files.
* Expand support for writing spoken text to a file.
  * More options for how to write to the file.
  * Options to clear the file or write extra lines after a specified time.\
    <https://wiki.freepascal.org/Multithreaded_Application_Tutorial>
* Option for a default message. Instead of writing a blank line to the output file, write a different message.
* Convert this to a GUI application? The UI would be much better.
* Port this to other platforms? A Mac and Linux port is possible, though both platforms have different speech interfaces.

## Completed

* Implement customization.
  * INI Files for configuration implemented.
    * INI file can be in the local directory or in the users configuration directory.
    * Backups of the configuration file.
* Expand the interactive user interface.
  * Add a way to cancel the text being typed.
  * Add options for how to exit the application.
* Sometimes when quitting VTV there is an exception updating the configuration file.
  * This was caused by the voices changing the directory. Fixed this by always using an absolute path for the configuration file.

## Reference

* FreePascal SAPI library used to interface with with Microsoft SAPI speech library.\
  <https://github.com/VioletBitKitten/SAPI>

## Copyright and License

Copyright (c) 2023 Violet Bit Kitten

Distributed under the MIT license. Please see the file LICENSE.
