# Work to be completed

This is a list of ideas and work to be completed. Tasks to be completed before the fist release are marked with `Release`.

## TODO

* `Release` - Tutorial video on using VTuberVoice.
* `Release` - Usage documentation covering:
  * The command line options.
  * The interactive interface.
  * The configuration file. The configuration file contains documentation, but this can be expanded upon in a separate document.
* Create Unit tests. `In Progress.`
  * Tests for the `vtvapp` Class.
  * Created tests for the `filelist` Class.
  * Created tests for the `vtvsettings` Class. This could use more work.
  * Created tests for the `vtvlog` Class.
* Find a way to handle Control-C. This is proving difficult.
  As far as I can tell I would have to use the Crt and Keyboard Units, which pose other issues.
* Expand the interactive user interface.
  * Add aliases for frequently spoken phrases. `In Progress.`
    * Simple alias support. Can make it better.
    * Add ability to create/delete/edit aliases from VTV.
    * Added Abbreviation support. Rather inefficient.
    * Add variables to be used with aliases? Like a variable for your name and a greeting alias.
  * Add support for "speaking" WAV files.
* Expand support for writing spoken text to a file.
  * More options for how to write to the file.
  * Options to clear the file or write extra lines after a specified time.\
    <https://wiki.freepascal.org/Multithreaded_Application_Tutorial>
* Option for a default message. Instead of writing a blank line to the output file, write a different message.
* Convert this to a GUI application? The UI would be much better.
* Port this to other platforms? A Mac and Linux port is possible, though both platforms have different speech interfaces.

## Completed

* Add a command to reload settings from the configuration file.
* Implement customization.
  * INI Files for configuration implemented.
    * INI file can be in the local directory or in the users configuration directory.
    * Backups of the configuration file.
* Expand the interactive user interface.
  * Add a way to cancel the text being typed.
  * Add options for how to exit the application.
* Sometimes when quitting VTV there is an exception updating the configuration file.
  * This was caused by the voices changing the directory. Fixed this by always using an absolute path for the configuration file.
* For some reason the default output device is not used. Find a way to identify the default audio output device. Backport the change to the SAPI library. Solved this with the mmdeviceapi.pas file.
* Better handling of writing a blank line to the output file. It is getting messy handling this special case.
* Cleanup the Variables in TVTVApp. They are a bit of a mess.
* Write spoken text to a log file.
  * Configurable timestamp.
  * Write spoken text before modification with a timestamp.
  * Write a message for startup and shutdown.
  * Events to log for a mode verbose mode?
* Add `-C` option to not load a configuration file. Mostly for testing.
