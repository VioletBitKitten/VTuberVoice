# Contributing to VTuberBVoice

All contributions are welcome provided they come with sufficient information to understand what is being fixed and why. If a bug is being fixed please make sure to include reproduction instructions and relevant logs.

## Coding Style

VTuberVoice does not have strict code requirements. The [Delphi style guide](https://wiki.delphi-jedi.org/wiki/Project_JEDI_Delphi_Language_Style_Guide) is loosely followed. Just try to match the existing style. I periodically go through and clean up formatting so don't stress too much.

## License

VTuberVoice is under the [MIT License](https://choosealicense.com/licenses/mit/), unless otherwise noted. By contributing you agree that your contributions will also be licensed under the MIT License.

## Testing

Before sending a pull request make sure both the unit tests and app tests pass. Where possible please add tests to exercise your change.

The unit tests can be run with the command: `build.bat test` \
The application tests can be run with the command: `testvtv.bat`

## Fixing a bug

[Please provide a bug report](https://github.com/VioletBitKitten/VTuberVoice/issues/new?assignees=VioletBitKitten&labels=bug&projects=&template=bug_report.md&title=%5BBUG%5D+) with reproduction instructions and relevant logs.

Enable full logging in the configuration file to help diagnose the bug.
The configuration file can be found with the command `vtv.exe -h`.
Change the following settings:

```conf
Diag=True
Enabled=True
Input=True
```
