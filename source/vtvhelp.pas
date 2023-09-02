{
  Help text for interactive mode of the VtuberVoice application.

  https://github.com/VioletBitKitten/VTuberVoice

  Copyright (c) 2023 Violet Bit Kitten

  Distributed under the MIT license. Please see the file LICENSE.
}

{ Modern Pascal Directives }
{$mode objfpc}{$H+}{$J-}
unit vtvhelp;

interface

uses
  sysutils;

procedure CommandHelp;
procedure CommandHelp(Arg : String);

implementation

const
  HelpTextMain : Array[1..16] of String = (
    'Commands:',
    '/A | /abbrev [Abbrev]  Lias all Abbreviations, or a specific Abbreviation',
    '/a | /alias [Alias]    List all Aliases, or a specific alias.',
    '/d | /diag [Bool]      Enable/Disable/Print diagnostic information.',
    '/h | /help [command]   Print this help text or detailed help for a command.',
    '/o | /output           Show or set the Audio Output Device.',
    '/O | /outputs          List the available audio output devices with ID #.',
    '/p | /priority         Show or set the priority for speaking.',
    '/q | /quit             Exit the application.',
    '/r | /rate [Rate]      Show or set the rate text is spoken.',
    '/s | /save             Save settings back to the configuration file.',
    '/v | /voice [Voice]    Show or set the voice used to speak text.',
    '/V | /voices           List the available voices with ID #. ',
    '/l | /volume [Volume]  Show or set the volume text is spoken at.',
    '',
    'End a line with "\" to cancel the input.'
  );

{ Print the main help text. }
procedure CommandHelp;
var
  OutputString   : String;
begin
  for OUtputString in HelpTextMain do
    WriteLn(OUtputString);
end;

{ Print help for a command, or the main help text. }
procedure CommandHelp(Arg : String);
var
  OutputString   : String;
begin
  if Length(Arg) = 0 then
  begin
    for OUtputString in HelpTextMain do
      WriteLn(OUtputString);
  end
  else
  begin
    WriteLn('Coming soon.');
  end;
end;

end.