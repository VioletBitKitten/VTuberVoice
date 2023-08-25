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

const
  HelpTextMain : Array[1..4] of String = (
    'Commands:',
    '/diag            Print diagnostic data.',
    '/help [command]  Print this help text or detailed help for a command.',
    '/quit            Exit the application.'
  );

procedure CommandHelp;
procedure CommandHelp(Args : TStringArray);

implementation

procedure CommandHelp;
var
  OutputString   : String;
begin
  for OUtputString in HelpTextMain do
    WriteLn(OUtputString);
end;

procedure CommandHelp(Args : TStringArray);
var
  OutputString   : String;
begin
  if Length(Args) = 0 then
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