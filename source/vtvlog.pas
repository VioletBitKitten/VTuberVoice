{
  VTuberVoice logging code.

  TTS program using the Free Pascal SAPI Library.

  https://github.com/VioletBitKitten/VTuberVoice

  Copyright (c) 2023 Violet Bit Kitten

  Distributed under the MIT license. Please see the file LICENSE.
}

{
  TODO:
    Change the log file from TextFile to a stream.
    Provide a method to return the stream object.
    This will make the test for this unit cleaner.
}

{ Modern Pascal Directives }
{$mode objfpc}{$H+}{$J-}

unit vtvlog;

interface

uses
  sysutils, classes, vtvsettings;

{ Prefixed for the various log messages.}
const
  VTVLogDiagPrefix    = ' (Diag): ';
  VTVLogInputPrefix   = ' (Input): ';
  VTVLogSpeechPrefix  = ' (Speech): ';
  VTVLogMessagePrefix = ': ';

type
  TVTVLog = class
  private
    { Logging Settings. }
    FLogDiag      : Boolean;
    FLogEnabled   : Boolean;
    FLogFile      : String;
    FLogFormat    : String;
    FLogInput     : Boolean;
    { Logging file handle. }
    FLogTextFile  : TextFile;
    public
    constructor Create(Settings : TVTVSettings);
    destructor Destroy; override;
    procedure LogDiag(Text : String);
    procedure LogInput(Text : String);
    procedure LogSpeech(Text : String);
    procedure LogMessage(Text : String);
    { Properties. }
    property Enabled : Boolean read FLogEnabled;
  end;
  EVTVLogException = class(Exception);

implementation

{ ----------========== VTVLog Public Methods ==========---------- }

{ Create the log object. }
constructor TVTVLog.Create(Settings : TVTVSettings);
begin
  if Settings = Nil then
    Exit;

  { Copy the settings to the private variables. }
  FLogDiag      := Settings.LogDiag;
  FLogEnabled   := Settings.LogEnabled;
  FLogFile      := Settings.LogFile;
  FLogFormat    := Settings.LogFormat;
  FLogInput     := Settings.LogInput;

  { If logging is disabled simple exit. }
  if not FLogEnabled then
    Exit;

  { Open the log TextFile. }
  AssignFile(FLogTextFile, FLogFile);
  try
    { Open the file. }
    if FileExists(FLogFile) then
      append(FLogTextFile)
    else
      rewrite(FLogTextFile);
  except
    on E: EInOutError do
      begin
        raise EVTVLogException.Create('Unable to open the file "' + FLogFile + '" for writing. ' + E.Message);
      end;
  end;
end;

{ Cleanup. }
destructor TVTVLog.Destroy;
begin
  if FLogEnabled then
    CloseFile(FLogTextFile);
  inherited;
end;

{ Log a diagnostic message, if enabled. }
procedure TVTVLog.LogDiag(Text : String);
var
  Timestamp : String;
begin
  if not FLogEnabled then
    Exit;
  if not FLogDiag then
    Exit;
  Timestamp := FormatDateTime(FLogFormat, Now);
  WriteLn(FLogTextFile, Timestamp, VTVLogDiagPrefix, Text);
  Flush(FLogTextFile);
end;

{ Log user input, if enabled. }
procedure TVTVLog.LogInput(Text : String);
var
  Timestamp : String;
begin
  if not FLogEnabled then
    Exit;
  if not FLogInput then
    Exit;
  Timestamp := FormatDateTime(FLogFormat, Now);
  WriteLn(FLogTextFile, Timestamp, VTVLogInputPrefix, Text);
  Flush(FLogTextFile);
end;

{ Log spoken text. }
procedure TVTVLog.LogSpeech(Text : String);
var
  Timestamp : String;
begin
  if not FLogEnabled then
    Exit;
  Timestamp := FormatDateTime(FLogFormat, Now);
  WriteLn(FLogTextFile, Timestamp, VTVLogSpeechPrefix, Text);
  Flush(FLogTextFile);
end;

{ Log a message as provided. }
procedure TVTVLog.LogMessage(Text : String);
var
  Timestamp : String;
begin
  if not FLogEnabled then
    Exit;
  Timestamp := FormatDateTime(FLogFormat, Now);
  WriteLn(FLogTextFile, Timestamp, VTVLogMessagePrefix, Text);
  Flush(FLogTextFile);
end;

end.