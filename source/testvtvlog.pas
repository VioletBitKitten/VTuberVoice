{
  Unit tests for the vtvsettings Unit.

  https://github.com/VioletBitKitten/SAPI

  Copyright (c) 2023 Violet Bit Kitten

  Distributed under the MIT license. Please see the file LICENSE.
}

{
  Note:
    Every test does a FreeAndNil on the log object so the log file can be read.
    The log object keeps the file open for writing.
    See the todo in the vtvlog.pas file for fixing this.
}

{ Modern Pascal Directives }
{$mode objfpc}{$H+}{$J-}

unit testvtvlog;

interface

uses
  sysutils, Contnrs, classes, fpcunit, testregistry, vtvsettings, vtvlog;

const
  TestSettingsFile  = 'testlog.ini';
  TestLogFile       = 'testlog.txt';
  TestLogFormat     = 'YYYY-MM-DD';
  TestDiagText      = 'Test diagnostic text.';
  TestInputText     = 'Test input text.';
  TestSpeechText    = 'Test speech text.';
  TestMessageText   = 'Test message text.';

type
  TVTVLogTest = class(TTestCase)
  private
    Settings : TVTVSettings;
    VTVLog   : TVTVLog;
    function ReadLogFile : String;
    function ReadLogSize : Int64;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Test Methods }
    procedure TestCreate;
    procedure TestLogDiag;
    procedure TestLogDiagDisabled;
    procedure TestLogInput;
    procedure TestLogInputDisabled;
    procedure TestLogSpeech;
    procedure TestLogMessage;
  end;

implementation

{ ----------========== Private Methods ==========---------- }

{ Reads the contents of the test log file. }
function TVTVLogTest.ReadLogFile : String;
var
  LogTextFile : TextFile;
  LogContents : String;
begin
  AssignFile(LogTextFile, TestLogFile);
  Reset(LogTextFile);
  ReadLn(LogTextFile, LogContents);
  CloseFile(LogTextFile);
  Result := LogContents;
end;

{ Reads the size of the test log file. }
function TVTVLogTest.ReadLogSize : Int64;
var
  LogTextFile : File;
  LogFileSize : Int64;
begin
  AssignFile(LogTextFile, TestLogFile);
  Reset(LogTextFile);
  LogFileSize := FileSize(LogTextFile);
  CloseFile(LogTextFile);
  Result := LogFileSize;
end;

{ ----------========== Protected Methods ==========---------- }

{ Create the settings object with settings for testing the log file. }
procedure TVTVLogTest.SetUp;
begin
  Settings := TVTVSettings.Create(TestSettingsFile);
  Settings.LogDiag    := False;
  Settings.LogFile    := TestLogFile;
  Settings.LogFormat  := TestLogFormat;
  Settings.LogEnabled := True;
  Settings.LogInput   := False;
end;

{ Cleanup the Settings object and file. }
procedure TVTVLogTest.TearDown;
begin
  FreeAndNil(Settings);
  Deletefile(TestSettingsFile);
  if FileExists(TestLogFile) then
    Deletefile(TestLogFile);
end;

{ ----------========== Test Methods ==========---------- }

{ Make sure the log file is created. }
procedure TVTVLogTest.TestCreate;
begin
  VTVLog := TVTVLog.Create(Settings);
  FreeAndNil(VTVLog);
  AssertTrue('Log file was created.', FileExists(TestLogFile));
end;

{ Make sure diagnostic messages are written to the log file. }
procedure TVTVLogTest.TestLogDiag;
var
  LogContents : String;
  LogCompare  : String;
begin
  { Enable diagnostic logging and write a message. }
  Settings.LogDiag := True;
  VTVLog := TVTVLog.Create(Settings);
  VTVLog.LogDiag(TestDiagText);
  FreeAndNil(VTVLog);
  LogContents := ReadLogFile;

  { Create the comparison string to check against. }
  Logcompare := FormatDateTime(TestLogFormat, Now) + VTVLogDiagPrefix + TestDiagText;

  { Verify the value is correct. }
  AssertEquals('Diagnostic message was written to the log file.', Logcompare, LogContents);
end;

{ Make sure diagnostic messages are not written to the log file when LogDiag is disabled. }
procedure TVTVLogTest.TestLogDiagDisabled;
var
  LogFileSize : Int64;
begin
  { Write a diagnostic message. }
  Settings.LogDiag := False;
  VTVLog := TVTVLog.Create(Settings);
  VTVLog.LogDiag(TestDiagText);
  FreeAndNil(VTVLog);
  LogFileSize := ReadLogSize;

  { Verify the log file is empty. }
  AssertEquals('Log should be empty when diagnostic logging is disabled.', LogFileSize, 0);
end;

{ Make sure input messages are written to the log file. }
procedure TVTVLogTest.TestLogInput;
var
  LogContents : String;
  LogCompare  : String;
begin
  { Enable input logging and write a message. }
  Settings.LogInput := True;
  VTVLog := TVTVLog.Create(Settings);
  VTVLog.LogInput(TestInputText);
  FreeAndNil(VTVLog);
  LogContents := ReadLogFile;

  { Create the comparison string to check against. }
  Logcompare := FormatDateTime(TestLogFormat, Now) + VTVLogInputPrefix + TestInputText;

  { Verify the value is correct. }
  AssertEquals('Input message was written to the log file.', Logcompare, LogContents);
end;

{ Make sure input messages are not written to the log file when LogInput is disabled. }
procedure TVTVLogTest.TestLogInputDisabled;
var
  LogFileSize : Int64;
begin
  { Write an input message. }
  Settings.LogInput := False;
  VTVLog := TVTVLog.Create(Settings);
  VTVLog.LogInput(TestInputText);
  FreeAndNil(VTVLog);
  LogFileSize := ReadLogSize;

  { Verify the log file is empty. }
  AssertEquals('Log should be empty when input logging is disabled.', LogFileSize, 0);
end;

{ Make sure speech messages are written to the log file. }
procedure TVTVLogTest.TestLogSpeech;
var
  LogContents : String;
  LogCompare  : String;
begin
  { Write a speech message. }
  VTVLog := TVTVLog.Create(Settings);
  VTVLog.LogSpeech(TestSpeechText);
  FreeAndNil(VTVLog);
  LogContents := ReadLogFile;

  { Create the comparison string to check against. }
  Logcompare := FormatDateTime(TestLogFormat, Now) + VTVLogSpeechPrefix + TestSpeechText;

  { Verify the value is correct. }
  AssertEquals('Speech message was written to the log file.', Logcompare, LogContents);
end;

{ Make sure generic messages are written to the log file. }
procedure TVTVLogTest.TestLogMessage;
var
  LogContents : String;
  LogCompare  : String;
begin
  { Write a generic message. }
  VTVLog := TVTVLog.Create(Settings);
  VTVLog.LogMessage(TestMessageText);
  FreeAndNil(VTVLog);
  LogContents := ReadLogFile;

  { Create the comparison string to check against. }
  Logcompare := FormatDateTime(TestLogFormat, Now) + VTVLogMessagePrefix + TestMessageText;

  { Verify the value is correct. }
  AssertEquals('Generic message was written to the log file.', Logcompare, LogContents);
end;

initialization
  RegisterTests([TVTVLogTest]);
end.