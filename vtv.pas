{
  TTS program using the Free Pascal SAPI Library.

  https://github.com/VioletBitKitten/VTuberVoice

  Copyright (c) 2023 Violet Bit Kitten

  Distributed under the MIT license. Please see the file LICENSE.
}

{ Modern Pascal Directives }
{$mode objfpc}{$H+}{$J-}

program vtv;

uses
  classes, custapp, sapi, sysutils;

type
  TTTSApp = Class(TCustomApplication)
  private
    LongOptions  : TStringList;
    OutputFile   : TextFile;
    SpVoice      : TSpVoice;
    SpFileStream : TSpFileStream;
    ShortOptions : String;
    WriteText    : Boolean;
    WriteWav     : Boolean;
  public
    { Application Setup }
    destructor Destroy; override;
    procedure DoRun; override;
    procedure Help;
    procedure Initialize; override;
    { Command Line Options }
    procedure ProcessOptions;
    procedure SetupOptions;
    { Helper Methods }
    procedure ListVoices;
    procedure ListOutputs;
    procedure SetAudioOutput(NewOutput : String);
    procedure SetPriority(NewPriority : String);
    procedure SetRate(NewRate : String);
    procedure SetupOutput(FileName : String; AppendFile : Boolean);
    procedure SetupOutputWav(FileName : String);
    procedure SetVoice(NewVoice : String);
    Procedure SetVolume(NewVolume : String);
    { Speech Methods }
    procedure ReadSpeakLoop;
    procedure SpeakFile(FileName : String);
    procedure SpeakList(List : TStringList);
    procedure SpeakText(Text : String);
  end;

{ ----------========== Application Setup ==========----- }

{ Perform cleanup. }
destructor TTTSApp.Destroy;
begin
FreeAndNil(SpVoice);
if WriteText then
  CloseFile(OutputFile);
if WriteWav then
  SpFileStream.Close;
inherited;

end;

{ Run the application. }
procedure TTTSApp.DoRun;
begin
  ProcessOptions;
  if not Terminated then
  begin
    ReadSpeakLoop;
    Terminate;
  end;
end;

{ Print the help text. }
procedure TTTSApp.Help;
begin
WriteLn(Title);
  WriteLn('Usage: ', ExtractFileName(ExeName), ' [OPTION] [TEXT]...');
  WriteLn('Speak text using Micrsoft SAPI.');
  WriteLn('If text is given ont he command line then the');
  WriteLn('text will be spoken and the program will exit.');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  -a , --append             Append text when writing text to a file.');
  WriteLn('  -f , --speak-file         Speak the contents of a text file.');
  WriteLn('  -h , --help               Prints this help.');
  WriteLn('  -l , --volume=VOLUME      Set the volume text is spoken at. ', SpVoice_volume_valid_values);
  WriteLn('  -o , --outut=OUTPUT       Set the putput text is spoken to by the name or ID number.');
  WriteLn('  -O , --outputs            List the available audio output devices with ID #.');
  WriteLn('  -p , --priority           Set the priority for speaking. ', SpVoice_priority_valid_values);
  WriteLn('  -r , --rate=RATE          Set the rate text is spoken. ', SpVoice_rate_valid_values);
  WriteLn('  -v , --voice=VOICE        Set the voice text is spoken with by the name or ID number.');
  WriteLn('  -V , --voices             List the available voices with ID #.');
  WriteLn('  -w , --write-to-file=FILE Write all text spoken to a text file.');
  WriteLn('  -W , --wav-file=FILE      All text is recorded to a WAV file instead of being spoken.');
  WriteLn;
  WriteLn('For more information see: https://github.com/VioletBitKitten/SAPI');
end;

{ Initialize the application. }
procedure TTTSApp.Initialize;
begin
  SetupOptions;
  SpVoice := TSpVoice.Create;
  SpVoice.ExceptionsEnabled := True;
  WriteText := False;
  WriteWav := False;
end;

{ ----------========== Command Line Options ==========----- }

{ Process the command line options. }
procedure TTTSApp.ProcessOptions;
var
  Text :       String;
  NonOptions : TStringList;
begin
  { Check the command line options. }
  NonOptions := TStringList.Create;
  Text := CheckOptions(ShortOptions, LongOptions, Nil, NonOptions);

  { Check for errors with the command line options. }
  if Length(Text) > 0 then
  begin
    WriteLn(Text);
    WriteLn;
    Help;
    Terminate;
    Exit;
  end;

  { These options will always exit the program immediately. }

  { Display the help text. }
  if HasOption('h', 'help') then
  begin
    Help;
    Terminate;
    Exit;
  end;

  { List the available Audio Outputs. }
  if HasOption('O', 'outputs') then
  begin
    ListOutputs;
    Terminate;
    Exit;
  end;

  { List the available voices. }
  if HasOption('V', 'voices') then
  begin
    ListVoices;
    Terminate;
    Exit;
  end;

  { Set various settings. }

  { Set the volume text is spoken at. }
  if HasOption('l', 'volume') then
  begin
    Text := GetOptionValue('l', 'volume');
    SetVolume(Text);
  end;

  { Set the Audio Output Device. }
  if HasOption('o', 'output') then
  begin
    SetAudioOutput(GetOptionValue('o', 'output'));
  end;

  { Set the priority text is spoken. }
  if HasOption('p', 'priority') then
  begin
    Text := GetOptionValue('p', 'priority');
    SetPriority(Text);
  end;

  { Set the rate text is spoken. }
  if HasOption('r', 'rate') then
  begin
    Text := GetOptionValue('r', 'rate');
    SetRate(Text);
  end;

  { Set the voice. }
  if HasOption('v', 'voice') then
  begin
    SetVoice(GetOptionValue('v', 'voice'));
  end;

  { Setup the file to write text to. }
  if HasOption('w', 'write-to-file') then
  begin
    SetupOutput(GetOptionValue('w', 'write-to-file'), HasOption('a', 'append'));
  end;

  { Setup the wav file to record text to. }
  if HasOption('W', 'wav-file') then
  begin
    SetupOutputWav(GetOptionValue('W', 'wav-file'));
  end;

  { These options must be last since they trigger a speech action. }

  { Speak the contents of a text file. }
  if HasOption('f', 'speak-file') then
  begin
    SpeakFile(GetOptionValue('f', 'speak-file'));
    Terminate;
    Exit;
  end;

  { If there are any non-Options speak them. }
  if (NonOptions.Count > 0) and not Terminated then
  begin
    SpeakList(NonOptions);
    Terminate;
    Exit
  end;
end;

{ Setup the command line options. }
procedure TTTSApp.SetupOptions;
begin
  ShortOptions := 'af:hl:Oo:p:r:Vv:w:W:';
  LongOptions := TStringList.Create;
  LongOptions.Add('help');
  LongOptions.Add('append');
  LongOptions.Add('output:');
  LongOptions.Add('outputs');
  LongOptions.Add('priority:');
  LongOptions.Add('rate:');
  LongOptions.Add('speak-file:');
  LongOptions.Add('voice:');
  LongOptions.Add('voices');
  LongOptions.Add('volume:');
  LongOptions.Add('write-to-file:');
  LongOptions.Add('wav-file:');
end;

{ ----------========== Helper Methods ==========----- }

{ Write a list of audio outputs, along with the output ID #, to STDOUT. }
procedure TTTSApp.ListOutputs();
var
  OutputIndex : Integer;
  Outputs     : TstringList;
begin
  Outputs := SpVoice.GetAudioOutputNames();
  WriteLn('Available Audio Outputs:');
  for OutputIndex := 0 to Outputs.Count - 1 do
    WriteLn(OutputIndex, ' - ', Outputs[OutputIndex]);
end;

{ Write a list of available voices, along with the Voice ID #, to STDOUT. }
procedure TTTSApp.ListVoices();
var
  VoiceIndex : Integer;
  Voices     : TstringList;
begin
  Voices := SpVoice.GetVoiceNames();
  WriteLn('Available voices:');
  for VoiceIndex := 0 to Voices.Count - 1 do
    WriteLn(VoiceIndex, ' - ', Voices[VoiceIndex]);
end;

{ Set the Audio Output Device }
procedure TTTSApp.SetAudioOutput(NewOutput : String);
var
  OutputID : Integer;
begin
try
    if TryStrToInt(NewOutput, OutputID) then
      SpVoice.SetAudioOutputID(OutputID)
    else
      SpVoice.SetAudioOutputName(NewOutput);
  except
    on E: EArgumentException do
    begin
      WriteLn(E.Message);
      WriteLn('Use the -O option to see available audio devices.');
      Terminate;
    end;
  end;
end;

procedure TTTSApp.SetPriority(NewPriority : String);
var
  Priority : Integer;
begin
  if TryStrToInt(NewPriority, Priority) then
    try
      SpVoice.Priority := Priority
    except
      on E: EArgumentOutOfRangeException do
      begin
        WriteLn('Invalid value for Priority: ', E.Message);
        Terminate;
      end;
    end
  else
  begin
    Writeln('Invalid Priority "', NewPriority, '". ', SpVoice_priority_valid_values);
    Terminate;
  end;
end;

procedure TTTSApp.SetRate(NewRate : String);
var
  Rate : Integer;
begin
  if TryStrToInt(NewRate, Rate) then
    try
      SpVoice.Rate := Rate
    except
      on E: EArgumentOutOfRangeException do
      begin
        WriteLn('Invalid value for Range: ', E.Message);
        Terminate;
      end;
    end
  else
  begin
    Writeln('Invalid Rate "', NewRate, '". ', SpVoice_rate_valid_values);
    Terminate;
  end;
end;

{ Setup the file to write text to. }
procedure TTTSApp.SetupOutput(FileName : String; AppendFile : Boolean);
begin
  WriteText := True;
  AssignFile(OutputFile, FileName);
  try
    { Open the file. }
    if AppendFile then
      append(OutputFile)
    else
      rewrite(OutputFile);
  except
    on E: EInOutError do
      writeln('Unable to o open the file "', FileName, '" for writing. ', E.Message);
  end;
end;

{ Setup the wav file to record text to. }
procedure TTTSApp.SetupOutputWav(FileName : String);
begin
  WriteWav := True;
  SpFileStream := TSpFileStream.Create;
  try
    { Open the wav file. }
    SpFileStream.OpenStreamWrite(FileName);
  except
    on E: EInOutError do
      writeln('Unable to o open the wav file "', FileName, '" for writing. ', E.Message);
  end;
  SpVoice.AudioOutputStream := SpFileStream;
end;

{ Set the Voice. }
procedure TTTSApp.SetVoice(NewVoice : String);
var
  VoiceID : Integer;
begin
  try
    if TryStrToInt(NewVoice, VoiceID) then
      SpVoice.SetVoiceID(VoiceID)
    else
      SpVoice.SetVoiceName(NewVoice);
  except
    on E: Exception do
    begin
      WriteLn(E.Message);
      WriteLn('Use the -V option to see available voices.');
      Terminate;
    end;
  end;
end;

{ Set the Volume. }
procedure TTTSApp.SetVolume(NewVolume : String);
var
  Volume : Integer;
begin
  if TryStrToInt(NewVolume, Volume) then
    try
      SpVoice.Volume := Volume
    except
      on E: EArgumentOutOfRangeException do
      begin
        WriteLn('Invalid value for Volume: ', E.Message);
        Terminate;
      end;
    end
  else
  begin
    Writeln('Invalid Volume "', NewVolume, '". ', SpVoice_volume_valid_values);
    Terminate;
  end;
end;

{ ----------========== Speech Methods ==========----- }

{ Read text from the user then speak it. }
procedure TTTSApp.ReadSpeakLoop();
var
  Text : String;
begin
  WriteLn('Text entered will be spoken.');
  WriteLn('Enter a blank line to exit.');
  if WriteText then
    Writeln('Enter "\" to speak an empty string and write a blank line.');
  while True do
  begin
    Write('> ');
    ReadLn(Text);
    if Length(Text) = 0 then
      Break;
    if Text = '\' then
      Text := '';
    SpeakText(Text);
  end;
end;

{ Speak te contents of a text file. }
procedure TTTSApp.SpeakFile(FileName : String);
var
  FileHandle : TextFile;
  Text :       String;
begin
  AssignFile(FileHandle, FileName);
  try
      { Open the file. }
      reset(FileHandle);
      { Speak the contents of the file. }
      while not eof(FileHandle) do
      begin
        readln(FileHandle, Text);
        SpeakText(Text);
      end;
      { Close the file. }
      CloseFile(FileHandle);
    except
      on E: EInOutError do
      writeln('Unable to o open the file "', FileName, '" for reading. ', E.Message);
    end;

end;

{ Speak a list of strings. }
procedure TTTSApp.SpeakList(List : TStringList);
var
  ListIndex : Integer;
begin
  for ListIndex := 0 to  List.Count - 1 do
  begin
    SpeakText(List[ListIndex]);
  end;
end;

{ Speak some text. Also writes the text to an output file. }
procedure TTTSApp.SpeakText(Text : String);
begin
  if WriteText then
  begin
    WriteLn(OutputFile, Text);
    Flush(OutputFile);
  end;
  SpVoice.Speak(Text);
end;

Var
  App : TTTSApp;

begin
  App := TTTSApp.Create(Nil);
  App.Initialize;
  App.Title := 'SAPI TTS Application.';
  App.Run;
  App.Free;
end.