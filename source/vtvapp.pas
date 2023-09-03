{
  VTuberVoice application code.

  TTS program using the Free Pascal SAPI Library.

  https://github.com/VioletBitKitten/VTuberVoice

  Copyright (c) 2023 Violet Bit Kitten

  Distributed under the MIT license. Please see the file LICENSE.
}

{ Modern Pascal Directives }
{$mode objfpc}{$H+}{$J-}

unit vtvapp;

interface

uses
  classes, custapp, sysutils, sapi, vtvsettings, vtvhelp;

type
  TVTVApp = Class(TCustomApplication)
  private
  // TODO: Clean some of these up. Organic growth is messy.
    AbbrevList      : TStringList;
    AliasList       : TStringList;
    Diagnostic      : Boolean;
    Interactive     : Boolean;
    LongOptions     : TStringList;
    LogFile         : TextFile;
    LogFormat       : String;
    LogOutput       : Boolean;
    NonOptions      : TStringList;
    OutputAppend    : Boolean;
    OutputFile      : TextFile;
    OutputFileName  : String;
    Settings        : TVTVSettings;
    SettingsFile    : String;
    ShortOptions    : String;
    SpFileStream    : TSpFileStream;
    SpVoice         : TSpVoice;
    WriteText       : Boolean;
    WriteWav        : Boolean;
  public
    { Application Setup }
    destructor Destroy; override;
    procedure DoRun; override;
    procedure Help;
    procedure Initialize; override;
    procedure LoadSettings;
    { Command Line Options }
    procedure ProcessOptions;
    procedure ProcessOptionsSettings;
    procedure ProcessOptionsSpeech;
    procedure SetupOptions;
    { Helper Methods }
    procedure ListVoices;
    procedure ListOutputs;
    procedure LogWriteOutput(Text : String);
    procedure PrintDiagMessage(Message : String);
    procedure PrintDiagData;
    procedure SetAudioOutput(NewOutput : String);
    procedure SetPriority(NewPriority : String);
    procedure SetRate(NewRate : String);
    procedure SetupLogOutput(FileName : String);
    procedure SetupOutput(FileName : String; AppendFile : Boolean);
    procedure SetupOutputWav(FileName : String);
    procedure SetVoice(NewVoice : String);
    Procedure SetVolume(NewVolume : String);
    procedure WriteOutputFile(Text : String);
    { Speech Methods }
    procedure SpeakAlias(AliasName : String);
    procedure SpeakFile(FileName : String);
    procedure SpeakList(List : TStringList);
    procedure SpeakText(Text : String);
    { Interactive Read-Speak Loop }
    procedure HandleCommand(UserInput : String);
    procedure ReadSpeakLoop;
    { Handle commands }
    procedure HandleCommandAbbrev(AbbrevName : String);
    procedure HandleCommandAlias(AliasName : String);
    procedure HandleCommandDiag(NewDiag : String);
    procedure HandleCommandOutput(NewOutput : String);
    procedure HandleCommandPriority(NewPriority : String);
    procedure HandleCommandRate(NewRate : String);
    procedure HandleCommandSaveSettings;
    procedure HandleCommandVoice(NewVoice : String);
    procedure HandleCommandVolume(NewVolume : String);
  end;

implementation

{ ----------========== Application Setup ==========----- }

{ Perform cleanup. }
destructor TVTVApp.Destroy;
begin
  LogWriteOutput('Shutdown of ' + Title);
  PrintDiagMessage('Shutting down VTuberVoice.');
  PrintDiagMessage('Freeing the Voice object.');
  FreeAndNil(SpVoice);

  if Settings <> Nil then
  begin
    PrintDiagMessage('Saving Settings.');
    try
      Settings.SaveSettings;
    except
      on E: EFCreateError do
      begin
        WriteLn('Unable to save settings: ', E.Message);
      end;
    end;
    PrintDiagMessage('Freeing the Settings object.');
    FreeandNil(Settings);
  end;

  if WriteText then
  begin
    PrintDiagMessage('Closing the output file.');
    CloseFile(OutputFile);
  end;

  if LogOutput then
  begin
    PrintDiagMessage('Closing the log file.');
    CloseFile(LogFile);
  end;

  if WriteWav then
  begin
    PrintDiagMessage('Closing the output wave file.');
    SpFileStream.Close;
  end;

  inherited;
end;

{ Run the application. }
procedure TVTVApp.DoRun;
begin
  ProcessOptions;
  if Terminated then Exit;
  Settings := TVTVSettings.Create(SettingsFile);
  LoadSettings;
  ProcessOptionsSettings;
  if Terminated then Exit;
  if Diagnostic then
    PrintDiagData;
  ProcessOptionsSpeech;
  if not Terminated then
  begin
    PrintDiagMessage('Entering Read/Speak loop.');
    ReadSpeakLoop;
    Terminate;
  end;
end;

{ Print the help text. }
procedure TVTVApp.Help;
begin
  WriteLn(Title);
  WriteLn('Usage: ', ExtractFileName(ExeName), ' [OPTION] [TEXT]...');
  WriteLn('Speak text using Micrsoft SAPI.');
  WriteLn('If text is given ont he command line then the');
  WriteLn('text will be spoken and the program will exit.');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  -a , --append             Append text when writing text to a file.');
  WriteLn('  -c , --config=FILE        Use the specificed file for the configuration.');
  WriteLn('  -D , --diag               Print out diagnostic information.');
  WriteLn('  -f , --speak-file         Speak the contents of a text file.');
  WriteLn('  -h , --help               Prints this help.');
  WriteLn('  -l , --volume=VOLUME      Set the volume text is spoken at. ', SpVoice_volume_valid_values);
  WriteLn('  -o , --outut=OUTPUT       Set the putput text is spoken to by the name or ID number.');
  WriteLn('  -O , --outputs            List the available audio output devices with ID #.');
  WriteLn('  -p , --priority=PRIORITY  Set the priority for speaking. ', SpVoice_priority_valid_values);
  WriteLn('  -r , --rate=RATE          Set the rate text is spoken. ', SpVoice_rate_valid_values);
  WriteLn('  -v , --voice=VOICE        Set the voice text is spoken with by the name or ID number.');
  WriteLn('  -V , --voices             List the available voices with ID #.');
  WriteLn('  -w , --write-to-file=FILE Write all text spoken to a text file.');
  WriteLn('  -W , --wav-file=FILE      All text is recorded to a WAV file instead of being spoken.');
  WriteLn;
  if Settings <> Nil then
    WriteLn('Configuration file: ', Settings.FileName);
  WriteLn('For more information see: https://github.com/VioletBitKitten/VTuberVoice');
end;

{ Initialize the application. }
procedure TVTVApp.Initialize;
begin
  SetupOptions;
  SpVoice := TSpVoice.Create;
  SpVoice.ExceptionsEnabled := True;
  Diagnostic := False;
  Interactive := False;
  SettingsFile := '';
  OutputFileName := '';
  WriteText := False;
  WriteWav := False;
  LogOutput := False;
end;

procedure TVTVApp.LoadSettings;
begin
  PrintDiagMessage('Loading settings from the configuration file: ' + Settings.FileName);

  { SpVoice Settings. }
  if Settings.AudioOutput <> '' then
    SetAudioOutput(Settings.AudioOutput);
  if Settings.OutputFile <> '' then
    SetupOutput(Settings.OutputFile, Settings.OutputAppend);
  SetPriority(IntToStr(Settings.Priority));
  SetRate(IntToStr(Settings.Rate));
  if Settings.Voice <> '' then
    SetVoice(Settings.Voice);
  SetVolume(IntToStr(Settings.Volume));
  AliasList := Settings.Aliases;
  AbbrevList := Settings.Abbreviations;

  { Log Settings. }
  if Settings.LogOutput then
  begin
    SetupLogOutput(Settings.LogFile);
    LogFormat := Settings.LogFormat;
    LogWriteOutput('Startup of ' + Title);
  end;
end;

{ ----------========== Command Line Options ==========----- }

{ Process the command line options that need to be handles first. }
procedure TVTVApp.ProcessOptions;
var
  Text : String;
begin
  { Check the command line options. }
  NonOptions := TStringList.Create;
  Text := CheckOptions(ShortOptions, LongOptions, Nil, NonOptions, True);

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

  { Settings that affect the program. }

  { Enable diagnostic mode. }
  if HasOption('D', 'diag') then
  begin
    Diagnostic := True;
  end;

  { Override the configuration file. }
  if HasOption('c', 'config') then
  begin
    SettingsFile := GetOptionValue('c', 'config')
  end;
end;

{ Process the command line options that change settings. }
procedure TVTVApp.ProcessOptionsSettings;
var
  Text : String;
begin
  {
    Display the help text. This will exit the program.
    Must wait until after the settings are loaded so the configuration file can be printed.
  }
  if HasOption('h', 'help') then
  begin
    Help;
    Terminate;
    Exit;
  end;

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
end;

{ Process the command line options that trigger a speech actions. }
procedure TVTVApp.ProcessOptionsSpeech;
begin
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
    PrintDiagMessage('Speaking text from command line.');
    SpeakList(NonOptions);
    Terminate;
    Exit
  end;
end;

{ Setup the command line options. }
procedure TVTVApp.SetupOptions;
begin
  ShortOptions := 'ac:Df:hl:Oo:p:r:Vv:w:W:';
  LongOptions := TStringList.Create;
  LongOptions.Add('help');
  LongOptions.Add('append');
  LongOptions.Add('config:');
  LongOptions.Add('diag');
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
procedure TVTVApp.ListOutputs();
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
procedure TVTVApp.ListVoices();
var
  VoiceIndex : Integer;
  Voices     : TstringList;
begin
  Voices := SpVoice.GetVoiceNames();
  WriteLn('Available voices:');
  for VoiceIndex := 0 to Voices.Count - 1 do
    WriteLn(VoiceIndex, ' - ', Voices[VoiceIndex]);
end;

{ Write text to the log file. }
procedure TVTVApp.LogWriteOutput(Text : String);
var
  Timestamp : String;
begin
  { Do not write to the log if it is not enabled. }
  if not LogOutput then
    Exit;

  { Do not bother with empty lines. }
  if Text = '' then
    Exit;

  Timestamp := FormatDateTime(LogFormat, Now);
  WriteLn(LogFile, Timestamp, ': ', Text);
  Flush(LogFile);
end;

{ Print a diagnostic message if Diagnostic is true. }
procedure TVTVApp.PrintDiagMessage(Message : String);
begin
if Diagnostic then
  WriteLn('Diagnostic: ', Message);
end;

{ Write diagnostic data. }
procedure TVTVApp.PrintDiagData;
var
  Temp : Variant;
begin
  WriteLn;
  WriteLn('Diagnostic Data:');
  Temp := SpVoice.AudioOutput;
  WriteLn('Output device: ', Temp.GetDescription);
  Temp := SpVoice.Voice;
  WriteLn('Voice: ', Temp.GetDescription);
  WriteLn('Volume: ', SpVoice.Volume);
  WriteLn('Rate: ', SpVoice.Rate);
  WriteLn('Priority: ', SpVoice.Priority);
  if WriteText then
  begin
    if OutputAppend then
      Write('Appending')
    else
      Write('Writing');
    WriteLn(' to the file: ', OutputFileName);
  end;
  Writeln;
end;

{ Set the Audio Output Device }
procedure TVTVApp.SetAudioOutput(NewOutput : String);
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
      if Interactive then
        WriteLn('Use "/outputs" to see available audio devices.')
      else
      begin
        WriteLn('Use the -O option to see available audio devices.');
        Terminate;
      end;
    end;
  end;
end;

{ Set the speech priority. }
procedure TVTVApp.SetPriority(NewPriority : String);
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
        if not Interactive then
          Terminate;
      end;
    end
  else
  begin
    Writeln('Invalid Priority "', NewPriority, '". ', SpVoice_priority_valid_values);
    if not Interactive then
      Terminate;
  end;
end;

{ Set the speech rate. }
procedure TVTVApp.SetRate(NewRate : String);
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
        if not Interactive then
          Terminate;
      end;
    end
  else
  begin
    Writeln('Invalid Rate "', NewRate, '". ', SpVoice_rate_valid_values);
    if not Interactive then
      Terminate;
  end;
end;


{ Setup the log file to write spoken text to. }
procedure TVTVApp.SetupLogOutput(FileName : String);
begin
  PrintDiagMessage('Writing output to the log file: ' + FileName);
  LogOutput := True;
  AssignFile(LogFile, FileName);
  try
    { Open the file. }
    if FileExists(FileName) then
      append(LogFile)
    else
      rewrite(LogFile);
  except
    on E: EInOutError do
      begin
        writeln('Unable to open the file "', FileName, '" for writing. ', E.Message);
        if not Interactive then
          Terminate;
      end
  end;
end;

{ Setup the file to write text to. }
procedure TVTVApp.SetupOutput(FileName : String; AppendFile : Boolean);
begin
  PrintDiagMessage('Writing output to the file: ' + FileName);
  WriteText := True;
  AssignFile(OutputFile, FileName);
  try
    { Open the file. }
    if AppendFile and FileExists(FileName) then
      append(OutputFile)
    else
      rewrite(OutputFile);
    OutputFileName := FileName;
    OutputAppend := AppendFile;
  except
    on E: EInOutError do
      begin
        writeln('Unable to open the file "', FileName, '" for writing. ', E.Message);
        if not Interactive then
          Terminate;
      end
  end;
end;

{ Setup the wav file to record text to. }
procedure TVTVApp.SetupOutputWav(FileName : String);
begin
  WriteWav := True;
  SpFileStream := TSpFileStream.Create;
  try
    { Open the wav file. }
    SpFileStream.OpenStreamWrite(FileName);
  except
    on E: EInOutError do
    begin
        writeln('Unable to o open the wav file "', FileName, '" for writing. ', E.Message);
        if not Interactive then
          Terminate;
      end;
  end;
  SpVoice.AudioOutputStream := SpFileStream;
end;

{ Set the Voice. }
procedure TVTVApp.SetVoice(NewVoice : String);
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
      if Interactive then
        WriteLn('Use "/voices" to see available voices.')
      else
      begin
        WriteLn('Use the -V option to see available voices.');
        Terminate;
      end;
    end;
  end;
end;

{ Set the Volume. }
procedure TVTVApp.SetVolume(NewVolume : String);
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
        if not Interactive then
          Terminate;
      end;
    end
  else
  begin
    Writeln('Invalid Volume "', NewVolume, '". ', SpVoice_volume_valid_values);
    if not Interactive then
      Terminate;
  end;
end;

{ Write to the output file. }
procedure TVTVApp.WriteOutputFile(Text : String);
begin
  WriteLn(OutputFile, Text);
  Flush(OutputFile);
end;

{ ----------========== Speech Methods ==========----- }

{ Speak an alias. }
procedure TVTVApp.SpeakAlias(AliasName : String);
var
  Text : String;
begin
  Text := AliasList.Values[AliasName];
  if Text <> '' then
    SpeakText(Text)
  else
    Writeln('Invalid alias: "', AliasName, '".');
end;

{ Speak te contents of a text file. }
procedure TVTVApp.SpeakFile(FileName : String);
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
procedure TVTVApp.SpeakList(List : TStringList);
var
  ListIndex : Integer;
begin
  for ListIndex := 0 to  List.Count - 1 do
  begin
    SpeakText(List[ListIndex]);
  end;
end;

{ Speak some text. Also writes the text to an output file. }
procedure TVTVApp.SpeakText(Text : String);
var
  AbbrevIndex : Integer;
  OldWord     : String;
  NewWord     : String;
begin
  { Write the text before making changes for speaking the text. }
  if WriteText then
  begin
    WriteOutputFile(Text);
  end;

  { Log the text is requested. }
  LogWriteOutput(Text);

  { Replace abbreviations before speaking. }
  for AbbrevIndex := 0 to AbbrevList.Count - 1 do
  begin
    OldWord := AbbrevList.Names[AbbrevIndex];
    NewWord := AbbrevList.ValueFromIndex[AbbrevIndex];
    Text := Text.Replace(OldWord, NewWord,[rfReplaceAll, rfIgnoreCase]);
  end;

  { Speak the updated text. }
  PrintDiagMessage('Speaking: ' + Text);
  SpVoice.Speak(Text);
end;

{ ----------========== Interactive Read-Speak Loop ==========----- }

{ Handle commands from the user. }
procedure TVTVApp.HandleCommand(UserInput : String);
var
  Command     : String;
  Arg         : String;
  TempIndex   : SizeInt ;
begin
  { Split the user input into the command and a list of arguments. }
  TempIndex := UserInput.IndexOf(' ');
  if (TempIndex > 0) then
  begin
    { Split the user input. }
    Command := UserInput.substring(1, TempIndex - 1);
    Arg := UserInput.substring(TempIndex + 1);
  end
  else
  begin
    { Use the whole string, minus the command character. }
    Command := UserInput.substring(1);
    Arg := '';
  end;

  { Do something with the command. }
  case Command of // Special cases for upper case characters.
    'A'             : HandleCommandAbbrev(Arg);
    'V'             : ListVoices;
    'O'             : ListOutputs;
    else
      case (LowerCase(Command)) of
              'abbrev'  : HandleCommandAbbrev(Arg);
        'a',  'alias'   : HandleCommandAlias(Arg);
        'd',  'diag'    : HandleCommandDiag(Arg);
        'h',  'help'    : CommandHelp(Arg);
              'outputs' : ListOutputs;
        'o',  'output'  : HandleCommandOutput(Arg);
        'p',  'priority': HandleCommandPriority(Arg);
        'q',  'quit'    : Terminate;
        'r',  'rate'    : HandleCommandRate(Arg);
        's',  'save'    : HandleCommandSaveSettings;
        'v',  'voice'   : HandleCommandVoice(Arg);
              'voices'  : ListVoices;
        'l',  'volume'  : HandleCommandVolume(Arg);
      else
        WriteLn('Unknown command "', Command, '". Type /help for a list of commands.');
      end;
  end;
end;

{ Read text from the user then speak it. }
procedure TVTVApp.ReadSpeakLoop();
var
  Text : String;
begin
  { Set interactive mode to change some error messages and prevent terminating the app. }
  Interactive := True;

  { Greet the user. }
  WriteLn('Text entered will be spoken.');
  WriteLn('Type "/help" for help');
  WriteLn('Type /quit to exit.');
  if WriteText then
    Writeln('Enter "\" to write a blank line to the output file.');
  WriteLn('End a line with "\" to cancel the input.');

  { Read-Speak loop. }
  while True do
  begin
    Write('/help > ');
    ReadLn(Text);
    if Length(Text) = 0 then
      Continue
    else if Text = '\' then
    begin
      WriteOutputFile('');
      Continue;
    end
    else if Text = '?' then
    begin
      CommandHelp;
      Continue;
    end
    else if Text[Length(Text)] = '\' then
      Continue
    else if Text[1] = '#' then
    begin
      SpeakAlias(Text.Substring(1));
      Continue;
    end
    else if Text[1] = '/' then
    begin
      HandleCommand(Text);
      if Terminated then
        Break;
      Continue;
    end;
    SpeakText(Text);
  end;
end;

{ ----------========== Handle commands ==========----- }

procedure TVTVApp.HandleCommandAbbrev(AbbrevName : String);
var
  AbbrevIndex : Integer;
begin
  if AbbrevName <> '' then
    WriteLn(AbbrevName, ' - ', AbbrevList.Values[AbbrevName])
  else
    for AbbrevIndex := 0 to AbbrevList.Count - 1 do
      WriteLn(AbbrevList.Names[AbbrevIndex], ' - ', AbbrevList.ValueFromIndex[AbbrevIndex]);
end;

procedure TVTVApp.HandleCommandAlias(AliasName : String);
var
  AliasIndex : Integer;
begin
  if AliasName <> '' then
    WriteLn(AliasName, ' - ', AliasList.Values[AliasName])
  else
    for AliasIndex := 0 to AliasList.Count - 1 do
      WriteLn(AliasList.Names[AliasIndex], ' - ', AliasList.ValueFromIndex[AliasIndex]);
end;

procedure TVTVApp.HandleCommandDiag(NewDiag : String);
begin
  if NewDiag <> '' then
  begin
    if LowerCase(NewDiag) = 'true' then
      Diagnostic := True
    else
      Diagnostic := False;
  end
  else
    PrintDiagData;
end;

procedure TVTVApp.HandleCommandOutput(NewOutput : String);
var
  CurrentOutput : Variant;
begin
  if Length(NewOutput) = 0 then
  begin
    CurrentOutput := SpVoice.AudioOutput;
    WriteLn('Current Audio Output: ', CurrentOutput.GetDescription);
  end
  else
  begin
    SetAudioOutput(NewOutput);
    CurrentOutput := SpVoice.AudioOutput;
    {$warn 6058 off} // Stop the annoying "marked as inline is not inlined" errors.
    Settings.AudioOutput := CurrentOutput.GetDescription;
  end;
end;

procedure TVTVApp.HandleCommandPriority(NewPriority : String);
begin
  if Length(NewPriority) = 0 then
  begin
    WriteLn('Current Priority: ', SpVoice.Priority);
  end
  else
  begin
    SetPriority(NewPriority);
    Settings.Priority := SpVoice.Priority;
  end;
end;

procedure TVTVApp.HandleCommandRate(NewRate : String);
begin
  if Length(NewRate) = 0 then
  begin
    WriteLn('Current Rate: ', SpVoice.Rate);
  end
  else
  begin
    SetRate(NewRate);
    Settings.Rate := SpVoice.Rate;
  end;
end;

procedure TVTVApp.HandleCommandSaveSettings;
begin
  WriteLn('Saving settings...');
  Settings.SaveSettings;
end;

procedure TVTVApp.HandleCommandVoice(NewVoice : String);
var
  CurrentVoice : Variant;
begin
  if Length(NewVoice) = 0 then
  begin
    CurrentVoice := SpVoice.Voice;
    WriteLn('Current Voice: ', CurrentVoice.GetDescription);
  end
  else
  begin
    SetVoice(NewVoice);
    CurrentVoice := SpVoice.Voice;
    {$warn 6058 off} // Stop the annoying "marked as inline is not inlined" errors.
    Settings.Voice := CurrentVoice.GetDescription;
  end;
end;

procedure TVTVApp.HandleCommandVolume(NewVolume : String);
begin
  if Length(NewVolume) = 0 then
  begin
    WriteLn('Current Volume: ', SpVoice.Volume);
  end
  else
  begin
    SetVolume(NewVolume);
    Settings.Volume := SpVoice.Volume;
  end;
end;

end.