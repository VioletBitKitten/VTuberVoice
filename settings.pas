{
  Settings for the VtuberVoice application.

  https://github.com/VioletBitKitten/VTuberVoice

  Copyright (c) 2023 Violet Bit Kitten

  Distributed under the MIT license. Please see the file LICENSE.
}

{ Modern Pascal Directives }
{$mode objfpc}{$H+}{$J-}

unit settings;

interface

uses
  inifiles, sysutils;

const
  { Header for a new INI file. }
  VTVDefaultINIHeader : Array[1..21] of String = (
    '; Configuration file for VTuberVoice.',
    '; TTS Software for VTubers who don''t or can''t speak.',
    ';',
    '; https://github.com/VioletBitKitten/VTuberVoice',
    ';',
    '; Copyright (c) 2023 Violet Bit Kitten',
    ';',
    '; Distributed under the MIT license. Please see the file LICENSE.',
    ';',
    '; This file is automatically updated when quitting VTS.',
    '; Comments will be preserved when updating the file.',
    ';',
    '; General settings.',
    '; AudioOutput = Speakers ; Set the audio output device.',
    '; OutputFile = tts_out.txt ; Set the output file spoken text is written to.',
    '; OutputAppend = 1 ; Append text to the output file, 1 for yes, 0 for no.',
    '; Priority = 0 ; Set the priorty for speech, 0 to 2. Rarely needed.',
    '; Rate = 0 ; Set the rate text is spoken, -10 to 10. Rarely needed.',
    '; Voice = Microsoft David ; Set the Voice text is spoken in.',
    '; Volume = 100 ; Set the volume text is spoken at.',
    '[General]'
  );

type
  TVTVSettings = class
  private
    { INI file settings. }
    IniFile     : TIniFile;
    IniFileName : String;
    { General Settings. }
    GeneralAudioOutput  : String;
    GeneralOutputFile   : String;
    GeneralOutputAppend : Boolean;
    GeneralPriority     : Integer;
    GeneralRate         : Integer;
    GeneralVoice        : String;
    GeneralVolume       : Integer;
    procedure CreateINIFile;
    { Property Methods }
    function GetFileName            : String;
    function GetGeneralAudioOutput  : String;
    function GetGeneralOutputAppend : Boolean;
    function GetGeneralOutputFile   : String;
    function GetGeneralPriority     : Integer;
    function GetGeneralRate         : Integer;
    function GetGeneralVoice        : String;
    function GetGeneralVolume       : Integer;
    procedure SetGeneralAudioOutput(NewOutput : String);
    procedure SetGeneralOutputAppend(NewAppend : Boolean);
    procedure SetGeneralOutputFile(NewFile : String);
    procedure SetGeneralPriority(NewPriority : Integer);
    procedure SetGeneralRate(NewRate : Integer);
    procedure SetGeneralVoice(NewVoice : String);
    procedure SetGeneralVolume(NewVolume : Integer);

  public
    constructor Create(OverrideFileName : String = '');
    destructor Destroy;override;
    procedure LoadSettings;
    procedure SaveSettings;
    property FileName     : String  read GetFileName;
    property AudioOutput  : String  read GetGeneralAudioOutput  write SetGeneralAudioOutput;
    property OutputAppend : Boolean read GetGeneralOutputAppend write SetGeneralOutputAppend;
    property OutputFile   : String  read GetGeneralOutputFile   write SetGeneralOutputFile;
    property Priority     : Integer read GetGeneralPriority     write SetGeneralPriority;
    property Rate         : Integer read GetGeneralRate         write SetGeneralRate;
    property Voice        : String  read GetGeneralVoice        write SetGeneralVoice;
    property Volume       : Integer read GetGeneralVolume       write SetGeneralVolume;
  end;

implementation

{ ----------========== VTVSettings Private Methods ==========---------- }

procedure TVTVSettings.CreateINIFile;
var
  OutputTextFile : TextFile;
  OutputString   : String;
begin
  { Open the INI file. }
  AssignFile(OutputTextFile, IniFileName);
  try
    Rewrite(OutputTextFile);
  except
    on E: EInOutError do
    begin
      OutputString := 'Unable to create the configuration file "' + IniFileName + '". Error: ' + E.MEssage;
      raise EInOutError.Create(OutputString);
      Halt;
    end;
  end;

  { Write out the default header to the file. }
  WriteLn('Creating new configuration file: ', IniFileName);
  for OUtputString in VTVDefaultINIHeader do
    WriteLn(OutputTextFile, OutputString);
  Flush(OutputTextFile);
  CloseFile(OutputTextFile);
end;

{ ----------========== VTVSettings Property Methods ==========---------- }

function TVTVSettings.GetFileName : String;
begin
  Result := IniFileName;
end;
function TVTVSettings.GetGeneralAudioOutput : String;
begin
  Result := GeneralAudioOutput;
end;
function TVTVSettings.GetGeneralOutputAppend : Boolean;
begin
  Result := GeneralOutputAppend;
end;
function TVTVSettings.GetGeneralOutputFile : String;
begin
  Result := GeneralOutputFile;
end;
function TVTVSettings.GetGeneralPriority : Integer;
begin
  Result := GeneralPriority;
end;
function TVTVSettings.GetGeneralRate : Integer;
begin
  Result := GeneralRate;
end;
function TVTVSettings.GetGeneralVoice : String;
begin
  Result := GeneralVoice;
end;
function TVTVSettings.GetGeneralVolume : Integer;
begin
  Result := GeneralVolume
end;
procedure TVTVSettings.SetGeneralAudioOutput(NewOutput : String);
begin
  GeneralAudioOutput := NewOutput;
  IniFile.WriteString('General', 'AudioOutput', GeneralAudioOutput);
end;
procedure TVTVSettings.SetGeneralOutputAppend(Newappend : Boolean);
begin
  GeneralOutputAppend := NewAppend;
  IniFile.WriteBool('General', 'OutputAppend', GeneralOutputAppend);
end;
procedure TVTVSettings.SetGeneralOutputFile(NewFile : String);
begin
  GeneralOutputFile := NewFile;
  IniFile.WriteString('General', 'OutputFile', GeneralOutputFile);
end;
procedure TVTVSettings.SetGeneralPriority(NewPriority : Integer);
begin
  GeneralPriority := NewPriority;
  IniFile.WriteInteger('General', 'Priority', GeneralPriority);
end;
procedure TVTVSettings.SetGeneralRate(NewRate : Integer);
begin
  GeneralRate := NewRate;
  IniFile.WriteInteger('General', 'Rate', GeneralRate);
end;
procedure TVTVSettings.SetGeneralVoice(NewVoice : String);
begin
  GeneralVoice := NewVoice;
  IniFile.WriteString('General', 'Voice', GeneralVoice);
end;
procedure TVTVSettings.SetGeneralVolume(NewVolume : Integer);
begin
  GeneralVolume := NewVolume;
  IniFile.WriteInteger('General', 'Volume', GeneralVolume);
end;

{ ----------========== VTVSettings Public Methods ==========---------- }

{ Create the VTVSettings object. }
constructor TVTVSettings.Create(OverrideFileName : String = '');
var
  ConfigDir    : String;
  NewFileName  : String;
  CreateResult : Boolean;
begin
  { Set the application configuration directory and INI file name. }
  ConfigDir := GetAppConfigDir(False);
  NewFileName := ApplicationName + '.ini';

  if OverrideFileName <> '' then
    { Override the INI file if the user requests. }
    IniFileName := OverrideFileName
  else
    { Look for the INI file. Check the current directory first. }
    if FileExists(NewFileName) then
      IniFileName := NewFileName
    else
    begin
      { Use the Global INI file. }
      IniFileName := ConfigDir + NewFileName;
      if not DirectoryExists(ConfigDir) then
      begin
        { Attempt to create the application configuration directory. }
        CreateResult := ForceDirectories(ConfigDir);
        if not CreateResult then
        begin
          { If the directory can not be created use a local file instead. }
          IniFileName := NewFileName;
        end;
      end;
    end;

  { If the INI file does not exist yet create a file with some useful comments. }
  if not FileExists(IniFileName) then
    CreateINIFile;

  { Create the INI Object. Cache objects by default. }
  IniFile := TIniFile.Create(IniFileName);
  IniFile.CacheUpdates := False;

  { Load settings from the INI file. }
  LoadSettings;
end;

destructor TVTVSettings.Destroy;
begin
  SaveSettings;
  FreeAndNil(IniFile);
  inherited;
end;

{ Load settings from an INI file. }
procedure TVTVSettings.LoadSettings;
begin
  GeneralAudioOutput := IniFile.ReadString('General', 'AudioOutput', '');
  GeneralOutputAppend:= IniFile.ReadBool('General', 'OutputAppend', False);
  GeneralOutputFile  := IniFile.ReadString('General', 'OutputFile', '');
  GeneralPriority    := IniFile.ReadInteger('General', 'Priority', 0);
  GeneralRate        := IniFile.ReadInteger('General', 'Rate', 0);
  GeneralVoice       := IniFile.ReadString('General', 'Voice', '');
  GeneralVolume      := IniFile.ReadInteger('General', 'Volume', 100);
end;

{ Write settings to an INI file. }
procedure TVTVSettings.SaveSettings;
begin
  IniFile.CacheUpdates := True;
  IniFile.WriteString('General', 'AudioOutput', GeneralAudioOutput);
  IniFile.WriteBool('General', 'OutputAppend', GeneralOutputAppend);
  IniFile.WriteString('General', 'OutputFile', GeneralOutputFile);
  IniFile.WriteInteger('General', 'Priority', GeneralPriority);
  IniFile.WriteInteger('General', 'Rate', GeneralRate);
  IniFile.WriteString('General', 'Voice', GeneralVoice);
  IniFile.WriteInteger('General', 'Volume', GeneralVolume);
  IniFile.CacheUpdates := False;
  IniFile.UpdateFile;
end;

end.