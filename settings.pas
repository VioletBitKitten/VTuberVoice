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
  VTVDefaultINIHeader : Array[1..17] of String = (
    '; Configuration file for VTuberVoice, vtv.exe',
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
    '; Voice = Microsoft David ; Set the Voice text is spoken in.',
    '[General]'
  );

type
  TVTVSettings = class
  private
    { INI file settings. }
    IniFile     : TIniFile;
    IniFileName : String;
    { General Settings. }
    GeneralAudioOutput : String;
    GeneralOutputFile  : String;
    GeneralVoice       : String;
    procedure CreateINIFile;
    { Property Methods }
    function GetFileName : String;
    function GetAudioOutput : String;
    function GetOutputFile : String;
    function GetVoice : String;
  public
    constructor Create;
    procedure LoadSettings;
    procedure SaveSettings;
    property FileName : String read GetFileName;
    property AudioOutput : String read GetAudioOutput;
    property OutputFile : String read GetOutputFile;
    property Voice : String read GetVoice;
  end;

implementation

{ ----------========== VTVSettings Private Methods ==========---------- }

procedure TVTVSettings.CreateINIFile;
var
  OutputTextFile : TextFile;
  OutputString : String;
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

function TVTVSettings.GetAudioOutput : String;
begin
  Result := GeneralAudioOutput;
end;

function TVTVSettings.GetOutputFile : String;
begin
  Result := GeneralOutputFile;
end;

function TVTVSettings.GetVoice : String;
begin
  Result := GeneralVoice;
end;

{ ----------========== VTVSettings Public Methods ==========---------- }

{ Create the VTVSettings object. }
constructor TVTVSettings.Create;
var
  ConfigDir    : String;
  NewFileName  : String;
  CreateResult : Boolean;
begin
  { Set the application configuration directory and INI file name. }
  ConfigDir := GetAppConfigDir(False);
  NewFileName := ApplicationName + '.ini';

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

{ Load settings from an INI file. }
procedure TVTVSettings.LoadSettings;
begin
  GeneralAudioOutput := IniFile.ReadString('General', 'AudioOutput', '');
  GeneralOutputFile  := IniFile.ReadString('General', 'OutputFile', '');
  GeneralVoice       := IniFile.ReadString('General', 'Voice', '');
end;

{ Write settings to an INI file. }
procedure TVTVSettings.SaveSettings;
begin
  IniFile.WriteString('General', 'AudioOutput', GeneralAudioOutput);
  IniFile.WriteString('General', 'OutputFile', GeneralOutputFile);
  IniFile.WriteString('General', 'Voice', GeneralVoice);
end;

end.