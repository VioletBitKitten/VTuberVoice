{
  Settings for the VtuberVoice application.

  https://github.com/VioletBitKitten/VTuberVoice

  Copyright (c) 2023 Violet Bit Kitten

  Distributed under the MIT license. Please see the file LICENSE.
}

{ Modern Pascal Directives }
{$mode objfpc}{$H+}{$J-}

unit vtvsettings;

interface

uses
  inifiles, sysutils, classes, filelist;

type
  TVTVSettings = class
  private
    { INI file settings. }
    IniFile               : TMemIniFile;
    IniFileName           : String;
    { General Settings. }
    FGeneralAudioOutput   : String;
    FGeneralOutputFile    : String;
    FGeneralOutputAppend  : Boolean;
    FGeneralPriority      : Integer;
    FGeneralRate          : Integer;
    FGeneralVoice         : String;
    FGeneralVolume        : Integer;
    { Backup Settings. }
    FBackupCreate         : Boolean;
    FBackupFormat         : String;
    FBackupWhen           : String;
    FBackupKeep           : Integer;
    FBackupFile           : String;
    { Logging Settings. }
    FLogDiag              : Boolean;
    FLogEnabled           : Boolean;
    FLogFile              : String;
    FLogFormat            : String;
    FLogInput             : Boolean;
    { Private Methods }
    procedure CleanBackups;
    procedure CreateBackupFile;
    procedure CreateINIFile;
    procedure MaybeBackup;
    { Property Methods }
    procedure SetGeneralAudioOutput(NewOutput : String);
    procedure SetGeneralOutputAppend(NewAppend : Boolean);
    procedure SetGeneralOutputFile(NewFile : String);
    procedure SetGeneralPriority(NewPriority : Integer);
    procedure SetGeneralRate(NewRate : Integer);
    procedure SetGeneralVoice(NewVoice : String);
    procedure SetGeneralVolume(NewVolume : Integer);
    procedure SetBackupCreate(NewCreate : Boolean);
    procedure SetBackupFormat(NewFormat : String);
    procedure SetBackupWhen(NewWhen : String);
    procedure SetBackupKeep(NewKeep : Integer);
    procedure SetBackupFile(NewFile : String);
    procedure SetLogDiag(NewDiag : Boolean);
    procedure SetLogEnabled(NewEnabled : Boolean);
    procedure SetLogFile(NewFile : String);
    procedure SetLogFormat(NewFormat : String);
    procedure SetLogInput(NewInput : Boolean);
    function GetAbbreviationList : TStringList;
    function GetAliasList : TStringList;
  public
    constructor Create(OverrideFileName : String = '');
    destructor Destroy; override;
    procedure LoadSettings;
    procedure SaveSettings;
    property FileName     : String  read IniFileName;
    { General Settings }
    property AudioOutput  : String  read FGeneralAudioOutput  write SetGeneralAudioOutput;
    property OutputAppend : Boolean read FGeneralOutputAppend write SetGeneralOutputAppend;
    property OutputFile   : String  read FGeneralOutputFile   write SetGeneralOutputFile;
    property Priority     : Integer read FGeneralPriority     write SetGeneralPriority;
    property Rate         : Integer read FGeneralRate         write SetGeneralRate;
    property Voice        : String  read FGeneralVoice        write SetGeneralVoice;
    property Volume       : Integer read FGeneralVolume       write SetGeneralVolume;
    { Backup Settings }
    property BackupCreate : Boolean read FBackupCreate        write SetBackupCreate;
    property BackupFormat : String  read FBackupFormat        write SetBackupFormat;
    property BackupWhen   : String  read FBackupWhen          write SetBackupWhen;
    property BackupKeep   : Integer read FBackupKeep          write SetBackupKeep;
    property BackupFile   : String  read FBackupFile          write SetBackupFile;
    { Logging Settings. }
    property LogDiag       : Boolean read FLogDiag            write SetLogDiag;
    property LogEnabled    : Boolean read FLogEnabled         write SetLogEnabled;
    property LogFile       : String  read FLogFile            write SetLogFile;
    property LogFormat     : String  read FLogFormat          write SetLogFormat;
    property LogInput      : Boolean read FLogInput           write SetLogInput;
    { Aliases and Abbreviations }
    property Aliases      : TStringList read GetAliasList;
    property Abbreviations: TStringList read GetAbbreviationList;
  end;

implementation

const
  { Default settings. }
  DefaultGeneralAudioOutput  = '';
  DefaultGeneralOutputAppend = False;
  DefaultGeneralOutputFile   = '';
  DefaultGeneralPriority     = 0;
  DefaultGeneralRate         = 0;
  DefaultGeneralVoice        = '';
  DefaultGeneralVolume       = 100;
  DefaultBackupCreate        = False;
  DefaultBackupFormat        = 'YYYY-MM-DD-hhmmsszz';
  DefaultBackupWhen          = 'Load';
  DefaultBackupKeep          = 20;
  DefaultBackupFile          = '';
  DefaultLogDiag             = False;
  DefaultLogEnabled          = False;
  DefaultLogFile             =  'log.txt';
  DefaultLogFormat           = 'YYYY-MM-DD-hh:mm:ss.zz';
  DefaultLogInput            = False;
  { Header for a new INI file. }
  VTVDefaultINIHeader : Array[1..51] of String = (
    '; Configuration file for VTuberVoice.',
    '; TTS Software for VTubers who don''t or can''t speak.',
    ';',
    '; https://github.com/VioletBitKitten/VTuberVoice',
    ';',
    '; Copyright (c) 2023 Violet Bit Kitten',
    ';',
    '; Distributed under the MIT license. Please see the file LICENSE.',
    ';',
    '; This file is automatically updated when quitting VTV.',
    '; Comments will be preserved when updating the file.',
    ';',
    '[General]',
    '; General settings.',
    '; AudioOutput = Speakers ; Set the audio output device.',
    '; OutputFile = tts_out.txt ; Set the output file spoken text is written to.',
    '; OutputAppend = 1 ; Append text to the output file, 1 for yes, 0 for no.',
    '; Priority = 0 ; Set the priorty for speech, 0 to 2. Rarely needed.',
    '; Rate = 0 ; Set the rate text is spoken, -10 to 10. Rarely needed.',
    '; Voice = Microsoft David ; Set the Voice text is spoken in.',
    '; Volume = 100 ; Set the volume text is spoken at.',
    '[Backup]',
    '; Backup settings.',
    '; Create = True ; Create backups of this configuration file, True or False.',
    '; Format = YYYY-MM-DD-hhmmsszz ; Date and time format for the bacup files.',
    '; When = Load ; When to create backups.',
    ';   Load = After loading the configuration file.',
    ';   Update = Before updating the configuration file.',
    '; Keep = 20 ; The number of backup files to keep.',
    ';    The oldest files will be deleted if there are more than the spcified files.',
    ';    Set to 0 to keep all backup files.',
    '; File = ; Automatically set to the latest backup file.',
    '[Log]',
    '; Logging settings.',
    '; By defauly logs all text spoken along with application startup and shutdown.',
    '; Diag = False ; Log diagnostic messages. True or False.',
    '; Enabled = False ; Enable logging. True or False.',
    '; File = log.txt ; File to log spoken text to.',
    '; Format = YYYY-MM-DD-hh:mm:ss.zz ; Format for the log entry timestamps.',
    '; Input = False ; Log user input. True or False.',
    '[Aliases]',
    '; Aliases are a shorthand for longer text.',
    '; To use an Alias enter # followed by the alias name.',
    '; Example alias for vtv.',
    'vtv=Vtuber Voice, for VTubers who don''t or can''t speak',
    '[Abbreviations]',
    '; Abbreviations are replaced anywhere in the text to be spoken.',
    '; They are intended for replacing words that are not spoken correctly.',
    '; They are only replaced when speaking text, not when writing to a file.',
    'vtuber=veetoober',
    'uwu=ooh woo'
  );

{ ----------========== VTVSettings Private Methods ==========---------- }

{ Clean up old INI backup files. }
procedure TVTVSettings.CleanBackups;
var
  BackupPath  : String;
  FileList    : TFileList;
  FileNames   : TStringList;
  FileIndex   : Integer;
begin
  { Get the list of backup files. }
  BackupPath := ExtractFilePath(IniFileName);
  FileList := TFileList.Create(BackupPath, ApplicationName + '_' + AllFilesMask);
  FileList.SortByTime(True);
  FileNames := FileList.GetFileNames;

  { If there are not enough backup files to clean just exit. }
  if (FBackupKeep <> 0) and (FileNames.Count < FBackupKeep) then
    Exit;

  { Delete old backup files. }
  for FileIndex := FBackupKeep to FileNames.Count - 1 do
    Deletefile(BackupPath + FileNames[FileIndex]);
end;

{ Create a backup of the current INI file. }
procedure TVTVSettings.CreateBackupFile;
var
  BackupPath    : String;
  BackupName    : String;
  BackupExt     : String;
  BackupNumber  : Integer;
  BackupFileName: String;
  MemBuffer     : TMemoryStream;
begin
  { Set the backup file name. }
  BackupPath := ExtractFilePath(IniFileName);
  BackupName := ChangeFileExt(ExtractFileName(IniFileName), ''); // Remove the file extension.
  BackupName := BackupName + '_' + FormatDateTime(BackupFormat, Now);
  BackupExt := ExtractFileExt(IniFileName);
  BackupFileName := BackupPath + BackupName + BackupExt;
  BackupNumber := 1;

  { Check if the backup file exists already. }
  if FileExists(BackupFileName) then
  begin
    { Add an incrementing number to avoid name clashes. }
    while FileExists(BackupPath + BackupName + '_' + IntToStr(BackupNumber) + BackupExt) do
      begin
        BackupNumber := BackupNumber + 1;
      end;
    BackupFileName := BackupPath + BackupName + '_' + IntToStr(BackupNumber) + BackupExt;
  end;

  { Create the bakcup. }
  MemBuffer := TMemoryStream.Create;
  try
    MemBuffer.LoadFromFile(IniFileName);
    MemBuffer.SaveToFile(BackupFileName);
  except
    on E:Exception do
      raise Exception.Create('Unable to create the backup file "' + BackupFileName + '". : ' + E.Message);
  end;
  MemBuffer.Free;

  { Save the backup file name to the INI file. }
  FBackupFile := BackupFileName;
  IniFile.WriteString('Backup', 'File', FBackupFile);

  { Cleanup old backup files. }
  if FBackupCreate and (FBackupKeep <> 0) then
    CleanBackups;
end;

{ Create a new INI file from the VTVDefaultINIHeader array above. }
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

procedure TVTVSettings.MaybeBackup;
begin
  if FBackupCreate and (LowerCase(FBackupWhen) = 'Update') then
    CreateBackupFile;
end;

{ ----------========== VTVSettings Property Methods ==========---------- }

procedure TVTVSettings.SetGeneralAudioOutput(NewOutput : String);
begin
  FGeneralAudioOutput := NewOutput;
  IniFile.WriteString('General', 'AudioOutput', FGeneralAudioOutput);
end;
procedure TVTVSettings.SetGeneralOutputAppend(Newappend : Boolean);
begin
  FGeneralOutputAppend := NewAppend;
  IniFile.WriteBool('General', 'OutputAppend', FGeneralOutputAppend);
end;
procedure TVTVSettings.SetGeneralOutputFile(NewFile : String);
begin
  FGeneralOutputFile := NewFile;
  IniFile.WriteString('General', 'OutputFile', FGeneralOutputFile);
end;
procedure TVTVSettings.SetGeneralPriority(NewPriority : Integer);
begin
  FGeneralPriority := NewPriority;
  IniFile.WriteInteger('General', 'Priority', FGeneralPriority);
end;
procedure TVTVSettings.SetGeneralRate(NewRate : Integer);
begin
  FGeneralRate := NewRate;
  IniFile.WriteInteger('General', 'Rate', FGeneralRate);
end;
procedure TVTVSettings.SetGeneralVoice(NewVoice : String);
begin
  FGeneralVoice := NewVoice;
  IniFile.WriteString('General', 'Voice', FGeneralVoice);
end;
procedure TVTVSettings.SetGeneralVolume(NewVolume : Integer);
begin
  FGeneralVolume := NewVolume;
  IniFile.WriteInteger('General', 'Volume', FGeneralVolume);
end;
procedure TVTVSettings.SetBackupCreate(NewCreate : Boolean);
begin
  FBackupCreate := NewCreate;
  IniFile.WriteBool('Backup', 'Create', FBackupCreate);
end;
procedure TVTVSettings.SetBackupFormat(NewFormat : String);
begin
  FBackupFormat := NewFormat;
  IniFile.WriteString('Backup', 'Format', FBackupFormat);
end;
procedure TVTVSettings.SetBackupWhen(NewWhen : String);
begin
  FBackupWhen := NewWhen;
  IniFile.WriteString('Backup', 'When', FBackupWhen);
end;
procedure TVTVSettings.SetBackupKeep(NewKeep : Integer);
begin
  FBackupKeep := NewKeep;
  IniFile.WriteInteger('Backup', 'Keep', FBackupKeep);
end;
procedure TVTVSettings.SetBackupFile(NewFile : String);
begin
  FBackupFile := NewFile;
  IniFile.WriteString('Backup', 'File', FBackupFile);
end;
procedure TVTVSettings.SetLogDiag(NewDiag : Boolean);
begin
  FLogDiag := NewDiag;
  IniFile.WriteBool('Log', 'Diag', FLogDiag);
end;
procedure TVTVSettings.SetLogEnabled(NewEnabled : Boolean);
begin
  FLogEnabled := NewEnabled;
  IniFile.WriteBool('Log', 'Enabled', FLogEnabled);
end;
procedure TVTVSettings.SetLogFile(NewFile : String);
begin
  FLogFile := NewFile;
  IniFile.WriteString('Log', 'File', FLogFile);
end;
procedure TVTVSettings.SetLogFormat(NewFormat : String);
begin
  FLogFormat := NewFormat;
  IniFile.WriteString('Log', 'Format', FLogFormat);
end;
procedure TVTVSettings.SetLogInput(NewInput : Boolean);
begin
  FLogInput := NewInput;
  IniFile.WriteBool('Log', 'Input', FLogInput);
end;
function TVTVSettings.GetAliasList : TStringList;
var
  AliasList : TStringList;
begin
  AliasList := TStringList.create;
  IniFile.ReadSectionValues('Aliases', AliasList);
  Result := AliasList;
end;
function TVTVSettings.GetAbbreviationList : TStringList;
var
  AbbrevaiationList : TStringList;
begin
  AbbrevaiationList := TStringList.create;
  IniFile.ReadSectionValues('Abbreviations', AbbrevaiationList);
  Result := AbbrevaiationList;
end;

{ ----------========== VTVSettings Public Methods ==========---------- }

{ Create the VTVSettings object. }
constructor TVTVSettings.Create(OverrideFileName : String = '');
var
  ConfigDir    : String;
  NewFileName  : String;
  CreateResult : Boolean;
  NewFile      : Boolean;
begin
  { Set the application configuration directory and INI file name. }
  ConfigDir := GetAppConfigDir(False);
  NewFileName := ApplicationName + '.ini';
  NewFile := False;

  if OverrideFileName <> '' then
    { Override the INI file if the user requests. }
    IniFileName := OverrideFileName
  else if FileExists(NewFileName) then
    { Look for the INI file. Check the current directory first. }
    IniFileName := ConcatPaths([GetCurrentDir, NewFileName])
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
        IniFileName := ConcatPaths([GetCurrentDir, NewFileName]);
      end;
    end;
  end;

  { If the INI file does not exist yet create a file with some useful comments. }
  if not FileExists(IniFileName) then
  begin
    Newfile := True;
    CreateINIFile;
  end;

  { Create the INI Object. Cache objects by default. }
  IniFile := TMemIniFile.Create(IniFileName);
  IniFile.CacheUpdates := True;

  { Set some options to make the INI files more friendly. }
  iniFile.Options := iniFile.Options + [ifoWriteStringBoolean];
  iniFile.BoolTrueStrings := ['True', 'true', 'yes', '1'];
  iniFile.BoolFalseStrings := ['False', 'false', 'no', '0'];

  { Load settings from the INI file. }
  LoadSettings;

  { If the file is new save the settings to set defaults. }
  if Newfile then
  begin
    NewFile := FBackupCreate;
    FBackupCreate := False;
    SaveSettings;
    FBackupCreate := NewFile
  end;

  { Create a backup file if enabled. }
  if FBackupCreate and (LowerCase(FBackupWhen) = 'load') then
    CreateBackupFile;
end;

destructor TVTVSettings.Destroy;
begin
  FreeAndNil(IniFile);
  inherited;
end;

{ Load settings from an INI file. }
procedure TVTVSettings.LoadSettings;
begin
  FGeneralAudioOutput  := IniFile.ReadString ('General', 'AudioOutput',  DefaultGeneralAudioOutput );
  FGeneralOutputAppend := IniFile.ReadBool   ('General', 'OutputAppend', DefaultGeneralOutputAppend);
  FGeneralOutputFile   := IniFile.ReadString ('General', 'OutputFile',   DefaultGeneralOutputFile  );
  FGeneralPriority     := IniFile.ReadInteger('General', 'Priority',     DefaultGeneralPriority    );
  FGeneralRate         := IniFile.ReadInteger('General', 'Rate',         DefaultGeneralRate        );
  FGeneralVoice        := IniFile.ReadString ('General', 'Voice',        DefaultGeneralVoice       );
  FGeneralVolume       := IniFile.ReadInteger('General', 'Volume',       DefaultGeneralVolume      );
  FBackupCreate        := IniFile.ReadBool   ('Backup',  'Create',       DefaultBackupCreate       );
  FBackupFormat        := IniFile.ReadString ('Backup',  'Format',       DefaultBackupFormat       );
  FBackupWhen          := IniFile.ReadString ('Backup',  'When',         DefaultBackupWhen         );
  FBackupKeep          := IniFile.ReadInteger('Backup',  'Keep',         DefaultBackupKeep         );
  FBackupFile          := IniFile.ReadString ('Backup',  'File',         DefaultBackupFile         );
  FLogDiag             := IniFile.ReadBool   ('Log',     'Diag',         DefaultLogDiag            );
  FLogEnabled          := IniFile.ReadBool   ('Log',     'Enabled',      DefaultLogEnabled         );
  FLogFile             := IniFile.ReadString ('Log',     'File',         DefaultLogFile            );
  FLogFormat           := IniFile.ReadString ('Log',     'Format',       DefaultLogFormat          );
  FLogInput            := IniFile.ReadBool   ('Log',     'Input',        DefaultLogInput           );
end;

{ Write settings to an INI file. }
procedure TVTVSettings.SaveSettings;
begin
  { Create a backup file if requested. }
  MaybeBackup;

  { Save the settings. }
  IniFile.WriteString ('General', 'AudioOutput',  FGeneralAudioOutput);
  IniFile.WriteBool   ('General', 'OutputAppend', FGeneralOutputAppend);
  IniFile.WriteString ('General', 'OutputFile',   FGeneralOutputFile);
  IniFile.WriteInteger('General', 'Priority',     FGeneralPriority);
  IniFile.WriteInteger('General', 'Rate',         FGeneralRate);
  IniFile.WriteString ('General', 'Voice',        FGeneralVoice);
  IniFile.WriteInteger('General', 'Volume',       FGeneralVolume);
  IniFile.WriteBool   ('Backup',  'Create',       FBackupCreate);
  IniFile.WriteString ('Backup',  'Format',       FBackupFormat);
  IniFile.WriteString ('Backup',  'When',         FBackupWhen);
  IniFile.WriteInteger('Backup',  'Keep',         FBackupKeep);
  IniFile.WriteString ('Backup',  'File',         FBackupFile);
  IniFile.WriteBool   ('Log',     'Diag',         FLogDiag);
  IniFile.WriteBool   ('Log',     'Enabled',      FLogEnabled);
  IniFile.WriteString ('Log',     'File',         FLogFile);
  IniFile.WriteString ('Log',     'Format',       FLogFormat);
  IniFile.WriteBool   ('Log',     'Input',        FLogInput);
  IniFile.UpdateFile;
end;

end.