{
  Unit tests for the vtvsettings Unit.

  https://github.com/VioletBitKitten/SAPI

  Copyright (c) 2023 Violet Bit Kitten

  Distributed under the MIT license. Please see the file LICENSE.
}

{ Modern Pascal Directives }
{$mode objfpc}{$H+}{$J-}

unit testvtvsettings;

interface

uses
  sysutils, Contnrs, classes, fpcunit, testregistry, vtvsettings;

const
  { File name for the test configuration file. }
  TestFileName      = 'testfile.ini';
  { Test settings for verifying settings are updated correctly. }
  TestAudioOutput   = 'Test Output';
  TestOutputAppend  = True;
  TestOutputFile    = 'Test File';
  TestPriority      = 1;
  TestRate          = 10;
  TestVoice         = 'Test Voice';
  TestVolume        = 50;
  TestBackupCreate  = True;
  TestBackupFormat  = 'YYYY-MM-DD';
  TestBackupWhen    = 'Load';
  TestBackupKeep    = 1;
  TestLogDiag       = False;
  TestLogEnabled    = False;
  TestLogFile       =  'log.txt';
  TestLogFormat     = 'YYYY-MM-DD-hh:mm:ss.zz';
  TestLogInput      = False;

type
  TVTVSettingsTest = class(TTestCase)
  private
    Settings : TVTVSettings;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    destructor Destroy; override;
  published
    { Test Methods }
    procedure TestCreate;
    procedure TestChanges;
    procedure TestLoadChanges;
    procedure TestBackup;
  end;

implementation

{ ----------========== Protected Methods ==========---------- }

{ Create the settings object and the configuration file. }
procedure TVTVSettingsTest.SetUp;
begin
  Settings := TVTVSettings.Create(TestFileName);
end;

procedure TVTVSettingsTest.TearDown;
begin
  FreeAndNil(Settings);
end;

{ ----------========== Public Methods ==========---------- }

{ Delete the test configuration file. }
destructor TVTVSettingsTest.Destroy;
begin
  Deletefile(TestFileName);
  inherited;
end;

{ ----------========== Test Methods ==========---------- }

{ Make sure the configuration file was created. }
procedure TVTVSettingsTest.TestCreate;
begin
  AssertTrue('Configuration file was created.', FileExists(TestFileName));
end;

{ Test all of the settings can be changed successfully. }
procedure TVTVSettingsTest.TestChanges;
begin
  { Change all of the settings. }
  Settings.AudioOutput  := TestAudioOutput ;
  Settings.OutputAppend := TestOutputAppend;
  Settings.OutputFile   := TestOutputFile  ;
  Settings.Priority     := TestPriority    ;
  Settings.Rate         := TestRate        ;
  Settings.Voice        := TestVoice       ;
  Settings.Volume       := TestVolume      ;
  Settings.BackupCreate := TestBackupCreate;
  Settings.BackupFormat := TestBackupFormat;
  Settings.BackupWhen   := TestBackupWhen  ;
  Settings.BackupKeep   := TestBackupKeep  ;
  Settings.SaveSettings;

  { Check that each of the settings has the right value. }
  AssertEquals('Setting AudioOutput should equal test value.',   TestAudioOutput ,Settings.AudioOutput );
  AssertEquals('Setting OutputAppend should equal test value.',  TestOutputAppend,Settings.OutputAppend);
  AssertEquals('Setting OutputFile should equal test value.',    TestOutputFile  ,Settings.OutputFile  );
  AssertEquals('Setting Priority should equal test value.',      TestPriority    ,Settings.Priority    );
  AssertEquals('Setting Rate should equal test value.',          TestRate        ,Settings.Rate        );
  AssertEquals('Setting Voice should equal test value.',         TestVoice       ,Settings.Voice       );
  AssertEquals('Setting Volume should equal test value.',        TestVolume      ,Settings.Volume      );
  AssertEquals('Setting BackupCreate should equal test value.',  TestBackupCreate,Settings.BackupCreate);
  AssertEquals('Setting BackupFormat should equal test value.',  TestBackupFormat,Settings.BackupFormat);
  AssertEquals('Setting BackupWhen should equal test value.',    TestBackupWhen  ,Settings.BackupWhen  );
  AssertEquals('Setting BackupKeep should equal test value.',    TestBackupKeep  ,Settings.BackupKeep  );
  AssertEquals('Setting BackupKeep should equal test value.',    TestLogDiag     ,Settings.LogDiag     );
  AssertEquals('Setting BackupKeep should equal test value.',    TestLogEnabled  ,Settings.LogEnabled  );
  AssertEquals('Setting BackupKeep should equal test value.',    TestLogFile     ,Settings.LogFile     );
  AssertEquals('Setting BackupKeep should equal test value.',    TestLogFormat   ,Settings.LogFormat   );
  AssertEquals('Setting BackupKeep should equal test value.',    TestLogInput    ,Settings.LogInput    );

end;

{ Load the settings and make sure they have the right values.}
procedure TVTVSettingsTest.TestLoadChanges;
begin
  { Check that each of the settings has the right value. }
  AssertEquals('Setting AudioOutput should equal test value.',   TestAudioOutput ,Settings.AudioOutput );
  AssertEquals('Setting OutputAppend should equal test value.',  TestOutputAppend,Settings.OutputAppend);
  AssertEquals('Setting OutputFile should equal test value.',    TestOutputFile  ,Settings.OutputFile  );
  AssertEquals('Setting Priority should equal test value.',      TestPriority    ,Settings.Priority    );
  AssertEquals('Setting Rate should equal test value.',          TestRate        ,Settings.Rate        );
  AssertEquals('Setting Voice should equal test value.',         TestVoice       ,Settings.Voice       );
  AssertEquals('Setting Volume should equal test value.',        TestVolume      ,Settings.Volume      );
  AssertEquals('Setting BackupCreate should equal test value.',  TestBackupCreate,Settings.BackupCreate);
  AssertEquals('Setting BackupFormat should equal test value.',  TestBackupFormat,Settings.BackupFormat);
  AssertEquals('Setting BackupWhen should equal test value.',    TestBackupWhen  ,Settings.BackupWhen  );
  AssertEquals('Setting BackupKeep should equal test value.',    TestBackupKeep  ,Settings.BackupKeep  );
  AssertEquals('Setting BackupKeep should equal test value.',    TestLogDiag     ,Settings.LogDiag     );
  AssertEquals('Setting BackupKeep should equal test value.',    TestLogEnabled  ,Settings.LogEnabled  );
  AssertEquals('Setting BackupKeep should equal test value.',    TestLogFile     ,Settings.LogFile     );
  AssertEquals('Setting BackupKeep should equal test value.',    TestLogFormat   ,Settings.LogFormat   );
  AssertEquals('Setting BackupKeep should equal test value.',    TestLogInput    ,Settings.LogInput    );

  { Reset BackupCreate so no further backup files are created. }
  Settings.BackupCreate := False;
  Settings.SaveSettings;
end;

{ Verify a backup file was created. }
procedure TVTVSettingsTest.TestBackup;
begin
  { Make sure the backup file was created. }
  AssertTrue('Backup file name should not be empty.', (Settings.BackupFile <> ''));
  AssertTrue('Backup file should be created.', FileExists(Settings.BackupFile));

  { Cleanup the backup file. }
  Deletefile(Settings.BackupFile);
end;

initialization
  RegisterTests([TVTVSettingsTest]);
end.