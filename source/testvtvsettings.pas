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
  { File name for the test configuration files. }
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

type
  TVTVSettingsTest = class(TTestCase)
  private
    Settings : TVTVSettings;

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Test Methods }
    procedure TestChanges;
    procedure TestCreate;


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
  Deletefile(TestFileName);
end;

{ ----------========== Test Methods ==========---------- }

{ Test all of the settings can be set correctly. }
procedure TVTVSettingsTest.TestChanges;
var
  TestSettings : TVTVSettings;
begin
  { Change all of the settings. }
  TestSettings := TVTVSettings.Create('test' + TestFileName);
  TestSettings.AudioOutput  := TestAudioOutput ;
  TestSettings.OutputAppend := TestOutputAppend;
  TestSettings.OutputFile   := TestOutputFile  ;
  TestSettings.Priority     := TestPriority    ;
  TestSettings.Rate         := TestRate        ;
  TestSettings.Voice        := TestVoice       ;
  TestSettings.Volume       := TestVolume      ;
  TestSettings.BackupCreate := TestBackupCreate;
  TestSettings.BackupFormat := TestBackupFormat;
  TestSettings.BackupWhen   := TestBackupWhen  ;
  TestSettings.BackupKeep   := TestBackupKeep  ;
  TestSettings.SaveSettings;
  FreeAndNil(TestSettings);

  { Load the settings into a new object. }
  TestSettings := TVTVSettings.Create('test' + TestFileName);

  { Check that each of the settings has the right value. }
  AssertEquals('Setting AudioOutput should equal test value.',  TestSettings.AudioOutput , TestAudioOutput );
  AssertEquals('Setting OutputAppend should equal test value.', TestSettings.OutputAppend, TestOutputAppend);
  AssertEquals('Setting OutputFile should equal test value.',   TestSettings.OutputFile  , TestOutputFile  );
  AssertEquals('Setting Priority should equal test value.',     TestSettings.Priority    , TestPriority    );
  AssertEquals('Setting Rate should equal test value.',         TestSettings.Rate        , TestRate        );
  AssertEquals('Setting Voice should equal test value.',        TestSettings.Voice       , TestVoice       );
  AssertEquals('Setting Volume should equal test value.',       TestSettings.Volume      , TestVolume      );
  AssertEquals('Setting BackupCreate should equal test value.', TestSettings.BackupCreate, TestBackupCreate);
  AssertEquals('Setting BackupFormat should equal test value.', TestSettings.BackupFormat, TestBackupFormat);
  AssertEquals('Setting BackupWhen should equal test value.',   TestSettings.BackupWhen  , TestBackupWhen  );
  AssertEquals('Setting BackupKeep should equal test value.',   TestSettings.BackupKeep  , TestBackupKeep  );

  { Make sure the backup file was created. }
  AssertTrue('Backup file name should not be empty.', (TestSettings.BackupFile <> ''));
  AssertTrue('Backup file should be created.', FileExists(TestSettings.BackupFile));

  { Cleanup the resulting files and the test settings object. }
  Deletefile(TestSettings.BackupFile);
  FreeAndNil(TestSettings);
  DeleteFile('test' + TestFileName);
end;

{ Make sure the configuration file was created. }
procedure TVTVSettingsTest.TestCreate;
begin
  AssertTrue('Configuration file was created.', FileExists(TestFileName));
end;


initialization
  RegisterTests([TVTVSettingsTest]);
end.