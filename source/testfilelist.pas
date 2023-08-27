{
  Unit tests for the filelist Unit.

  https://github.com/VioletBitKitten/SAPI

  Copyright (c) 2023 Violet Bit Kitten

  Distributed under the MIT license. Please see the file LICENSE.
}

{ Modern Pascal Directives }
{$mode objfpc}{$H+}{$J-}

unit testfilelist;

interface

uses
  sysutils, Contnrs, classes, fpcunit, testregistry, filelist;

type
  TFileListTest = class(TTestCase)
  private
    FileList : TFileList;

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Test FileList Methods }
    procedure TestFileListCreate;
    procedure TestFileListSortByName;
    procedure TestFileListRefreshFiles;
    procedure TestFileListSortBySize;
    procedure TestFileListSortByTime;

  end;

implementation

{ ----------========== Protected Methods ==========---------- }

procedure TFileListTest.SetUp;
begin
  FileList := TFileList.Create;
end;

procedure TFileListTest.TearDown;
begin
  FreeAndNil(FileList);
end;

{ ----------========== Test FileList Methods ==========---------- }

procedure TFileListTest.TestFileListCreate;
begin
  AssertNotNull('File Path should not be null.', FileList.FilePath);
  AssertNotNull('File List should not be null.', FileList.FileList);
end;

procedure TFileListTest.TestFileListRefreshFiles;
begin
FileList.RefreshFiles;
  AssertNotNull('File Path should not be null.', FileList.FilePath);
  AssertNotNull('File List should not be null.', FileList.FileList);
end;

procedure TFileListTest.TestFileListSortByName;
var
  SortedNormal  : TStringList;
  SortedReverse : TStringList;
begin
  FileList.SortByName(False);
  SortedNormal := FileList.GetFileNames;
  FileList.SortByName(True);
  SortedReverse := FileList.GetFileNames;
  AssertTrue('Sorted files by name should be different when reversed.', (SortedNormal[0] <> SortedReverse[0]));
end;

procedure TFileListTest.TestFileListSortBySize;
var
  SortedNormal  : TStringList;
  SortedReverse : TStringList;
begin
  FileList.SortBySize(False);
  SortedNormal := FileList.GetFileNames;
  FileList.SortBySize(True);
  SortedReverse := FileList.GetFileNames;
  AssertTrue('Sorted files by size should be different when reversed.', (SortedNormal[0] <> SortedReverse[0]));
end;

procedure TFileListTest.TestFileListSortByTime;
var
  SortedNormal  : TStringList;
  SortedReverse : TStringList;
begin
  FileList.SortByTime(False);
  SortedNormal := FileList.GetFileNames;
  FileList.SortByTime(True);
  SortedReverse := FileList.GetFileNames;
  AssertTrue('Sorted files by time should be different when reversed.', (SortedNormal[0] <> SortedReverse[0]));
end;

initialization

  RegisterTests([TFileListTest]);

end.