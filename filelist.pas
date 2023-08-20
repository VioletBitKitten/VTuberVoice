{
  List of files in a path.
  The List can be sorted by various file attributes.

  Much of this was taken from a forum post:
  https://forum.lazarus.freepascal.org/index.php?topic=29552.0

  https://github.com/VioletBitKitten/VTuberVoice

  Copyright (c) 2023 Violet Bit Kitten

  Distributed under the MIT license. Please see the file LICENSE.
}

{ Modern Pascal Directives }
{$mode objfpc}{$H+}{$J-}

unit filelist;

interface

uses
  sysutils, contnrs, classes;

type
  { Class to store file details in TObjectList. }
  TFileDetails = class
    Attr : LongInt;
    Name : String;
    Size : Int64;
    Time : LongInt;
  end;

  TFileList = class
  private
    SortFunction : TListSortCompare;
    procedure GetFiles;
  public
    FileList  : TObjectList;
    FilePath  : String;
    constructor Create(Path : String = ''; Mask : String = AllFilesMask);
    procedure SortByName(Reverse : Boolean = False);
    procedure SortBySize(Reverse : Boolean = False);
    procedure SortByTime(Reverse : Boolean = False);
    function GetFileNames : TStringList;
    procedure RefreshFiles;
  end;

implementation

{ ----------========== Comparison Function for Sorting ==========---------- }

function CompareFileName(A, B: Pointer): Integer;
begin
  Result := CompareStr(TFileDetails(A).Name, TFileDetails(B).Name);
end;
function CompareFileSize(A, B: Pointer): Integer;
begin
  Result := TFileDetails(A).Size - TFileDetails(B).Size;
end;
function CompareFileTime(A, B: Pointer): Integer;
begin
  Result := TFileDetails(A).Time - TFileDetails(B).Time;
end;
function CompareFileNameReverse(A, B: Pointer): Integer;
begin
  Result := CompareStr(TFileDetails(B).Name, TFileDetails(A).Name);
end;
function CompareFileSizeReverse(A, B: Pointer): Integer;
begin
  Result := TFileDetails(B).Size - TFileDetails(A).Size;
end;
function CompareFileTimeReverse(A, B: Pointer): Integer;
begin
  Result := TFileDetails(B).Time - TFileDetails(A).Time;
end;

{ ----------========== FileList Private Methods ==========---------- }

procedure TFileList.GetFiles;
var
  FileDetails : TFileDetails;
  SearchRecord : TSearchRec;
begin
  if FindFirst(FilePath, 0, SearchRecord) = 0 then
  begin
    repeat
      with SearchRecord do
      begin
        FileDetails := TFileDetails.Create;
        FileDetails.Attr := Attr;
        FileDetails.Name := Name;
        FileDetails.Size := Size;
        FileDetails.Time := Time;
        FileList.Add(FileDetails);
      end;
    until FindNext(SearchRecord) <> 0;
    FindClose(SearchRecord);
  end;
end;

{ ----------========== FileList Public Methods ==========---------- }

constructor TFileList.Create(Path : String = ''; Mask : String = AllFilesMask);
begin
  { Setup the path. }
  if (Path = '') then
    FilePath := Mask
  else if not DirectoryExists(Path) then
    raise Exception.Create('Path does not exist: ' + Path)
  else
    FilePath := Path + DirectorySeparator + Mask;

  { Create the list of files. }
  SortFunction := @CompareFileName;
  FileList := TObjectList.Create;
  GetFiles;
  FileList.Sort(SortFunction);
end;

{ Sort the list by the file Name. }
procedure TFileList.SortByName(Reverse : Boolean = False);
begin
  if Reverse then
    SortFunction := @CompareFileNameReverse
  else
    SortFunction := @CompareFileName;
  FileList.Sort(SortFunction);
end;

{ Sort the list by the file size. }
procedure TFileList.SortBySize(Reverse : Boolean = False);
begin
  if Reverse then
    SortFunction := @CompareFileSizeReverse
  else
    SortFunction := @CompareFileSize;
  FileList.Sort(SortFunction);
end;

{ Sort the list by the file time. }
procedure TFileList.SortByTime(Reverse : Boolean = False);
begin
  if Reverse then
    SortFunction := @CompareFileTimeReverse
  else
    SortFunction := @CompareFileTime;
  FileList.Sort(SortFunction);
end;

{ Return a list of just the file names. }
function TFileList.GetFileNames : TStringList;
var
  FileIndex   : Integer;
  FileDetails : TFileDetails;
begin
  Result := TStringList.Create;
  for FileIndex := 0 to FileList.Count - 1 do
  begin
    FileDetails := TFileDetails(FileList[FileIndex]);
    Result.Add(FileDetails.Name);
  end;
end;

{ Recreate the file list. }
procedure TFileList.RefreshFiles;
begin
  FreeAndNil(FileList);
  FileList := TObjectList.Create;
  GetFiles;
  FileList.Sort(SortFunction);
end;

end.