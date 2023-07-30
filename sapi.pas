{
  Free Pascal interface to Microsoft SAPI.
  This will only work on Windows.

  See the file doc/SAPI.md for documentation on this library.

  Documentation on SAPI SpVoice:
  https://learn.microsoft.com/en-us/previous-versions/windows/desktop/ee125640(v=vs.85)

  Documentation on SAPI SpFileStream:
  https://learn.microsoft.com/en-us/previous-versions/windows/desktop/ee125548(v=vs.85)

  Copyright (c) 2023 Violet Bit Kitten

  Distributed under the MIT license. Please see the file LICENSE.
}

{ Modern Pascal Directives }
{$mode objfpc}{$H+}{$J-}

unit sapi;

interface

uses
  classes, comobj, sysutils;

type
  TSpFileStream = class
  private
    SpFileStream : Variant;
    function GetStream : Variant;
  public
    constructor Create;
    procedure Close;
    procedure OpenStreamRead(FileName : String);
    procedure OpenStreamWrite(FileName : String);
    procedure OpenStream(FileName : String; Flags : Integer);
    property Stream : Variant read GetStream;
  end;

  TSpVoice = class
  private
    SpVoice : Variant;
    function GetAudioOutput : Variant;
    procedure SetAudioOutput(NewAudioOutput : Variant);
    function GetAudioOutputStream : Variant;
    procedure SetAudioOutputStream(NewAudioOutputStream : Variant);
    procedure SetAudioOutputStream(NewAudioOutputStream : TSpFileStream);
    function GetPriority : Integer;
    procedure SetPriority(NewPriority : Integer);
    function GetRate : Integer;
    procedure SetRate(NewRate : Integer);
    function GetStatus : Variant;
    function GetSynchronousSpeakTimeout : Integer;
    procedure SetSynchronousSpeakTimeout(NewSynchronousSpeakTimeout : Integer);
    function GetVoice : Variant;
    procedure SetVoice(NewVoice : Variant);
    function GetVolume : Integer;
    procedure SetVolume(NewVolume : Integer);
  public
    ExceptionsEnabled : Boolean;
    constructor Create;
    function GetAudioOutputs : Variant;
    function GetAudioOutputNames : TstringList;
    function GetVoices : Variant;
    function GetVoiceNames : TstringList;
    procedure SetAudioOutputID(OutputID : Integer);
    procedure SetAudioOutputName(OutputName : String);
    procedure SetVoiceID(VoiceID : Integer);
    procedure SetVoiceName(VoiceName : String);
    procedure Speak(Text : String; Flags : Integer);
    procedure Speak(Text : String);
    procedure SpeakStream(Stream : Variant);
    procedure SpeakStream(Stream : TSpFileStream);
    function WaitUntilDone : Boolean;
    function WaitUntilDone(Timeout : Integer) : Boolean;
    property AudioOutput : Variant read GetAudioOutput write SetAudioOutput;
    property AudioOutputStream : Variant read GetAudioOutputStream write SetAudioOutputStream;
    property Priority : Integer read GetPriority write SetPriority;
    property Rate : Integer read GetRate write SetRate;
    property Status : Variant read GetStatus;
    property SynchronousSpeakTimeout : Integer read GetSynchronousSpeakTimeout write SetSynchronousSpeakTimeout;
    property Voice : Variant read GetVoice write SetVoice;
    property Volume : Integer read GetVolume write SetVolume;
  end;

const
  { SpeechVoiceSpeakFlags Flags for the SpVoice Speak method. }
  SVSFDefault          = 0;
  SVSFlagsAsync        = 1;
  SVSFPurgeBeforeSpeak = 2;
  SVSFIsFilename       = 4;
  SVSFIsXML            = 8;
  SVSFIsNotXML         = 16;
  SVSFPersistXML       = 32;
  { SpeechVoicePriority flags for the SpVoice Priority property. }
  SVPNormal = 0;
  SVPAlert  = 1;
  SVPOver   = 2;
  { SpeechStreamFileMode flags for the SpFileStream  Open method. }
  SSFMOpenForRead    = 0;
  SSFMOpenReadWrite  = 1; { Marked as hidden. }
  SSFMCreate         = 2; { Marked as hidden. }
  SSFMCreateForWrite = 3;
  { SpVoice related Messages. }
  SpVoice_invalid_audio_output_id   = 'Invalid Audio Output device ID.';
  SpVoice_invalid_audio_output_name = 'Invalid Audio Output device name.';
  SpVoice_invalid_voice_id          = 'Invalid Voice ID.';
  SpVoice_invalid_voice_name        = 'Invalid Voice name.';
  SpVoice_priority_valid_values     = 'Valid values 0 to 3.';
  SpVoice_rate_valid_values         = 'Valid values -10 to 10.';
  Spvoice_volume_valid_values       = 'Valid values 0 to 100.';

implementation

{ ----------========== SpVoice Private Methods ==========---------- }

{ Get the current output device text is spoken to. }
function TSpVoice.GetAudioOutput : Variant;
begin
  Result := SpVoice.AudioOutput ;
end;

{ Set the output text device is spoken to. }
procedure TSpVoice.SetAudioOutput(NewAudioOutput : Variant);
begin
  SpVoice.AudioOutput := NewAudioOutput;
end;

{ Get the current output stream text is spoken to. }
function TSpVoice.GetAudioOutputStream : Variant;
begin
  Result := SpVoice.AudioOutputStream ;
end;

{ Set the output text stream is spoken to. }
procedure TSpVoice.SetAudioOutputStream(NewAudioOutputStream : Variant);
begin
  SpVoice.AudioOutputStream := NewAudioOutputStream;
end;

{ Set the output text stream is spoken to. }
procedure TSpVoice.SetAudioOutputStream(NewAudioOutputStream : TSpFileStream);
begin
  SpVoice.AudioOutputStream := NewAudioOutputStream.Stream;
end;

{ Get the current priority text is spoken at. }
function TSpVoice.GetPriority : Integer;
begin
  Result := SpVoice.Priority ;
end;

{ Set the priority text is spoken at. Valid values 0 to 3. }
procedure TSpVoice.SetPriority(NewPriority : Integer);
begin
  if (ExceptionsEnabled and (NewPriority < 0) or (NewPriority > 3)) then
    raise EArgumentOutOfRangeException.Create(SpVoice_priority_valid_values);
  if NewPriority > 3 then NewPriority := 3;
  if NewPriority < 0 then NewPriority := 0;
  SpVoice.Priority := NewPriority;
end;

{ Get the current rate text is spoken. }
function TSpVoice.GetRate : Integer;
begin
  Result := SpVoice.Rate;
end;

{ Set the rate text is spoken. Valid values -10 to 10 }
procedure TSpVoice.SetRate(NewRate : Integer);
begin
  if (ExceptionsEnabled and (NewRate < -10) or (NewRate > 10)) then
    raise EArgumentOutOfRangeException.Create(SpVoice_rate_valid_values);
  if NewRate > 10 then NewRate := 10;
  if NewRate < -10 then NewRate := -10;
  SpVoice.Rate := NewRate;
end;

{ Get the current SpVoice satus. }
function TSpVoice.GetStatus : Variant;
begin
  Result := SpVoice.Status;
end;


{ Get the current rate text is spoken. }
function TSpVoice.GetSynchronousSpeakTimeout : Integer;
begin
  Result := SpVoice.SynchronousSpeakTimeout;
end;

{ Set the synchonous timeout for getting access to the output device. }
procedure TSpVoice.SetSynchronousSpeakTimeout(NewSynchronousSpeakTimeout : Integer);
begin
  SpVoice.SynchronousSpeakTimeout := NewSynchronousSpeakTimeout;
end;

{ Get the current Voice text is spoken with. }
function TSpVoice.GetVoice : Variant;
begin
  Result := SpVoice.Voice;
end;

{ Set the Voice text is spoken with. }
procedure TSpVoice.SetVoice(NewVoice : Variant);
begin
  SpVoice.Voice := NewVoice;
end;

{ Get the current Volume text is spoken. }
function TSpVoice.GetVolume : Integer;
begin
  Result := SpVoice.Volume;
end;

{ Set the Volume text is spoken. Valid values 0 to 100}
procedure TSpVoice.SetVolume(NewVolume : Integer);
begin
  if (ExceptionsEnabled and (NewVolume < 0) or (NewVolume > 100)) then
    raise EArgumentOutOfRangeException.Create(Spvoice_volume_valid_values);
  if NewVolume > 100 then NewVolume := 100;
  if NewVolume < 0 then NewVolume := 0;
  SpVoice.Volume := NewVolume;
end;

{ ----------========== SpVoice Public Methods ==========---------- }

{ Create the SpVoice Object. }
constructor TSpVoice.Create;
begin
  inherited;
  { Get the Voice OLE Object. }
  SpVoice := CreateOleObject('SAPI.SpVoice');

  { By default do not throw exceptions. }
  ExceptionsEnabled := False;
end;

{ Return the available audio outputs. }
function TSpVoice.GetAudioOutputs : Variant;
begin
  { Get an object containing the available voices. }
  Result := SpVoice.GetAudioOutputs;
end;

{ Return a TstringList of the available audio output descriptions. }
function TSpVoice.GetAudioOutputNames : TstringList;
var
  OutputToken : Variant;     { The current output in the for loop. }
  OutputIndex : Integer;     { Index in the for loop. }
  OutputList  : TstringList; { List of output descriptions to be returned. }
  Outputs     : Variant;     { The list of outputs from GetOutput. }
begin
  { Get an object containing the available voices. }
  Outputs := SpVoice.GetAudioOutputs;

  { Initialize the list of voices to be returned. }
  OutputList := TstringList.Create;

  { Get the description for each voice. }
  for OutputIndex := 0 to Outputs.Count - 1 do
  begin
    OutputToken := Outputs.Item(OutputIndex);
    OutputList.Add(OutputToken.GetDescription);
  end;

  { Return the list of voices. }
  Result := OutputList;
end;

{ Return the available voices. }
function TSpVoice.GetVoices : Variant;
begin
  { Get an object containing the available voices. }
  Result := SpVoice.GetVoices;
end;

{ Return a TstringList of the available voice descriptions. }
function TSpVoice.GetVoiceNames : TstringList;
var
  Voices     : Variant;     { The list of voices from GetVoices. }
  VoiceToken : Variant;     { The current voice in the for loop. }
  VoiceIndex : Integer;     { Index in the for loop. }
  VoiceList  : TstringList; { List of voice descriptions to be returned. }
begin
  { Get an object containing the available voices. }
  Voices := SpVoice.GetVoices;

  { Initialize the list of voices to be returned. }
  VoiceList := TstringList.Create;

  { Get the description for each voice. }
  for VoiceIndex := 0 to Voices.Count - 1 do
  begin
    VoiceToken := Voices.Item(VoiceIndex);
    VoiceList.Add(VoiceToken.GetDescription);
  end;

  { Return the list of voices. }
  Result := VoiceList;
end;

{ Set the Output to be used when speaking text by the Output ID #. }
procedure TSpVoice.SetAudioOutputID(OutputID : Integer);
var
  Outputs : Variant;
begin
  Outputs := GetAudioOutputs;
  if (ExceptionsEnabled and ((OutputID < 0) or (OutputID > Outputs.Count))) then
    raise EArgumentException.Create(SpVoice_invalid_audio_output_id);
  SpVoice.AudioOutput := Outputs.item(OutputID);
end;

{ Set the Output to be used when speaking text by the Output name. }
procedure TSpVoice.SetAudioOutputName(OutputName : String);
var
  Outputs     : TstringList;
  OutputID    : Integer;
  OutputIndex : Integer;
begin
  { Get the available outputs. }
  Outputs := GetAudioOutputNames;

  { Try an exact name match. }
  OutputID := Outputs.IndexOf(OutputName);

  { Try a partial name match. }
  if OutputID < 0 then
  begin
    for OutputIndex := 0 to Outputs.Count - 1 do
      if Outputs[OutputIndex].StartsWith(OutputName) then
        OutputID := OutputIndex;
  end;

    if (ExceptionsEnabled and (OutputID < 0)) then
    raise EArgumentException.Create(SpVoice_invalid_audio_output_name);
  if OutputID >= 0 then
    SetAudioOutputID(OutputID);
end;

{ Set the voice to be used when speaking text by the Voice ID #. }
procedure TSpVoice.SetVoiceID(VoiceID : Integer);
var
  Voices : Variant;
begin
  Voices := GetVoices;
  if (ExceptionsEnabled and ((VoiceID < 0) or (VoiceID > Voices.Count))) then
    raise EArgumentException.Create(SpVoice_invalid_voice_id);
  SpVoice.Voice := Voices.item(VoiceID);
end;

{ Set the voice to be used when speaking text by the Voice name. }
procedure TSpVoice.SetVoiceName(VoiceName : String);
var
  Voices     : TstringList;
  VoiceID    : Integer;
  VoiceIndex : Integer;
begin
  { Get the available voices. }
  Voices := GetVoiceNames;

  { Try an exact name match. }
  VoiceID := Voices.IndexOf(VoiceName);

  { Try a partial name match. }
  if VoiceID < 0 then
  begin

    for VoiceIndex := 0 to Voices.Count - 1 do
      if Voices[VoiceIndex].StartsWith(VoiceName) then
        VoiceID := VoiceIndex;
  end;

  if (ExceptionsEnabled and (VoiceID < 0)) then
    raise EArgumentException.Create(SpVoice_invalid_voice_name);
  if VoiceID >= 0 then
    SetVoiceID(VoiceID);
end;

{ Speak a string with default flags. }
procedure TSpVoice.Speak(Text : String);
begin
  Speak(Text, SVSFDefault);
end;

{ Speak a string. }
{ Taken from https://forum.lazarus.freepascal.org/index.php/topic,17852.0 }
procedure TSpVoice.Speak(Text : String; Flags : Integer);
var
  SavedCW : Word;
begin
    { Change FPU interrupt mask to avoid SIGFPE exceptions. }
    SavedCW := Get8087CW;
    try
      Set8087CW(SavedCW or $4);
      SpVoice.Speak(Text, Flags);
    finally
      { Restore FPU mask. }
      Set8087CW(SavedCW);
    end;
end;

{ Basically play a WAV file. }
procedure TSpVoice.SpeakStream(Stream : Variant);
begin
  SpVoice.SpeakStream(Stream);
end;
procedure TSpVoice.SpeakStream(Stream : TSpFileStream);
begin
  SpVoice.SpeakStream(Stream.Stream);
end;

{ Block until the voice has finished speaking. }
function TSpVoice.WaitUntilDone : Boolean;
begin
  Result := WaitUntilDone(-1);
end;

{ Block until the voice has finished speaking with a timeout. }
function TSpVoice.WaitUntilDone(Timeout : Integer) : Boolean;
begin
  Result := SpVoice.WaitUntilDone(Timeout);
end;

{ ----------========== SpFileStream Private Methods ==========---------- }

{ Return the stream object. }
function TSpFileStream.GetStream : Variant;
begin
  Result := SpFileStream;
end;

{ ----------========== SpFileStream Public Methods ==========---------- }

{ Create the SpFileStream Object. }
constructor TSpFileStream.Create;
begin
  inherited;
  { Get the Voice OLE Object. }
  SpFileStream := CreateOleObject('SAPI.SpFileStream');
end;

procedure TSpFileStream.Close;
begin
  SpFileStream.Close;
end;

{ Open a flestream for reading. }
procedure TSpFileStream.OpenStreamRead(FileName : String);
begin
  OpenStream(FileName, SSFMOpenForRead);
end;

{ Open a flestream for writing. }
procedure TSpFileStream.OpenStreamWrite(FileName : String);
begin
  OpenStream(FileName, SSFMCreateForWrite);
end;

{ Open a flestream with the mode specified by the Flags. }
procedure TSpFileStream.OpenStream(FileName : String; Flags : Integer);
begin
  SpFileStream.Open(FileName, Flags);
end;

end.