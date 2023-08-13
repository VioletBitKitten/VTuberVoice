{
  VTuberVoice

  TTS program using the Free Pascal SAPI Library.

  https://github.com/VioletBitKitten/VTuberVoice

  Copyright (c) 2023 Violet Bit Kitten

  Distributed under the MIT license. Please see the file LICENSE.
}

{ Modern Pascal Directives }
{$mode objfpc}{$H+}{$J-}

program vtv;

uses
  classes, custapp, sysutils, vtvapp, sapi, settings;

Var
  VTVApp : TVTVApp;

begin
  VTVApp := TVTVApp.Create(Nil);
  AVTVApppp.Initialize;
  VTVApp.Title := 'VTuberVoice TTS Application.';
  VTVApp.Run;
  VTVApp.Free;
end.