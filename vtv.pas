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
  custapp, vtvapp;

Var
  App : TVTVApp;

begin
  App := TVTVApp.Create(Nil);
  App.Initialize;
  App.Title := 'VTuberVoice TTS Application.';
  App.Run;
  App.Free;
end.