{
  Unit tests for the Free Pascal interface to Microsoft SAPI.

  https://github.com/VioletBitKitten/SAPI

  Copyright (c) 2023 Violet Bit Kitten

  Distributed under the MIT license. Please see the file LICENSE.
}

{ Modern Pascal Directives }
{$MODE objfpc}{$H+}{$J-}

program runtests;

uses
  fpcunit, testregistry, consoletestrunner, testfilelist, testvtvsettings, testvtvlog;

Var
  Application : TTestRunner;

begin
{$IF FPC_FULLVERSION > 30100}
  TTestCase.CheckAssertCalled := true;
{$ENDIF}
  DefaultFormat := fPlain;
  DefaultRunAllTests := True;
  Application := TTestRunner.Create(Nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.