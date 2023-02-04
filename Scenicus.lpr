program Scenicus;

{$mode objfpc}{$H+}

{ Raspberry Pi AV player }
{ Gavin McIntosh 2023 }

uses
  {$IFDEF RPI}
  RaspberryPi,
  {$ENDIF}
  {$IFDEF RPI2}
  RaspberryPi2,
  {$ENDIF}
  {$IFDEF RPI3}
  RaspberryPi3,
  {$ENDIF}
  {$IFDEF RPI4}
  RaspberryPi4,
  {$ENDIF}
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  VC4,
  SysUtils,
  Syscalls,
  Classes,
  Player,
  dispmanx,
  Ultibo
  { Add additional units here };

var
  Console1: TWindowHandle;
  AudioThread: TAudioThread;
  VideoThread: TVideoThread;
  title:string;


 procedure WaitForSDDrive;
  begin
    while not DirectoryExists('C:\') do
      sleep(500);
  end;

begin
  { Add your program code here }
  begin

  WaitForSDDrive;

  BCMHostInit;

  AudioThread := TAudioThread.Create('C:\files\skyrim.wav', True);
  VideoThread := TVideoThread.Create('C:\files\skyrim.h264', True);

  // Start audio thread (Video thread starts immediately)
  AudioThread.Start;

  title := 'Skyrim';

  while True do

    begin
       sleep(5000);
    end;

  BCMHostDeinit;

  //ConsoleWindowWriteLn(Console1, 'Halted.');
  ThreadHalt(0);

  end;

end.

