program Scenicus;

{$mode objfpc}{$H+}

{ Raspberry Pi Zero Application                                                }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

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
  Console,
  Framebuffer,
  Player,
  dispmanx,
  UltiboUtils,  {Include Ultibo utils for some command line manipulation}
  Ultibo
  { Add additional units here };

var
  Console1: TWindowHandle;
  AudioThread: TAudioThread;
  VideoThread: TVideoThread;
  DefFrameBuff : PFrameBufferDevice;
  Properties : TWindowProperties;
  title:string;


 procedure WaitForSDDrive;
  begin
    while not DirectoryExists('C:\') do
      sleep(500);
  end;

begin
  { Add your program code here }
  begin

  Console1 := ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_FULLSCREEN, True);

  ConsoleWindowWriteLn(Console1, 'Audio / Video Test using OpenMax (OMX)');

  DefFrameBuff := FramebufferDeviceGetDefault;


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

  ConsoleWindowWriteLn(Console1, 'Halted.');
  ThreadHalt(0);

  end;

end.

