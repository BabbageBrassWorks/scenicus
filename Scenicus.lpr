program Scenicus;

{$mode objfpc}{$H+}

{ Raspberry Pi Zero Application                                                }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

uses
  RaspberryPi,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  OpenVG,
  VC4,
  SysUtils,
  Syscalls,
  Classes,
  Console,
  Framebuffer,
  Player,
  VGShapes,
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
  Width: Integer;
  Height: Integer;
  title:string;
  Fontsize:Integer;
  //SpacingX, SpacingY: Integer;

  //Watts, KPH, Volts, Amps :VGfloat;

 procedure WaitForSDDrive;
  begin
    while not DirectoryExists('C:\') do
      sleep(500);
  end;

begin
  { Add your program code here }
  begin

  Console1 := ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_FULLSCREEN, True);

  ConsoleWindowWriteLn(Console1, 'Audio / Video Test using OpenMax (OMX) with OpenVG overlays.');

  DefFrameBuff := FramebufferDeviceGetDefault;
  Width := 1680;
  Height := 1050;

  WaitForSDDrive;

  {Initialize OpenVG and the VGShapes unit}
  vgshapessetlayer(2);

  vgshapesinit(Width, Height, DISPMANX_FLAGS_ALPHA_FROM_SOURCE);
  {Start a picture the full width and height of the screen}

  VGShapesStart(Width, Height);
  VGShapesBackgroundRGB(0,0,0, 0.0);
  VGShapesWindowOpacity(180);

  AudioThread := TAudioThread.Create('C:\files\skyrim.wav', True);
  VideoThread := TVideoThread.Create('C:\files\skyrim.h264', True);

  // Start audio thread (Video thread starts immediately)
  AudioThread.Start;

  title := 'Gereg';
  Fontsize:=Trunc(Height * 0.05);
  VGShapesFill(80,255,128,1);

  while True do

    begin
      //VGShapesWindowClear;
      //VGShapesTextMid(Width * 0.5 , Height * 0.1, title,VGShapesSerifTypeface,Fontsize);
      {End our picture and render it to the screen}
      //VGShapesEnd;
      sleep(5000);
    end;

   {VGShapes calls BCMHostInit during initialization, we should also call BCMHostDeinit to cleanup}
  BCMHostDeinit;

  ConsoleWindowWriteLn(Console1, 'Halted.');
  ThreadHalt(0);

  end;

end.

