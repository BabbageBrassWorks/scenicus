unit player;

{$mode delphi}{$H+}
{$hints off}
{$notes off}

{ OMX Audio / Video Test Program }
{ pjde 2018 / Garry Wood 2021}

Interface


uses
  //RaspberryPi,
  //GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  VC4,
  SysUtils,
  uIL_Client,
  uOMX,
  SysCalls,
  Classes,
  Console,
  //uLog,
  UltiboUtils,
  Ultibo { Add additional units here };

const
  ny: array [boolean] of string = ('NO', 'YES');

var

  AudioStream: TMemoryStream;
  AudioHandle: THandle;
  AudioStart: LongWord;
  AudioSize: LongWord;
  AudioOffset: LongWord;
  ch: char;
  client: PILCLIENT_T;
  Channels: word;
  SampleRate: longword;
  BitsPerSample: word;
  comps: array of PCOMPONENT_T;
  eos: boolean;



type
  // Audio and Video thread classes
  TAudioThread = class(TThread)
  private
    FFilename: String;
    FRepeat: Boolean;
  public
    constructor Create(const AFilename: String; ARepeat: Boolean);
    procedure Execute; override;
  end;

  TVideoThread = class(TThread)
  private
    FFilename: String;
    FRepeat: Boolean;
  public
    constructor Create(const AFilename: String; ARepeat: Boolean);
    procedure Execute; override;
  end;

Implementation


// Forward declarations for thread classes
procedure OpenFile(const AFilename: String); forward;
procedure PlayFile(const ADestination, AFilename: string; ARepeat: Boolean); forward;
function PlayVideo(const AFilename: String; ARepeat: Boolean): integer; forward;

constructor TAudioThread.Create(const AFilename: String; ARepeat: Boolean);
begin
  inherited Create(True); // Create suspended, call Start to begin playback

  FFilename := AFilename;
  FRepeat := ARepeat;

  // Open audio file
  OpenFile(FFilename);
end;

procedure TAudioThread.Execute;
begin
  ThreadSetName(ThreadGetCurrent, 'Audio Thread');

  // Play audio file
  PlayFile('hdmi', FFilename, FRepeat);

  // Free stream
  AudioStream.Free;
end;

constructor TVideoThread.Create(const AFilename: String; ARepeat: Boolean);
begin
  inherited Create(True);

  FFilename := AFilename;
  FRepeat := ARepeat;

  // Start thread
  Start;
end;

procedure TVideoThread.Execute;
begin
  ThreadSetName(ThreadGetCurrent, 'Video Thread');

  // Play video file
  PlayVideo(FFilename, FRepeat);
end;

var
  AudioThread: TAudioThread;
  VideoThread: TVideoThread;


// Audio specific routines
function ComponentName(comp: PCOMPONENT_T): string;
var
  n: array [0..127] of char;
  cv, sv: OMX_VERSIONTYPE;
  cu: OMX_UUIDTYPE;
begin
  Result := '';
  n[0] := #0;
  FillChar(n, 128, 0);
  if OMX_GetComponentVersion(ilclient_get_handle(comp), n, @cv, @sv, @cu) = OMX_ErrorNone then
  begin
    Result := string(n);
    Result := Copy(Result, 14, length(Result) - 13); // remove OMX.Broadcom.
  end;
end;

//procedure PrintState(handle: OMX_HANDLETYPE);
//var
//  state: OMX_STATETYPE;
//  err: OMX_ERRORTYPE;
//begin
//  err := OMX_GetState(handle, @state);
//  if (err <> OMX_ErrorNone) then
//    Log('Error getting state.')
//  else
//    case state of
//      OMX_StateLoaded: Log('State Loaded.');
//      OMX_StateIdle: Log('State Idle.');
//      OMX_StateExecuting: Log('State Executing.');
//      OMX_StatePause: Log('State Pause.');
//      OMX_StateWaitForResources: Log('State Wait.');
//      OMX_StateInvalid: Log('State Invalid.');
//      else
//        Log('State Unknown');
//    end;
//end;

procedure OpenFile(const AFilename: String);
var
  f: TFileStream;
  id: string;
  w: word;
  l, s: longword;
begin
  AudioHandle := INVALID_HANDLE_VALUE;
  AudioStart := 0;
  AudioSize := 0;
  AudioOffset := 0;
  try
    //Log('Opening file ' + AFilename);
    f := TFileStream.Create(AFilename, fmOpenRead);
    f.Seek(0, soFromBeginning);
    while f.Position + 8 <= f.Size do
    begin
      SetLength(id, 4);
      f.Read(id[1], 4);
      f.Read(s, 4);
      if id = 'RIFF' then  // main chunk
        f.Read(id[1], 4)  // read id
      else                 // sub chunk
      begin
        if id = 'fmt ' then
        begin
          f.Read(w, 2);           // Audio Format
          f.Read(Channels, 2);
          //Log('Num Channels : ' + Channels.ToString);
          f.Read(SampleRate, 4);
          //Log('Sample Rate : ' + SampleRate.ToString);
          f.Read(l, 4);          // Byte Rate
          f.Read(w, 2);          // Block Align
          f.Read(BitsPerSample, 2);
          //Log('Bits per sample : ' + BitsPerSample.ToString);
          if s > 16 then
            f.Seek(s - 16, soFromCurrent); // ignore extras for moment
        end
        else if id = 'data' then
        begin
          // Save start and size of audio data
          AudioStart := f.Position;
          AudioSize := s;
          f.Seek(s, soFromCurrent);

          //Log('Audio Data is ' + AudioSize.ToString + ' bytes long.');
        end
        else
          f.Seek(s, soFromCurrent);
      end;
    end;
    f.Free;
  except
    on e: Exception do
      //Log('Error opening "' + AFilename + '". ' + e.Message);
  end;
end;

procedure port_settings_callback(userdata: pointer; comp: PCOMPONENT_T; Data: longword); cdecl;
begin
  // Nothing
end;

procedure empty_buffer_callback(userdata: pointer; comp: PCOMPONENT_T; Data: longword); cdecl;
begin
  // Nothing
end;

procedure fill_buffer_callback(userdata: pointer; comp: PCOMPONENT_T; Data: longword); cdecl;
begin
  // Nothing
end;

procedure eos_callback(userdata: pointer; comp: PCOMPONENT_T; Data: longword); cdecl;
begin
  //Log(ComponentName(comp) + ' End Of Stream Detected.');
  eos := True;
end;

procedure error_callback(userdata: pointer; comp: PCOMPONENT_T; Data: longword); cdecl;
begin
  //Log(ComponentName(comp) + ' Error : ' + OMX_ErrToStr(Data));
end;

function read_into_buffer_and_empty(comp: PCOMPONENT_T; buff: POMX_BUFFERHEADERTYPE; loop: Boolean): OMX_ERRORTYPE;
var
  buff_size: integer;
  Read: integer;
begin
  buff_size := buff.nAllocLen;
  Read := AudioSize - AudioOffset;
  if Read > buff_size then
    Read := buff_size;
  AudioOffset := AudioOffset + FileRead(AudioHandle, buff.pBuffer^, Read);
  buff.nFilledLen := Read;

  if AudioOffset = AudioSize then
  begin
    if loop then
    begin
      FileSeek(AudioHandle, AudioStart, soFromBeginning);
      AudioOffset := AudioStart;
    end
    else
      buff.nFlags := buff.nFlags or OMX_BUFFERFLAG_EOS;
  end;

  Result := OMX_EmptyThisBuffer(ilclient_get_handle(comps[0]), buff);
end;


procedure OMXCheck(n: string; r: OMX_ERRORTYPE);
begin
  if r <> OMX_ErrorNone then
    raise Exception.Create(n + ' ' + OMX_ErrToStr(r));
end;

procedure ILCheck(n: string; e: integer);
begin
  if e <> 0 then
    raise Exception.Create(n + ' Failed');
end;

procedure PlayFile(const ADestination, AFilename: string; ARepeat: Boolean);
var
  res: integer;
  param: OMX_PARAM_PORTDEFINITIONTYPE;
  pcm: OMX_AUDIO_PARAM_PCMMODETYPE;
  dest: OMX_CONFIG_BRCMAUDIODESTINATIONTYPE;
  hdr: POMX_BUFFERHEADERTYPE;
begin
  //Log('Start of Audio Playback ...');

  if not (Channels in [1 .. 2]) then
    exit;

  if AudioHandle <> INVALID_HANDLE_VALUE then
    exit;

  if AudioSize = 0 then
    exit;

  AudioHandle := FileOpen(AFilename, fmOpenRead or fmShareDenyNone);
  if AudioHandle = INVALID_HANDLE_VALUE then
    Exit;
  FileSeek(AudioHandle, AudioStart, soFromBeginning);
  AudioOffset := AudioStart;

  SetLength(comps, 2);
  client := nil;
  eos := False;
  for res := low(comps) to high(comps) do
    comps[res] := nil;

  // initialise OMX and IL client
  try
    OMXCheck('OMX Init', OMX_Init);
    client := ilclient_init;

    // set callbacks
    ilclient_set_port_settings_callback(client, @port_settings_callback, nil);
    ilclient_set_empty_buffer_done_callback(client, @empty_buffer_callback, nil);
    ilclient_set_fill_buffer_done_callback(client, @fill_buffer_callback, nil);
    ilclient_set_eos_callback(client, @eos_callback, nil);
    ilclient_set_error_callback(client, @error_callback, nil);

    // create render
    ILCheck('Create Render', ilclient_create_component(client, @comps[0], 'audio_render', ILCLIENT_ENABLE_INPUT_BUFFERS or ILCLIENT_DISABLE_ALL_PORTS));

    // confirm port is set to pcm
    FillChar(param, sizeof(OMX_PARAM_PORTDEFINITIONTYPE), 0);
    param.nSize := sizeof(OMX_PARAM_PORTDEFINITIONTYPE);
    param.nVersion.nVersion := OMX_VERSION;
    param.nPortIndex := 100; // audio input
    OMXCheck('Get Port', OMX_GetParameter(ilclient_get_handle(comps[0]), OMX_IndexParamPortDefinition, @param));
    param.format.audio.eEncoding := OMX_AUDIO_CodingPCM;
    OMXCheck('Set Port ', OMX_SetParameter(ilclient_get_handle(comps[0]), OMX_IndexParamPortDefinition, @param));

    // set sampling rate, channels and bits per sample
    FillChar(pcm, sizeof(OMX_AUDIO_PARAM_PCMMODETYPE), 0);
    pcm.nSize := sizeof(OMX_AUDIO_PARAM_PCMMODETYPE);
    pcm.nVersion.nVersion := OMX_VERSION;
    pcm.nPortIndex := 100;
    OMXCheck('Get PCM', OMX_GetParameter(ilclient_get_handle(comps[0]), OMX_IndexParamAudioPcm, @pcm));
    pcm.nChannels := Channels;
    pcm.nBitPerSample := BitsPerSample;
    pcm.nSamplingRate := SampleRate;
    FillChar(pcm.eChannelMapping, sizeof(pcm.eChannelMapping), 0);
    case Channels of
      1:
      begin
        pcm.eChannelMapping[0] := OMX_AUDIO_ChannelCF;
      end;
      2:
      begin
        pcm.eChannelMapping[1] := OMX_AUDIO_ChannelRF;
        pcm.eChannelMapping[0] := OMX_AUDIO_ChannelLF;
      end;
    end;
    OMXCheck('Set PCM', OMX_SetParameter(ilclient_get_handle(comps[0]), OMX_IndexParamAudioPcm, @pcm));

    // set to idle and enable buffers
    ilclient_change_component_state(comps[0], OMX_StateIdle);
    ILCheck('Enable Buffers', ilclient_enable_port_buffers(comps[0], 100, nil, nil, nil));
    ilclient_enable_port(comps[0], 100);

    // set to executing
    ilclient_change_component_state(comps[0], OMX_StateExecuting);

    // set destination
    dest.nSize := sizeof(OMX_CONFIG_BRCMAUDIODESTINATIONTYPE);
    dest.nVersion.nVersion := OMX_VERSION;
    FillChar(dest.sName, sizeof(dest.sName), 0);
    dest.sName := ADestination;
    OMXCheck('Set Dest', OMX_SetConfig(ilclient_get_handle(comps[0]), OMX_IndexConfigBrcmAudioDestination, @dest));
    while AudioOffset < AudioSize do
    begin
      hdr := ilclient_get_input_buffer(comps[0], 100, 1);
      if hdr <> nil then
        read_into_buffer_and_empty(comps[0], hdr, ARepeat);
    end;

    while not eos do
      sleep(100);
  finally
    ilclient_change_component_state(comps[0], OMX_StateLoaded);
    ilclient_cleanup_components(@comps[0]);
    ilclient_destroy(client);
    client := nil;
    OMX_DeInit;
  end;

  FileClose(AudioHandle);
  AudioHandle := INVALID_HANDLE_VALUE;

  //Log('End of Audio Playback ...');
end;

// Video specific routines
function PlayVideo(const AFilename: String; ARepeat: Boolean): integer;
var
  format: OMX_VIDEO_PARAM_PORTFORMATTYPE;
  cstate: OMX_TIME_CONFIG_CLOCKSTATETYPE;
  video_decode, video_scheduler, video_render, clock: PCOMPONENT_T;
  list: array[0..4] of PCOMPONENT_T;
  tunnel: array[0..3] of TUNNEL_T;
  client: PILCLIENT_T;
  infile: THandle;
  status: integer;
  data_len: cardinal;
  buf: POMX_BUFFERHEADERTYPE;
  port_settings_changed: integer;
  first_packet: integer;
  dest: PByte;
begin
  //Log('Start of Video Playback ...');

  video_decode := nil;
  video_scheduler := nil;
  video_render := nil;
  clock := nil;
  status := 0;
  data_len := 0;

  FillChar(list, SizeOf(list), 0);
  FillChar(tunnel, SizeOf(tunnel), 0);

  // Open file
  infile := FileOpen(AFilename, fmOpenReadWrite or fmShareDenyNone);
  if infile = INVALID_HANDLE_VALUE then
    Exit(-2);

  // Init IL client
  client := ilclient_init;
  if client = nil then
    Exit(-3);

  // Init OMX
  if OMX_Init <> OMX_ErrorNone then
  begin
    ilclient_destroy(client);
    FileClose(infile);
    Exit(-4);
  end;

  // Create video_decode
  if (ilclient_create_component(client, @video_decode, 'video_decode', ILCLIENT_DISABLE_ALL_PORTS or ILCLIENT_ENABLE_INPUT_BUFFERS) <> 0) then
    status := -14;
  list[0] := video_decode;

  // Create video_render
  if (status = 0) and (ilclient_create_component(client, @video_render, 'video_render', ILCLIENT_DISABLE_ALL_PORTS) <> 0) then
    status := -14;
  list[1] := video_render;

  // Create clock
  if (status = 0) and (ilclient_create_component(client, @clock, 'clock', ILCLIENT_DISABLE_ALL_PORTS) <> 0) then
    status := -14;
  list[2] := clock;

  FillChar(cstate, SizeOf(cstate), 0);
  cstate.nSize := SizeOf(cstate);
  cstate.nVersion.nVersion := OMX_VERSION;
  cstate.eState := OMX_TIME_ClockStateWaitingForStartTime;
  cstate.nWaitMask := 1;
  if (clock <> nil) and (OMX_SetParameter(ilclient_get_handle(clock), OMX_IndexConfigTimeClockState, @cstate) <> OMX_ErrorNone) then
    status := -13;

  // Create video_scheduler
  if (status = 0) and (ilclient_create_component(client, @video_scheduler, 'video_scheduler', ILCLIENT_DISABLE_ALL_PORTS) <> 0) then
    status := -14;
  list[3] := video_scheduler;

  set_tunnel(@tunnel[0], video_decode, 131, video_scheduler, 10);
  set_tunnel(@tunnel[1], video_scheduler, 11, video_render, 90);
  set_tunnel(@tunnel[2], clock, 80, video_scheduler, 12);

  // Setup clock tunnel first
  if (status = 0) and (ilclient_setup_tunnel(@tunnel[2], 0, 0) <> 0) then
    status := -15
  else
    ilclient_change_component_state(clock, OMX_StateExecuting);

  if (status = 0) then
    ilclient_change_component_state(video_decode, OMX_StateIdle);

  FillChar(format, SizeOf(OMX_VIDEO_PARAM_PORTFORMATTYPE), 0);
  format.nSize := SizeOf(OMX_VIDEO_PARAM_PORTFORMATTYPE);
  format.nVersion.nVersion := OMX_VERSION;
  format.nPortIndex := 130;
  format.eCompressionFormat := OMX_VIDEO_CodingAVC;
  if (status = 0)
    and (OMX_SetParameter(ilclient_get_handle(video_decode), OMX_IndexParamVideoPortFormat, @format) = OMX_ErrorNone)
    and (ilclient_enable_port_buffers(video_decode, 130, nil, nil, nil) = 0) then
  begin
    port_settings_changed := 0;
    first_packet := 1;

    ilclient_change_component_state(video_decode, OMX_StateExecuting);

    buf := ilclient_get_input_buffer(video_decode, 130, 1);
    while (buf <> nil) do
    begin
      // Feed data and wait until we get port settings changed
      dest := buf.pBuffer;
      data_len := data_len + FileRead(infile, dest^, buf.nAllocLen - data_len);

      if (data_len = 0) and ARepeat then
      begin
        FileSeek(infile, 0, soFromBeginning);
        data_len := FileRead(infile, dest^, buf.nAllocLen);
      end;

      if (port_settings_changed = 0)
        and (((data_len > 0) and (ilclient_remove_event(video_decode, OMX_EventPortSettingsChanged, 131, 0, 0, 1) = 0))
          or ((data_len = 0) and (ilclient_wait_for_event(video_decode, OMX_EventPortSettingsChanged, 131, 0, 0, 1, ILCLIENT_EVENT_ERROR or ILCLIENT_PARAMETER_CHANGED, 10000) = 0))) then
      begin
        port_settings_changed := 1;

        if ilclient_setup_tunnel(@tunnel[0], 0, 0) <> 0 then
        begin
          status := -7;
          Break;
        end;

        ilclient_change_component_state(video_scheduler, OMX_StateExecuting);

        // Now setup tunnel to video_render
        if ilclient_setup_tunnel(@tunnel[1], 0, 1000) <> 0 then
        begin
          status := -12;
          Break;
        end;

        ilclient_change_component_state(video_render, OMX_StateExecuting);
      end;

      if data_len = 0 then
        Break;

      buf.nFilledLen := data_len;
      data_len := 0;
      buf.nOffset := 0;

      if first_packet = 1 then
      begin
        buf.nFlags := OMX_BUFFERFLAG_STARTTIME;
        first_packet := 0;
      end
      else
        buf.nFlags := OMX_BUFFERFLAG_TIME_UNKNOWN;

      if OMX_EmptyThisBuffer(ilclient_get_handle(video_decode), buf) <> OMX_ErrorNone then
      begin
        status := -6;
        Break;
      end;

      buf := ilclient_get_input_buffer(video_decode, 130, 1);
    end;

    buf.nFilledLen := 0;
    buf.nFlags := OMX_BUFFERFLAG_TIME_UNKNOWN or OMX_BUFFERFLAG_EOS;

    if OMX_EmptyThisBuffer(ilclient_get_handle(video_decode), buf) <> OMX_ErrorNone then
      status := -20;

    // Wait for EOS from render
    ilclient_wait_for_event(video_render, OMX_EventBufferFlag, 90, 0, OMX_BUFFERFLAG_EOS, 0, ILCLIENT_BUFFER_FLAG_EOS, -1);

    // Need to flush the renderer to allow video_decode to disable its input port
    ilclient_flush_tunnels(@tunnel[0], 0);
  end;

  FileClose(infile);

  ilclient_disable_tunnel(@tunnel[0]);
  ilclient_disable_tunnel(@tunnel[1]);
  ilclient_disable_tunnel(@tunnel[2]);
  ilclient_disable_port_buffers(video_decode, 130, nil, nil, nil);
  ilclient_teardown_tunnels(@tunnel[0]);

  ilclient_state_transition(list[0], OMX_StateIdle);
  ilclient_state_transition(list[0], OMX_StateLoaded);

  ilclient_cleanup_components(list[0]);

  OMX_Deinit;

  ilclient_destroy(client);

  Result := status;

  //Log('End of Video Playback ...');
end;


end.
