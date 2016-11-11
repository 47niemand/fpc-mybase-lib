unit utimers;

interface

{.$DEFINE WINXPMODE}

//function timer_GetTicks: double;
function timer_GetTick: int64;
function tickToTime(const I: int64): double;
function tickToTimeStr(const I: int64): string;


implementation

uses Windows, sysutils;

var
  timerFrequency: int64;
  timerFreq: double;
  timerStart: double;

function timer_GetTicks: double;
var
  t: int64;
{$IFDEF WINXPMODE}
  M: longword;
{$ENDIF}
begin
{$IFDEF WINXPMODE}
  M := SetThreadAffinityMask(GetCurrentThread(), 1);
{$ENDIF}
  QueryPerformanceCounter(t);
  Result := 1000 * t * timerFreq - timerStart;
{$IFDEF WINXPMODE}
  SetThreadAffinityMask(GetCurrentThread(), M);
{$ENDIF}
end;

function timer_GetTick: int64;
{$IFDEF WINXPMODE}
var
  M: longword;
{$ENDIF}
begin
{$IFDEF WINXPMODE}
  M := SetThreadAffinityMask(GetCurrentThread(), 1);
{$ENDIF}
  QueryPerformanceCounter(Result);
{$IFDEF WINXPMODE}
  SetThreadAffinityMask(GetCurrentThread(), M);
{$ENDIF}
end;

function tickToTime(const I: int64): double;
begin
  Result := 1000 * I * timerFreq;
end;

function tickToTimeStr(const I: int64): string;
begin
  Result := format('%.0f ms', [tickToTime(I)]);
end;

{$IFDEF WINXPMODE}
var
  M: longword;
{$ENDIF}

initialization
{$IFDEF WINXPMODE}
  M := SetThreadAffinityMask(GetCurrentThread(), 1);
{$ENDIF}
  QueryPerformanceFrequency(timerFrequency);
{$IFDEF WINXPMODE}
  SetThreadAffinityMask(GetCurrentThread(), M);
{$ENDIF}
  timerFreq := 1 / timerFrequency;
  timerStart := timer_GetTicks();
end.
