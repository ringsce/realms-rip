program hud;

{$mode objfpc}

uses
  SysUtils,
  Lua;

const
  LuaScriptFileName = 'hud.lua';

var
  L: PLuaState;

function LuaPrint(L: PLuaState): Integer; cdecl;
var
  NumArgs, I: Integer;
  Args: TStringList;
begin
  NumArgs := lua_gettop(L);
  Args := TStringList.Create;
  try
    for I := 1 to NumArgs do
      Args.Add(lua_tostring(L, I));
    WriteLn(Args.DelimitedText);
  finally
    Args.Free;
  end;
  Result := 0;
end;

procedure RunLuaScript(const FileName: string);
begin
  L := luaL_newstate();
  luaL_openlibs(L);

  // Register Lua print function
  lua_register(L, 'print', @LuaPrint);

  if luaL_loadfile(L, PAnsiChar(FileName)) <> 0 then
    WriteLn('Error loading Lua script: ', lua_tostring(L, -1))
  else if lua_pcall(L, 0, LUA_MULTRET, 0) <> 0 then
    WriteLn('Error executing Lua script: ', lua_tostring(L, -1));

  lua_close(L);
end;

begin
  try
    RunLuaScript(LuaScriptFileName);
  except
    on E: Exception do
      WriteLn('Exception: ', E.Message);
  end;
end.
