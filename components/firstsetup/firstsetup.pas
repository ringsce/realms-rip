program firstsetup;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, Sockets, IniFiles, CipherRC4;

const
  MAX_CLIENTS = 1024;
  SERVER_DIR = '$HOME/.server'; // Server directory on macOS
  INI_FILE_PATH = SERVER_DIR + '/server_config.ini';
  RC4_KEY = 'YourRC4Key'; // RC4 encryption key

type
  TClientHandler = class(TThread)
  private
    FClientSocket: TSocket;
    FRC4: String;
    function IsSuspiciousRequest(const Request: AnsiString): Boolean;
  public
    constructor Create(ClientSocket: TSocket);
    destructor Destroy; override;
    procedure Execute; override;
    procedure SendUpdateInfo;
  end;

var
  ServerSocket: TSocket;
  ServerAddr: TInetSockAddr;
  ClientAddrLen: TSocketLen;
  ServerINI: TIniFile;

constructor TClientHandler.Create(ClientSocket: TSocket);
begin
  FClientSocket := ClientSocket;
  FRC4 := TRC4.Create(RC4_KEY);
  inherited Create(False); // Create thread and start execution immediately
end;

destructor TClientHandler.Destroy;
begin
  FRC4.Free;
  inherited Destroy;
end;

function TClientHandler.IsSuspiciousRequest(const Request: AnsiString): Boolean;
const
  SuspiciousPatterns: array [0..1] of AnsiString = ('DROP TABLE', 'DELETE FROM');
var
  Pattern: AnsiString;
begin
  Result := False;
  for Pattern in SuspiciousPatterns do
  begin
    if Pos(Pattern, Request) > 0 then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TClientHandler.SendUpdateInfo;
var
  UpdateAvailable: string;
  UpdateURL: string;
  EncryptedUpdateAvailable: AnsiString;
  EncryptedUpdateURL: AnsiString;
begin
  UpdateAvailable := ServerINI.ReadString('Update', 'Available', '');
  UpdateURL := ServerINI.ReadString('Update', 'URL', '');

  // Encrypt update information
  EncryptedUpdateAvailable := FRC4.EncodeString(UpdateAvailable);
  EncryptedUpdateURL := FRC4.EncodeString(UpdateURL);

  // Send encrypted update information to client
  Sockets.Send(FClientSocket, PAnsiChar(EncryptedUpdateAvailable), Length(EncryptedUpdateAvailable), 0);
  Sockets.Send(FClientSocket, PAnsiChar(EncryptedUpdateURL), Length(EncryptedUpdateURL), 0);
end;

procedure TClientHandler.Execute;
var
  Request: AnsiString;
begin
  try
    // Read client request
    SetLength(Request, 1024);
    Sockets.Recv(FClientSocket, PAnsiChar(Request), Length(Request), 0);

    // Check for suspicious activity
    if IsSuspiciousRequest(Request) then
    begin
      Writeln('Suspicious activity detected from client: ', Request);
      Exit; // Terminate the thread
    end;

    // Check if client is requesting updates
    if Request = 'UPDATE_CHECK' then
      SendUpdateInfo;
  finally
    // Close client socket
    Sockets.CloseSocket(FClientSocket);
    Free; // Free the thread object
  end;
end;

begin
  // Create the server directory if it does not exist
  if not DirectoryExists(ExpandFileName(SERVER_DIR)) then
    CreateDir(ExpandFileName(SERVER_DIR));

  ServerINI := TIniFile.Create(INI_FILE_PATH);
  try
    ServerSocket := Sockets.Socket(AF_INET, SOCK_STREAM, 0);
    if ServerSocket = INVALID_SOCKET then
    begin
      Writeln('Error creating server socket');
      Halt;
    end;

    FillChar(ServerAddr, SizeOf(ServerAddr), 0);
    ServerAddr.sin_family := AF_INET;
    ServerAddr.sin_port := htons(12345);
    ServerAddr.sin_addr.s_addr := INADDR_ANY;

    if Sockets.Bind(ServerSocket, ServerAddr, SizeOf(ServerAddr)) = -1 then
    begin
      Writeln('Error binding server socket');
      Halt;
    end;

    if Sockets.Listen(ServerSocket, SOMAXCONN) = -1 then
    begin
      Writeln('Error listening on server socket');
      Halt;
    end;

    Writeln('Server started. Listening for connections...');

    while True do
    begin
      ClientAddrLen := SizeOf(ServerAddr);
      var ClientSocket := Sockets.Accept(ServerSocket, @ServerAddr, @ClientAddrLen);

      if ClientSocket = INVALID_SOCKET then
      begin
        Writeln('Error accepting connection');
        Continue;
      end;

      // Check if the maximum number of clients has been reached
      if TThread.ThreadCount >= MAX_CLIENTS then
      begin
        Writeln('Maximum number of clients reached. Connection rejected.');
        Sockets.CloseSocket(ClientSocket);
        Continue;
      end;

      // Create a thread to handle client communication
      TClientHandler.Create(ClientSocket);
    end;
  finally
    ServerINI.Free;
    // Close server socket
    Sockets.CloseSocket(ServerSocket);
  end;
end.
