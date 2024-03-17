procedure TUDPListener.Execute;
var
  ClientSocket: TSocket;
  _cli: TUDPBlockSocket;
begin
  FSocket.CreateSocket;
  FSocket.SetLinger(True, 10000);
  FSocket.Bind(FEndPoint.Address, FEndPoint.Port);
  if FSocket.LastError = 0 then
    WriteLn('Bound on ' + FEndPoint.Address + ':' + FEndPoint.Port)
  else
    WriteLn('Bind Error: ' + FSocket.LastErrorDesc);
 
  FSocket.Listen;
  while not FTerminated do
  begin
    if FSocket.CanRead(100) then
    begin
      WriteLn('CanRead');
      ClientSocket := FSocket.Accept;
      _cli := TUDPBlockSocket.Create;
      _cli.Socket := ClientSocket;
      _cli.GetSins;
      WriteLn(_cli.RecvPacket(5000));
      _cli.SendString('Hello There');
 
      if FSocket.LastError = 0 then
        WriteLn('Client Accepted!')
      else
        WriteLn('Unable to Accept Client');
    end;
    // Relinquish the CPU
    sleep(10);
  end;
 
end;
 
