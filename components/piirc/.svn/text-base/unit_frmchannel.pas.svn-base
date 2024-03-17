unit unit_frmChannel;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
    StdCtrls, ExtCtrls, PairSplitter, CheckLst, uCmdBox, lNetComponents, lNet,
    unit_TOptionManager;

type

    { TfrmChannel }

    TfrmChannel = class(TForm)
        lstNickList: TListBox;
        termDisplay: TCmdBox;
        OptionManager: TOptionManager;
        Splitter: TSplitter;
        tnParentSocket: TLTelnetClientComponent;
        txtInput: TEdit;

        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure txtInputKeyPress(Sender: TObject; var Key: char);

        procedure WriteLn( sTxt: String );

        procedure ListFill( sTxt: String);
        procedure ListAdd( sTxt: String);
        procedure ListRemove( sTxt: String);

        private
            p_ID: String;
        public
            property ID : String Read p_ID Write p_ID;
    end;


    { TChannelManager }

    TChannelManager = Class(TObject)
        private
            {}
        public
            Items: Array of TfrmChannel;
            function Join( zID: String ): Integer;
            function Part( zID: String ): Boolean;
            function Add( Chn: TfrmChannel ): Integer;
            function Remove( zID: String ): Boolean;
            function IndexOf( zID: String ): Integer;
            function Count(): Integer;
    end;


implementation

function TChannelManager.Part( zID: String ): boolean;
var
    x: Integer;
begin

    x := Self.IndexOf( zID );

    if (x > -1) then begin
        { close the window }
        Self.Items[x].Close();

        { delete it from the array of windows }
        Result := Self.Remove( zID );
    end;

end;

function TChannelManager.Join( zID: String ): Integer;
var

    x: Integer;
    newChan: TfrmChannel;

begin

    newChan := TfrmChannel.Create(nil);
    x := Self.Add(newChan);

    if (x > -1) then begin
        newChan.ID := zID;
        newChan.Caption := zID;
        newChan.Show();

        Result := x;
    end;

end;

function TChannelManager.Count(): Integer;
begin
    Result := Length(Self.Items);
end;


function TChannelManager.Add( Chn: TfrmChannel ): Integer;
var
    x: Integer;
begin

    if (not ( Self.IndexOf( Chn.ID ) = -1 )) then begin
        { if we return anything but -1 then the item already exists }
        Result := -1;  Exit;
    end;

    x := (Self.Count + 1);

    SetLength(Self.Items, x);

    x := (Self.Count - 1);

    Self.Items[x] := Chn;

    Result := x;
end;

function TChannelManager.Remove( zID: String ): Boolean;
var
    i: Integer; { index }
    x: Integer;
begin

    i := Self.IndexOf( zID );

    if (not (i = -1)) then begin

        x := (Self.Count - 1);  { this should be the last item in our array }

        { move the item in our array over the item to be deleted }
        Self.Items[i] := Self.Items[x];

        { reduce the size of our array }
        SetLength(Self.Items, (x));

        Result := True;
    end else begin
        Result := False;
    end;
end;

function TChannelManager.IndexOf( zID: String ): Integer;
var
    i: Integer;
begin

    for i := 0 to (Self.Count - 1) do begin
        { single case only }
        if (lowercase(Self.Items[i].ID) = lowercase(zID)) then begin
            Result := i;  Exit;
        end;
    end;

    { no item found }
    Result := -1; Exit;
end;

{ TfrmChannel }

procedure TfrmChannel.ListAdd( sTxt: String);
var
    x: Integer;
begin

    x := Self.lstNickList.Items.Add(sTxt);

end;

procedure TfrmChannel.ListRemove( sTxt: String);
var
    x: Integer;
begin

    for x := 0 to (Self.lstNickList.Items.Count - 1) do begin
        if (lowercase(Self.lstNickList.Items[x]) = lowercase(sTxt)) then begin
            Self.lstNickList.Items.Delete(x);
            Break;
        end;
    end;

end;

procedure TfrmChannel.ListFill( sTxt: String);
var
    x: Integer;
    slTemp: TStringList;
    sNick: String;
begin

    slTemp := TStrIngList.Create();
    slTemp.Delimiter := ' ';
    slTemp.StrictDelimiter := True;
    slTemp.DelimitedText := sTxt;

    Self.lstNickList.Clear();

    for x := 0 to (slTemp.Count - 1) do begin
        if (not (slTemp[x] = '')) then begin

            Self.lstNickList.Items.Add(slTemp[x]);
        end;
    end;
end;

procedure TfrmChannel.WriteLn(sTxt: String);
begin
    Self.termDisplay.WriteLn( (#27 + '[30;47m') + sTxt + (#27 + '[0m') );
end;

procedure TfrmChannel.txtInputKeyPress(Sender: TObject; var Key: char);
begin
    if (key = #13) then begin
        Self.WriteLn( '<' + OptionManager.Nickname + '> ' + txtInput.Text );
        tnParentSocket.SendMessage( 'PRIVMSG ' + Self.ID + ' :' + txtInput.Text + #13#10 );
        txtInput.Text := '';
    end;
end;

procedure TfrmChannel.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    Self.tnParentSocket.SendMessage('PART ' + Self.ID + (#13#10));
end;


initialization
    {$I unit_frmchannel.lrs}

end.

