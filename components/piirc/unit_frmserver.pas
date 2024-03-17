unit unit_frmServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, uCmdBox, lNetComponents, lNet, unit_frmChannel, unit_TOptionManager;

type

    { TfrmServer }

    TfrmServer = class(TForm)
        tnSocket: TLTelnetClientComponent;
        termDisplay: TCmdBox;
        txtInput: TEdit;

        function IsNumeric(S:String): Boolean;

        procedure FormCreate(Sender: TObject);
        procedure tnSocketConnect(aSocket: TLSocket);
        procedure tnSocketDisconnect(aSocket: TLSocket);
        procedure tnSocketError(const msg: string; aSocket: TLSocket);
        procedure tnSocketReceive(aSocket: TLSocket);
        procedure txtInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

        function FromHostGetNick(sLine: String): String;
        function FromHostGetUser(sLine: String): String;
        function FromHostGetAddr(sLine: String): String;

        procedure ProcessLine(sLine: String);
        procedure ProcessString(sPrefix, sCommand, sMid, sTrail: String);
        procedure ProcessNumeric(sPrefix, sCommand, sMid, sTrail: String);

        procedure Buffer( sTxt: String );
        procedure WriteLn(sTxt: String);

        private
            { private declarations }
        public
            { public declarations }
    end;


var
    p_Buffer: String;
    frmServer: TfrmServer;
    OptionManager: TOptionManager;
    ChannelManager: TChannelManager;

implementation


procedure TfrmServer.WriteLn(sTxt: String);
begin
    Self.termDisplay.WriteLn( (#27 + '[30;47m') + sTxt + (#27 + '[0m') );
end;


function TfrmServer.IsNumeric(S:String): Boolean;
var
    V: Integer;
    C: Word;
begin

    Val(S, V, C);

    if (C <> 0) then begin
        Result := False;
    end else begin
        Result := True;
    end;
end;

function TfrmServer.FromHostGetNick(sLine: String): String;
var
    x: Integer;

begin

    x := Pos('!', sLine);
    if (x = 0) then begin
        Exit;
    end else begin
        Result := Copy(sLine, 1, (x - 1));
    end;

end;

function TfrmServer.FromHostGetUser(sLine: String): String;
var
    x: Integer;
    y: Integer;
begin

    x := Pos('!', sLine);
    y := Pos('@', sLine);

    if ((x = 0) or (y = 0)) then begin
        Exit;
    end else begin
        Result := Copy(sLine, x, (y - 1));
    end;

end;

function TfrmServer.FromHostGetAddr(sLine: String): String;
var
    x: Integer;
begin

    x := Pos('@', sLine);

    if (x = 0) then begin
        Exit;
    end else begin
        Result := Copy(sLine, x, Length(sLine));
    end;

end;

{
This is the main parsing sub.

The IRC Protocal is divided into three parts
The Prefix (optional), Command, and Arguments.

I get a little more specific & break it down
to four parts. A prefix (optional), Command,
Middle Arguments, and Trailing Arguments.

Middle and Trail may be delimited with a white space
or null, where Prefix and Command CAN NOT be.

PREFIX               COMMAND     MIDDLE       TRAIL
:[server | hostmask] [str | int] {[str] ... } :[str]

After we have everthing parsed up we send it along its
way for further processing.
}
procedure TfrmServer.ProcessLine(sLine: String);
var
    x: Integer;

    sPrefix: String;
    sCommand: String;
    sMidTmp: String;
    sMiddle: Array of String;
    sTrailing: String;

    slParser: TStringList;
begin

    if (Pos(' ', sLine) = 0) then begin
        Exit;
    end;

    slParser := TStringList.Create();
    slParser.StrictDelimiter := True;

    if (LeftStr(sLine, 1) = ':') then begin
        slParser.Delimiter := ' ';
        slParser.DelimitedText := sLine;

        sPrefix := RightStr( slParser[0], ( Length(slParser[0]) - 1)  );
        sCommand := slParser[1];

    end else begin
        slParser.Delimiter := ' ';
        slParser.DelimitedText := sLine;

        sPrefix := '';
        sCommand := slParser[0];
    end;

    sLine := Copy(sLine, (Pos(sCommand, sLine) + Length(sCommand) + 1), Length(sLine));

    slParser.Delimiter := ' ';
    slParser.DelimitedText := sLine;

    for x := 0 to (slParser.Count - 1) do begin
        if (LeftStr(slParser[x], 1) = ':') then begin
            { the first argument that begins with a : starts the trailing
            message }
            sTrailing := Copy(sLine, Pos(slParser[x], sLine), Length(sLine));
            Break;
        end else begin
            { all other pieces are middle arguments }
            sMidTmp := sMidTmp + slParser[x] + ' ';
        end;
    end;

    sMidTmp := Trim(sMidTmp);

    if (LeftStr(sTrailing, 1) = ':') then begin
        sTrailing := Copy(sTrailing, 2, Length(sTrailing));
    end;

    //Self.WriteLn( '[' + sPrefix + '] {' + sCommand + '} ' + '<' + sMidTmp + '> (' + sTrailing + ')');

    if Self.IsNumeric(sCommand) then begin
        Self.ProcessNumeric(sPrefix, sCommand, sMidTmp, sTrailing)
    end else begin
        Self.ProcessString(sPrefix, sCommand, sMidTmp, sTrailing)
    end;

end;


procedure TfrmServer.ProcessNumeric(sPrefix, sCommand, sMid, sTrail: String);
var
    x, j: Integer;
    slMiddle: TStringList;
    slTemp: TStringList;
begin

    slTemp := TStrIngList.Create();
    slTemp.Delimiter := ' ';
    slTemp.StrictDelimiter := True;

    slMiddle := TStrIngList.Create();
    slMiddle.Delimiter := ' ';
    slMiddle.StrictDelimiter := True;
    slMiddle.DelimitedText := sMid;

    sCommand := uppercase(sCommand);

    if (sCommand = '000') then begin
    end else if (sCommand = '004') then begin
        Self.Caption := 'pIRCc (' + slMiddle[1] + ')'
    end else if (sCommand = '200') then begin //RPL_TRACELINK           "Link <version & debug level> <destination> <next server>"
    end else if (sCommand = '201') then begin //RPL_TRACECONNECTING     "Try. <class> <server>"
    end else if (sCommand = '202') then begin //RPL_TRACEHANDSHAKE      "H.S. <class> <server>"
    end else if (sCommand = '203') then begin //RPL_TRACEUNKNOWN        "???? <class> [<client IP address in dot form>]"
    end else if (sCommand = '204') then begin //RPL_TRACEOPERATOR       "Oper <class> <nick>"
    end else if (sCommand = '205') then begin //RPL_TRACEUSER           "User <class> <nick>"
    end else if (sCommand = '206') then begin //RPL_TRACESERVER         "Serv <class> <int>S <int>C <server> <nick!user|*!*>@<host|server>"
    end else if (sCommand = '208') then begin //RPL_TRACENEWTYPE        "<newtype> 0 <client name>"
    end else if (sCommand = '211') then begin //RPL_STATSLINKINFO       "<linkname> <sendq> <sent messages> <sent bytes> <received messages> <received bytes> <time open>"
    end else if (sCommand = '212') then begin //RPL_STATSCOMMANDS       "<command> <count>"
    end else if (sCommand = '213') then begin //RPL_STATSCLINE          "C <host> * <name> <port> <class>"
    end else if (sCommand = '214') then begin //RPL_STATSNLINE          "N <host> * <name> <port> <class>"
    end else if (sCommand = '215') then begin //RPL_STATSILINE          "I <host> * <host> <port> <class>"
    end else if (sCommand = '216') then begin //RPL_STATSKLINE          "K <host> * <username> <port> <class>"
    end else if (sCommand = '218') then begin //RPL_STATSYLINE          "Y <class> <ping frequency> <connect frequency> <max sendq>"
    end else if (sCommand = '219') then begin //RPL_ENDOFSTATS          "<stats letter> :End of /STATS report"
    end else if (sCommand = '221') then begin //RPL_UMODEIS             "<user mode string>"
    end else if (sCommand = '241') then begin //RPL_STATSLLINE          "L <hostmask> * <servername> <maxdepth>"
    end else if (sCommand = '242') then begin //RPL_STATSUPTIME         ":Server Up %d days %d:%02d:%02d"
    end else if (sCommand = '243') then begin //RPL_STATSOLINE          "O <hostmask> * <name>"
    end else if (sCommand = '244') then begin //RPL_STATSHLINE          "H <hostmask> * <servername>"
    end else if (sCommand = '251') then begin //RPL_LUSERCLIENT         ":There are <integer> users and <integer> invisible on <integer> servers"
    end else if (sCommand = '252') then begin //RPL_LUSEROP             "<integer> :operator(s) online"
    end else if (sCommand = '253') then begin //RPL_LUSERUNKNOWN        "<integer> :unknown connection(s)"
    end else if (sCommand = '254') then begin //RPL_LUSERCHANNELS       "<integer> :channels formed"
    end else if (sCommand = '255') then begin //RPL_LUSERME             ":I have <integer> clients and <integer> servers"
    end else if (sCommand = '256') then begin //RPL_ADMINME             "<server> :Administrative info"
    end else if (sCommand = '257') then begin //RPL_ADMINLOC1           ":<admin info>"
    end else if (sCommand = '258') then begin //RPL_ADMINLOC2           ":<admin info>"
    end else if (sCommand = '259') then begin //RPL_ADMINEMAIL          ":<admin info>"
    end else if (sCommand = '261') then begin //RPL_TRACELOG            "File <logfile> <debug level>"
    end else if (sCommand = '300') then begin //RPL_NONE                Dummy reply number. Not used.
    end else if (sCommand = '301') then begin //RPL_AWAY                "<nick> :<away message>"
    end else if (sCommand = '302') then begin //RPL_USERHOST            ":[<reply>{<space><reply>}]"
    end else if (sCommand = '303') then begin //RPL_ISON                ":[<nick> {<space><nick>}]"
    end else if (sCommand = '305') then begin //RPL_UNAWAY              ":You are no longer marked as being away"
        {Echo frmStatus, sTrail}
    end else if (sCommand = '306') then begin //RPL_NOWAWAY             ":You have been marked as being away"
        {Echo frmStatus, sTrail}
    end else if (sCommand = '311') then begin //RPL_WHOISUSER           "<nick> <user> <host> * :<real name>"
    end else if (sCommand = '312') then begin //RPL_WHOISSERVER         "<nick> <server> :<server info>"
    end else if (sCommand = '313') then begin //RPL_WHOISOPERATOR       "<nick> :is an IRC operator"
    end else if (sCommand = '314') then begin //RPL_WHOWASUSER          "<nick> <user> <host> * :<real name>"
    end else if (sCommand = '315') then begin //RPL_ENDOFWHO            "<name> :End of /WHO list"
    end else if (sCommand = '317') then begin //RPL_WHOISIDLE           "<nick> <integer> :seconds idle"
    end else if (sCommand = '318') then begin //RPL_ENDOFWHOIS          "<nick> :End of /WHOIS list"
    end else if (sCommand = '319') then begin //RPL_WHOISCHANNELS       "<nick> :{[@|+]<channel><space>}"
    end else if (sCommand = '321') then begin //RPL_LISTSTART           "Channel :Users Name"
    end else if (sCommand = '322') then begin //RPL_LIST                "<channel> <# visible> :<topic>"
    end else if (sCommand = '323') then begin //RPL_LISTEND             ":End of /LIST"
    end else if (sCommand = '324') then begin //RPL_CHANNELMODEIS       "<channel> <mode> <mode params>"
    end else if (sCommand = '328') then begin //RPL_URLDATA?            "<channel> :<url>
    end else if (sCommand = '331') then begin //RPL_NOTOPIC             "<channel> :No topic is set"
    end else if (sCommand = '332') then begin //RPL_TOPIC               "<channel> :<topic>"
        {I = IndexOfChannel(sMiddle(1))
        If I > -1 Then
            Forms(I).Topic = StripMIRC(sTrail)
            Echo Forms(I), "*** Topic for " & sMiddle(1) & ": '" & sTrail & "'"
        End If}
    end else if (sCommand = '333') then begin //RPL_TOPICSETBY?
        {I = IndexOfChannel(sMiddle(1))
        If I > -1 Then Echo Forms(I), "*** Topic set by " & sMiddle(2) & " on " & IRC_Time(CLng(sMiddle(3)))}
    end else if (sCommand = '341') then begin //RPL_INVITING            "<channel> <nick>"
    end else if (sCommand = '342') then begin //RPL_SUMMONING           "<user> :Summoning user to IRC"
    end else if (sCommand = '351') then begin //RPL_VERSION             "<version>.<debuglevel> <server> :<comments>"
    end else if (sCommand = '352') then begin //RPL_WHOREPLY            "<channel> <user> <host> <server> <nick> <H|G>[*][@|+] :<hopcount> <real name>"}
    end else if (sCommand = '353') then begin //RPL_NAMREPLY            "<channel> :[[@|+]<nick> [[@|+]<nick> [...]]]"
        j := ChannelManager.IndexOf( slMiddle[2] );
        if (j = -1) then begin j := ChannelManager.IndexOf( slMiddle[1] ); end;
        if (j = -1) Then begin j := ChannelManager.IndexOf( slMiddle[0] ); end;

        if (j > -1) then begin
            ChannelManager.Items[j].ListFill( sTrail );
        end else begin
            Self.WriteLn('Names (' + slMiddle[2] + '): ' + sTrail);
        end;
    end else if (sCommand = '364') then begin //RPL_LINKS               "<mask> <server> :<hopcount> <server info>"
    end else if (sCommand = '365') then begin //RPL_ENDOFLINKS          "<mask> :End of /LINKS list"}
    end else if (sCommand = '366') then begin //RPL_ENDOFNAMES          "<channel> :End of /NAMES list"
        {I = IndexOfChannel(sMiddle(1))
        If I > -1 Then Forms(I).FilledNames = True
        Echo frmStatus, "" & sTrail & " (" & sMiddle(1) & ")"}
    end else if (sCommand = '367') then begin //RPL_BANLIST             "<channel> <banid>"
    end else if (sCommand = '368') then begin //RPL_ENDOFBANLIST        "<channel> :End of channel ban list"
    end else if (sCommand = '369') then begin //RPL_ENDOFWHOWAS         "<nick> :End of WHOWAS"
    end else if (sCommand = '371') then begin //RPL_INFO                ":<string>"
    end else if (sCommand = '372') then begin //RPL_MOTD                ":- <text>"
        {Echo frmStatus, sTrail}
    end else if (sCommand = '374') then begin //RPL_ENDOFINFO           ":End of /INFO list"
    end else if (sCommand = '375') then begin //RPL_MOTDSTART           ":- <server> Message of the day - "
        {Echo frmStatus, sTrail}
    end else if (sCommand = '376') then begin //RPL_ENDOFMOTD           ":End of /MOTD command"
        {Echo frmStatus, sTrail}
    end else if (sCommand = '381') then begin //RPL_YOUREOPER           ":You are now an IRC operator"
    end else if (sCommand = '382') then begin //RPL_REHASHING           "<config file> :Rehashing"
    end else if (sCommand = '391') then begin //RPL_TIME                "<server> :<string showing server's local time>"
    end else if (sCommand = '392') then begin //RPL_USERSSTART          ":UserID Terminal Host"
    end else if (sCommand = '393') then begin //RPL_USERS               ":%-8s %-9s %-8s"
    end else if (sCommand = '394') then begin //RPL_ENDOFUSERS          ":End of users"
    end else if (sCommand = '395') then begin //RPL_NOUSERS             ":Nobody logged in"
    end else if (sCommand = '401') then begin //ERR_NOSUCHNICK          "<nickname> :No such nick/channel"
    end else if (sCommand = '402') then begin //ERR_NOSUCHSERVER        "<server name> :No such server"
    end else if (sCommand = '403') then begin //ERR_NOSUCHCHANNEL       "<channel name> :No such channel"
    end else if (sCommand = '404') then begin //ERR_CANNOTSENDTOCHAN    "<channel name> :Cannot send to channel"
    end else if (sCommand = '405') then begin //ERR_TOOMANYCHANNELS     "<channel name> :You have joined too many channels"
    end else if (sCommand = '406') then begin //ERR_WASNOSUCHNICK       "<nickname> :There was no such nickname"
    end else if (sCommand = '407') then begin //ERR_TOOMANYTARGETS      "<target> :Duplicate recipients. No message delivered"
    end else if (sCommand = '409') then begin //ERR_NOORIGIN            ":No origin specified"
    end else if (sCommand = '411') then begin //ERR_NORECIPIENT         ":No recipient given (<command>)"
    end else if (sCommand = '412') then begin //ERR_NOTEXTTOSEND        ":No text to send"
    end else if (sCommand = '413') then begin //ERR_NOTOPLEVEL          "<mask> :No toplevel domain specified"
    end else if (sCommand = '414') then begin //ERR_WILDTOPLEVEL        "<mask> :Wildcard in toplevel domain"
    end else if (sCommand = '421') then begin //ERR_UNKNOWNCOMMAND      "<command> :Unknown command"
    end else if (sCommand = '422') then begin //ERR_NOMOTD              ":MOTD File is missing"
    end else if (sCommand = '423') then begin //ERR_NOADMININFO         "<server> :No administrative info available"
    end else if (sCommand = '424') then begin //ERR_FILEERROR           ":File error doing <file op> on <file>"
    end else if (sCommand = '431') then begin //ERR_NONICKNAMEGIVEN     ":No nickname given"
    end else if (sCommand = '432') then begin //ERR_ERRONEUSNICKNAME    "<nick> :Erroneus nickname"
    end else if (sCommand = '433') then begin //ERR_NICKNAMEINUSE       "<nick> :Nickname is already in use"
        {'Options.Item("Nickname") = Options.Item("Nickname") & "_"
        'Socket.TCP.wsSend "NICK " & Options.Item("Nickname") & vbCrLf

        Echo frmStatus, HexColor(Color.IN) & "*** " & sTrail
        frmStatus.txtInput.SelText = "/nick "}
    end else if (sCommand = '436') then begin //ERR_NICKCOLLISION       "<nick> :Nickname collision KILL"
    end else if (sCommand = '441') then begin //ERR_USERNOTINCHANNEL    "<nick> <channel> :They aren't on that channel"
    end else if (sCommand = '442') then begin //ERR_NOTONCHANNEL        "<channel> :You're not on that channel"
    end else if (sCommand = '443') then begin //ERR_USERONCHANNEL       "<user> <channel> :is already on channel"
    end else if (sCommand = '444') then begin //ERR_NOLOGIN             "<user> :User not logged in"
    end else if (sCommand = '445') then begin //ERR_SUMMONDISABLED      ":SUMMON has been disabled"
    end else if (sCommand = '446') then begin //ERR_USERSDISABLED       ":USERS has been disabled"
    end else if (sCommand = '451') then begin //ERR_NOTREGISTERED       ":You have not registered"
    end else if (sCommand = '461') then begin //ERR_NEEDMOREPARAMS      "<command> :Not enough parameters"
    end else if (sCommand = '462') then begin //ERR_ALREADYREGISTRED    ":You may not reregister"
    end else if (sCommand = '463') then begin //ERR_NOPERMFORHOST       ":Your host isn't among the privileged"
    end else if (sCommand = '464') then begin //ERR_PASSWDMISMATCH      ":Password incorrect"
    end else if (sCommand = '465') then begin //ERR_YOUREBANNEDCREEP    ":You are banned from this server"
    end else if (sCommand = '467') then begin //ERR_KEYSET              "<channel> :Channel key already set"
    end else if (sCommand = '471') then begin //ERR_CHANNELISFULL       "<channel> :Cannot join channel (+l)"
    end else if (sCommand = '472') then begin //ERR_UNKNOWNMODE         "<char> :is unknown mode char to me"
    end else if (sCommand = '473') then begin //ERR_INVITEONLYCHAN      "<channel> :Cannot join channel (+i)"
    end else if (sCommand = '474') then begin //ERR_BANNEDFROMCHAN      "<channel> :Cannot join channel (+b)"
    end else if (sCommand = '475') then begin //ERR_BADCHANNELKEY       "<channel> :Cannot join channel (+k)"
    end else if (sCommand = '481') then begin //ERR_NOPRIVILEGES        ":Permission Denied- You're not an IRC operator"
    end else if (sCommand = '482') then begin //ERR_CHANOPRIVSNEEDED    "<channel> :You're not channel operator"
    end else if (sCommand = '483') then begin //ERR_CANTKILLSERVER      ":You cant kill a server!"
    end else if (sCommand = '491') then begin //ERR_NOOPERHOST          ":No O-lines for your host"
    end else if (sCommand = '501') then begin //ERR_UMODEUNKNOWNFLAG    ":Unknown MODE flag"
    end else if (sCommand = '502') then begin //ERR_USERSDONTMATCH      ":Cant change mode for other users"
    end else if (sCommand = '512') then begin //                        ":Authorization required to use this nickname"
    end else begin
        {}
    end;

    Self.WriteLn( '[' + sCommand + '] ' + sMid + ' - ' + sTrail);

end;

procedure TfrmServer.ProcessString(sPrefix, sCommand, sMid, sTrail: String);
var
    x, j: Integer;
    slMiddle: TStringList;
begin

    slMiddle := TStrIngList.Create();

    slMiddle.Delimiter := ' ';
    slMiddle.StrictDelimiter := True;
    slMiddle.DelimitedText := sMid;

    sCommand := uppercase(sCommand);

    if (sCommand = 'INVITE') then begin
        {
        If LCase$(sMiddle(0)) = LCase$(Options.Item("Nickname")) Then
            Echo frmMDI.ActiveForm, HexColor(Color.AC) & " -> " & FHGN$(sPrefix) & " has invited you to join " & sTrail
            If Options.Item("Auto.JoinOnInvite") Then
                Socket.TCP.wsSend "JOIN " & sTrail & vbCrLf
            End If
        End If
        }
    end else if (sCommand = 'JOIN') then begin
        if (lowercase(Self.FromHostGetNick(sPrefix)) = OptionManager.Nickname) then begin
            j := ChannelManager.Join( sTrail );

            { our channel needs to know which socket to use to send data }
            //j := ChannelManager.IndexOf( sTrail );
            if (j > -1) then begin
                ChannelManager.Items[j].tnParentSocket := Self.tnSocket;
                ChannelManager.Items[j].OptionManager := OptionManager;
            end;

        end else begin
            j := ChannelManager.IndexOf( sTrail );
            if (j > -1) then begin
                ChannelManager.Items[j].ListAdd( Self.FromHostGetNick(sPrefix) );
                ChannelManager.Items[j].WriteLn('[ *** ] ' + Self.FromHostGetNick(sPrefix) + ' has joined ' + sTrail);
            end;
        end;

    end else if (sCommand = 'KICK') then begin
        {
        J = IndexOfChannel(sMiddle(0))
        If LCase$(sMiddle(1)) = LCase$(Options.Item("Nickname")) Then
            Forms(J).FilledNames = False
            Forms(J)!lstUsers.Clear

            Echo Forms(J), HexColor(Color.KI) & "*** " & FHGN$(sPrefix) & " kicked You from " & sMiddle(0) & " (" & sTrail & HexColor(Color.KI) & ")"

            If Options.Item("Auto.JoinOnKick") Then
                Echo Forms(J), HexColor(Color.IN) & "*** Rejoining Channel..."
                Socket.TCP.wsSend "JOIN " & sMiddle(0) & vbCrLf
            End If
        Else
            If J > -1 Then
                NickList_Remove Forms(J), sMiddle(1)
                Echo Forms(J), HexColor(Color.KI) & "*** " & FHGN$(sPrefix) & " kicked " & sMiddle(1) & " (" & sTrail & HexColor(Color.KI) & ")"
            End If
        End If
        }
    end else if (sCommand = 'MODE') then begin
    {
        If LCase$(sPrefix) = LCase$(Options.Item("Nickname")) Then
            Echo frmStatus, HexColor(Color.MO) & "[L] " & Options.Item("Nickname") & ": Your usermode is now [" & sTrail & "]"
        Else
            Dim C As Long
            Dim bGiveMode As Boolean
            Dim sArg As String
            Dim sMode As String

            J = IndexOfChannel(sMiddle(0))
            If J > -1 Then
                Echo Forms(J), HexColor(Color.MO) & "*** " & FHGN$(sPrefix) & " sets mode: " & Mid$(sMid, InStr(sMid, " ") + 1)
                Do
                    I = I + 1
                    If Mid$(sMiddle(1), I, 1) = "+" Then
                        bGiveMode = True
                    ElseIf Mid$(sMiddle(1), I, 1) = "-" Then
                        bGiveMode = False
                    Else
                        C = C + 1
                        If C + 1 <= UBound(sMiddle()) Then
                            sArg = sMiddle(C + 1)
                        Else
                            sArg = ""
                        End If
                        sMode = Mid$(sMiddle(1), I, 1)
                        Channel_HandleMode Forms(J), bGiveMode, sMode, sArg
                    End If
                Loop Until I = Len(sMiddle(1))
            End If
        End If
    }

    end else if (sCommand = 'NICK') then begin
    {
        If LCase$(FHGN$(sPrefix)) = LCase$(Options.Item("Nickname")) Then
            Options.Item("Nickname") = sTrail
            Echo frmStatus, HexColor(Color.NI) & "*** You are now known as " & Options.Item("Nickname")
            NickList_ChangeNick FHGN$(sPrefix), sTrail, HexColor(Color.NI) & "*** You are now known as " & Options.Item("Nickname")
        Else
            NickList_ChangeNick FHGN$(sPrefix), sTrail, HexColor(Color.NI) & "*** " & FHGN$(sPrefix) & " is now known as " & sTrail
        End If
    }
    end else if (sCommand = 'PART') then begin

        //ShowMessage( concat(':', sPrefix, '::', sCommand, '::', sMid, '::', sTrail, ':') );

        if (lowercase(Self.FromHostGetNick(sPrefix)) = OptionManager.Nickname) then begin
            ChannelManager.Part( sMid );
        end else begin
            j := ChannelManager.IndexOf( sMid );
            if (j > -1) then begin
                ChannelManager.Items[j].ListRemove( Self.FromHostGetNick(sPrefix) );
                ChannelManager.Items[j].WriteLn('[ *** ] ' + Self.FromHostGetNick(sPrefix) + ' has left ' + sMid + ' (' + sTrail + ')');
            end;
        end;

    end else if (sCommand = 'PING') then begin
    {
        Socket.TCP.wsSend "PONG :" & sTrail & vbCrLf
        If Options.Item("Show.Pong") Then Echo frmStatus, HexColor(Color.IN) & "PING? PONG!"
    }
    end else if (sCommand = 'PRIVMSG') then begin

        if ((LeftStr(sTrail, 1) = #1) and (RightStr(sTrail, 1) = #1)) then begin
            //HandleCTCP sPrefix, sCommand, sMid, sTrail
        end else begin
            if (lowercase(slMiddle[0]) = lowercase(OptionManager.Nickname)) then begin
                { J = IndexOfQuery(FHGN(sPrefix))
                If J > -1 Then
                    Echo Forms(J), HexColor(Color.PU) & "<" & FHGN(sPrefix) & HexColor(Color.PU) & "> " & sTrail
                ElseIf J = -1 Then
                    If Options.Item("DedicateQueries") Then
                        Query_Create FHGN(sPrefix)
                        J = IndexOfQuery(FHGN(sPrefix))
                        Echo Forms(J), HexColor(Color.PU) & "<" & FHGN(sPrefix) & HexColor(Color.PU) & "> " & sTrail
                    Else
                        Echo frmMDI.ActiveForm, HexColor(Color.PR) & " -> [" & FHGN(sPrefix) & ": " & sTrail & HexColor(Color.PR) & "]"
                    End If
                End If }
            end else begin
                j := ChannelManager.IndexOf( slMiddle[0] );
                if (j > -1) then begin
                    ChannelManager.Items[j].WriteLn('<' + Self.FromHostGetNick(sPrefix) + '> ' + sTrail);
                end;
            end;
        end;
    end else if (sCommand = 'QUIT') then begin
    {
        NickList_RemoveAll FHGN$(sPrefix), HexColor(Color.QU) & "*** " & FHGN$(sPrefix) & " (" & FHGU$(sPrefix) & "@" & FHGA$(sPrefix) & ") has quit chatting (" & sTrail & HexColor(Color.QU) & ")"
    }
    end else if (sCommand = 'TOPIC') then begin
    {
        J = IndexOfChannel(sMiddle(0))
        If J > -1 Then
            Forms(J).Topic = StripMIRC(sTrail)
            If LCase$(FHGN$(sPrefix)) = LCase$(Options.Item("Nickname")) Then
                Echo Forms(J), "*** Topic is now: " & sTrail
            Else
                Echo Forms(J), "*** " & FHGN$(sPrefix) & " changed the topic: " & sTrail
            End If
        End If
    }
    end else begin
            Self.WriteLn( sPrefix + '[' + sCommand + '(' + sMid + ')] ' + sTrail );
    end;

end;


{ TfrmServer }
procedure TfrmServer.Buffer( sTxt: String );
var
    x: Integer;
    slParser: TStringList;
begin

    slParser:= TStringList.Create();

    p_Buffer := p_Buffer + sTxt;

    if (RightStr(p_Buffer, 1) = #10) then begin
        if (RightStr(p_Buffer, 1) = #13) then begin
            p_Buffer := LeftStr(p_Buffer, Length(p_Buffer) - 1);
        end;

        slParser.Text := p_Buffer;
        p_Buffer := '';

        for x := 0 to (slParser.Count - 1) do begin
            Self.ProcessLine( slParser[x] );
        end;

        Application.ProcessMessages();
    end;
end;

procedure TfrmServer.tnSocketConnect(aSocket: TLSocket);
begin
    Self.WriteLn((#10+#13) + '>>>  Connected ' + tnSocket.Host + (#13#10));

    tnSocket.SendMessage( 'PASS none' + (#13#10) );
    tnSocket.SendMessage( 'NICK ' + OptionManager.Nickname + (#13#10) );
    tnSocket.SendMessage( 'USER ' + OptionManager.Email + ' - - :pIRCc (Development)' + (#13#10) );
    tnSocket.SendMessage( 'USERHOST ' + OptionManager.Email + ' - - :pIRCc (Development)' + (#13#10) );

end;

procedure TfrmServer.FormCreate(Sender: TObject);
begin

    OptionManager := TOptionManager.Create();
    ChannelManager := TChannelManager.Create();

    OptionManager.Nickname := 'l0wrd__';
    OptionManager.Email := 'subjugator@gmail.com';

end;

procedure TfrmServer.tnSocketDisconnect(aSocket: TLSocket);
begin
    Self.WriteLn((#10+#13) + '>>>  Disconnected ' + tnSocket.Host + (#10+#13));
end;

procedure TfrmServer.tnSocketError(const msg: string; aSocket: TLSocket);
begin
    Self.WriteLn((#10+#13) + '>>>  Error: ' + msg + (#13#10));
end;

procedure TfrmServer.tnSocketReceive(aSocket: TLSocket);
var
    sTxt: String;
begin

    if (tnSocket.GetMessage( sTxt ) > 0) then begin
        Self.Buffer( sTxt );
    end;
end;


procedure TfrmServer.txtInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
    slCmd: TStringList;
begin

    if (key = 13) then begin
        if (LeftStr(txtInput.Text, 1) = '/') then begin
            { process command }

            slCmd := TStringList.Create();

            slCmd.StrictDelimiter := True;
            slCmd.Delimiter := ' ';
            slCmd.DelimitedText := txtInput.Text;

            if (lowercase(slCmd[0]) = '/connect') then begin
                if (slCmd.Count = 3) then begin
                    tnSocket.Connect( slCmd[1], StrToInt(slCmd[2]) );
                end;
            end;

            txtInput.Text := '';

        end else begin
            Self.WriteLn( txtInput.Text );
            tnSocket.SendMessage( txtInput.Text + #13#10 );
            txtInput.Text := '';
        end;
    end;
end;


initialization
  {$I unit_frmServer.lrs}

end.

