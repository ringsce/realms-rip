unit unit_TOptionManager;

{$mode objfpc}{$H+}{$M+}

interface

uses
    Classes, SysUtils;

type

    TOptionManager = Class(TObject)
        {}
        private
            {
            .Add "Nickname", ""
            .Add "Email", ""
            .Add "Password", ""
            .Add "LastServer", ""
            .Add "ModeFixes", ".@%+"

            .Add "Show.Pong", False
            .Add "Show.JoinPart", True

            .Add "DedicateQueries", False

            .Add "Auto.SpellCheck", True
            .Add "Auto.JoinOnInvite", False
            .Add "Auto.JoinOnKick", True
            .Add "Auto.Reconnect", False

            .Add "Ident.Port", 113
            .Add "Ident.UserID", "XIRC"
            .Add "Ident.System", "UNIX"
            .Add "Ident.Enabled", True

            .Add "OnConnect.Enabled", False
            .Add "OnConnect.Command", ""

            .Add "NickServ.Mode", "Other"
            }

            p_Nickname: String;
            p_Email: String;
            p_Password: String;

        public

            property Nickname : String Read p_Nickname Write p_Nickname;
            property Email : String Read p_Email Write p_Email;
            property Password : String Read p_Password Write p_Password;
    end;


implementation

end.

