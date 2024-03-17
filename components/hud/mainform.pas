unit mainform;

{$MODE DELPHI}{$H+}

interface

uses Forms, StdCtrls;

type
  TMyForm = class(TForm)
  public
    MyButton: TButton;
    procedure ButtonClick(ASender: TObject);
    constructor Create(AOwner: TComponent); override;
  end;

implementation

procedure TMyForm.ButtonClick(ASender:TObject);
begin
  Close;
end;

constructor TMyForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //Hint: FormCreate() is called BEFORE Create() !
  //so You can also put this code into FormCreate()
  //(This is not the case when creating components ..)

  Position := poScreenCenter;
  Height := 400;
  Width := 400;

  VertScrollBar.Visible := False;
  HorzScrollBar.Visible := False;

  MyButton := TButton.Create(Self);
  with MyButton do
  begin
    Height := 30;
    Left := 100;
    Top := 100;
    Width := 100;
    Caption := 'Close';
    OnClick := ButtonClick;
    Parent := Self;
  end;

  // Add other component creation and property setting code here
  with TButton.Create(Form1) do begin
  // 1. creating a button sets the default size
  // 2. change position. No side effects, because Parent=nil
  SetBounds(10,10,Width,Height);
  // 3. change size depending on theme. Not yet, because Parent=nil
  AutoSize:=true;
  // 4. changing size because of AutoSize=true. Not yet, because Parent=nil
  Caption:='Ok';
  // 5. Set Parent. Now all the above takes place, but in a single action.
  Parent:=Form1;
end;

end.