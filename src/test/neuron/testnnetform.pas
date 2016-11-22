unit TestNNetForm;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ExtDlgs, ActnList, ComCtrls, uBaseBitmap, NNWork;

type

  { TfrmNNetTest }

  TfrmNNetTest = class(TForm)
    ObjActions: TActionList;
    aLoad: TAction;
    MenuActions: TActionList;
    aIntencityFilter: TAction;
    Image1: TImage;
    ImageList1: TImageList;
    OpenPictureDialog1: TOpenPictureDialog;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure aLoadExecute(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    destructor Destroy; override;
    class procedure Execute;
  end;

var
  frmNNetTest: TfrmNNetTest;

implementation

{$R *.lfm}

{ TfrmNNetTest }

procedure TfrmNNetTest.aLoadExecute(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
end;

destructor TfrmNNetTest.Destroy;
begin
  if frmNNetTest = Self then
    frmNNetTest := nil;
  inherited Destroy;
end;

class procedure TfrmNNetTest.Execute;
begin
  if not Assigned(frmNNetTest) then
    Application.CreateForm(TfrmNNetTest, frmNNetTest);
  frmNNetTest.Show;
end;

end.


