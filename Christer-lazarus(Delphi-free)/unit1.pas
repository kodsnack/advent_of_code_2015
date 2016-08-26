unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    Button1: TButton;
    edResult: TEdit;
    GroupBox1: TGroupBox;
    OpenDialog1: TOpenDialog;
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  exit;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  myFile:TextFile;
  s:string;
  c:char;
  l,r:integer;
begin
  l := 0;
  r := 0;
  if openDialog1.execute then
  begin
    AssignFile(myFile,openDialog1.fileName);
    try
      reset(myFile);
      while not eof(myFile) do
      begin
           readln(myFile,s);
           for c in s do
           begin
               if c = '(' then
                  l := l+1
               else
                   r := r+1;
           end;
      end;
      edResult.text := IntToStr(l-r);
    finally
      closeFile(myFile);
    end;

  end;
  GroupBox1.Caption:='Day1';
end;

end.

