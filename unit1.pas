unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    MainMenu1: TMainMenu;
    MenuDrawLine: TMenuItem;
    MenuDrawPixel: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    XLabel: TLabel;
    YLabel: TLabel;
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuDrawLineClick(Sender: TObject);
    procedure MenuDrawPixelClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  operation: Integer;
  draw: boolean;
  x1, x2, y1, y2: Integer;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MenuDrawPixelClick(Sender: TObject);
begin
  operation := 1; //draw pixels on the image
end;

procedure TForm1.MenuDrawLineClick(Sender: TObject);
begin
  operation := 2; //draw lines on the image
end;

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if(operation = 1) then draw := true;
  if(operation = 2) then
  begin
    Image1.Canvas.LineTo(X,Y);
    x1 := X;
    y1 := Y;
  end;
end;

procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if(operation = 1) and (draw = true) then Image1.Canvas.Pixels[X,Y] := clred;
end;

procedure TForm1.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  inc, m: Integer;
  xi, yi: Integer;
begin
  Image1.Canvas.Pen.Color := clRed;

  if (operation = 1) then draw := false;
  if (operation = 2) then
  begin
    x2 := X;
    y2 := Y;

    if (x2 = x1) then
    begin
      for yi := y1 to y2 do
        Image1.Canvas.Pixels[x1, yi] := clRed;
      Exit;
    end;

    m := Round((y2-y1)/(x2-x1));

    if (x1 < x2) then inc := 1
    else inc := -1;

    xi := x1;
    while (xi <> x2) do
    begin
      yi := Round(m * (xi - x1) + y1);
      Image1.Canvas.Pixels[xi, yi] := clRed;
      xi := xi + inc;
    end;
  end;
end;


end.

