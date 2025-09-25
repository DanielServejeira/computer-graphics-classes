unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonCleanScreen: TButton;
    Image1: TImage;
    LabelY: TLabel;
    LabelX: TLabel;
    MainMenu1: TMainMenu;
    MenuDrawLine: TMenuItem;
    MenuDrawPixel: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuGeneralEquation: TMenuItem;
    Bresenham: TMenuItem;
    MenuParametricEquation: TMenuItem;
    XLabel: TLabel;
    YLabel: TLabel;
    procedure BresenhamClick(Sender: TObject);
    procedure ButtonCleanScreenClick(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuDrawPixelClick(Sender: TObject);
    procedure MenuGeneralEquationClick(Sender: TObject);
    procedure MenuParametricEquationClick(Sender: TObject);
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

procedure TForm1.MenuGeneralEquationClick(Sender: TObject);
begin
  operation := 2; //draw lines on the image using general equation
end;

procedure TForm1.MenuParametricEquationClick(Sender: TObject);
begin
  operation := 3; //draw lines on the image using parametric equation
end;

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if(operation = 1) then
  begin
    draw := true;
    Image1.Canvas.Pixels[X,Y] := clred; //draw right on click
  end
  else
  begin
    x1 := X; //save initial position
    y1 := Y;
  end;
end;

procedure TForm1.ButtonCleanScreenClick(Sender: TObject);
var
  xi, yi: Integer;
begin
  for yi := 0 to Image1.Canvas.Height do
    for xi := 0 to Image1.Canvas.Width do
      Image1.Canvas.Pixels[xi,yi] := clblack; //clean screen with black
end;

procedure TForm1.BresenhamClick(Sender: TObject);
begin
  operation := 4; //draw lines on the image using bresenham
end;

procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  LabelX.Caption := IntToStr(X); //show mouse coordinates
  LabelY.Caption := IntToStr(Y);

  if(operation = 1) and (draw = true) then Image1.Canvas.Pixels[X,Y] := clred; //draw pixels
end;

procedure TForm1.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  inc, incx, incy: Integer;
  xi, yi, dx, dy, d, dE, dNE, dR, dNR: Integer;
  m, t: Double;
begin
  Image1.Canvas.Pen.Color := clRed;

  if (operation = 1) then draw := false;

  if (operation = 2) then
  begin
    x2 := X;
    y2 := Y;

    if (x1 = x2) then
    begin
      for yi := y1 to y2 do
        Image1.Canvas.Pixels[x1, yi] := clRed;
      Exit;
    end;

    if (y1 = y2) then
    begin
      for xi := x1 to x2 do
        Image1.Canvas.Pixels[xi, y1] := clRed;
      Exit;
    end;

    m := (y2-y1)/(x2-x1);

    if (Abs(x2-x1) >= Abs(y2-y1)) then //delta module
    begin
      if (x1 < x2) then inc := 1 else inc := -1;

      xi := x1;
      while (xi <> x2) do
      begin
        yi := Round(m*(xi-x1) + y1);
        Image1.Canvas.Pixels[xi, yi] := clRed;
        xi := xi + inc;
      end;
    end
    else
    begin
      if (y1 < y2) then inc := 1 else inc := -1;

      yi := y1;
      while (yi <> y2) do
      begin
        xi := Round((yi-y1)/m) + x1;
        Image1.Canvas.Pixels[xi, yi] := clRed;
        yi := yi + inc;
      end;
    end;
  end;

  if (operation = 3) then
  begin
    x2 := X;
    y2 := Y;

    t := 0;
    while t <= 1.0 do
    begin
      xi := Round(x1 + (x2-x1)*t);
      yi := Round(y1 + (y2-y1)*t);

      Image1.Canvas.Pixels[xi,yi] := clRed;

      t := t + 0.001;
    end;
  end;

  if (operation = 4) then
  begin
    x2 := X;
    y2 := Y;

    xi := x1;
    yi := y1;

    if (x1 < x2) then incx := 1 else incx := -1;
    if (y1 < y2) then incy := 1 else incy := -1;

    dx := Abs(x2-x1);
    dy := Abs(y2-y1);

    Image1.Canvas.Pixels[xi, yi] := clRed;

    if (dx > dy) then
    begin
      d := 2*dy - dx;
      dE := 2*dy;
      dNE := 2*(dy-dx);

      while xi <> x2 do
      begin
        if d < 0 then d := d + dE
        else
        begin
          d := d + dNE;
          yi := yi + incy;
        end;
        
        xi := xi + incx;
        Image1.Canvas.Pixels[xi,yi] := clRed;
      end;
    end

    else
    begin
      d := 2 * dx - dy;
      dE := 2 * dx;
      dNE := 2 * (dx - dy);

      while yi <> y2 do
      begin
        if d < 0 then
        begin
          d := d + dE
        end
        else
        begin
          d := d + dNE;
          xi := xi + incx;
        end;

        yi := yi + incy;
        Image1.Canvas.Pixels[xi,yi] := clRed;
      end;
    end;
  end;
end;


end.

