unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  StdCtrls, Unit2;

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
    TransformationPanel: TMenuItem;
    MenuItem2: TMenuItem;
    MenuCircleBresenham: TMenuItem;
    MenuCircleIterativeRotation: TMenuItem;
    MenuLineGeneralEquation: TMenuItem;
    MenuLineBresenham: TMenuItem;
    MenuCircumference: TMenuItem;
    MenuCircleParametricEquation: TMenuItem;
    MenuCircleStandartEquation: TMenuItem;
    MenuLineParametricEquation: TMenuItem;
    XLabel: TLabel;
    YLabel: TLabel;
    procedure MenuCircleBresenhamClick(Sender: TObject);
    procedure MenuCircleIterativeRotationClick(Sender: TObject);
    procedure MenuLineBresenhamClick(Sender: TObject);
    procedure ButtonCleanScreenClick(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuDrawPixelClick(Sender: TObject);
    procedure MenuLineGeneralEquationClick(Sender: TObject);
    procedure MenuCircleParametricEquationClick(Sender: TObject);
    procedure MenuCircleStandartEquationClick(Sender: TObject);
    procedure MenuLineParametricEquationClick(Sender: TObject);
    procedure TransformationPanelClick(Sender: TObject);
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

function PlotCirclePoints(Canvas: TCanvas; cx, cy, x, y: Integer; Cor: TColor):
begin
  Canvas.Pixels[cx + x, cy + y] := Cor;
  Canvas.Pixels[cx - x, cy + y] := Cor;
  Canvas.Pixels[cx + x, cy - y] := Cor;
  Canvas.Pixels[cx - x, cy - y] := Cor;
  Canvas.Pixels[cx + y, cy + x] := Cor;
  Canvas.Pixels[cx - y, cy + x] := Cor;
  Canvas.Pixels[cx + y, cy - x] := Cor;
  Canvas.Pixels[cx - y, cy - x] := Cor;
end;

procedure TForm1.MenuDrawPixelClick(Sender: TObject);
begin
  operation := 1; //draw pixels on the image
end;

procedure TForm1.MenuLineGeneralEquationClick(Sender: TObject);
begin
  operation := 2; //draw lines on the image using general equation
end;

procedure TForm1.MenuCircleParametricEquationClick(Sender: TObject);
begin
  operation := 6; //draw circumferences on the image using parametric equation of a circle
end;

procedure TForm1.MenuCircleStandartEquationClick(Sender: TObject);
begin
  operation := 5; //draw circumferences on the image using standart equation of a circle
end;

procedure TForm1.MenuLineParametricEquationClick(Sender: TObject);
begin
  operation := 3; //draw lines on the image using parametric equation
end;

procedure TForm1.TransformationPanelClick(Sender: TObject);
begin
  Form2 := TForm2.Create(Self);
  Form2.Show;
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

procedure TForm1.MenuLineBresenhamClick(Sender: TObject);
begin
  operation := 4; //draw lines on the image using Bresenham
end;

procedure TForm1.MenuCircleIterativeRotationClick(Sender: TObject);
begin
  operation := 7; //draw circles on the image using iterative rotation
end;

procedure TForm1.MenuCircleBresenhamClick(Sender: TObject);
begin
  operation := 8; //draw circles on the image using MenuLineBresenham
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
  xi, yi, xc, yc, dx, dy, d, dE, dNE, i: Integer;
  m, t, r, h, temp, a, sen1, cos1, xiD, yiD: Double;
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

  if (operation = 4) then //bresenham
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

  if (operation = 5) then
  begin
    xc := x1;
    yc := y1;
    r := Sqrt(Sqr(X-xc) + Sqr(Y-yc));

    for dx := -Round(r) to Round(r) do
    begin
      temp := r*r - dx*dx;
      if temp < 0 then temp := 0;  // sqrt only for positive numbers
      dy := Round(Sqrt(temp));

      if (xc+dx >= 0) and (xc + dx < Image1.Width) then
      begin
        if (yc+dy >= 0) and (yc+dy < Image1.Height) then
          Image1.Canvas.Pixels[xc+dx, yc+dy] := clRed;
        if (yc-dy >= 0) and (yc-dy < Image1.Height) then
          Image1.Canvas.Pixels[xc+dx, yc-dy] := clRed;
      end;
    end;
  end;

  if(operation = 6) then
  begin
    xc := x1;
    yc := y1;
    r := Sqrt(Sqr(X-xc) + Sqr(Y-yc));

    a := 0.0;
    while a <= 2 * Pi do
    begin
      xi := Round(r * cos(a));
      yi := Round(r * sin(a));
      Image1.Canvas.Pixels[xc+xi, yc+yi] := clRed;

      a := a + 0.01; // rad incrementation
    end;
  end;

  if(operation = 7) then
  begin
    r := Sqrt(Sqr(X-xc) + Sqr(Y-yc));

    xiD := r;
    yiD := 0;
    h := 1 - r;

    sen1 := sin(57.2958);
    cos1 := cos(57.2958);

    for i := 1 to 360 do
    begin
      temp := xiD * cos1 - yiD * sen1;
      yiD := xiD * sen1 + yiD * cos1;
      xiD := temp;
      xi := Round(xiD);
      yi := Round(yiD);
      Image1.Canvas.Pixels[xc+xi,yc+yi] := clRed;
    end;
  end;

  if(operation = 8) then
  begin
    xc := x1;
    yc := y1;

    xi := 0;
    yi := Round(Sqrt(Sqr(X - xc) + Sqr(Y - yc)));
    h := 1 - r;

    PlotCirclePoints(Image1.Canvas, xc, yc, xi, yi, clRed);

    while xi < yi do
    begin
      if h < 0 then
        h := h + 2*xi + 3
      else
      begin
        h := h + 2*(xi-yi) + 5;
        yi := yi - 1;
      end;

      xi := xi + 1;
      PlotCirclePoints(Image1.Canvas, xc, yc, xi, yi, clRed);
    end;
  end;
end;


end.

