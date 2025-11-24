unit Unit3;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Menus, Math;

type

  TPoint4D = record
    X, Y, Z, W: Real;
  end;


  TMatrix4x4 = array[0..3, 0..3] of Real;


  TFace = record
    V1, V2, V3: Integer; // Indices of the vertices in the Vertices array
    FaceColor: TColor;
  end;


  TProjectedPoint = record
    X, Y: Integer;
    Z: Real;
  end;

  { TForm3 }

  TForm3 = class(TForm)
    ButtonRun: TButton;
    ButtonReset: TButton;
    Image1: TImage;
    procedure ButtonResetClick(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonRunClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

    Vertices: array of TPoint4D; // Vértices do objeto 3D gerado
    TransformedVertices: array of TPoint4D; // Vértices transformados
    Faces: array of TFace; // Faces do objeto 3D gerado
    ZBuffer: array of Real; // Z-Buffer array
    ImageWidth, ImageHeight: Integer;
    ProfilePoints: array of TPoint4D; // Pontos 2D do perfil desenhado (X, Y, 0, 1)
    DrawingProfile: Boolean;


    function Project(Point4D: TPoint4D): TProjectedPoint;
    procedure InitializeZBuffer;
    procedure DrawPixel(X, Y: Integer; Z: Real; AColor: TColor);
    procedure DrawLineSegment(X1, Y1, Z1, X2, Y2, Z2: Real; AColor: TColor);
    procedure DrawTriangle(const P1, P2, P3: TPoint4D; AColor: TColor);
    procedure FillTriangle(const P1, P2, P3: TPoint4D; AColor: TColor);
    procedure RedrawObject;
    procedure ApplyTransformation(const M: TMatrix4x4);
    function CreateTranslationMatrix(Tx, Ty, Tz: Real): TMatrix4x4;
    function CreateRotationMatrix(Axis: Char; AngleRad: Real): TMatrix4x4;
    function CreateScaleMatrix(Sx, Sy, Sz: Real): TMatrix4x4;


    procedure GenerateRotationalSweepObject(Steps: Integer);

  public

  end;

var
  Form3: TForm3;
  draw: boolean; // Variável global do template, mantida por compatibilidade

implementation

{$R *.lfm}


function IdentityMatrix: TMatrix4x4;
var i, j: Integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      if i = j then
        Result[i, j] := 1.0
      else
        Result[i, j] := 0.0;
end;

function MultiplyMatrix(const A, B: TMatrix4x4): TMatrix4x4;
var i, j, k: Integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
    begin
      Result[i, j] := 0;
      for k := 0 to 3 do
        Result[i, j] := Result[i, j] + A[i, k] * B[k, j];
    end;
end;

function MultiplyPointMatrix(const P: TPoint4D; const M: TMatrix4x4): TPoint4D;
begin
  Result.X := (P.X * M[0, 0]) + (P.Y * M[1, 0]) + (P.Z * M[2, 0]) + (P.W * M[3, 0]);
  Result.Y := (P.X * M[0, 1]) + (P.Y * M[1, 1]) + (P.Z * M[2, 1]) + (P.W * M[3, 1]);
  Result.Z := (P.X * M[0, 2]) + (P.Y * M[1, 2]) + (P.Z * M[2, 2]) + (P.W * M[3, 2]);
  Result.W := (P.X * M[0, 3]) + (P.Y * M[1, 3]) + (P.Z * M[2, 3]) + (P.W * M[3, 3]);
end;

function TForm3.CreateTranslationMatrix(Tx, Ty, Tz: Real): TMatrix4x4;
begin
  Result := IdentityMatrix;
  Result[3, 0] := Tx;
  Result[3, 1] := Ty;
  Result[3, 2] := Tz;
end;

function TForm3.CreateScaleMatrix(Sx, Sy, Sz: Real): TMatrix4x4;
begin
  Result := IdentityMatrix;
  Result[0, 0] := Sx;
  Result[1, 1] := Sy;
  Result[2, 2] := Sz;
end;

function TForm3.CreateRotationMatrix(Axis: Char; AngleRad: Real): TMatrix4x4;
var
  C, S: Real;
begin
  Result := IdentityMatrix;
  C := Cos(AngleRad);
  S := Sin(AngleRad);

  case UpCase(Axis) of
    'X':
      begin
        Result[1, 1] := C;
        Result[1, 2] := S;
        Result[2, 1] := -S;
        Result[2, 2] := C;
      end;
    'Y':
      begin
        Result[0, 0] := C;
        Result[0, 2] := -S;
        Result[2, 0] := S;
        Result[2, 2] := C;
      end;
    'Z':
      begin
        Result[0, 0] := C;
        Result[0, 1] := S;
        Result[1, 0] := -S;
        Result[1, 1] := C;
      end;
  end;
end;


function TForm3.Project(Point4D: TPoint4D): TProjectedPoint;
const
  ProjectionFactor = 0.5;
  OffsetX = 200;
  OffsetY = 200;
  ScaleFactor = 2.0;
var
  x2d, y2d: Real;
begin
  if Point4D.W <> 0 then
  begin
    Point4D.X := Point4D.X / Point4D.W;
    Point4D.Y := Point4D.Y / Point4D.W;
    Point4D.Z := Point4D.Z / Point4D.W;
  end;


  x2d := Point4D.X + (ProjectionFactor * Point4D.Z);
  y2d := Point4D.Y - (ProjectionFactor * Point4D.Z);


  Result.X := Round(x2d * ScaleFactor + OffsetX);
  Result.Y := Round(OffsetY - y2d * ScaleFactor);
  Result.Z := Point4D.Z;
end;

procedure TForm3.InitializeZBuffer;
var i: Integer;
begin
  ImageWidth := Image1.Width;
  ImageHeight := Image1.Height;
  SetLength(ZBuffer, ImageWidth * ImageHeight);

  for i := 0 to High(ZBuffer) do
    ZBuffer[i] := Infinity;

  Image1.Canvas.Brush.Color := clBlack;
  Image1.Canvas.FillRect(Image1.ClientRect);
end;

procedure TForm3.DrawPixel(X, Y: Integer; Z: Real; AColor: TColor);
var
  Index: Integer;
begin
  if (X >= 0) and (X < ImageWidth) and (Y >= 0) and (Y < ImageHeight) then
  begin
    Index := Y * ImageWidth + X;
    if Z < ZBuffer[Index] then
    begin
      ZBuffer[Index] := Z;
      Image1.Canvas.Pixels[X, Y] := AColor;
    end;
  end;
end;

procedure TForm3.DrawLineSegment(X1, Y1, Z1, X2, Y2, Z2: Real; AColor: TColor);
var
  dx, dy, dz, steps, x, y, z, xinc, yinc, zinc: Real;
  i: Integer;
begin
  dx := X2 - X1;
  dy := Y2 - Y1;
  dz := Z2 - Z1;

  steps := Abs(dx);
  if Abs(dy) > steps then steps := Abs(dy);
  if Abs(dz) > steps then steps := Abs(dz);

  if steps = 0 then
  begin
    DrawPixel(Round(X1), Round(Y1), Z1, AColor);
    Exit;
  end;

  xinc := dx / steps;
  yinc := dy / steps;
  zinc := dz / steps;

  x := X1;
  y := Y1;
  z := Z1;

  for i := 0 to Round(steps) do
  begin
    DrawPixel(Round(x), Round(y), z, AColor);
    x := x + xinc;
    y := y + yinc;
    z := z + zinc;
  end;
end;

procedure TForm3.DrawTriangle(const P1, P2, P3: TPoint4D; AColor: TColor);
var
  PP1, PP2, PP3: TProjectedPoint;
begin
  PP1 := Project(P1);
  PP2 := Project(P2);
  PP3 := Project(P3);

  DrawLineSegment(PP1.X, PP1.Y, PP1.Z, PP2.X, PP2.Y, PP2.Z, AColor);
  DrawLineSegment(PP2.X, PP2.Y, PP2.Z, PP3.X, PP3.Y, PP3.Z, AColor);
  DrawLineSegment(PP3.X, PP3.Y, PP3.Z, PP1.X, PP1.Y, PP1.Z, AColor);
end;

procedure TForm3.FillTriangle(const P1, P2, P3: TPoint4D; AColor: TColor);
type
  TIntersection = record
    x, z: Real;
    valid: Boolean;
  end;
var
  PP1, PP2, PP3: TProjectedPoint;
  minY, maxY, y: Integer;
  x1, x2, z1, z2: Real;
  scanX, scanZ: Real;
  startX, endX: Integer;

  procedure InterpolateLine(y: Integer; const PA, PB: TProjectedPoint; out x, z: Real);
  var
    t: Real;
  begin
    if PB.Y = PA.Y then
    begin
      x := PA.X;
      z := PA.Z;
    end
    else
    begin
      t := (y - PA.Y) / (PB.Y - PA.Y);
      x := PA.X + t * (PB.X - PA.X);
      z := PA.Z + t * (PB.Z - PA.Z);
    end;
  end;

  procedure ScanLine(y: Integer);
  var
    intersections: array[0..2] of TIntersection;
    count, i, j: Integer;
    temp: TIntersection;
  begin
    count := 0;


    if ((PP1.Y <= y) and (y < PP2.Y)) or ((PP2.Y <= y) and (y < PP1.Y)) then
    begin
      InterpolateLine(y, PP1, PP2, intersections[count].x, intersections[count].z);
      intersections[count].valid := True;
      Inc(count);
    end;


    if ((PP2.Y <= y) and (y < PP3.Y)) or ((PP3.Y <= y) and (y < PP2.Y)) then
    begin
      InterpolateLine(y, PP2, PP3, intersections[count].x, intersections[count].z);
      intersections[count].valid := True;
      Inc(count);
    end;


    if ((PP3.Y <= y) and (y < PP1.Y)) or ((PP1.Y <= y) and (y < PP3.Y)) then
    begin
      InterpolateLine(y, PP3, PP1, intersections[count].x, intersections[count].z);
      intersections[count].valid := True;
      Inc(count);
    end;


    for i := 0 to count - 2 do
      for j := i + 1 to count - 1 do
        if intersections[j].x < intersections[i].x then
        begin
          temp := intersections[i];
          intersections[i] := intersections[j];
          intersections[j] := temp;
        end;


    i := 0;
    while i < count - 1 do
    begin
      startX := Round(intersections[i].x);
      endX := Round(intersections[i + 1].x);
      z1 := intersections[i].z;
      z2 := intersections[i + 1].z;

      if endX > startX then
      begin
        for j := startX to endX do
        begin
          scanZ := z1 + (j - startX) * (z2 - z1) / (endX - startX);
          DrawPixel(j, y, scanZ, AColor);
        end;
      end
      else if startX = endX then
        DrawPixel(startX, y, z1, AColor);

      Inc(i, 2);
    end;
  end;

begin
  PP1 := Project(P1);
  PP2 := Project(P2);
  PP3 := Project(P3);


  minY := PP1.Y;
  maxY := PP1.Y;
  if PP2.Y < minY then minY := PP2.Y;
  if PP3.Y < minY then minY := PP3.Y;
  if PP2.Y > maxY then maxY := PP2.Y;
  if PP3.Y > maxY then maxY := PP3.Y;


  for y := minY to maxY do
    ScanLine(y);
end;

procedure TForm3.RedrawObject;
var
  i: Integer;
  P1, P2, P3: TPoint4D;
begin
  InitializeZBuffer; // Reset Z-Buffer and clear screen


  Image1.Canvas.Pen.Color := clWhite;
  Image1.Canvas.MoveTo(ImageWidth div 2, 0);
  Image1.Canvas.LineTo(ImageWidth div 2, ImageHeight);


  if Length(ProfilePoints) > 0 then
  begin
    Image1.Canvas.Pen.Color := clWhite;
    Image1.Canvas.MoveTo(Round(ProfilePoints[0].X), Round(ProfilePoints[0].Y));
    for i := 1 to High(ProfilePoints) do
      Image1.Canvas.LineTo(Round(ProfilePoints[i].X), Round(ProfilePoints[i].Y));
  end;


  for i := 0 to High(Faces) do
  begin
    P1 := TransformedVertices[Faces[i].V1];
    P2 := TransformedVertices[Faces[i].V2];
    P3 := TransformedVertices[Faces[i].V3];


    FillTriangle(P1, P2, P3, Faces[i].FaceColor);
  end;
  Image1.Repaint; // Força a atualização da tela
end;

procedure TForm3.ApplyTransformation(const M: TMatrix4x4);
var
  i: Integer;
begin

  for i := 0 to High(Vertices) do
    TransformedVertices[i] := MultiplyPointMatrix(Vertices[i], M);

  RedrawObject;
end;



procedure TForm3.FormShow(Sender: TObject);
begin
  InitializeZBuffer;

  Image1.Canvas.Pen.Color := clWhite;
  Image1.Canvas.MoveTo(Image1.Width div 2, 0);
  Image1.Canvas.LineTo(Image1.Width div 2, Image1.Height);

  // Inicializa o array de pontos do perfil
  SetLength(ProfilePoints, 0);
  DrawingProfile := False;
end;

procedure TForm3.ButtonResetClick(Sender: TObject);
begin

  InitializeZBuffer;
  SetLength(ProfilePoints, 0);
  SetLength(Vertices, 0);
  SetLength(TransformedVertices, 0);
  SetLength(Faces, 0);


  Image1.Canvas.Pen.Color := clGray;
  Image1.Canvas.MoveTo(Image1.Width div 2, 0);
  Image1.Canvas.LineTo(Image1.Width div 2, Image1.Height);
end;

procedure TForm3.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  NewPoint: TPoint4D;
begin

  DrawingProfile := True;


  NewPoint.X := X;
  NewPoint.Y := Y;
  NewPoint.Z := 0; // O perfil é 2D no plano XY (Z=0)
  NewPoint.W := 1;

  SetLength(ProfilePoints, Length(ProfilePoints) + 1);
  ProfilePoints[High(ProfilePoints)] := NewPoint;


  Image1.Canvas.Pixels[X, Y] := clWhite;
end;

procedure TForm3.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  NewPoint: TPoint4D;
begin
  if DrawingProfile then
  begin
    // Desenha a linha
    Image1.Canvas.Pen.Color := clWhite;
    Image1.Canvas.MoveTo(Round(ProfilePoints[High(ProfilePoints)].X), Round(ProfilePoints[High(ProfilePoints)].Y));
    Image1.Canvas.LineTo(X, Y);

    // Adiciona o novo ponto
    NewPoint.X := X;
    NewPoint.Y := Y;
    NewPoint.Z := 0;
    NewPoint.W := 1;

    SetLength(ProfilePoints, Length(ProfilePoints) + 1);
    ProfilePoints[High(ProfilePoints)] := NewPoint;
  end;
end;

procedure TForm3.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DrawingProfile := False;
end;


procedure TForm3.GenerateRotationalSweepObject(Steps: Integer);
const

  AxisX = 200;
var
  i, j, k: Integer;
  AngleStep: Real;
  RotationMatrix: TMatrix4x4;
  CurrentPoint: TPoint4D;
  ProfileSize: Integer;
  VertexIndex: Integer;
  FaceIndex: Integer;

  function ScreenToWorld(const P: TPoint4D): TPoint4D;
  begin

    Result.X := P.X - (ImageWidth div 2);

    Result.Y := -(P.Y - (ImageHeight div 2));

    Result.Z := 0;
    Result.W := 1;
  end;

begin
  ProfileSize := Length(ProfilePoints);
  if ProfileSize < 2 then Exit;


  SetLength(Vertices, ProfileSize * Steps);
  SetLength(Faces, (ProfileSize - 1) * Steps * 2);

  AngleStep := 2 * Pi / Steps;
  VertexIndex := 0;
  FaceIndex := 0;


  for j := 0 to Steps - 1 do // j = passo de rotação
  begin

    RotationMatrix := CreateRotationMatrix('Y', j * AngleStep);

    for i := 0 to ProfileSize - 1 do // i = ponto do perfil
    begin

      CurrentPoint := ScreenToWorld(ProfilePoints[i]);


      Vertices[VertexIndex] := MultiplyPointMatrix(CurrentPoint, RotationMatrix);
      Inc(VertexIndex);
    end;
  end;


  for j := 0 to Steps - 1 do
  begin
    k := (j + 1) mod Steps;

    for i := 0 to ProfileSize - 2 do
    begin

      Faces[FaceIndex].V1 := i + j * ProfileSize;
      Faces[FaceIndex].V2 := (i + 1) + j * ProfileSize;
      Faces[FaceIndex].V3 := (i + 1) + k * ProfileSize;
      Faces[FaceIndex].FaceColor := clWhite;
      Inc(FaceIndex);


      Faces[FaceIndex].V1 := i + j * ProfileSize;
      Faces[FaceIndex].V2 := (i + 1) + k * ProfileSize;
      Faces[FaceIndex].V3 := i + k * ProfileSize;
      Faces[FaceIndex].FaceColor := clWhite;
      Inc(FaceIndex);
    end;
  end;


  SetLength(TransformedVertices, Length(Vertices));
  TransformedVertices := Vertices;
end;

procedure TForm3.ButtonRunClick(Sender: TObject);
var
  M: TMatrix4x4;
begin
  InitializeZBuffer;

  GenerateRotationalSweepObject(20);

  M := CreateRotationMatrix('X', -Pi/4);
  ApplyTransformation(M);
  Image1.Repaint;
end;

end.
