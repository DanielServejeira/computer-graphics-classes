unit Unit2;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Menus, Math;

type
  TObjectProcedure = procedure of object; // Type for method pointers

  // 4D Point in Homogeneous Coordinates
  TPoint4D = record
    X, Y, Z, W: Real;
  end;

  // 4x4 Transformation Matrix
  TMatrix4x4 = array[0..3, 0..3] of Real;

  // Structure to define a face (triangle)
  TFace = record
    V1, V2, V3: Integer; // Indices of the vertices in the Vertices array
    FaceColor: TColor; // Renamed from 'Color' to avoid conflict with Graphics.TColor
  end;

  // Structure to store a projected 2D point with its Z-depth
  TProjectedPoint = record
    X, Y: Integer;
    Z: Real;
  end;

  // TForm2 definition (must match the user's interface)
  TForm2 = class(TForm)
    Image1: TImage;
    ButtonExecute: TButton;
    ButtonReset: TButton;
    Edit_a11: TEdit;
    Edit_a12: TEdit;
    Edit_a13: TEdit;
    Edit_a14: TEdit;
    Edit_a21: TEdit;
    Edit_a22: TEdit;
    Edit_a23: TEdit;
    Edit_a24: TEdit;
    Edit_a31: TEdit;
    Edit_a32: TEdit;
    Edit_a33: TEdit;
    Edit_a34: TEdit;
    Edit_a41: TEdit;
    Edit_a42: TEdit;
    Edit_a43: TEdit;
    Edit_a44: TEdit;
    EditLocalX: TEdit;
    EditLocalY: TEdit;
    EditLocalZ: TEdit;
    EditGlobal: TEdit;
    EditTranslationX: TEdit;
    EditTranslationY: TEdit;
    EditTranslationZ: TEdit;
    EditRotationAxis: TEdit;
    EditRotationDegrees: TEdit;
    LabelShearing: TLabel;
    LabelAxis: TLabel;
    LabelDegrees: TLabel;
    LabelRotation: TLabel;
    LabelTranslationX: TLabel;
    LabelTranslationY: TLabel;
    LabelTranslationZ: TLabel;
    LabelTranslation: TLabel;
    LabelScaleX: TLabel;
    LabelScaleZ: TLabel;
    LabelScaleY: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    RadioButtonShearing: TRadioButton;
    RadioButtonObjectCenter: TRadioButton;
    RadioButtonRotationOrigin: TRadioButton;
    RadioButtonTranslation: TRadioButton;
    RadioButtonGlobal: TRadioButton;
    RadioButtonLocal: TRadioButton;
    Scale: TLabel;
    procedure ButtonExecuteClick(Sender: TObject);
    procedure ButtonResetClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuObject1Click(Sender: TObject);
    procedure MenuObject2Click(Sender: TObject);
    procedure MenuObject3Click(Sender: TObject);
    procedure MenuObject4Click(Sender: TObject);
    procedure MenuObject5Click(Sender: TObject);
    procedure MenuObject6Click(Sender: TObject);
    procedure SanitizeAllFloatEdits;
  private
    Vertices: array of TPoint4D; // Original vertices (after initial load/translation)
    TransformedVertices: array of TPoint4D; // Transformed vertices
    Faces: array of TFace; // Object faces (triangles)
    ZBuffer: array of Real; // Z-Buffer array
    ImageWidth, ImageHeight: Integer;
    CurrentObjectLoader: TObjectProcedure; // Pointer to the current object loading procedure

    function Project(Point4D: TPoint4D): TProjectedPoint; // Project 4D point to 2D screen with Z-depth
    procedure InitializeZBuffer;
    procedure DrawPixel(X, Y: Integer; Z: Real; AColor: TColor);
    procedure DrawTriangle(const P1, P2, P3: TPoint4D; AColor: TColor); // Scanline fill with Z-buffer
    procedure RedrawObject;
    procedure ApplyTransformation(const M: TMatrix4x4);
    procedure LoadHouseObject;
    procedure LoadObject1;
    procedure LoadObject2;
    procedure LoadObject3;
    procedure LoadObject4;
    procedure LoadObject5;
    function GetObjectCenter: TPoint4D;
    function CreateTranslationMatrix(Tx, Ty, Tz: Real): TMatrix4x4;
    function CreateRotationMatrix(Axis: Char; AngleRad: Real): TMatrix4x4;
    function CreateScaleMatrix(Sx, Sy, Sz: Real): TMatrix4x4;
  public
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

// Helper function to create an identity matrix
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

// Helper function for matrix multiplication: C = A * B
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

// Helper function for point-matrix multiplication: P' = P * M
function MultiplyPointMatrix(const P: TPoint4D; const M: TMatrix4x4): TPoint4D;
begin
  Result.X := (P.X * M[0, 0]) + (P.Y * M[1, 0]) + (P.Z * M[2, 0]) + (P.W * M[3, 0]);
  Result.Y := (P.X * M[0, 1]) + (P.Y * M[1, 1]) + (P.Z * M[2, 1]) + (P.W * M[3, 1]);
  Result.Z := (P.X * M[0, 2]) + (P.Y * M[1, 2]) + (P.Z * M[2, 2]) + (P.W * M[3, 2]);
  Result.W := (P.X * M[0, 3]) + (P.Y * M[1, 3]) + (P.Z * M[2, 3]) + (P.W * M[3, 3]);
end;

// --- Transformation Matrix Creation Functions ---

function TForm2.CreateTranslationMatrix(Tx, Ty, Tz: Real): TMatrix4x4;
begin
  Result := IdentityMatrix;
  Result[3, 0] := Tx;
  Result[3, 1] := Ty;
  Result[3, 2] := Tz;
end;

function TForm2.CreateScaleMatrix(Sx, Sy, Sz: Real): TMatrix4x4;
begin
  Result := IdentityMatrix;
  Result[0, 0] := Sx;
  Result[1, 1] := Sy;
  Result[2, 2] := Sz;
end;

function TForm2.CreateRotationMatrix(Axis: Char; AngleRad: Real): TMatrix4x4;
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

// --- Z-Buffer and Drawing Functions ---

procedure TForm2.InitializeZBuffer;
var i: Integer;
begin
  ImageWidth := Image1.Width;
  ImageHeight := Image1.Height;
  SetLength(ZBuffer, ImageWidth * ImageHeight);

  // Initialize Z-Buffer with maximum depth (far plane)
  for i := 0 to High(ZBuffer) do
    ZBuffer[i] := Infinity;

  // Clear screen
  Image1.Canvas.Brush.Color := clBlack;
  Image1.Canvas.FillRect(Image1.ClientRect);
end;

procedure TForm2.DrawPixel(X, Y: Integer; Z: Real; AColor: TColor);
var
  Index: Integer;
begin
  // Check if point is within the image bounds
  if (X >= 0) and (X < ImageWidth) and (Y >= 0) and (Y < ImageHeight) then
  begin
    Index := Y * ImageWidth + X;
    // Z-Buffer test: only draw if the new Z is closer (smaller) than the stored Z
    if Z < ZBuffer[Index] then
    begin
      ZBuffer[Index] := Z;
      Image1.Canvas.Pixels[X, Y] := AColor;
    end;
  end;
end;

// Simple 3D to 2D Oblique Projection
function TForm2.Project(Point4D: TPoint4D): TProjectedPoint;
const
  ProjectionFactor = 0.5; // Inclination factor
  OffsetX = 200;  // Center the object
  OffsetY = 200;
  ScaleFactor = 1.0; // Base scale for viewing
var
  x2d, y2d: Real;
begin
  // Perspective division (if W is not 1, which it should be after transformations)
  if Point4D.W <> 0 then
  begin
    Point4D.X := Point4D.X / Point4D.W;
    Point4D.Y := Point4D.Y / Point4D.W;
    Point4D.Z := Point4D.Z / Point4D.W;
  end;

  // Oblique projection formula
  x2d := Point4D.X + (ProjectionFactor * Point4D.Z);
  y2d := Point4D.Y - (ProjectionFactor * Point4D.Z); // Y is inverted on the canvas

  // Apply viewing scale and offset
  Result.X := Round(x2d * ScaleFactor + OffsetX);
  Result.Y := Round(OffsetY - y2d * ScaleFactor); // Final Y inverted -> correct orientation
  Result.Z := Point4D.Z; // Store the Z-depth for the Z-buffer
end;

// Bresenham's line algorithm for drawing a line segment
procedure DrawLineSegment(X1, Y1, Z1, X2, Y2, Z2: Real; AColor: TColor; Form: TForm2);
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
    Form.DrawPixel(Round(X1), Round(Y1), Z1, AColor);
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
    Form.DrawPixel(Round(x), Round(y), z, AColor);
    x := x + xinc;
    y := y + yinc;
    z := z + zinc;
  end;
end;

procedure TForm2.DrawTriangle(const P1, P2, P3: TPoint4D; AColor: TColor);
var
  PP1, PP2, PP3: TProjectedPoint;
begin
  PP1 := Project(P1);
  PP2 := Project(P2);
  PP3 := Project(P3);

  // Draw the wireframe of the triangle
  DrawLineSegment(PP1.X, PP1.Y, PP1.Z, PP2.X, PP2.Y, PP2.Z, AColor, Self);
  DrawLineSegment(PP2.X, PP2.Y, PP2.Z, PP3.X, PP3.Y, PP3.Z, AColor, Self);
  DrawLineSegment(PP3.X, PP3.Y, PP3.Z, PP1.X, PP1.Y, PP1.Z, AColor, Self);
end;

procedure TForm2.RedrawObject;
var
  i: Integer;
  P1, P2, P3: TPoint4D;
begin
  InitializeZBuffer; // Reset Z-Buffer and clear screen

  // Draw all faces (triangles)
  for i := 0 to High(Faces) do
  begin
    P1 := TransformedVertices[Faces[i].V1];
    P2 := TransformedVertices[Faces[i].V2];
    P3 := TransformedVertices[Faces[i].V3];
    DrawTriangle(P1, P2, P3, Faces[i].FaceColor);
  end;

  Image1.Repaint;
end;

// --- Object Loading Functions ---

procedure TForm2.LoadHouseObject;
var
  M: TMatrix4x4;
  i: Integer;
begin
  CurrentObjectLoader := @LoadHouseObject;
  // 10 vertices for the house (cube + roof)
  SetLength(Vertices, 10);
  SetLength(Faces, 11); // 6 faces for cube, 4 for roof, 1 for the missing edge

  // Vertices (as defined by the user)
  Vertices[0].X := 0; Vertices[0].Y := 0; Vertices[0].Z := 0; Vertices[0].W := 1;
  Vertices[1].X := 100; Vertices[1].Y := 0; Vertices[1].Z := 0; Vertices[1].W := 1;
  Vertices[2].X := 100; Vertices[2].Y := 100; Vertices[2].Z := 0; Vertices[2].W := 1;
  Vertices[3].X := 0; Vertices[3].Y := 100; Vertices[3].Z := 0; Vertices[3].W := 1;
  Vertices[4].X := 0; Vertices[4].Y := 0; Vertices[4].Z := 100; Vertices[4].W := 1;
  Vertices[5].X := 100; Vertices[5].Y := 0; Vertices[5].Z := 100; Vertices[5].W := 1;
  Vertices[6].X := 100; Vertices[6].Y := 100; Vertices[6].Z := 100; Vertices[6].W := 1;
  Vertices[7].X := 0; Vertices[7].Y := 100; Vertices[7].Z := 100; Vertices[7].W := 1;
  Vertices[8].X := 50; Vertices[8].Y := 150; Vertices[8].Z := 0; Vertices[8].W := 1;
  Vertices[9].X := 50; Vertices[9].Y := 150; Vertices[9].Z := 100; Vertices[9].W := 1;

  // Apply initial centering translation (-130 on X-axis)
  M := CreateTranslationMatrix(-130, 0, 0);
  for i := 0 to High(Vertices) do
    Vertices[i] := MultiplyPointMatrix(Vertices[i], M);

  // Faces (Triangles for wireframe)
  // Base (Z=0)
  Faces[0].V1 := 0; Faces[0].V2 := 1; Faces[0].V3 := 2; Faces[0].FaceColor := clWhite;
  Faces[1].V1 := 0; Faces[1].V2 := 2; Faces[1].V3 := 3; Faces[1].FaceColor := clWhite;
  // Top (Z=100)
  Faces[2].V1 := 4; Faces[2].V2 := 5; Faces[2].V3 := 6; Faces[2].FaceColor := clWhite;
  Faces[3].V1 := 4; Faces[3].V2 := 6; Faces[3].V3 := 7; Faces[3].FaceColor := clWhite;
  // Sides (X=0, X=100, Y=0, Y=100) - simplified to 4 faces for wireframe
  Faces[4].V1 := 0; Faces[4].V2 := 4; Faces[4].V3 := 7; Faces[4].FaceColor := clWhite;
  Faces[5].V1 := 0; Faces[5].V2 := 7; Faces[5].V3 := 3; Faces[5].FaceColor := clWhite;
  Faces[6].V1 := 1; Faces[6].V2 := 5; Faces[6].V3 := 6; Faces[6].FaceColor := clWhite;
  Faces[7].V1 := 1; Faces[7].V2 := 6; Faces[7].V3 := 2; Faces[7].FaceColor := clWhite;
  // Roof (Front and Back)
  Faces[8].V1 := 3; Faces[8].V2 := 2; Faces[8].V3 := 8; Faces[8].FaceColor := clWhite;
  Faces[9].V1 := 7; Faces[9].V2 := 6; Faces[9].V3 := 9; Faces[9].FaceColor := clWhite;
  // Missing edge: connect the two roof apexes (V8 and V9)
  Faces[10].V1 := 8; Faces[10].V2 := 9; Faces[10].V3 := 9; Faces[10].FaceColor := clWhite;

  // Initialize TransformedVertices
  SetLength(TransformedVertices, Length(Vertices));
  TransformedVertices := Vertices;
end;

procedure TForm2.LoadObject1; // z=x^2+y with x in [10,30] and y in [20,40] (Blue)
var
  x, y: Real;
  idx: Integer;
  X_MIN, X_MAX, Y_MIN, Y_MAX: Real;
  STEP: Real = 2.0; // Use a step for a manageable point cloud/mesh
  M: TMatrix4x4;
  i: Integer;
begin
  CurrentObjectLoader := @LoadObject1;
  X_MIN := 10; X_MAX := 30;
  Y_MIN := 20; Y_MAX := 40;

  idx := 0;
  SetLength(Vertices, 0);
  SetLength(Faces, 0);

  // Generate point cloud
  x := X_MIN;
  while x <= X_MAX do
  begin
    y := Y_MIN;
    while y <= Y_MAX do
    begin
      SetLength(Vertices, Length(Vertices) + 1);
      with Vertices[High(Vertices)] do begin
        X := x;
        Y := y;
        Z := Sqr(x) + y;
        W := 1;
      end;
      Inc(idx);
      y := y + STEP;
    end;
    x := x + STEP;
  end;

  // Apply initial centering translation (-130 on X-axis)
  M := CreateTranslationMatrix(-130, 0, 0);
  for i := 0 to High(Vertices) do
    Vertices[i] := MultiplyPointMatrix(Vertices[i], M);

  // Since it's a surface, we'll just draw the point cloud for simplicity
  // We'll use a single 'dummy' face for the color in RedrawObject
  SetLength(Faces, 1);
  Faces[0].V1 := 0; Faces[0].V2 := 0; Faces[0].V3 := 0; Faces[0].FaceColor := clWhite;

  SetLength(TransformedVertices, Length(Vertices));
  TransformedVertices := Vertices;
end;

procedure TForm2.LoadObject2; // z=3x-2y+5 with x in [50,100] and y in [30,80] (Red)
var
  x, y: Real;
  idx: Integer;
  X_MIN, X_MAX, Y_MIN, Y_MAX: Real;
  STEP: Real = 5.0;
  M: TMatrix4x4;
  i: Integer;
begin
  CurrentObjectLoader := @LoadObject2;
  X_MIN := 50; X_MAX := 100;
  Y_MIN := 30; Y_MAX := 80;

  idx := 0;
  SetLength(Vertices, 0);
  SetLength(Faces, 0);

  // Generate point cloud
  x := X_MIN;
  while x <= X_MAX do
  begin
    y := Y_MIN;
    while y <= Y_MAX do
    begin
      SetLength(Vertices, Length(Vertices) + 1);
      with Vertices[High(Vertices)] do begin
        X := x;
        Y := y;
        Z := 3*x - 2*y + 5;
        W := 1;
      end;
      Inc(idx);
      y := y + STEP;
    end;
    x := x + STEP;
  end;

  // Apply initial centering translation (-130 on X-axis)
  M := CreateTranslationMatrix(-130, 0, 0);
  for i := 0 to High(Vertices) do
    Vertices[i] := MultiplyPointMatrix(Vertices[i], M);

  SetLength(Faces, 1);
  Faces[0].V1 := 0; Faces[0].V2 := 0; Faces[0].V3 := 0; Faces[0].FaceColor := clWhite;

  SetLength(TransformedVertices, Length(Vertices));
  TransformedVertices := Vertices;
end;

procedure TForm2.LoadObject3; // x=30+cos(a).t, y=50+sen(a).t, z=10+t with t in [0,50] and a in [0,2pi] (Yellow) - Spiral
var
  t, a: Real;
  idx: Integer;
  T_MAX: Real = 50;
  A_MAX: Real = 2 * Pi;
  T_STEP: Real = 1.0;
  A_STEP: Real = Pi / 16;
  M: TMatrix4x4;
  i: Integer;
begin
  CurrentObjectLoader := @LoadObject3;
  idx := 0;
  SetLength(Vertices, 0);
  SetLength(Faces, 0);

  // Generate points for the spiral
  t := 0;
  while t <= T_MAX do
  begin
    a := 0;
    while a <= A_MAX do
    begin
      SetLength(Vertices, Length(Vertices) + 1);
      with Vertices[High(Vertices)] do begin
        X := 30 + Cos(a) * t;
        Y := 50 + Sin(a) * t;
        Z := 10 + t;
        W := 1;
      end;
      Inc(idx);
      a := a + A_STEP;
    end;
    t := t + T_STEP;
  end;

  // Apply initial centering translation (-130 on X-axis)
  M := CreateTranslationMatrix(-130, 0, 0);
  for i := 0 to High(Vertices) do
    Vertices[i] := MultiplyPointMatrix(Vertices[i], M);

  SetLength(Faces, 1);
  Faces[0].V1 := 0; Faces[0].V2 := 0; Faces[0].V3 := 0; Faces[0].FaceColor := clWhite;

  SetLength(TransformedVertices, Length(Vertices));
  TransformedVertices := Vertices;
end;

procedure TForm2.LoadObject4; // x=100+30.cos(a).cos(b), y=50+30.cos(a).sen(b), z=20+30.sen(a) (Green) - Sphere
var
  a, b: Real;
  idx: Integer;
  A_MAX: Real = 2 * Pi;
  B_MAX: Real = 2 * Pi;
  STEP: Real = Pi / 12;
  R: Real = 30;
  CX: Real = 100;
  CY: Real = 50;
  CZ: Real = 20;
  M: TMatrix4x4;
  i: Integer;
begin
  CurrentObjectLoader := @LoadObject4;
  idx := 0;
  SetLength(Vertices, 0);
  SetLength(Faces, 0);

  // Generate points for the sphere
  a := 0;
  while a <= A_MAX do
  begin
    b := 0;
    while b <= B_MAX do
    begin
      SetLength(Vertices, Length(Vertices) + 1);
      with Vertices[High(Vertices)] do begin
        X := CX + R * Cos(a) * Cos(b);
        Y := CY + R * Cos(a) * Sin(b);
        Z := CZ + R * Sin(a);
        W := 1;
      end;
      Inc(idx);
      b := b + STEP;
    end;
    a := a + STEP;
  end;

  // Apply initial centering translation (-130 on X-axis)
  M := CreateTranslationMatrix(-130, 0, 0);
  for i := 0 to High(Vertices) do
    Vertices[i] := MultiplyPointMatrix(Vertices[i], M);

  SetLength(Faces, 1);
  Faces[0].V1 := 0; Faces[0].V2 := 0; Faces[0].V3 := 0; Faces[0].FaceColor := clWhite;

  SetLength(TransformedVertices, Length(Vertices));
  TransformedVertices := Vertices;
end;

procedure TForm2.LoadObject5; // Cube of side 40 centered at the origin (White)
var
  HalfSide: Real = 20;
  M: TMatrix4x4;
  i: Integer;
begin
  CurrentObjectLoader := @LoadObject5;
  // 8 vertices for the cube
  SetLength(Vertices, 8);
  SetLength(Faces, 12); // 6 faces * 2 triangles/face = 12 triangles

  // Vertices (centered at origin)
  Vertices[0].X := -HalfSide; Vertices[0].Y := -HalfSide; Vertices[0].Z := -HalfSide; Vertices[0].W := 1;
  Vertices[1].X := HalfSide; Vertices[1].Y := -HalfSide; Vertices[1].Z := -HalfSide; Vertices[1].W := 1;
  Vertices[2].X := HalfSide; Vertices[2].Y := HalfSide; Vertices[2].Z := -HalfSide; Vertices[2].W := 1;
  Vertices[3].X := -HalfSide; Vertices[3].Y := HalfSide; Vertices[3].Z := -HalfSide; Vertices[3].W := 1;
  Vertices[4].X := -HalfSide; Vertices[4].Y := -HalfSide; Vertices[4].Z := HalfSide; Vertices[4].W := 1;
  Vertices[5].X := HalfSide; Vertices[5].Y := -HalfSide; Vertices[5].Z := HalfSide; Vertices[5].W := 1;
  Vertices[6].X := HalfSide; Vertices[6].Y := HalfSide; Vertices[6].Z := HalfSide; Vertices[6].W := 1;
  Vertices[7].X := -HalfSide; Vertices[7].Y := HalfSide; Vertices[7].Z := HalfSide; Vertices[7].W := 1;

  // Apply initial centering translation (-130 on X-axis)
  M := CreateTranslationMatrix(-130, 0, 0);
  for i := 0 to High(Vertices) do
    Vertices[i] := MultiplyPointMatrix(Vertices[i], M);

  // Faces (12 triangles for the 6 faces)
  // Back face (Z=-HalfSide)
  Faces[0].V1 := 0; Faces[0].V2 := 1; Faces[0].V3 := 2; Faces[0].FaceColor := clWhite;
  Faces[1].V1 := 0; Faces[1].V2 := 2; Faces[1].V3 := 3; Faces[1].FaceColor := clWhite;
  // Front face (Z=HalfSide)
  Faces[2].V1 := 4; Faces[2].V2 := 5; Faces[2].V3 := 6; Faces[2].FaceColor := clWhite;
  Faces[3].V1 := 4; Faces[3].V2 := 6; Faces[3].V3 := 7; Faces[3].FaceColor := clWhite;
  // Right face (X=HalfSide)
  Faces[4].V1 := 1; Faces[4].V2 := 5; Faces[4].V3 := 6; Faces[4].FaceColor := clWhite;
  Faces[5].V1 := 1; Faces[5].V2 := 6; Faces[5].V3 := 2; Faces[5].FaceColor := clWhite;
  // Left face (X=-HalfSide)
  Faces[6].V1 := 0; Faces[6].V2 := 4; Faces[6].V3 := 7; Faces[6].FaceColor := clWhite;
  Faces[7].V1 := 0; Faces[7].V2 := 7; Faces[7].V3 := 3; Faces[7].FaceColor := clWhite;
  // Top face (Y=HalfSide)
  Faces[8].V1 := 3; Faces[8].V2 := 2; Faces[8].V3 := 6; Faces[8].FaceColor := clWhite;
  Faces[9].V1 := 3; Faces[9].V2 := 6; Faces[9].V3 := 7; Faces[9].FaceColor := clWhite;
  // Bottom face (Y=-HalfSide)
  Faces[10].V1 := 0; Faces[10].V2 := 1; Faces[10].V3 := 5; Faces[10].FaceColor := clWhite;
  Faces[11].V1 := 0; Faces[11].V2 := 5; Faces[11].V3 := 4; Faces[11].FaceColor := clWhite;

  SetLength(TransformedVertices, Length(Vertices));
  TransformedVertices := Vertices;
end;

// --- Transformation Logic ---

procedure TForm2.ApplyTransformation(const M: TMatrix4x4);
var
  i: Integer;
begin
  // Apply the transformation matrix M to all vertices
  for i := 0 to High(Vertices) do
    TransformedVertices[i] := MultiplyPointMatrix(Vertices[i], M);

  RedrawObject;
end;

function TForm2.GetObjectCenter: TPoint4D;
var
  i: Integer;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Z := 0;
  Result.W := 1;

  if Length(Vertices) = 0 then Exit;

  for i := 0 to High(Vertices) do
  begin
    Result.X := Result.X + Vertices[i].X;
    Result.Y := Result.Y + Vertices[i].Y;
    Result.Z := Result.Z + Vertices[i].Z;
  end;

  Result.X := Result.X / Length(Vertices);
  Result.Y := Result.Y / Length(Vertices);
  Result.Z := Result.Z / Length(Vertices);
end;

// --- Event Handlers ---

procedure TForm2.SanitizeAllFloatEdits;
var
  i: Integer;
  E: TEdit;
begin
  // Replace '.' with ',' for float conversion in Brazilian locale
  for i := 0 to ComponentCount - 1 do
  begin
    if Components[i] is TEdit then
    begin
      E := TEdit(Components[i]);
      // The user's original code had a check for EditRotationAxis, which I'll keep
      if E <> EditRotationAxis then
      begin
        E.Text := StringReplace(Trim(E.Text), '.', ',', [rfReplaceAll]);
      end;
    end;
  end;
end;

procedure TForm2.ButtonExecuteClick(Sender: TObject);
var
  scaleX, scaleY, scaleZ, scaleGlobal: Real;
  translationX, translationY, translationZ: Real;
  degree, rad: Real;
  axis: String;
  cx, cy, cz: Real;
  transformationMatrix, Mtemp, M1, M2, M3: TMatrix4x4;
  Center: TPoint4D;
begin
  SanitizeAllFloatEdits;

  // Start with the identity matrix
  transformationMatrix := IdentityMatrix;

  try
    // 1. Local Scale
    if RadioButtonLocal.Checked then
    begin
      scaleX := StrToFloat(EditLocalX.Text);
      scaleY := StrToFloat(EditLocalY.Text);
      scaleZ := StrToFloat(EditLocalZ.Text);
      Mtemp := CreateScaleMatrix(scaleX, scaleY, scaleZ);
      transformationMatrix := MultiplyMatrix(transformationMatrix, Mtemp);
    end;

    // 2. Global Scale
    if RadioButtonGlobal.Checked then
    begin
      scaleGlobal := StrToFloat(EditGlobal.Text);
      Mtemp := CreateScaleMatrix(scaleGlobal, scaleGlobal, scaleGlobal);
      transformationMatrix := MultiplyMatrix(transformationMatrix, Mtemp);
    end;

    // 3. Translation
    if RadioButtonTranslation.Checked then
    begin
      translationX := StrToFloat(EditTranslationX.Text);
      translationY := StrToFloat(EditTranslationY.Text);
      translationZ := StrToFloat(EditTranslationZ.Text);
      Mtemp := CreateTranslationMatrix(translationX, translationY, translationZ);
      transformationMatrix := MultiplyMatrix(transformationMatrix, Mtemp);
    end;

    // 4. Rotation around Origin
    if RadioButtonRotationOrigin.Checked then
    begin
      axis := UpCase(EditRotationAxis.Text);
      degree := StrToFloat(EditRotationDegrees.Text);
      rad := degree * Pi / 180.0;
      Mtemp := CreateRotationMatrix(axis[1], rad);
      transformationMatrix := MultiplyMatrix(transformationMatrix, Mtemp);
    end;

    // 5. Rotation around Object Center
    if RadioButtonObjectCenter.Checked then
    begin
      axis := UpCase(EditRotationAxis.Text);
      degree := StrToFloat(EditRotationDegrees.Text);
      rad := degree * Pi / 180.0;

      Center := GetObjectCenter;
      cx := Center.X;
      cy := Center.Y;
      cz := Center.Z;

      // M = T(-c) * R * T(c)
      M1 := CreateTranslationMatrix(-cx, -cy, -cz); // Translate to origin
      M2 := CreateRotationMatrix(axis[1], rad);     // Rotate
      M3 := CreateTranslationMatrix(cx, cy, cz);     // Translate back

      Mtemp := MultiplyMatrix(M1, M2);
      Mtemp := MultiplyMatrix(Mtemp, M3);
      transformationMatrix := MultiplyMatrix(transformationMatrix, Mtemp);
    end;

    // 6. Shearing (User-defined matrix)
    if RadioButtonShearing.Checked then
    begin
      // Read user-defined matrix from Edit controls
      Mtemp[0, 0] := StrToFloat(Edit_a11.Text); Mtemp[0, 1] := StrToFloat(Edit_a12.Text); Mtemp[0, 2] := StrToFloat(Edit_a13.Text); Mtemp[0, 3] := StrToFloat(Edit_a14.Text);
      Mtemp[1, 0] := StrToFloat(Edit_a21.Text); Mtemp[1, 1] := StrToFloat(Edit_a22.Text); Mtemp[1, 2] := StrToFloat(Edit_a23.Text); Mtemp[1, 3] := StrToFloat(Edit_a24.Text);
      Mtemp[2, 0] := StrToFloat(Edit_a31.Text); Mtemp[2, 1] := StrToFloat(Edit_a32.Text); Mtemp[2, 2] := StrToFloat(Edit_a33.Text); Mtemp[2, 3] := StrToFloat(Edit_a34.Text);
      Mtemp[3, 0] := StrToFloat(Edit_a41.Text); Mtemp[3, 1] := StrToFloat(Edit_a42.Text); Mtemp[3, 2] := StrToFloat(Edit_a43.Text); Mtemp[3, 3] := StrToFloat(Edit_a44.Text);

      transformationMatrix := MultiplyMatrix(transformationMatrix, Mtemp);
    end;

    // Apply the final accumulated transformation
    ApplyTransformation(transformationMatrix);

  except
    on EConvertError do
      ShowMessage('Erro de conversão: Por favor, verifique se todos os campos numéricos contêm valores válidos (use vírgula como separador decimal).');
    on E: Exception do
      ShowMessage('Erro: ' + E.Message);
  end;
end;

procedure TForm2.ButtonResetClick(Sender: TObject);
begin
    LoadHouseObject;
  RedrawObject;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  LoadHouseObject;
  RedrawObject;
end;

procedure TForm2.MenuObject1Click(Sender: TObject);
begin
  LoadObject1;
  RedrawObject;
end;

procedure TForm2.MenuObject2Click(Sender: TObject);
begin
  LoadObject2;
  RedrawObject;
end;

procedure TForm2.MenuObject3Click(Sender: TObject);
begin
  LoadObject3;
  RedrawObject;
end;

procedure TForm2.MenuObject4Click(Sender: TObject);
begin
  LoadObject4;
  RedrawObject;
end;

procedure TForm2.MenuObject5Click(Sender: TObject);
begin
  LoadObject5;
  RedrawObject;
end;

procedure TForm2.MenuObject6Click(Sender: TObject);
begin

end;

end.
