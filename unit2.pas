unit Unit2;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TForm2 }

  TPoint4D = record
    X, Y, Z, W: Real;
  end;

  TMatrix4x4 = array[0..3, 0..3] of Real;

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
    procedure SanitizeAllFloatEdits;
  private
    Vertices: array[0..9] of TPoint4D; // Store vertices of the 3D object using homogeneous coordinates
    function Project(Point4D: TPoint4D): TPoint; // convert a 3D point to a 2D screen point
    procedure DrawLine(P1, P2: TPoint4D); // draw a line between two 3D points
    procedure RedrawObject;
    procedure ApplyTransformation(const M: TMatrix4x4);
  public

  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

function MultiplyMatrix(const A, B: TMatrix4x4): TMatrix4x4;
var i,j,k: Integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
    begin
      Result[i,j] := 0;
      for k := 0 to 3 do
        Result[i,j] := Result[i,j] + A[i,k] * B[k,j];
    end;
end;

procedure TForm2.SanitizeAllFloatEdits;
var
  i: Integer;
  E: TEdit;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    if Components[i] is TEdit then
    begin
      E := TEdit(Components[i]);
      if E <> EditRotationAxis then
      begin
        E.Text := StringReplace(Trim(E.Text), '.', ',', [rfReplaceAll]);
      end;
    end;
  end;
end;

procedure TForm2.ApplyTransformation(const M: TMatrix4x4);
var
  i: Integer;
  tempV: TPoint4D;
begin
  for i := 0 to 9 do
  begin
    tempV := Vertices[i]; // store the original value

    // main matrix tempV * matrix M (transformation)
    Vertices[i].X := (tempV.X * M[0,0]) + (tempV.Y * M[1,0]) + (tempV.Z * M[2,0]) + (tempV.W * M[3,0]);
    Vertices[i].Y := (tempV.X * M[0,1]) + (tempV.Y * M[1,1]) + (tempV.Z * M[2,1]) + (tempV.W * M[3,1]);
    Vertices[i].Z := (tempV.X * M[0,2]) + (tempV.Y * M[1,2]) + (tempV.Z * M[2,2]) + (tempV.W * M[3,2]);
    Vertices[i].W := (tempV.X * M[0,3]) + (tempV.Y * M[1,3]) + (tempV.Z * M[2,3]) + (tempV.W * M[3,3]);
  end;

  RedrawObject;
end;

procedure TForm2.RedrawObject;
begin
  // clean screen
  Image1.Canvas.Brush.Color := clWhite;
  Image1.Canvas.FillRect(Image1.ClientRect);
  Image1.Canvas.Pen.Color := clBlack;

  DrawLine(Vertices[0], Vertices[1]);
  DrawLine(Vertices[1], Vertices[2]);
  DrawLine(Vertices[3], Vertices[0]);

  DrawLine(Vertices[4], Vertices[5]);
  DrawLine(Vertices[5], Vertices[6]);
  DrawLine(Vertices[7], Vertices[4]);

  DrawLine(Vertices[0], Vertices[4]);
  DrawLine(Vertices[1], Vertices[5]);
  DrawLine(Vertices[2], Vertices[6]);
  DrawLine(Vertices[3], Vertices[7]);

  DrawLine(Vertices[3], Vertices[8]);
  DrawLine(Vertices[2], Vertices[8]);
  DrawLine(Vertices[7], Vertices[9]);
  DrawLine(Vertices[6], Vertices[9]);
  DrawLine(Vertices[8], Vertices[9]);
end;

function TForm2.Project(Point4D: TPoint4D): TPoint;
const
  ProjectionFactor = 0.5; // Inclination factor
  OffsetX = 50;  // Offset to avoid drawing in the top-left corner
  OffsetY = 200;
var
  x2d, y2d: Integer;
begin
  // oblique projection formula
  x2d := Round(Point4D.X + (ProjectionFactor * Point4D.Z));
  y2d := Round(Point4D.Y - (ProjectionFactor * Point4D.Z)); // Y is inverted on the canvas

  // Add the offsets and return the final 2D point
  Result.X := x2d + OffsetX;
  Result.Y := OffsetY - y2d; //final Y inverted -> correct orientation
end;

// simplifies the drawing process
procedure TForm2.DrawLine(P1, P2: TPoint4D);
var
  Point2D_1, Point2D_2: TPoint;
begin
  // Convert the two 3D points to 2D
  Point2D_1 := Project(P1);
  Point2D_2 := Project(P2);

  Image1.Canvas.MoveTo(Point2D_1.X, Point2D_1.Y);
  Image1.Canvas.LineTo(Point2D_2.X, Point2D_2.Y);
end;


procedure TForm2.FormShow(Sender: TObject);
begin
  Image1.Canvas.Brush.Color := clWhite;
  Image1.Canvas.FillRect(Image1.ClientRect);
  Image1.Canvas.Pen.Color := clBlack;

  // 3D coordinates of the object
  Vertices[0].X := 0;   Vertices[0].Y := 0;   Vertices[0].Z := 0;   Vertices[0].W := 1;
  Vertices[1].X := 100; Vertices[1].Y := 0;   Vertices[1].Z := 0;   Vertices[1].W := 1;
  Vertices[2].X := 100; Vertices[2].Y := 100; Vertices[2].Z := 0;   Vertices[2].W := 1;
  Vertices[3].X := 0;   Vertices[3].Y := 100; Vertices[3].Z := 0;   Vertices[3].W := 1;
  Vertices[4].X := 0;   Vertices[4].Y := 0;   Vertices[4].Z := 100; Vertices[4].W := 1;
  Vertices[5].X := 100; Vertices[5].Y := 0;   Vertices[5].Z := 100; Vertices[5].W := 1;
  Vertices[6].X := 100; Vertices[6].Y := 100; Vertices[6].Z := 100; Vertices[6].W := 1;
  Vertices[7].X := 0;   Vertices[7].Y := 100; Vertices[7].Z := 100; Vertices[7].W := 1;
  Vertices[8].X := 50;  Vertices[8].Y := 150; Vertices[8].Z := 0;   Vertices[8].W := 1;
  Vertices[9].X := 50;  Vertices[9].Y := 150; Vertices[9].Z := 100; Vertices[9].W := 1;

  // z=0
  DrawLine(Vertices[0], Vertices[1]);
  DrawLine(Vertices[1], Vertices[2]);
  DrawLine(Vertices[3], Vertices[0]);

  // z=100
  DrawLine(Vertices[4], Vertices[5]);
  DrawLine(Vertices[5], Vertices[6]);
  DrawLine(Vertices[7], Vertices[4]);

  // Connect lines between the faces
  DrawLine(Vertices[0], Vertices[4]);
  DrawLine(Vertices[1], Vertices[5]);
  DrawLine(Vertices[2], Vertices[6]);
  DrawLine(Vertices[3], Vertices[7]);

  //roof
  DrawLine(Vertices[3], Vertices[8]);
  DrawLine(Vertices[2], Vertices[8]);
  DrawLine(Vertices[7], Vertices[9]);
  DrawLine(Vertices[6], Vertices[9]);
  DrawLine(Vertices[8], Vertices[9]);

  RedrawObject;
end;

procedure TForm2.ButtonExecuteClick(Sender: TObject);
var
  scaleX, scaleY, scaleZ, scaleGlobal: Real;
  translationX, translationY, translationZ: Real;
  pi, rad, degree: Real;
  axis: String;
  cx, cy, cz: Real;
  i: Integer;
  a11, a12, a13, a14, a21, a22, a23, a24, a31, a32, a33, a34, a41, a42, a43, a44: Real;
  transformationMatrix, Mtemp: TMatrix4x4;
begin
  SanitizeAllFloatEdits;

  if RadioButtonLocal.Checked then
  begin
      try
      // identity matrix
      transformationMatrix := Default(TMatrix4x4);
      transformationMatrix[3,3] := 1.0;

      scaleX := StrToFloat(EditLocalX.Text);
      scaleY := StrToFloat(EditLocalY.Text);
      scaleZ := StrToFloat(EditLocalZ.Text);

      // scaling factors on the main diagonal
      transformationMatrix[0,0] := scaleX;
      transformationMatrix[1,1] := scaleY;
      transformationMatrix[2,2] := scaleZ;

      // scaling matrix to the vertices
      ApplyTransformation(transformationMatrix);

    except
      on EConvertError do
        ShowMessage('Por favor, insira um valor numérico válido para as escalas locais.');
    end;
  end;

  if RadioButtonGlobal.Checked then
  begin
    try
      // identity matrix
      transformationMatrix := Default(TMatrix4x4);
      transformationMatrix[3,3] := 1.0;

      scaleGlobal := StrToFloat(EditGlobal.Text);

      // scaling factors on the main diagonal
      transformationMatrix[0,0] := scaleGlobal;
      transformationMatrix[1,1] := scaleGlobal;
      transformationMatrix[2,2] := scaleGlobal;

      // scaling matrix to the vertices
      ApplyTransformation(transformationMatrix);

    except
      on EConvertError do
        ShowMessage('Por favor, insira um valor numérico válido para a escala global.');
    end;
  end;

  if RadioButtonTranslation.Checked then
  begin
    try
      translationX := StrToFloat(EditTranslationX.Text);
      translationY := StrToFloat(EditTranslationY.Text);
      translationZ := StrToFloat(EditTranslationZ.Text);

      // identity matrix
      transformationMatrix := Default(TMatrix4x4);
      transformationMatrix[0,0] := 1.0;
      transformationMatrix[1,1] := 1.0;
      transformationMatrix[2,2] := 1.0;
      transformationMatrix[3,3] := 1.0;

      //translation values in the fourth line
      transformationMatrix[3,0] := translationX;
      transformationMatrix[3,1] := translationY;
      transformationMatrix[3,2] := translationZ;

      ApplyTransformation(transformationMatrix);

    except
      on EConvertError do
        ShowMessage('Por favor, insira valores numéricos válidos para as translações.');
    end;
  end;

  if RadioButtonRotationOrigin.Checked then
  begin
    try
      axis := UpCase(EditRotationAxis.Text);
      degree := StrToFloat(EditRotationDegrees.Text);
      if(degree < 0) or (degree > 360) then Exit;

      pi := 3.1415;
      rad := degree*pi/180.0;

      // identity matrix
      transformationMatrix := Default(TMatrix4x4);
      transformationMatrix[0,0] := 1.0;
      transformationMatrix[1,1] := 1.0;
      transformationMatrix[2,2] := 1.0;
      transformationMatrix[3,3] := 1.0;

      if(axis = 'X') then
      begin
        transformationMatrix[1,1] := cos(rad);
        transformationMatrix[1,2] := sin(rad);
        transformationMatrix[2,1] := -sin(rad);
        transformationMatrix[2,2] := cos(rad);
      end

      else if(axis = 'Y') then
      begin
        transformationMatrix[0,0] := cos(rad);
        transformationMatrix[0,2] := -sin(rad);
        transformationMatrix[2,0] := sin(rad);
        transformationMatrix[2,2] := cos(rad);
      end

      else if(axis = 'Z') then
      begin
        transformationMatrix[0,0] := cos(rad);
        transformationMatrix[0,1] := sin(rad);
        transformationMatrix[1,0] := -sin(rad);
        transformationMatrix[1,1] := cos(rad);
      end
      else Exit;

      ApplyTransformation(transformationMatrix);

    except
      on EConvertError do
        ShowMessage('Por favor, insira valores válidos para o eixo (X, Y ou Z) e para os graus de rotação (0 a 360).');
    end;
  end;

  if RadioButtonObjectCenter.Checked then
  begin
    try
      axis := UpCase(EditRotationAxis.Text);
      degree := StrToFloat(EditRotationDegrees.Text);
      if (degree < 0) or (degree > 360) then Exit;

      rad := degree*pi/180.0;

      cx := 0;
      cy := 0;
      cz := 0;
      for i := 0 to High(Vertices) do
      begin
        cx := cx + Vertices[i].X;
        cy := cy + Vertices[i].Y;
        cz := cz + Vertices[i].Z;
      end;
      cx := cx/(High(Vertices)+1);
      cy := cy/(High(Vertices)+1);
      cz := cz/(High(Vertices)+1);

      // translation to the center
      transformationMatrix := Default(TMatrix4x4);
      transformationMatrix[0,0] := 1.0;
      transformationMatrix[1,1] := 1.0;
      transformationMatrix[2,2] := 1.0;
      transformationMatrix[3,3] := 1.0;
      transformationMatrix[3,0] := -cx;
      transformationMatrix[3,1] := -cy;
      transformationMatrix[3,2] := -cz;

      Mtemp := transformationMatrix;

      // rotation
      transformationMatrix := Default(TMatrix4x4);
      transformationMatrix[0,0] := 1.0;
      transformationMatrix[1,1] := 1.0;
      transformationMatrix[2,2] := 1.0;
      transformationMatrix[3,3] := 1.0;

      if (axis = 'X') then
      begin
        transformationMatrix[1,1] := cos(rad);
        transformationMatrix[1,2] := sin(rad);
        transformationMatrix[2,1] := -sin(rad);
        transformationMatrix[2,2] := cos(rad);
      end
      else if (axis = 'Y') then
      begin
        transformationMatrix[0,0] := cos(rad);
        transformationMatrix[0,2] := -sin(rad);
        transformationMatrix[2,0] := sin(rad);
        transformationMatrix[2,2] := cos(rad);
      end
      else if (axis = 'Z') then
      begin
        transformationMatrix[0,0] := cos(rad);
        transformationMatrix[0,1] := sin(rad);
        transformationMatrix[1,0] := -sin(rad);
        transformationMatrix[1,1] := cos(rad);
      end
      else Exit;

      Mtemp := MultiplyMatrix(Mtemp, transformationMatrix);

      // translation
      transformationMatrix := Default(TMatrix4x4);
      transformationMatrix[0,0] := 1.0;
      transformationMatrix[1,1] := 1.0;
      transformationMatrix[2,2] := 1.0;
      transformationMatrix[3,3] := 1.0;
      transformationMatrix[3,0] := cx;
      transformationMatrix[3,1] := cy;
      transformationMatrix[3,2] := cz;

      Mtemp := MultiplyMatrix(Mtemp, transformationMatrix);
      ApplyTransformation(Mtemp);

    except
      on EConvertError do
        ShowMessage('Por favor, insira valores válidos para o eixo (X, Y ou Z) e para os graus de rotação (0 a 360).');
    end;
  end;

  if RadioButtonShearing.Checked then
  begin
    try
      a11 := StrToFloat(Edit_a11.Text); a12 := StrToFloat(Edit_a12.Text); a13 := StrToFloat(Edit_a13.Text); a14 := StrToFloat(Edit_a14.Text);
      a21 := StrToFloat(Edit_a21.Text); a22 := StrToFloat(Edit_a22.Text); a23 := StrToFloat(Edit_a23.Text); a24 := StrToFloat(Edit_a24.Text);
      a31 := StrToFloat(Edit_a31.Text); a32 := StrToFloat(Edit_a32.Text); a33 := StrToFloat(Edit_a33.Text); a34 := StrToFloat(Edit_a34.Text);
      a41 := StrToFloat(Edit_a41.Text); a42 := StrToFloat(Edit_a42.Text); a43 := StrToFloat(Edit_a43.Text); a44 := StrToFloat(Edit_a44.Text);

      transformationMatrix := Default(TMatrix4x4);
      transformationMatrix[0,0] := a11; transformationMatrix[0,1] := a12; transformationMatrix[0,2] := a13; transformationMatrix[0,3] := a14;
      transformationMatrix[1,0] := a21; transformationMatrix[1,1] := a22; transformationMatrix[1,2] := a23; transformationMatrix[1,3] := a24;
      transformationMatrix[2,0] := a31; transformationMatrix[2,1] := a32; transformationMatrix[2,2] := a33; transformationMatrix[2,3] := a34;
      transformationMatrix[3,0] := a41; transformationMatrix[3,1] := a42; transformationMatrix[3,2] := a43; transformationMatrix[3,3] := a44;

      ApplyTransformation(transformationMatrix);

    except
      on EConvertError do
        ShowMessage('Por favor, insira valores numéricos válidos para o shearing.');
    end;
  end;
end;

procedure TForm2.ButtonResetClick(Sender: TObject);
begin
  Image1.Canvas.Brush.Color := clWhite;
  Image1.Canvas.FillRect(Image1.ClientRect);
  Image1.Canvas.Pen.Color := clBlack;

  // 3D coordinates of the object
  Vertices[0].X := 0;   Vertices[0].Y := 0;   Vertices[0].Z := 0;   Vertices[0].W := 1;
  Vertices[1].X := 100; Vertices[1].Y := 0;   Vertices[1].Z := 0;   Vertices[1].W := 1;
  Vertices[2].X := 100; Vertices[2].Y := 100; Vertices[2].Z := 0;   Vertices[2].W := 1;
  Vertices[3].X := 0;   Vertices[3].Y := 100; Vertices[3].Z := 0;   Vertices[3].W := 1;
  Vertices[4].X := 0;   Vertices[4].Y := 0;   Vertices[4].Z := 100; Vertices[4].W := 1;
  Vertices[5].X := 100; Vertices[5].Y := 0;   Vertices[5].Z := 100; Vertices[5].W := 1;
  Vertices[6].X := 100; Vertices[6].Y := 100; Vertices[6].Z := 100; Vertices[6].W := 1;
  Vertices[7].X := 0;   Vertices[7].Y := 100; Vertices[7].Z := 100; Vertices[7].W := 1;
  Vertices[8].X := 50;  Vertices[8].Y := 150; Vertices[8].Z := 0;   Vertices[8].W := 1;
  Vertices[9].X := 50;  Vertices[9].Y := 150; Vertices[9].Z := 100; Vertices[9].W := 1;

  // z=0
  DrawLine(Vertices[0], Vertices[1]);
  DrawLine(Vertices[1], Vertices[2]);
  DrawLine(Vertices[3], Vertices[0]);

  // z=100
  DrawLine(Vertices[4], Vertices[5]);
  DrawLine(Vertices[5], Vertices[6]);
  DrawLine(Vertices[7], Vertices[4]);

  // Connect lines between the faces
  DrawLine(Vertices[0], Vertices[4]);
  DrawLine(Vertices[1], Vertices[5]);
  DrawLine(Vertices[2], Vertices[6]);
  DrawLine(Vertices[3], Vertices[7]);

  //roof
  DrawLine(Vertices[3], Vertices[8]);
  DrawLine(Vertices[2], Vertices[8]);
  DrawLine(Vertices[7], Vertices[9]);
  DrawLine(Vertices[6], Vertices[9]);
  DrawLine(Vertices[8], Vertices[9]);

  RedrawObject;
end;

end.

