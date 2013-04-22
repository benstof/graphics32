unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  gr32, GR32_Polygons, gr32_layers, GR32_Image, Vcl.StdCtrls;

type

  coords = record
    x,y : single;
  end;

  Line = object
    points : array of coords;
  end;

  TForm1 = class(TForm)
    image: TImage32;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure PaintSimpleDrawingHandler(Sender: TObject; Buffer: TBitmap32);
    procedure imageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure imageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  private
    { Private declarations }
    Polygon1: TPolygon32;
    Polygon2: TPolygon32;
    L, B: TPositionedLayer;
    FSelection: TPositionedLayer;
    procedure SetSelection(Value: TPositionedLayer);
  public
    { Public declarations }
    x,y : integer;
    from_x, from_y, to_x, to_y : single;

    property Selection: TPositionedLayer read FSelection write SetSelection;
  end;

var
  Form1: TForm1;
  nl : Line;
  rand : array[0..10000] of tpoint;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses

{$IFDEF Darwin}
  MacOSAll,
{$ENDIF}
{$IFNDEF FPC}
  JPEG;
{$ELSE}
  LazJPG;
{$ENDIF}

procedure TForm1.SetSelection(Value: TPositionedLayer);
begin

end;

procedure TForm1.Button2Click(Sender: TObject);
var i, j : integer;
begin

end;

procedure TForm1.PaintSimpleDrawingHandler(Sender: TObject; Buffer: TBitmap32);
var
  Cx, Cy: Single;
  W2, H2: Single;
  I, j, len, rn, rnn : Integer;
begin

 // if Sender is TPositionedLayer then

  // Rubber Layer
  if TPositionedLayer(Sender).tag = 1 then
  begin
    with TPositionedLayer(Sender).GetAdjustedLocation do
    begin
      W2 := (Right - Left) * 0.5;
      H2 := (Bottom - Top) * 0.5;
      Cx := Left + W2;
      Cy := Top + H2;

      Buffer.PenColor := clRed32;

      for I := 0 to 100000 do
      begin
         Buffer.MoveToF(i+1,10);
         Buffer.LineToFS(i+1, cy);
      end;


      {if length(nl.points) > 0 then
      begin

         len := length(nl.points) - 1;
         from_x := nl.points[len].x;
         from_y := nl.points[len].y;

         // Teken rubber lyn van laaste punt van lyn
         Buffer.MoveToF(from_x,from_y);
         Buffer.LineToFS(Cx, Cy);

      end;}

     end;
  end;

  // Background Layer
  if TPositionedLayer(Sender).tag = 0 then
  begin

      for I := 0 to 9999 do
      begin
         Buffer.MoveToF(rand[i].x,rand[i].y);
         Buffer.LineToFS(rand[i+1].x,rand[i+1].y);
      end;

      // Teken al die lyne
      {if length(nl.points) > 1 then
      begin

         for I := 0 to length(nl.points) - 2 do
         begin
            Buffer.MoveToF(nl.points[i].x,nl.points[i].y);
            Buffer.LineToFS(nl.points[i+1].x,nl.points[i+1].y);
         end;

      end; }

  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
var points : TArrayofFixedPoint;
p1, p2 : TFixedPoint;
P: TPoint;
rn, rnn,i : integer;

begin

      for I := 0 to 9999 do
      begin
         rn := Random(10000);
         rnn := Random(10000);
         rand[i].X := rnn;
         rand[i].y := rn;

         rand[i+1].X := rnn;
         rand[i+1].y := rn;

        // Buffer.MoveToF(rnn,rn);
        // Buffer.LineToFS(rn, rnn);
      end;

   Image.SetupBitmap;
   Image.Bitmap.Clear(clWhite32);

  // get coordinates of the center of viewport
  with Image.GetViewportRect do
    P := Image.ControlToBitmap(GR32.Point((Right + Left) div 2, (Top + Bottom) div 2));

  L := TPositionedLayer.Create(Image.Layers);
  L.Location := FloatRect(P.X - 32, P.Y - 32, P.X + 32, P.Y + 32);
  L.Scaled := True;
  L.MouseEvents := True;
  //L.OnMouseDown := LayerMouseDown;

  L.OnPaint := PaintSimpleDrawingHandler;
  L.Tag := 1;
  //Selection := L;

  B := TPositionedLayer.Create(Image.Layers);
  B.Location := FloatRect(Image.GetViewportRect);
  B.Scaled := True;
  B.MouseEvents := True;
  //L.OnMouseDown := LayerMouseDown;

  B.OnPaint := PaintSimpleDrawingHandler;
  B.Tag := 0;
  B.Update;

end;


procedure TForm1.imageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  var len : integer;
begin

   len := length(nl.points);

   setlength(nl.points, len+1);
   nl.points[len].x := x;
   nl.points[len].y := y;

end;

procedure TForm1.imageMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer; Layer: TCustomLayer);
begin

     L.Location := FloatRect(x - 32,y -32,x+32,y+32);

end;

end.
