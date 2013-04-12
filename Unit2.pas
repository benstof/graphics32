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
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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

procedure TForm1.Button1Click(Sender: TObject);
var points : TArrayofFixedPoint;
p1, p2 : TFixedPoint;

begin

   Polygon1.NewLine;
   Polygon1.Add(GR32.FixedPoint(10, 10));
   Polygon1.Add(GR32.FixedPoint(500, 500));

  Image.Bitmap.BeginUpdate;

  try
    //Image.Bitmap.Clear(clWhite32);
    Polygon1.DrawEdge(Image.Bitmap, SetAlpha(clBlack32, 255));

  finally
    Image.Bitmap.EndUpdate;
  end;

  Image.Bitmap.Changed;
  Image.Refresh; // force repaint


end;

procedure TForm1.Button2Click(Sender: TObject);
var i, j : integer;
begin

{
   Polygon1.Points[1][0].X := 500;// (GR32.FixedPoint(500, 10));
   Polygon1.Points[1][0].Y := 10;// (GR32.FixedPoint(500, 10));
}

     L.Location :=
     FloatRect(
     L.Location.Left + 10,
     L.Location.Top + 10,
     L.Location.Right + 10,
     L.Location.Bottom + 10);

    i := random(100);
    j := random(100);
    //Polygon2.Add(GR32.FixedPoint(i,j));

   {Polygon2.Points[1][0].X := Polygon2.Points[1][0].X + 1000;// (GR32.FixedPoint(500, 10));
   Polygon2.Points[1][0].Y := Polygon2.Points[1][0].Y + 1000;// (GR32.FixedPoint(500, 10));
   Polygon2.Points[1][1].X := Polygon2.Points[1][1].X + 1000;// (GR32.FixedPoint(500, 10));
   Polygon2.Points[1][1].Y := Polygon2.Points[1][1].Y + 1000;// (GR32.FixedPoint(500, 10)); }


   {x := x + 10;
   y := y + 10; }

  { Polygon2.Clear;
   Polygon2.NewLine;
   Polygon2.Add(GR32.FixedPoint(0, 0));
   Polygon2.Add(GR32.FixedPoint(x,y));

   Polygon2.Draw(Buffer,clBlack32,clRed32); }


end;

procedure TForm1.PaintSimpleDrawingHandler(Sender: TObject; Buffer: TBitmap32);
var
  Cx, Cy: Single;
  W2, H2: Single;
  I, j, len: Integer;
begin


 // if Sender is TPositionedLayer then

  if TPositionedLayer(Sender).tag = 1 then
  begin
    with TPositionedLayer(Sender).GetAdjustedLocation do
    begin
      W2 := (Right - Left) * 0.5;
      H2 := (Bottom - Top) * 0.5;
      Cx := Left + W2;
      Cy := Top + H2;

      Buffer.PenColor := clRed32;

      {Buffer.MoveToF(Cx,Cy);
      for I := 0 to 240 do
      begin
        Buffer.LineToFS(Cx + W2 * I / 200 * Cos(I / 8), Cy + H2 * I / 200 * Sin(I / 8));
      end;}
      if length(nl.points) > 0 then
      begin
         len := length(nl.points) - 1;
      from_x := nl.points[len].x;
      from_y := nl.points[len].y;

      Buffer.MoveToF(from_x,from_y);
      Buffer.LineToFS(Cx, Cy);
      end;

     end;
  end;


  if TPositionedLayer(Sender).tag = 0 then
  begin

      if length(nl.points) > 0 then
      begin

         for I := 0 to length(nl.points) - 1 do
         begin
            Buffer.MoveToF(nl.points[i].x,nl.points[i].y);
            Buffer.LineToFS(nl.points[i+1].x,nl.points[i+1].y);
         end;

      end;

  end;




end;

procedure TForm1.FormCreate(Sender: TObject);
var points : TArrayofFixedPoint;
p1, p2 : TFixedPoint;
P: TPoint;



begin

  {x := 0;
  y := 0;

  Polygon1 := TPolygon32.Create;
  Polygon1.Closed := false;
  Polygon1.Antialiased := true; //cbAntialiased.Checked;
  Polygon1.AntialiasMode := am32times; //TAntialiasMode(rgAntialiasMode.ItemIndex);


  Polygon2 := TPolygon32.Create;
  Polygon2.Closed := false;
  Polygon2.Antialiased := true; //cbAntialiased.Checked;
  Polygon2.AntialiasMode := am32times; //TAntialiasMode(rgAntialiasMode.ItemIndex);

   Polygon2.NewLine;
   Polygon2.Add(GR32.FixedPoint(10, 10));
   Polygon2.Add(GR32.FixedPoint(50, 50));    }

   {setlength(nl.points, 2);
   nl.points[0].x := 10;
   nl.points[0].y := 10;
   nl.points[1].x := 100;
   nl.points[1].y := 100; }


   Image.SetupBitmap;
   Image.Bitmap.Clear(clWhite32);

   from_x :=0;
   from_y :=0;
   to_x :=0;
   to_y :=0;

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
   from_x := x;
   from_y := y;

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
