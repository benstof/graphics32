unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  gr32, GR32_Polygons, gr32_layers, GR32_Image,
  gr32_resamplers, Vcl.StdCtrls, Vcl.ExtDlgs, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxRibbonSkins, cxClasses, dxRibbon,
  dxBar, dxRibbonGallery, dxRibbonBackstageView, dxRibbonMiniToolbar,
  dxStatusBar, dxRibbonStatusBar, Vcl.ButtonGroup, Vcl.ToolWin, Vcl.ActnMan,
  Vcl.ActnCtrls, MSHTML,Vcl.Ribbon, ActiveX, Vcl.RibbonLunaStyleActnCtrls, Vcl.OleCtrls, SHDocVw,
  cxPC, dxDockControl, dxDockPanel, Vcl.ExtCtrls;


const

HTMLStr: AnsiString =
'<html> '+
'<head> '+
'<script type="text/javascript"> '+
'  function setText(title, text) { '+


'}'+

'</script>'+

'</head> '+
''+
'<body  scroll=no onload="" style="color:#000; font-family:arial; background-color:#fff; padding:0px; margin:0px; border:0; "> '+

'<table border="0" bordercolor="#fff" style="background-color:#FFF" width="100%" cellpadding="3" cellspacing="3">      '+
'	<tr>' +
'		<td><b>Irrigation Filename:<b></td>  '+
'	</tr>                              '+
'	<tr>                                 '+
'		<td><b>List of Nodes & Pipes</b></td>                '+
'	</tr>                                 '+
'	<tr>                                   '+
'		<td><b>Date</b>: 20/5/2013    <b>Time:</b> 14:13</td>                   '+
'	</tr>                                    '+
'	<tr>                                     '+
'		<td>Materials for BLOCK # 0</td>                    '+
'	</tr>                                    '+
'	<tr>                                     '+
'		<td>Please note, information regarding the block valves are shown in the mainline'+
 'and also repeated in the individual blocks.</td>                    '+
'	</tr>                                    '+
'	<tr>                                     '+
'		<td><b>Summary of total nodes</b> </td>                    '+
'	</tr>                                    '+
'	<tr>                                     '+
'		<td>HYDRO PC 16MM X 2.2L/H X 0.6M Nozzle     #       8</td>                     '+
'	</tr>                                    '+
'	<tr>                                     '+
'		<td><b>Detail of node sizes with OD pipe sizes</b></td>                    '+
'	</tr>                                    '+
'	<tr>                                     '+
'		<td>HYDRO PC 16MM X 2.2L/H X 0.6M Nozzle     #      8 > </td>                    '+
'	</tr>                                    '+
'	<tr>                                     '+
'		<td>PIPES                                          OD           Total length m</td>                    '+
'	</tr>                                    '+
'</table> '+

'</body> '+
'</html> ';

type
  coords = record
    x,y : single;
  end;

  Line = object
    points : array of coords;
  end;

  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    OpenPictureDialog: TOpenPictureDialog;
    dxRibbonStatusBar1: TdxRibbonStatusBar;
    dxBarManager1: TdxBarManager;
    dxBarManager1Bar1: TdxBar;
    dxBarButton1: TdxBarButton;
    dxRibbon1: TdxRibbon;
    dxRibbon1Tab1: TdxRibbonTab;
    dxRibbon1Tab2: TdxRibbonTab;
    dxRibbon1Tab3: TdxRibbonTab;
    dxBarManager1Bar2: TdxBar;
    dxBarManager1Bar3: TdxBar;
    dxBarButton2: TdxBarButton;
    dxDockSite1: TdxDockSite;
    dxDockPanel1: TdxDockPanel;
    dxDockPanel2: TdxDockPanel;
    dxTabContainerDockSite1: TdxTabContainerDockSite;
    browser: TWebBrowser;
    image: TImage32;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    dxLayoutDockSite1: TdxLayoutDockSite;
    Panel1: TPanel;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure PaintSimpleDrawingHandler(Sender: TObject; Buffer: TBitmap32);
    procedure imageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure imageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
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
      I: TBitmapLayer;

    property Selection: TPositionedLayer read FSelection write SetSelection;
  end;

var
  Form1: TForm1;
  nl : Line;
  rand : array[0..10000] of tpoint;
  HTMLWindow2: IHTMLWindow2;

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
var
  B: TBitmap32;
  W, H: Integer;
begin
  { deselect everything }
 { Selection := nil;
  W := image.Bitmap.Width;
  H := image.Bitmap.Height;

  B := TBitmap32.Create;
  try
    B.SetSize(W, H);
    image.PaintTo(B, Rect(0, 0, W, H));

    image.Layers.Delete(0);//.Clear;
    image.Bitmap := B;
  finally
    B.Free;
  end;  }
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  P: TPoint;
  W, H: Single;
begin

  {with OpenPictureDialog do
    if Execute then
    begin  }
      I := TBitmapLayer.Create(image.Layers);
      with I do
      try
        Bitmap.LoadFromFile('sunset.jpg');
        Bitmap.DrawMode := dmBlend;

        with image.GetViewportRect do
          P := image.ControlToBitmap(GR32.Point((Right + Left) div 2, (Top + Bottom) div 2));

        W := Bitmap.Width * 0.5;
        H := Bitmap.Height * 0.5;

      //  with image.Bitmap do
          I.Location := GR32.FloatRect(P.X - W, P.Y - H, P.X + W, P.Y + H);

        Scaled := True;

        //OnMouseDown := LayerMouseDown;
      except
        Free;
        raise;
      end;
      Selection := B;

    //end;

end;

procedure TForm1.Button3Click(Sender: TObject);
var
  L: TFloatRect;
  b : TBitmap32;
begin

   image.SetupBitmap(true, clwhite32);
   b := image.Bitmap;

   b.Clear($FFFFFFFF);
   b.DrawMode := dmOpaque;
   b.Canvas.Font.Name := 'verdana';
   b.Canvas.Font.Color := $00000000;
   b.Canvas.Font.Size := 30;
   b.Canvas.Textout(100,100, 'kllkjljl');

end;

procedure TForm1.Button5Click(Sender: TObject);
begin

   //opopo

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

      Buffer.PenColor := clgreen32;

     // from_x:=X;
     // from_y:=y;

      for I := 0 to 10 do
      begin
         Buffer.PenColor:=clgreen32+i*200;
         Buffer.MoveToF(i+1+from_x,from_y-100);
         Buffer.LineToFS(i+1+from_x, 100+from_y);
      end;


    {  if length(nl.points) > 0 then
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
      Buffer.PenColor := clred32;
      for I := 0 to 9999 do
      begin
         Buffer.PenColor := clred32+i;
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

var
   aStream     : TMemoryStream;
begin


  browser.Navigate('about:blank');
    if Assigned(browser.Document) then
    begin
      aStream := TMemoryStream.Create;
      try
         aStream.WriteBuffer(Pointer(HTMLStr)^, Length(HTMLStr));
         //aStream.Write(HTMLStr[1], Length(HTMLStr));
         aStream.Seek(0, soFromBeginning);
         (browser.Document as IPersistStreamInit).Load(TStreamAdapter.Create(aStream));
          //maps_browser.OleObject.Document.Body.Style.OverflowX := 'hidden';

      finally
         aStream.Free;
      end;
      HTMLWindow2 := (browser.Document as IHTMLDocument2).parentWindow;
    end;

   //BarButtonItem bAdd = new BarButtonItem();
   dxBarButton1.Name := 'bStuff';
  // dxBarButton1.Content = "stuff";
  // dxBarButton1.Glyph = bmp;

  // RibbonGroup1.Items.Add;

 {     for I := 0 to 9999 do
      begin
         rn := Random(800);
         rnn := Random(800);
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

      B := TPositionedLayer.Create(Image.Layers);
  B.Location := FloatRect(Image.GetViewportRect);
  B.Scaled := True;
  B.MouseEvents := True;
  //L.OnMouseDown := LayerMouseDown;

  B.OnPaint := PaintSimpleDrawingHandler;
  B.Tag := 0;
  B.Update;

  L := TPositionedLayer.Create(Image.Layers);
  L.Location := FloatRect(P.X - 32, P.Y - 32, P.X + 32, P.Y + 32);
  L.Scaled := True;
  L.MouseEvents := True;
  //L.OnMouseDown := LayerMouseDown;

  L.OnPaint := PaintSimpleDrawingHandler;
  L.Tag := 1;
  //Selection := L;  }







end;


procedure TForm1.imageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  var len : integer;
begin

 {  len := length(nl.points);

   setlength(nl.points, len+1);
   nl.points[len].x := x;
   nl.points[len].y := y;

   nl.points[0].x := x;
   nl.points[0].y := y; }


end;

procedure TForm1.imageMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer; Layer: TCustomLayer);
begin

    { L.Location := FloatRect(x - 32,y -32,x+32,y+32);

     from_x:=x;
     from_y:=y;  }


end;

end.
