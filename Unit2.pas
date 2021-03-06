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
  cxPC, dxDockControl, dxDockPanel, Vcl.ExtCtrls	;


const

HTMLStr: AnsiString =
'<!DOCTYPE html> '+
'<head> '+
'<script src="http://www.google.com/jsapi" type="text/javascript"></script> '+
'<script type="text/javascript">google.load("jquery", "1.3.2");</script>  '+
'<script type="text/javascript"> '+

'  function setText(title, text) { '+

' $("body").html(title); '+

'}'+

'</script>'+

'</head> '+
''+
'<body  scroll=no onload="" style="color:#000; font-family:arial; background-color:#fff; padding:0px; margin:0px; border:0; "> '+
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
    ScrollBox1: TScrollBox;
    browser: TWebBrowser;
    dxBarManager1Bar4: TdxBar;
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
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
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


procedure CallFoo(S: string; I: Integer);
  { Calls JavaScript foo() function }
var
  Doc: IHTMLDocument2;      // current HTML document
  HTMLWindow: IHTMLWindow2; // parent window of current HTML document
  JSFn: string;             // stores JavaScipt function call
begin
  // Get reference to current document
  Doc := Form1.Browser.Document as IHTMLDocument2;
  if not Assigned(Doc) then
    Exit;
  // Get parent window of current document
  HTMLWindow := Doc.parentWindow;
  if not Assigned(HTMLWindow) then
    Exit;
  // Run JavaScript
  try
    JSFn := Format('setText("%s",%d)', [S, I]);  // build function call
    HTMLWindow.execScript(JSFn, 'JavaScript'); // execute function
  except
    // handle exception in case JavaScript fails to run
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
var report : string;
x : integer;
bgc : string;
begin

  report := '<div style=''width:500px; font-family:arial; border:0px solid #000; padding : 10px; background-color:#f7f7f7;''>';
   report := report + '<div style=''font-size:24px; font-family:arial; font-weight:bolder; color:#58595B''>List of Pivots</div>';
   report := report + '<div style=''border-bottom:1px dashed #c6c6c6; margin-bottom:20px; padding-bottom:20px;''>';
   report := report + '<span style=''font-size:14px; color:#6d6e71; font-family:arial;''>22/5/13 | 10:55am</span></div>';

   report := report + '<div><span style=''font-size:16px; font-family:arial; color:#58595B''>Materials for Block</span></div>';
   report := report + '<div style=''border-bottom:1px dashed #c6c6c6; margin-bottom:20px; color:#6d6e71;  padding-bottom:20px;''>';
   report := report + '<span style=''font-size:11px; font-family:arial;''>Please note information regarding block valves are shown in the mainline and also repeated in the individual blocsk.</span></div>';

   report := report + '<div style=''margin-bottom:20px;''><span style=''font-size:16px;  font-weight:bold; font-family:arial;  color:#58595B''>Summary of total Nodes</span></div>';

   for x := 1  to 3 do
   begin

      if x mod 2 = 0 then bgc := 'f1f2f2' else bgc := 'e6e7e8';

      report := report + '<div style=''background-color:#'+bgc+'; color:#6d6e71; padding: 5px; font-size:14px;''><span style=''float:left;margin-right:250px;''>Spillhouse 80/20 55Kw Pump</span>';
      report := report + '<span style=''float:rght; font-weight:bold; margin-right:15px;''>'+inttostr(x)+'</span>';
      report := report + '</div>';

   end;

   report := report + '<div style=''border-bottom:1px dashed #c6c6c6; margin-bottom:20px; padding-bottom:20px;''><span style=''font-size:11px; font-family:arial;''></span></div>';


   report := report + '<div style=''margin-bottom:20px;''><span style=''font-size:16px;  font-weight:bold; font-family:arial;  color:#58595B''>Detail of node sizes with OD pipe sizes</span></div>';

   for x := 1  to 3 do
   begin

      if x mod 2 = 0 then bgc := 'f1f2f2' else bgc := 'e6e7e8';

      report := report + '<div style=''background-color:#'+bgc+'; color:#6d6e71; padding: 5px; font-size:14px;''><span style=''float:left;margin-right:250px;''>Spillhouse 80/20 55Kw Pump</span>';
      report := report + '<span style=''float:rght; font-weight:bold; margin-right:15px;''>'+inttostr(x)+'</span>';
      report := report + '</div>';

   end;
   report := report + '<div style=''border-bottom:1px dashed #c6c6c6; margin-bottom:20px; padding-bottom:20px;''><span style=''font-size:11px; font-family:arial;''></span></div>';


   report := report + '<div style=''margin-bottom:20px; padding-bottom:20px;''>';

   report := report + '<span style=''font-size:11px; float:left; font-family:arial; border-bottom:0px dashed #c6c6c6; margin-right:10px; margin-top:11px; width:78%;''></span><span style=''font-size:20px; font-weight:bold; color:#58595B;''>Total: 26</span>';

   report := report + '</div></div>';



   CallFoo(report,2);
end;

procedure TForm1.Button6Click(Sender: TObject);
var report : string;
x : integer;
bgc : string;
begin
report := '<div style=''width:500px; font-family:arial; border:0px solid #000; padding : 10px; background-color:#f7f7f7;''>';
   report := report + '<div style=''font-size:24px; font-family:arial; font-weight:bolder; color:#58595B''>List of Pipes</div>';
   report := report + '<div style=''border-bottom:1px dashed #c6c6c6; margin-bottom:20px; padding-bottom:20px;''>';
   report := report + '<span style=''font-size:14px; color:#6d6e71; font-family:arial;''>22/5/13 | 10:55am</span></div>';

   report := report + '<div><span style=''font-size:16px; font-family:arial; color:#58595B''>Materials for Block</span></div>';
   report := report + '<div style=''border-bottom:1px dashed #c6c6c6; margin-bottom:20px; color:#6d6e71;  padding-bottom:20px;''>';
   report := report + '<span style=''font-size:11px; font-family:arial;''>Please note information regarding block valves are shown in the mainline and also repeated in the individual blocsk.</span></div>';

   report := report + '<div style=''margin-bottom:20px;''><span style=''font-size:16px;  font-weight:bold; font-family:arial;  color:#58595B''>Summary of total Nodes</span></div>';

   for x := 1  to 3 do
   begin

      if x mod 2 = 0 then bgc := 'f1f2f2' else bgc := 'e6e7e8';

      report := report + '<div style=''background-color:#'+bgc+'; color:#6d6e71; padding: 5px; font-size:14px;''><span style=''float:left;margin-right:250px;''>Spillhouse 80/20 55Kw Pump</span>';
      report := report + '<span style=''float:rght; font-weight:bold; margin-right:15px;''>'+inttostr(x)+'</span>';
      report := report + '</div>';

   end;

   report := report + '<div style=''border-bottom:1px dashed #c6c6c6; margin-bottom:20px; padding-bottom:20px;''><span style=''font-size:11px; font-family:arial;''></span></div>';


   report := report + '<div style=''margin-bottom:20px;''><span style=''font-size:16px;  font-weight:bold; font-family:arial;  color:#58595B''>Detail of node sizes with OD pipe sizes</span></div>';

   for x := 1  to 3 do
   begin

      if x mod 2 = 0 then bgc := 'f1f2f2' else bgc := 'e6e7e8';

      report := report + '<div style=''background-color:#'+bgc+'; color:#6d6e71; padding: 5px; font-size:14px;''><span style=''float:left;margin-right:250px;''>Spillhouse 80/20 55Kw Pump</span>';
      report := report + '<span style=''float:rght; font-weight:bold; margin-right:15px;''>'+inttostr(x)+'</span>';
      report := report + '</div>';

   end;
   report := report + '<div style=''border-bottom:1px dashed #c6c6c6; margin-bottom:20px; padding-bottom:20px;''><span style=''font-size:11px; font-family:arial;''></span></div>';


   report := report + '<div style=''margin-bottom:20px; padding-bottom:20px;''>';

   report := report + '<span style=''font-size:11px; float:left; font-family:arial; border-bottom:0px dashed #c6c6c6; margin-right:10px; margin-top:11px; width:78%;''></span><span style=''font-size:20px; font-weight:bold; color:#58595B;''>Total: 26</span>';

   report := report + '</div></div>';



   CallFoo(report,2);
end;

procedure TForm1.Button7Click(Sender: TObject);
var report : string;
x : integer;
bgc : string;
begin

   report := '<div style=''width:500px; font-family:arial; border:0px solid #000; padding : 10px; background-color:#f7f7f7;''>';
   report := report + '<div style=''font-size:24px; font-family:arial; font-weight:bolder; color:#58595B''>List of Nodes and Pipes</div>';
   report := report + '<div style=''border-bottom:1px dashed #c6c6c6; margin-bottom:20px; padding-bottom:20px;''>';
   report := report + '<span style=''font-size:14px; color:#6d6e71; font-family:arial;''>22/5/13 | 10:55am</span></div>';

   report := report + '<div><span style=''font-size:16px; font-family:arial; color:#58595B''>Materials for Block</span></div>';
   report := report + '<div style=''border-bottom:1px dashed #c6c6c6; margin-bottom:20px; color:#6d6e71;  padding-bottom:20px;''>';
   report := report + '<span style=''font-size:11px; font-family:arial;''>Please note information regarding block valves are shown in the mainline and also repeated in the individual blocsk.</span></div>';

   report := report + '<div style=''margin-bottom:20px;''><span style=''font-size:16px;  font-weight:bold; font-family:arial;  color:#58595B''>Summary of total Nodes</span></div>';

   for x := 1  to 3 do
   begin

      if x mod 2 = 0 then bgc := 'f1f2f2' else bgc := 'e6e7e8';

      report := report + '<div style=''background-color:#'+bgc+'; color:#6d6e71; padding: 5px; font-size:14px;''><span style=''float:left;margin-right:250px;''>Spillhouse 80/20 55Kw Pump</span>';
      report := report + '<span style=''float:rght; font-weight:bold; margin-right:15px;''>'+inttostr(x)+'</span>';
      report := report + '</div>';

   end;

   report := report + '<div style=''border-bottom:1px dashed #c6c6c6; margin-bottom:20px; padding-bottom:20px;''><span style=''font-size:11px; font-family:arial;''></span></div>';


   report := report + '<div style=''margin-bottom:20px;''><span style=''font-size:16px;  font-weight:bold; font-family:arial;  color:#58595B''>Detail of node sizes with OD pipe sizes</span></div>';

   for x := 1  to 3 do
   begin

      if x mod 2 = 0 then bgc := 'f1f2f2' else bgc := 'e6e7e8';

      report := report + '<div style=''background-color:#'+bgc+'; color:#6d6e71; padding: 5px; font-size:14px;''><span style=''float:left;margin-right:250px;''>Spillhouse 80/20 55Kw Pump</span>';
      report := report + '<span style=''float:rght; font-weight:bold; margin-right:15px;''>'+inttostr(x)+'</span>';
      report := report + '</div>';

   end;
   report := report + '<div style=''border-bottom:1px dashed #c6c6c6; margin-bottom:20px; padding-bottom:20px;''><span style=''font-size:11px; font-family:arial;''></span></div>';


   report := report + '<div style=''margin-bottom:20px; padding-bottom:20px;''>';

   report := report + '<span style=''font-size:11px; float:left; font-family:arial; border-bottom:0px dashed #c6c6c6; margin-right:10px; margin-top:11px; width:78%;''></span><span style=''font-size:20px; font-weight:bold; color:#58595B;''>Total: 26</span>';

   report := report + '</div></div>';

   CallFoo(report,2);
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

   aStream     : TMemoryStream;
   exePath, htmPath : string;

const
 LOCAL_PAGE ='C:/report.htm';

begin

  exePath := ExtractFilePath(Application.ExeName);
  htmPath := exePath + 'report.html';

 // browser.Navigate( 'file://' + GetCurrentDir + '/report.htm' );
  //browser.Navigate(htmPath);

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
 //  dxBarButton1.Name := 'bStuff';
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
