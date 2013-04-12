object FormLineStippling: TFormLineStippling
  Left = 216
  Top = 109
  Caption = 'Line Stippling Example'
  ClientHeight = 636
  ClientWidth = 744
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage32
    Left = 8
    Top = 8
    Width = 200
    Height = 200
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
  end
  object ScrollBar: TScrollBar
    Left = 224
    Top = 8
    Width = 16
    Height = 201
    Kind = sbVertical
    Min = 1
    PageSize = 0
    Position = 50
    TabOrder = 1
    OnChange = ScrollBarChange
  end
  object PaintBox321: TPaintBox32
    Left = 288
    Top = 192
    Width = 281
    Height = 233
    TabOrder = 2
  end
  object Button1: TButton
    Left = 288
    Top = 464
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 3
    OnClick = Button1Click
  end
end
