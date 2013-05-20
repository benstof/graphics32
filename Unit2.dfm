object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 511
  ClientWidth = 762
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object dxRibbonStatusBar1: TdxRibbonStatusBar
    Left = 0
    Top = 488
    Width = 762
    Height = 23
    Panels = <>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clDefault
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ExplicitWidth = 724
  end
  object dxRibbon1: TdxRibbon
    Left = 0
    Top = 0
    Width = 762
    Height = 117
    BarManager = dxBarManager1
    ColorSchemeName = 'Blue'
    Style = rs2010
    Contexts = <>
    TabOrder = 3
    TabStop = False
    ExplicitWidth = 724
    object dxRibbon1Tab1: TdxRibbonTab
      Active = True
      Caption = 'Irrigation'
      Groups = <
        item
          ToolbarName = 'dxBarManager1Bar1'
        end
        item
          ToolbarName = 'dxBarManager1Bar2'
        end
        item
          ToolbarName = 'dxBarManager1Bar3'
        end>
      Index = 0
    end
    object dxRibbon1Tab2: TdxRibbonTab
      Caption = 'Cad'
      Groups = <
        item
          ToolbarName = 'dxBarManager1Bar1'
        end
        item
        end
        item
        end>
      Index = 1
    end
    object dxRibbon1Tab3: TdxRibbonTab
      Caption = 'Dtm'
      Groups = <>
      Index = 2
    end
  end
  object dxDockSite1: TdxDockSite
    Left = 0
    Top = 117
    Width = 762
    Height = 371
    Align = alClient
    ExplicitLeft = 55
    ExplicitTop = 123
    ExplicitWidth = 666
    ExplicitHeight = 359
    DockingType = 5
    OriginalWidth = 762
    OriginalHeight = 371
    object dxLayoutDockSite1: TdxLayoutDockSite
      Left = 0
      Top = 0
      Width = 762
      Height = 371
      DockingType = 0
      OriginalWidth = 300
      OriginalHeight = 200
    end
    object dxTabContainerDockSite1: TdxTabContainerDockSite
      Left = 0
      Top = 0
      Width = 762
      Height = 371
      ActiveChildIndex = 0
      AllowFloating = True
      AutoHide = False
      DockingType = 0
      OriginalWidth = 441
      OriginalHeight = 140
      object dxDockPanel2: TdxDockPanel
        Left = 0
        Top = 0
        Width = 758
        Height = 323
        AllowFloating = True
        AutoHide = False
        Caption = 'Reports'
        ExplicitWidth = 441
        ExplicitHeight = 116
        DockingType = 0
        OriginalWidth = 441
        OriginalHeight = 140
        object Panel1: TPanel
          Left = 0
          Top = 0
          Width = 98
          Height = 323
          Align = alLeft
          TabOrder = 0
          object Button5: TButton
            Left = 0
            Top = 0
            Width = 97
            Height = 33
            Caption = 'Pivots Report'
            TabOrder = 0
            OnClick = Button5Click
          end
          object Button6: TButton
            Left = 0
            Top = 32
            Width = 97
            Height = 33
            Caption = 'Pipes Report'
            TabOrder = 1
            OnClick = Button5Click
          end
          object Button7: TButton
            Left = 0
            Top = 64
            Width = 97
            Height = 33
            Caption = 'Nodes Report'
            TabOrder = 2
            OnClick = Button5Click
          end
        end
        object ScrollBox1: TScrollBox
          Left = 98
          Top = 0
          Width = 660
          Height = 323
          Align = alClient
          TabOrder = 1
          ExplicitLeft = 296
          ExplicitTop = 72
          ExplicitWidth = 281
          ExplicitHeight = 137
          object browser: TWebBrowser
            Left = 0
            Top = 0
            Width = 656
            Height = 319
            Align = alClient
            TabOrder = 0
            ExplicitLeft = 146
            ExplicitTop = 20
            ExplicitWidth = 539
            ExplicitHeight = 323
            ControlData = {
              4C000000DF3F00002D2100000000000000000000000000000000000000000000
              000000004C000000000000000000000001000000E0D057007335CF11AE690800
              2B2E126208000000000000004C0000000114020000000000C000000000000046
              8000000000000000000000000000000000000000000000000000000000000000
              00000000000000000100000000000000000000000000000000000000}
          end
        end
      end
      object dxDockPanel1: TdxDockPanel
        Left = 0
        Top = 0
        Width = 758
        Height = 323
        AllowFloating = True
        AutoHide = False
        Caption = 'dxDockPanel1'
        ExplicitWidth = 441
        ExplicitHeight = 116
        DockingType = 0
        OriginalWidth = 441
        OriginalHeight = 140
        object image: TImage32
          Left = 8
          Top = 48
          Width = 105
          Height = 147
          Bitmap.ResamplerClassName = 'TNearestResampler'
          BitmapAlign = baTopLeft
          Scale = 1.000000000000000000
          ScaleMode = smNormal
          TabOrder = 0
          OnMouseDown = imageMouseDown
          OnMouseMove = imageMouseMove
        end
        object Button1: TButton
          Left = 127
          Top = 104
          Width = 75
          Height = 25
          Caption = 'Button1'
          TabOrder = 1
          OnClick = Button1Click
        end
        object Button2: TButton
          Left = 240
          Top = 104
          Width = 75
          Height = 25
          Caption = 'Button2'
          TabOrder = 2
          OnClick = Button2Click
        end
        object Button3: TButton
          Left = 352
          Top = 104
          Width = 75
          Height = 25
          Caption = 'Button3'
          TabOrder = 3
          OnClick = Button3Click
        end
        object Button4: TButton
          Left = 363
          Top = 104
          Width = 75
          Height = 25
          Caption = 'Button4'
          TabOrder = 4
        end
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 544
    Top = 48
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 608
    Top = 48
  end
  object dxBarManager1: TdxBarManager
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Categories.Strings = (
      'Default')
    Categories.ItemsVisibles = (
      2)
    Categories.Visibles = (
      True)
    PopupMenuLinks = <>
    UseSystemFont = True
    Left = 8
    Top = 136
    DockControlHeights = (
      0
      0
      0
      0)
    object dxBarManager1Bar1: TdxBar
      Caption = 'Nodes'
      CaptionButtons = <>
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 750
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton2'
        end
        item
          Visible = True
          ItemName = 'dxBarButton1'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object dxBarManager1Bar2: TdxBar
      Caption = 'Pipes'
      CaptionButtons = <>
      DockedLeft = 48
      DockedTop = 0
      FloatLeft = 750
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton2'
        end
        item
          Visible = True
          ItemName = 'dxBarButton1'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object dxBarManager1Bar3: TdxBar
      Caption = 'Pivots'
      CaptionButtons = <>
      DockedLeft = 96
      DockedTop = 0
      FloatLeft = 750
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton2'
        end
        item
          Visible = True
          ItemName = 'dxBarButton1'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object dxBarButton1: TdxBarButton
      Caption = 'Add'
      Category = 0
      Hint = 'Add'
      Visible = ivAlways
    end
    object dxBarButton2: TdxBarButton
      Caption = 'Delete'
      Category = 0
      Hint = 'Delete'
      Visible = ivAlways
    end
  end
end
