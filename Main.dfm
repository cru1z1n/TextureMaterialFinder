object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Marks Texture Material Finder'
  ClientHeight = 928
  ClientWidth = 1095
  Color = clMedGray
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  StyleName = 'Windows'
  OnCreate = FormCreate
  DesignSize = (
    1095
    928)
  TextHeight = 15
  object Label3: TLabel
    Left = 88
    Top = 31
    Width = 125
    Height = 27
    Caption = 'Found Textures'
    Color = clRed
    Font.Charset = ANSI_CHARSET
    Font.Color = clYellow
    Font.Height = -20
    Font.Name = 'Rockwell Nova Cond'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object SelectedImage: TImage
    Left = 760
    Top = 96
    Width = 305
    Height = 372
    Center = True
    Proportional = True
    Stretch = True
  end
  object Label1: TLabel
    Left = 88
    Top = 506
    Width = 116
    Height = 27
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Linked rvmats'
    Color = clRed
    Font.Charset = ANSI_CHARSET
    Font.Color = clYellow
    Font.Height = -20
    Font.Name = 'Rockwell Nova Cond'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Label2: TLabel
    Left = 142
    Top = 469
    Width = 44
    Height = 28
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Filter'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 440
    Top = 31
    Width = 111
    Height = 27
    Caption = 'Found rvMats'
    Color = clRed
    Font.Charset = ANSI_CHARSET
    Font.Color = clYellow
    Font.Height = -20
    Font.Name = 'Rockwell Nova Cond'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Label5: TLabel
    Left = 824
    Top = 31
    Width = 163
    Height = 27
    Caption = 'Selected Image Path'
    Color = clRed
    Font.Charset = ANSI_CHARSET
    Font.Color = clYellow
    Font.Height = -20
    Font.Name = 'Rockwell Nova Cond'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Label6: TLabel
    Left = 467
    Top = 506
    Width = 159
    Height = 27
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Selected rvmat Path'
    Color = clRed
    Font.Charset = ANSI_CHARSET
    Font.Color = clYellow
    Font.Height = -20
    Font.Name = 'Rockwell Nova Cond'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object ListBoxResults: TListBox
    Left = 88
    Top = 536
    Width = 364
    Height = 384
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clGray
    ItemHeight = 15
    TabOrder = 0
    OnClick = ListBoxResultsClick
  end
  object TreeViewPngFiles: TTreeView
    Left = 88
    Top = 64
    Width = 289
    Height = 404
    CheckStyles = [csExclusion]
    Color = clTeal
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    HotTrack = True
    Indent = 19
    ParentFont = False
    SortType = stText
    TabOrder = 1
    OnChange = TreeViewPngFilesChange
  end
  object Memo1: TMemo
    Left = 467
    Top = 568
    Width = 537
    Height = 352
    Anchors = [akTop, akRight, akBottom]
    Color = clGray
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object ImageFilePath: TEdit
    Left = 760
    Top = 64
    Width = 305
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    BorderStyle = bsNone
    Color = clMedGray
    ReadOnly = True
    TabOrder = 3
    StyleElements = []
    OnDblClick = ImageFilePathDblClick
  end
  object TreeFilter: TEdit
    Left = 197
    Top = 474
    Width = 121
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Color = clMedGray
    TabOrder = 4
    Text = 'TreeFilter'
    OnChange = TreeFilterChange
  end
  object ApplyFilter: TButton
    Left = 324
    Top = 472
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'ApplyFilter'
    TabOrder = 5
    OnClick = ApplyFilterClick
  end
  object RVMatFilePath: TEdit
    Left = 467
    Top = 536
    Width = 537
    Height = 26
    Anchors = [akTop, akRight]
    BorderStyle = bsNone
    Color = clMedGray
    ReadOnly = True
    TabOrder = 6
  end
  object TreeViewRVmats: TTreeView
    Left = 440
    Top = 64
    Width = 297
    Height = 404
    Color = clTeal
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    Indent = 19
    ParentFont = False
    TabOrder = 7
    OnChange = TreeViewRVmatsChange
  end
end
