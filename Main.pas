unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, PNGImage, System.IOUtils, System.Types,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Menus, System.StrUtils, System.Threading, System.Diagnostics,
  Vcl.Clipbrd, System.Generics.Collections;

type
  TForm1 = class(TForm)
    ListBoxResults: TListBox;
    Label3: TLabel;
    SelectedImage: TImage;
    TreeViewPngFiles: TTreeView;
    Memo1: TMemo;
    Label1: TLabel;
    ImageFilePath: TEdit;
    TreeFilter: TEdit;
    Label2: TLabel;
    ApplyFilter: TButton;
    RVMatFilePath: TEdit;
    TreeViewRVmats: TTreeView;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    TreeViewp3d: TTreeView;
    Label7: TLabel;
    Label8: TLabel;
    popupImage: TImage;
    Panel1: TPanel;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeViewPngFilesChange(Sender: TObject; Node: TTreeNode);
    procedure ListBoxResultsClick(Sender: TObject);
    procedure TreeFilterChange(Sender: TObject);
    procedure ApplyFilterClick(Sender: TObject);
    procedure ImageFilePathDblClick(Sender: TObject);
    procedure TreeViewRVmatsChange(Sender: TObject; Node: TTreeNode);
    procedure SelectedImageMouseEnter(Sender: TObject);
    procedure SelectedImageMouseLeave(Sender: TObject);

  private
    OriginalPngFiles: TStringDynArray;
    LastFilterTime: TStopwatch;
    procedure LoadPngFiles;
    procedure SearchRvmatsForPng(const PngName: string);
    procedure DisplaySelectedImage(const PngFilePath: string);
    function FindOrCreateNode(Root: TTreeNode; const NodeText: string): TTreeNode;
    procedure DisplayRvmatContent(const RvmatFilePath: string);
    procedure FilterTreeView(const FilterText: string);
    procedure UpdateListBox(const Files: TArray<string>);
    procedure DoFilterTreeView;
    function FindPaaFilesInRvmat(const RvmatContent: string): TArray<string>;
    procedure AddRvmatToTree(const RvmatFile: string; const PaaFiles: TArray<string>);
    procedure LoadRvmats;

    procedure LoadP3dFiles;
    function FindOrCreateNodeInTreeView(TreeView: TTreeView; Root: TTreeNode; const NodeText: string): TTreeNode;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function TForm1.FindOrCreateNodeInTreeView(TreeView: TTreeView; Root: TTreeNode; const NodeText: string): TTreeNode;
var
  I: Integer;
begin
  Result := nil;
  if Assigned(Root) then
  begin
    for I := 0 to Root.Count - 1 do
      if Root.Item[I].Text = NodeText then
        Exit(Root.Item[I]);
  end
  else
  begin
    for I := 0 to TreeView.Items.Count - 1 do
      if TreeView.Items[I].Text = NodeText then
        Exit(TreeView.Items[I]);
  end;
  Result := TreeView.Items.AddChild(Root, NodeText);
end;


procedure TForm1.LoadP3dFiles;
var
  P3dFiles: TStringDynArray;
  RootDir, P3dFile, RelativePath, SubDir: string;
begin
  RootDir := 'P:\DZ\structures';
  P3dFiles := TDirectory.GetFiles(RootDir, '*.p3d', TSearchOption.soAllDirectories);

  TThread.Queue(nil,
    procedure
    begin
      TreeViewp3d.Items.Clear;
    end);

  for P3dFile in P3dFiles do
  begin
    TThread.Synchronize(nil,
      procedure
      var
        RootNode, SubNode: TTreeNode;
      begin
        RelativePath := ExtractRelativePath(RootDir + '\', P3dFile);
        SubDir := ExtractFilePath(RelativePath);
        RootNode := FindOrCreateNodeInTreeView(TreeViewp3d, nil, SubDir);
        SubNode := TreeViewp3d.Items.AddChild(RootNode, TPath.GetFileName(P3dFile));
        SubNode.Data := StrNew(PChar(P3dFile));
      end);
  end;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  LastFilterTime := TStopwatch.StartNew;

  TTask.Run(LoadPngFiles);
  TTask.Run(LoadRvmats);
  TTask.Run(LoadP3dFiles);

  OnDestroy := FormDestroy;
end;


procedure TForm1.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to TreeViewPngFiles.Items.Count - 1 do
    if TreeViewPngFiles.Items[I].Data <> nil then
      StrDispose(PChar(TreeViewPngFiles.Items[I].Data));
end;

procedure TForm1.LoadPngFiles;
var
  PngFiles: TStringDynArray;
  RootDir, PngFile, RelativePath, SubDir: string;
begin
  RootDir := 'P:\DZ\structures';
  PngFiles := TDirectory.GetFiles(RootDir, '*.png', TSearchOption.soAllDirectories);

  TThread.Queue(nil,
    procedure
    begin
      TreeViewPngFiles.Items.Clear;
      OriginalPngFiles := PngFiles;
    end);

  for PngFile in PngFiles do
  begin
    TThread.Synchronize(nil,
      procedure
      var
        RootNode, SubNode: TTreeNode;
      begin
        RelativePath := ExtractRelativePath(RootDir + '\', PngFile);
        SubDir := ExtractFilePath(RelativePath);
        RootNode := FindOrCreateNode(nil, SubDir);
        SubNode := TreeViewPngFiles.Items.AddChild(RootNode, TPath.GetFileName(PngFile));
        SubNode.Data := StrNew(PChar(PngFile));
      end);
  end;
end;

procedure TForm1.LoadRvmats;
var
  RvmatFiles: TStringDynArray;
  RootDir, RvmatFile: string;
begin
  RootDir := 'P:\DZ\structures';
  RvmatFiles := TDirectory.GetFiles(RootDir, '*.rvmat', TSearchOption.soAllDirectories);

  TThread.Queue(nil,
    procedure
    begin
      TreeViewRVmats.Items.Clear;
    end);

  for RvmatFile in RvmatFiles do
  begin
    TThread.Synchronize(nil,
      procedure
      var
        FileContent: TStringList;
        PaaFiles: TArray<string>;
      begin
        FileContent := TStringList.Create;
        try
          FileContent.LoadFromFile(RvmatFile);
          PaaFiles := FindPaaFilesInRvmat(FileContent.Text);
          AddRvmatToTree(RvmatFile, PaaFiles);
        finally
          FileContent.Free;
        end;
      end);
  end;
end;

function TForm1.FindPaaFilesInRvmat(const RvmatContent: string): TArray<string>;
var
  Lines: TArray<string>;
  Line, PaaPath: string;
  PaaFiles: TList<string>;
begin
  PaaFiles := TList<string>.Create;
  try
    Lines := RvmatContent.Split([sLineBreak], TStringSplitOptions.ExcludeEmpty);
    for Line in Lines do
    begin
      if Line.Contains('texture="') and Line.Contains('.paa"') then
      begin
        PaaPath := Line.Substring(Line.IndexOf('texture="') + 9);
        PaaPath := PaaPath.Substring(0, PaaPath.IndexOf('"'));
        PaaPath := PaaPath.Replace('.paa', '.png');

        if not PaaPath.StartsWith('P:\') then
          PaaPath := 'P:\' + PaaPath;

        PaaFiles.Add(PaaPath);
      end;
    end;
    Result := PaaFiles.ToArray;
  finally
    PaaFiles.Free;
  end;
end;

procedure TForm1.AddRvmatToTree(const RvmatFile: string; const PaaFiles: TArray<string>);
var
  RvmatNode, PaaNode: TTreeNode;
  PaaFile: string;
begin
  RvmatNode := TreeViewRVmats.Items.AddChild(nil, TPath.GetFileName(RvmatFile));
  RvmatNode.Data := StrNew(PChar(RvmatFile));

  for PaaFile in PaaFiles do
  begin
    PaaNode := TreeViewRVmats.Items.AddChild(RvmatNode, PaaFile);
    PaaNode.Data := StrNew(PChar(PaaFile));
  end;
end;

procedure TForm1.TreeViewPngFilesChange(Sender: TObject; Node: TTreeNode);
var
  SelectedPng, PngFileName: string;
begin
  if Assigned(Node) and Assigned(Node.Data) then
  begin
    SelectedPng := PChar(Node.Data);
    PngFileName := TPath.GetFileNameWithoutExtension(SelectedPng);

    TThread.Queue(nil,
      procedure
      begin
        ImageFilePath.Text := SelectedPng.Replace('.png','.paa');
        ListBoxResults.Clear;
        DisplaySelectedImage(SelectedPng);
      end);

    TTask.Run(procedure begin SearchRvmatsForPng(PngFileName); end);
  end;
end;

procedure TForm1.TreeViewRVmatsChange(Sender: TObject; Node: TTreeNode);
var
  SelectedPath, RvmatPath, ImagePath, BasePath: string;
  ImageFound: Boolean;
begin
  if Assigned(Node) and Assigned(Node.Data) then
  begin
    SelectedPath := PChar(Node.Data);
    if not SelectedPath.StartsWith('P:\') then
      SelectedPath := 'P:\' + SelectedPath;

    if SelectedPath.ToLower.Contains('.rvmat') then
    begin
      RVMatFilePath.Text := SelectedPath;
      DisplayRvmatContent(SelectedPath);
      ImageFilePath.Clear;
      SelectedImage.Picture := nil;
    end
    else
    begin
      ImagePath := SelectedPath;
      if Assigned(Node.Parent) and Assigned(Node.Parent.Data) then
      begin
        RvmatPath := PChar(Node.Parent.Data);
        if not RvmatPath.StartsWith('P:\') then
          RvmatPath := 'P:\' + RvmatPath;

        RVMatFilePath.Text := RvmatPath;
        DisplayRvmatContent(RvmatPath);
      end;
      ImageFilePath.Text := ImagePath;
      DisplaySelectedImage(ImagePath);
    end;
  end
  else
  begin
    ImageFilePath.Clear;
    SelectedImage.Picture := nil;
    RVMatFilePath.Clear;
    Memo1.Clear;
  end;

  if (SelectedImage.Picture = nil) or (SelectedImage.Picture.Graphic = nil) then
  begin
    ImageFound := False;
    if Assigned(Node) and Assigned(Node.Data) then
    begin
      BasePath := ChangeFileExt(SelectedPath, '');
      if FileExists(BasePath + '_co.png') then
      begin
        ImagePath := BasePath + '_co.png';
        ImageFound := True;
      end
      else if FileExists(BasePath + '_ca.png') then
      begin
        ImagePath := BasePath + '_ca.png';
        ImageFound := True;
      end;

      if ImageFound then
      begin
        ImageFilePath.Text := ImagePath;
        DisplaySelectedImage(ImagePath);
      end;
    end;
  end;
end;

procedure TForm1.ListBoxResultsClick(Sender: TObject);
var
  SelectedRvmat: string;
  I: Integer;
  Node: TTreeNode;
begin
  if ListBoxResults.ItemIndex <> -1 then
  begin
    SelectedRvmat := ListBoxResults.Items[ListBoxResults.ItemIndex];
    RVMatFilePath.Text := SelectedRvmat;
    DisplayRvmatContent(SelectedRvmat);

    for I := 0 to TreeViewRVmats.Items.Count - 1 do
    begin
      Node := TreeViewRVmats.Items[I];
      if Assigned(Node.Data) and (PChar(Node.Data) = SelectedRvmat) then
      begin
        TreeViewRVmats.Selected := Node;
        Node.MakeVisible;
        Node.Expand(True);
        Break;
      end;
    end;
  end;
end;

procedure TForm1.ImageFilePathDblClick(Sender: TObject);
var
  SelectedNode: TTreeNode;
begin
  SelectedNode := TreeViewPngFiles.Selected;
  if Assigned(SelectedNode) then
    Clipboard.AsText := ImageFilePath.Text
  else
    ShowMessage('No file selected. Please select a file from the list.');
end;

function TForm1.FindOrCreateNode(Root: TTreeNode; const NodeText: string): TTreeNode;
var
  I: Integer;
begin
  Result := nil;
  if Assigned(Root) then
  begin
    for I := 0 to Root.Count - 1 do
      if Root.Item[I].Text = NodeText then
        Exit(Root.Item[I]);
  end
  else
  begin
    for I := 0 to TreeViewPngFiles.Items.Count - 1 do
      if TreeViewPngFiles.Items[I].Text = NodeText then
        Exit(TreeViewPngFiles.Items[I]);
  end;
  Result := TreeViewPngFiles.Items.AddChild(Root, NodeText);
end;

procedure TForm1.SearchRvmatsForPng(const PngName: string);
var
  Files: TStringDynArray;
  ResultsList: TThreadList<string>;
begin
  TThread.Queue(nil,
    procedure
    begin
      ListBoxResults.Items.BeginUpdate;
      try
        ListBoxResults.Clear;
        ListBoxResults.Items.Add('Searching...');
      finally
        ListBoxResults.Items.EndUpdate;
      end;
    end);

  Files := TDirectory.GetFiles('P:\DZ\structures', '*.rvmat', TSearchOption.soAllDirectories);
  ResultsList := TThreadList<string>.Create;
  try
    TParallel.For(0, High(Files),
      procedure(Index: Integer)
      var
        FileStream: TFileStream;
        FileContent: TStringList;
      begin
        try
          FileStream := TFileStream.Create(Files[Index], fmOpenRead or fmShareDenyWrite);
          try
            FileContent := TStringList.Create;
            try
              FileContent.LoadFromStream(FileStream);
              if ContainsText(FileContent.Text, PngName) then
                ResultsList.Add(Files[Index]);
            finally
              FileContent.Free;
            end;
          finally
            FileStream.Free;
          end;
        except
          on E: Exception do
            TThread.Queue(nil,
              procedure
              begin
                ShowMessage('Error processing file: ' + Files[Index] + sLineBreak + E.Message);
              end);
        end;
      end);

    var ResultsArray: TArray<string>;
    with ResultsList.LockList do
    try
      ResultsArray := ToArray;
    finally
      ResultsList.UnlockList;
    end;

    TThread.Queue(nil,
      procedure
      begin
        if Length(ResultsArray) = 0 then
          ListBoxResults.Items.Text := 'No results found.'
        else
          UpdateListBox(ResultsArray);
      end);
  finally
    ResultsList.Free;
  end;
end;

procedure TForm1.SelectedImageMouseEnter(Sender: TObject);
begin
    popupImage.Visible  :=  True;
    Panel1.Visible  :=  True;
    popupImage.Picture :=  SelectedImage.Picture;
    popupImage.BringToFront;
end;

procedure TForm1.SelectedImageMouseLeave(Sender: TObject);
begin
  popupImage.Visible  :=  False;
  panel1.Visible  :=  false;
end;

procedure TForm1.UpdateListBox(const Files: TArray<string>);
begin
  TThread.Queue(nil,
    procedure
    begin
      ListBoxResults.Items.BeginUpdate;
      try
        ListBoxResults.Clear;
        for var FileName in Files do
          ListBoxResults.Items.Add(FileName);
      finally
        ListBoxResults.Items.EndUpdate;
      end;
    end);
end;

procedure TForm1.DisplaySelectedImage(const PngFilePath: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      try
        if FileExists(PngFilePath) then
          SelectedImage.Picture.LoadFromFile(PngFilePath)
        else
          ShowMessage('File not found: ' + PngFilePath);
      except
        on E: Exception do
          ShowMessage('Error loading image: ' + E.Message);
      end;
    end);
end;

procedure TForm1.ApplyFilterClick(Sender: TObject);
begin
  if LastFilterTime.ElapsedMilliseconds > 300 then
  begin
    LastFilterTime := TStopwatch.StartNew;
    TTask.Run(DoFilterTreeView);
  end;
end;

procedure TForm1.DisplayRvmatContent(const RvmatFilePath: string);
var
  FileContent: TStringList;
begin
  TThread.Queue(nil,
    procedure
    begin
      FileContent := TStringList.Create;
      try
        if FileExists(RvmatFilePath) then
        begin
          FileContent.LoadFromFile(RvmatFilePath);
          Memo1.Lines.Text := FileContent.Text;
        end
        else
          Memo1.Lines.Text := 'File not found: ' + RvmatFilePath;
      finally
        FileContent.Free;
      end;
    end);
end;

procedure TForm1.TreeFilterChange(Sender: TObject);
begin
  if LastFilterTime.ElapsedMilliseconds > 300 then
  begin
    LastFilterTime := TStopwatch.StartNew;
    TTask.Run(DoFilterTreeView);
  end;
end;

procedure TForm1.DoFilterTreeView;
begin
  FilterTreeView(TreeFilter.Text);
end;

procedure TForm1.FilterTreeView(const FilterText: string);
var
  PngFile, RootDir, RelativePath, SubDir: string;
begin
  RootDir := 'P:\DZ\structures';

  TThread.Queue(nil,
    procedure
    begin
      TreeViewPngFiles.Items.BeginUpdate;
      try
        TreeViewPngFiles.Items.Clear;
      finally
        TreeViewPngFiles.Items.EndUpdate;
      end;
    end);

  for PngFile in OriginalPngFiles do
  begin
    if (FilterText = '') or ContainsText(PngFile, FilterText) then
    begin
      TThread.Synchronize(nil,
        procedure
        var
          RootNode, SubNode: TTreeNode;
        begin
          RelativePath := ExtractRelativePath(RootDir + '\', PngFile);
          SubDir := ExtractFilePath(RelativePath);
          RootNode := FindOrCreateNode(nil, SubDir);
          SubNode := TreeViewPngFiles.Items.AddChild(RootNode, TPath.GetFileName(PngFile));
          SubNode.Data := StrNew(PChar(PngFile));
        end);
    end;
  end;
end;

end.
