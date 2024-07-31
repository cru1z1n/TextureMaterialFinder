unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, PNGImage, System.IOUtils, System.Types,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Menus, System.StrUtils, System.Threading, System.Diagnostics,
  Vcl.Clipbrd,  System.Generics.Collections;

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

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeViewPngFilesChange(Sender: TObject; Node: TTreeNode);
    procedure ListBoxResultsClick(Sender: TObject);
    procedure TreeFilterChange(Sender: TObject);
    procedure ApplyFilterClick(Sender: TObject);
    procedure ImageFilePathDblClick(Sender: TObject);
    procedure TreeViewRVmatsChange(Sender: TObject; Node: TTreeNode); // New event for TreeFilter change
  private
    OriginalPngFiles: TStringDynArray;
    LastFilterTime: TStopwatch;
    procedure LoadPngFiles;

    procedure UpdateLoadingAnimation(var LastUpdate: TStopwatch; const State: Integer);

    procedure SearchRvmatsForPng(const PngName: string);

    procedure DisplaySelectedImage(const PngFilePath: string);
    function FindOrCreateNode(Root: TTreeNode; const NodeText: string): TTreeNode;
    procedure DisplayRvmatContent(const RvmatFilePath: string);
    procedure FilterTreeView(const FilterText: string); // New method for filtering tree view
    procedure UpdateListBox(const Files: TArray<string>); // New method to update ListBox
    procedure DoFilterTreeView; // Debounce filter method

    function FindPaaFilesInRvmat(const RvmatContent: string): TArray<string>;
    procedure AddRvmatToTree(const RvmatFile: string; const PaaFiles: TArray<string>);
    procedure LoadRvmats;

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


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
        // Extract the path and change .paa to .png
        PaaPath := Line.Substring(Line.IndexOf('texture="') + 9);
        PaaPath := PaaPath.Substring(0, PaaPath.IndexOf('"'));
        PaaPath := PaaPath.Replace('.paa', '.png');

        // Ensure the path is prefixed with P:\
        if not PaaPath.StartsWith('P:\') then
        begin
          PaaPath := 'P:\' + PaaPath;
        end;

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
  RvmatNode.Data := StrNew(PChar(RvmatFile)); // Store the .rvmat file path in the parent node's data

  for PaaFile in PaaFiles do
  begin
    PaaNode := TreeViewRVmats.Items.AddChild(RvmatNode, PaaFile);
    PaaNode.Data := StrNew(PChar(PaaFile)); // Store the .png path or name as needed
  end;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  LastFilterTime := TStopwatch.StartNew;

  // Load PNG files in a background thread to keep the UI responsive
  TTask.Run(
    procedure
    begin
      LoadPngFiles;
    end);

  // Load RVMat files in a background thread to keep the UI responsive
  TTask.Run(
    procedure
    begin
      LoadRvmats;
    end);

  OnDestroy := FormDestroy;
end;


procedure TForm1.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to TreeViewPngFiles.Items.Count - 1 do
  begin
    if TreeViewPngFiles.Items[I].Data <> nil then
    begin
      StrDispose(PChar(TreeViewPngFiles.Items[I].Data));
    end;
  end;
end;


procedure TForm1.ImageFilePathDblClick(Sender: TObject);
var
  SelectedNode: TTreeNode;
begin
  // Get the selected node from the TreeView
  SelectedNode := TreeViewPngFiles.Selected;

  // Check if a node is selected
  if Assigned(SelectedNode) then
  begin
    // Assuming ImageFilePath is an Edit control where the selected node's path is displayed
    Clipboard.AsText := ImageFilePath.Text;
  end
  else
  begin
    ShowMessage('No file selected. Please select a file from the list.');
  end;
end;

function TForm1.FindOrCreateNode(Root: TTreeNode; const NodeText: string): TTreeNode;
var
  I: Integer;
begin
  Result := nil;
  if Assigned(Root) then
  begin
    for I := 0 to Root.Count - 1 do
    begin
      if Root.Item[I].Text = NodeText then
      begin
        Result := Root.Item[I];
        Exit;
      end;
    end;
  end
  else
  begin
    for I := 0 to TreeViewPngFiles.Items.Count - 1 do
    begin
      if TreeViewPngFiles.Items[I].Text = NodeText then
      begin
        Result := TreeViewPngFiles.Items[I];
        Exit;
      end;
    end;
  end;

  Result := TreeViewPngFiles.Items.AddChild(Root, NodeText);
end;

procedure TForm1.LoadPngFiles;
var
  PngFiles: TStringDynArray;
  RootDir, PngFile: string;
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
        RelativePath, SubDir: string;
        RootNode, SubNode: TTreeNode;
      begin
        RelativePath := ExtractRelativePath(RootDir + '\', PngFile);
        SubDir := ExtractFilePath(RelativePath);
        RootNode := FindOrCreateNode(nil, SubDir);  // Add or find the directory node

        // Add the PNG file as a child node under its directory
        SubNode := TreeViewPngFiles.Items.AddChild(RootNode, TPath.GetFileName(PngFile));
        SubNode.Data := StrNew(PChar(PngFile));  // Use StrNew to allocate memory for the path string
      end);
  end;
end;

procedure TForm1.TreeViewPngFilesChange(Sender: TObject; Node: TTreeNode);
var
  SelectedPng, PngFileName: string;
begin
  if Node <> nil then
  begin
    if Node.Data <> nil then
    begin
      SelectedPng := PChar(Node.Data);  // Properly typecast the Data property
      PngFileName := TPath.GetFileNameWithoutExtension(SelectedPng);

      TThread.Queue(nil,
        procedure
        begin
          ImageFilePath.Text := SelectedPng; // Set the file path to the edit control
          ListBoxResults.Clear;
          DisplaySelectedImage(SelectedPng);  // Display the selected image
        end);

      TTask.Run(
        procedure
        begin
          SearchRvmatsForPng(PngFileName);
        end);
    end;
  end;
end;

procedure TForm1.TreeViewRVmatsChange(Sender: TObject; Node: TTreeNode);
var
  SelectedPath, RvmatPath, ImagePath: string;
begin
  if Assigned(Node) and Assigned(Node.Data) then
  begin
    SelectedPath := PChar(Node.Data);

    // Ensure the path is prefixed with P:\
    if not SelectedPath.StartsWith('P:\') then
    begin
      SelectedPath := 'P:\' + SelectedPath;
    end;

    if SelectedPath.ToLower.Contains('.rvmat') then
    begin
      // Update the RVMatFilePath edit control with the selected RVMat path
      RVMatFilePath.Text := SelectedPath;

      // Display the content of the selected RVMat file
      DisplayRvmatContent(SelectedPath);

      // Clear the ImageFilePath edit control and image
      ImageFilePath.Clear;
      SelectedImage.Picture := nil;
    end
    else
    begin
      ImagePath := SelectedPath;
      if Assigned(Node.Parent) and Assigned(Node.Parent.Data) then
      begin
        RvmatPath := PChar(Node.Parent.Data);

        // Ensure the path is prefixed with P:\
        if not RvmatPath.StartsWith('P:\') then
        begin
          RvmatPath := 'P:\' + RvmatPath;
        end;

        // Update the RVMatFilePath edit control with the parent node's RVMat path
        RVMatFilePath.Text := RvmatPath;

        // Display the content of the parent node's RVMat file
        DisplayRvmatContent(RvmatPath);
      end;

      // Update the ImageFilePath edit control with the selected image path
      ImageFilePath.Text := ImagePath;

      // Display the selected image
      DisplaySelectedImage(ImagePath);
    end;
  end
  else
  begin
    // Clear the image and the path if no valid node is selected
    ImageFilePath.Clear;
    SelectedImage.Picture := nil;
    RVMatFilePath.Clear;
    Memo1.Clear;
  end;
end;

procedure TForm1.ListBoxResultsClick(Sender: TObject);
var
  SelectedRvmat: string;
begin
  if ListBoxResults.ItemIndex <> -1 then
  begin
    SelectedRvmat := ListBoxResults.Items[ListBoxResults.ItemIndex];
    RVMatFilePath.Text := SelectedRvmat; // Set the selected RVMat file path
    DisplayRvmatContent(SelectedRvmat);
  end;
end;

procedure TForm1.UpdateLoadingAnimation(var LastUpdate: TStopwatch; const State: Integer);
const
  LoadingTexts: array[0..3] of string = ('Searching.', 'Searching..', 'Searching...', 'Searching....');
begin
  if LastUpdate.ElapsedMilliseconds > 500 then // Update every 500 ms
  begin
    LastUpdate := TStopwatch.StartNew;
    TThread.Queue(nil,
      procedure
      begin
        ListBoxResults.Items.BeginUpdate;
        try
          ListBoxResults.Clear;
          ListBoxResults.Items.Add(LoadingTexts[State]);
        finally
          ListBoxResults.Items.EndUpdate;
        end;
      end);
  end;
end;

procedure TForm1.SearchRvmatsForPng(const PngName: string);
var
  Files: TStringDynArray;
  ResultsList: TThreadList<string>;
  AnimationState: Integer;
  LastUpdate: TStopwatch;
begin
  // Show the initial searching message
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
    AnimationState := 0;
    LastUpdate := TStopwatch.StartNew;

    // Use TParallel.For to process files in parallel
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
              begin
                ResultsList.Add(Files[Index]);
              end;
            finally
              FileContent.Free;
            end;
          finally
            FileStream.Free;
          end;
        except
          on E: Exception do
          begin
            TThread.Queue(nil,
              procedure
              begin
                ShowMessage('Error processing file: ' + Files[Index] + sLineBreak + E.Message);
              end);
          end;
        end;

        // Update the loading animation
        UpdateLoadingAnimation(LastUpdate, AnimationState);
        AnimationState := (AnimationState + 1) mod 4;
      end);

    // Collect results from the thread-safe list
    var ResultsArray: TArray<string>;
    with ResultsList.LockList do
    try
      ResultsArray := ToArray;
    finally
      ResultsList.UnlockList;
    end;

    // Update the ListBox on the main thread
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

procedure TForm1.UpdateListBox(const Files: TArray<string>);
begin
  TThread.Queue(nil,
    procedure
    begin
      ListBoxResults.Items.BeginUpdate;
      try
        ListBoxResults.Clear;
        for var FileName in Files do
        begin
          ListBoxResults.Items.Add(FileName);
        end;
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
        begin
          SelectedImage.Picture.LoadFromFile(PngFilePath);
        end
        else
        begin
          ShowMessage('File not found: ' + PngFilePath);
        end;
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
    TTask.Run(
      procedure
      begin
        DoFilterTreeView;
      end);
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
        begin
          Memo1.Lines.Text := 'File not found: ' + RvmatFilePath;
        end;
      finally
        FileContent.Free;
      end;
    end);
end;

procedure TForm1.TreeFilterChange(Sender: TObject);
begin
  // Debounce the filter input
  // if LastFilterTime.ElapsedMilliseconds > 300 then
  // begin
  //   LastFilterTime := TStopwatch.StartNew;
  //   TTask.Run(
  //     procedure
  //    begin
  //       DoFilterTreeView;
  //    end);
  // end;
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
          RootNode := FindOrCreateNode(nil, SubDir);  // Add or find the directory node
          // Add the PNG file as a child node under its directory
          SubNode := TreeViewPngFiles.Items.AddChild(RootNode, TPath.GetFileName(PngFile));
          SubNode.Data := StrNew(PChar(PngFile));  // Use StrNew to allocate memory for the path string
        end);
    end;
  end;
end;


end.

