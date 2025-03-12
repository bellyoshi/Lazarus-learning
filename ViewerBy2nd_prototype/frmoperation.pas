unit frmOperation;

// Todo: movie
// Todo: slim normal window size
// Todo: ドラッグ＆ドロップでファイルを登録できるようにする。
// Todo: Form Ratio Windows size Panel sizeで共通化

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, ComCtrls, frmViewer, ViewerModel, RepogitoryUnit, Generics.Collections,
  FormSizeCustomizerUnit, PageFormUnit, SettingFormUnit, IViewUnit, ZoomUnit,
  AboutUnit, ZoomRateFormUnit, SettingLoaderUnit;

type

  { TOperationForm }

  TOperationForm = class(TForm, IView)
    DelteButton: TButton;
    DeselectButton: TButton;
    FilesListBox: TListBox;
    FilesListPanel: TPanel;
    OpenButton: TButton;
    PreviewPanel: TPanel;
    SelectAllButton: TButton;
    StatusBar1: TStatusBar;
    ZoomRateMenuItem: TMenuItem;
    ZoomOutMenuItem: TMenuItem;
    ZoomInMenuItem: TMenuItem;
    Rotate000Button: TButton;
    Rotate090Button: TButton;
    Rotate180Button: TButton;
    Rotate270Button: TButton;

    BackGroundDisplayButton: TButton;
    AutoUpdateCheckBox: TCheckBox;
    FitWindowButton: TButton;
    ViewAllButton: TButton;
    ZoomRateLabel: TLabel;
    ZoomInButton: TButton;
    ZoomOutButton: TButton;
    LastPageButton: TButton;
    FirstPageButton: TButton;
    FileInfoLabel: TLabel;
    ViewerDisplayButton: TButton;
    ViewerCloseButton: TButton;
    ViewerGroupBox: TGroupBox;
    Label1: TLabel;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    ListMenu: TMenuItem;
    DisplayMenu: TMenuItem;
    BackgroundDisplayMenu: TMenuItem;
    FirstPageMenu: TMenuItem;
    LastPageMenu: TMenuItem;
    HelpMenu: TMenuItem;
    AboutMenu: TMenuItem;
    DisplaySettingMenu: TMenuItem;
    AutoUpdateSettingMenu: TMenuItem;
    DefaultSizeMenu: TMenuItem;
    MinimumSizeMenu: TMenuItem;
    SlimSizeMenu: TMenuItem;
    OperationFormSizeMenu: TMenuItem;
    SettingMenu: TMenuItem;
    PageIndexMenu: TMenuItem;
    PreviousPageMenu: TMenuItem;
    NextPageMenu: TMenuItem;
    Rotate270Menu: TMenuItem;
    Rotate180Menu: TMenuItem;
    Rotate090Menu: TMenuItem;
    Rotate000Menu: TMenuItem;
    ViewerCloseMenu: TMenuItem;
    ViewerDisplayMenu: TMenuItem;
    ViewerMenu: TMenuItem;
    VideoPlayMenu: TMenuItem;
    ZoomMenu: TMenuItem;
    PageNavigationMenu: TMenuItem;
    RotateMenu: TMenuItem;
    OpenMenu: TMenuItem;
    PageCountLabel: TLabel;
    NextButton: TButton;
    PreviousButton: TButton;
    Image1: TImage;
    OpenDialog1: TOpenDialog;
    ThumbnailPanel: TPanel;
    procedure AboutMenuClick(Sender: TObject);
    procedure AutoUpdateCheckBoxChange(Sender: TObject);
    procedure AutoUpdateSettingMenuClick(Sender: TObject);
    procedure BackGroundDisplayButtonClick(Sender: TObject);
    procedure BackgroundDisplayMenuClick(Sender: TObject);
    procedure FirstPageMenuClick(Sender: TObject);
    procedure FitWindowButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseLeave(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LastPageMenuClick(Sender: TObject);
    procedure NextPageMenuClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure DelteButtonClick(Sender: TObject);
    procedure DeselectButtonClick(Sender: TObject);
    procedure DisplaySettingMenuClick(Sender: TObject);
    procedure FilesListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure LastPageButtonClick(Sender: TObject);
    procedure FirstPageButtonClick(Sender: TObject);
    procedure OpenMenuClick(Sender: TObject);
    procedure PageIndexMenuClick(Sender: TObject);
    procedure PreviewPanelResize(Sender: TObject);
    procedure PreviousPageMenuClick(Sender: TObject);
    procedure Rotate000MenuClick(Sender: TObject);
    procedure Rotate090MenuClick(Sender: TObject);
    procedure Rotate000ButtonClick(Sender: TObject);
    procedure Rotate180ButtonClick(Sender: TObject);
    procedure Rotate180MenuClick(Sender: TObject);
    procedure Rotate270ButtonClick(Sender: TObject);
    procedure Rotate270MenuClick(Sender: TObject);
    procedure Rotate090ButtonClick(Sender: TObject);
    procedure SelectAllButtonClick(Sender: TObject);
    procedure ViewAllButtonClick(Sender: TObject);
    procedure ViewerCloseButtonClick(Sender: TObject);
    procedure ViewerCloseMenuClick(Sender: TObject);
    procedure ViewerDisplayButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FileMenuClick(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure ListMenuClick(Sender: TObject);
    procedure PageCountLabelClick(Sender: TObject);
    procedure PreviousButtonClick(Sender: TObject);
    procedure UpdateView;
    procedure UpdateAuto;
    procedure ViewerDisplayMenuClick(Sender: TObject);
    procedure ZoomInButtonClick(Sender: TObject);
    procedure ZoomOutButtonClick(Sender: TObject);
    procedure ListMenuChileClick(Sender: TObject);
    procedure ZoomOutMenuItemClick(Sender: TObject);
    procedure ZoomRateLabelClick(Sender: TObject);
    procedure ZoomRateMenuItemClick(Sender: TObject);
    procedure ZoomInMenuItemClick(Sender: TObject);
  private
    AutoUpdateCheckBoxCheckedChanging : Boolean;
    FFilesListBoxLoaded : Boolean;
        IsMouseDown : Boolean;
    procedure LoadBitmap;
    procedure SetCtlEnabled();
    procedure SetPanelSize();
    procedure LoadList();
    procedure PopulateFileMenu();
    procedure LoadListBox(fileList : TStringList);
    function GetRepository() : TRepogitory;
    property Repos : TRepogitory read GetRepository;

  public

  end;

var
  OperationForm: TOperationForm;

implementation

{$R *.lfm}

{ TOperationForm }

function TOperationForm.GetRepository(): TRepogitory;
begin
  Result := model.Repogitory;
  end;

procedure TOperationForm.SetCtlEnabled();
begin
  ViewerDisplayButton.Enabled:=model.HasOperationDocument;
  NextButton.Enabled:= model.CanNext;
  PreviousButton.Enabled := model.CanPrevious;
  LastPageButton.Enabled:=model.CanLast;
  FirstPageButton.Enabled := model.CanFirst;

  PageCountLabel.Enabled:=model.CanPrevious or model.CanNext;
  if PageCountLabel.Enabled Then
  begin
    PageCountLabel.Caption:= Format('%d / %d', [model.PageIndex + 1, model.PageCount]);
  end else begin
    PageCountLabel.Caption:= '';
  end;
  ZoomInButton.Enabled:= model.CanZoomIn;
  ZoomOutButton.Enabled:=model.CanZoomOut;
  ZoomRateLabel.Enabled := (model.CanZoomIn) or (model.CanZoomOut);

  FitWindowButton.Enabled:=model.CanZoom;
  ViewAllButton.Enabled:=model.CanZoom;
  Rotate000Button.Enabled:=model.CanRotate;
  Rotate180Button.Enabled:=model.CanRotate;
  Rotate270Button.Enabled:=model.CanRotate;
  Rotate090Button.Enabled:=model.CanRotate;

  ViewerDisplayMenu.Enabled:=ViewerDisplayButton.Enabled;
  NextPageMenu.Enabled:= NextButton.Enabled;
  PreviousPageMenu.Enabled := PreviousButton.Enabled ;
  LastPageMenu.Enabled := LastPageButton.Enabled;
  FirstPageMenu.Enabled :=FirstPageButton.Enabled ;
  PageIndexMenu.Enabled:=PageCountLabel.Enabled;

  ZoomInMenuItem.Enabled:= ZoomInButton.Enabled;
  ZoomOutMenuItem.Enabled := ZoomOutButton.Enabled;
  ZoomRateMenuItem.Enabled := ZoomRateLabel.Enabled;

  Rotate000Menu.Enabled:=Rotate000Button.Enabled;
  Rotate180Menu.Enabled:=Rotate180Button.Enabled;
  Rotate270Menu.Enabled:=Rotate270Button.Enabled;
  Rotate090Menu.Enabled:=Rotate090Button.Enabled;


end;

procedure TOperationForm.FormCreate(Sender: TObject);
begin
    model := TViewerModel.Create;
    UpdateView;


end;

procedure TOperationForm.FileMenuClick(Sender: TObject);
begin

end;

procedure TOperationForm.Label1Click(Sender: TObject);
begin

end;
procedure TOperationForm.UpdateAuto();
begin
  UpdateView;
  if AutoUpdateCheckBox.Checked then
  begin
    ViewerForm.ShowDocument()
  end;
end;

procedure TOperationForm.SetPanelSize();
var
  rect : TRect;
  w,h : Integer;
begin

  If not Assigned(FormSizeCustomizer) Then
  begin
    exit;
  end;
  rect := FormSizeCustomizer.CurrentWindowSize;
  w := ThumbnailPanel.Width;
  h := (w * rect.Height) div rect.Width;
  ThumbnailPanel.Height := h;

  StatusBar1.SimpleText := Format('Width: %d, Height: %d',
    [rect.Width, rect.Height]);

end;

procedure TOperationForm.UpdateView();
begin
  SetCtlEnabled();
  LoadList();
  SetPanelSize();
  LoadBitmap();
  ThumbnailPanel.Color:=model.Background.Color;
  if model.HasOperationDocument then
  begin
    ZoomRateLabel.Caption:= FloatToStr(model.Zoom.Rate * 100) + '%';
  end
  else
  begin
    ZoomRateLabel.Caption:= '';
  end;
  {
  if AutoUpdateCheckBox.Checked then
  begin
    ViewerForm.ShowDocument()
  end;
  }
end;

procedure TOperationForm.ViewerDisplayMenuClick(Sender: TObject);
begin
  ViewerDisplayButtonClick(Sender);
end;

procedure TOperationForm.ZoomInButtonClick(Sender: TObject);
var
  zoom : TZoom;
begin
  zoom := model.Zoom;
  zoom.ZoomIn();
  UpdateAuto;
end;

procedure TOperationForm.ZoomOutButtonClick(Sender: TObject);
var
  zoom : TZoom;
begin
  zoom := model.Zoom;
  zoom.ZoomOut();
  UpdateAuto;

end;

procedure TOperationForm.LoadList();
var
  fileList: TStringList;
begin
  FFilesListBoxLoaded := True;

  fileList := model.repogitory.GetFileNames; // Get the filenames from the model
  if Assigned(fileList) then
  begin
    LoadListBox(fileList);
    PopulateFileMenu();
  end;


  FFilesListBoxLoaded := False;
end;

procedure TOperationForm.LoadListBox(fileList : TStringList);
var
  i : Integer;
begin
  FilesListBox.Items.Clear; // Clear any existing items
  try
    FilesListBox.Items.Assign(fileList); // Assign the list to ListBox
  finally
    fileList.Free; // Free the TStringList
  end;

  for i := 0 to FilesListBox.Items.Count - 1 do
  begin
       FilesListBox.Selected[i] := Repos.Selected[i];
  end;
end;

procedure TOperationForm.NextButtonClick(Sender: TObject);
begin
  model.Next;
  UpdateAuto;
end;

procedure TOperationForm.ListMenuClick(Sender: TObject);
begin

end;

procedure TOperationForm.PageCountLabelClick(Sender: TObject);
begin
  if not model.HasOperationDocument then
  begin
    Exit;
  end;
  PageForm.Display();
end;

procedure TOperationForm.PreviousButtonClick(Sender: TObject);
begin
  model.Previous;
  UpdateAuto;

end;

procedure TOperationForm.LoadBitmap;
var
  Bitmap : TBitmap;
begin

  //FitImageSize(Image1, ThumbnailPanel.Width, ThumbnailPanel.Height, model.ThumbnailRatio);

  try
    // PDFium ページを Delphi ビットマップに描画
    Bitmap := model.GetThumbnailBitmap(ThumbnailPanel.Width, ThumbnailPanel.Height);
    Image1.Width := Bitmap.Width;
    Image1.Height := Bitmap.Height;
    Image1.Left := (ThumbnailPanel.Width - Bitmap.Width) div 2;
    Image1.Top := (ThumbnailPanel.Height - Bitmap.Height) div 2;

    Image1.Picture.Bitmap.Assign(Bitmap);
  finally
    Bitmap.Free;
  end;

end;
procedure TOperationForm.ListMenuChileClick(Sender: TObject);
var
  index : integer;
begin
  index := StrToInt(TMenuItem(Sender).Hint);
  Repos.Disselect;
  Repos.Selected[index] := True;
  UpdateAuto;
end;

procedure TOperationForm.ZoomOutMenuItemClick(Sender: TObject);
begin
  ZoomOutButtonClick(Sender);
end;

procedure TOperationForm.ZoomRateLabelClick(Sender: TObject);
begin
  if not model.HasOperationDocument then
  begin
    Exit;
  end;
  ZoomRateForm.Display();
end;

procedure TOperationForm.ZoomRateMenuItemClick(Sender: TObject);
begin
  ZoomRateLabelClick(Sender);
end;

procedure TOperationForm.ZoomInMenuItemClick(Sender: TObject);
begin
  ZoomInButtonClick(Sender);
end;

procedure TOperationForm.PopulateFileMenu();
var
  i: Integer;
  MenuItem: TMenuItem;
begin
  // 子メニューをすべてクリア
  ListMenu.Clear;


  // TStringList の内容をメニュー項目として追加
  for i := 0 to Repos.GetCount() - 1 do
  begin
    MenuItem := TMenuItem.Create(ListMenu);
    MenuItem.Caption := ExtractFileName(Repos.GetFileName(i)); // 表示名はファイル名
    MenuItem.Hint := IntToStr(i);
    MenuItem.OnClick := @(ListMenuChileClick);
    ListMenu.Add(MenuItem);
  end;
end;

procedure TOperationForm.OpenButtonClick(Sender: TObject);
var
  i : Integer;
begin
  // PDF、画像、およびすべてのファイルを選択可能にする
  OpenDialog1.Filter := 'PDF Files|*.pdf|Image Files|*.jpg;*.jpeg;*.png;*.bmp|All Files|*.*';
  OpenDialog1.Options:=OpenDialog1.Options+[ofAllowMultiSelect];

  if not OpenDialog1.Execute then Exit;

    for i:=0 to OpenDialog1.Files.Count-1 do
      begin
           model.Open(OpenDialog1.Files[i]);
      end;


  UpdateAuto;

end;

procedure TOperationForm.DelteButtonClick(Sender: TObject);
begin
  model.Repogitory.Delete;
  UpdateAuto();
end;

procedure TOperationForm.DeselectButtonClick(Sender: TObject);
begin
  model.Repogitory.Disselect;
  UpdateAuto();
end;

procedure TOperationForm.BackGroundDisplayButtonClick(Sender: TObject);
begin
  model.Repogitory.Disselect;
  ViewerForm.ShowDocument() ;
  UpdateAuto();
end;

procedure TOperationForm.BackgroundDisplayMenuClick(Sender: TObject);
begin
  BackGroundDisplayButtonClick(Sender);
end;

procedure TOperationForm.FirstPageMenuClick(Sender: TObject);
begin
 FirstPageButtonClick(Sender);
end;

procedure TOperationForm.AboutMenuClick(Sender: TObject);
begin
  AboutForm.Show;
end;

procedure TOperationForm.AutoUpdateCheckBoxChange(Sender: TObject);
begin
  if AutoUpdateCheckBoxCheckedChanging then Exit;

  AutoUpdateSettingMenu.Checked:=AutoUpdateCheckBox.Checked;
  if AutoUpdateCheckBox.Checked then
  begin
    UpdateAuto;
  end;
end;

procedure TOperationForm.AutoUpdateSettingMenuClick(Sender: TObject);
begin
  AutoUpdateSettingMenu.Checked:=Not AutoUpdateSettingMenu.Checked;
  AutoUpdateCheckBoxCheckedChanging := True;
  AutoUpdateCheckBox.Checked:= AutoUpdateSettingMenu.Checked;
  AutoUpdateCheckBoxCheckedChanging := False;
    if AutoUpdateSettingMenu.Checked then
  begin
    UpdateAuto;
  end;
end;

procedure TOperationForm.FitWindowButtonClick(Sender: TObject);
begin
    model.Zoom.fitWindow(ThumbnailPanel.Width, ThumbnailPanel.Height);
    UpdateAuto;
end;

procedure TOperationForm.FormResize(Sender: TObject);
var
  smallMode : Boolean;
const
  DEF_PREVIEW_WIDTH = 300;
  MARGIN_WIDTH = 20;
begin
  smallMode := Width < FilesListPanel.Width + DEF_PREVIEW_WIDTH + MARGIN_WIDTH;
   if smallMode then
   begin
     PreviewPanel.Left := 0;
     PreviewPanel.Width:= Width;
   end else
   begin
     PreviewPanel.Left := FilesListPanel.Width + MARGIN_WIDTH;
     PreviewPanel.Width:= DEF_PREVIEW_WIDTH;
   end;

   FilesListPanel.Visible := not smallMode;

end;

procedure TOperationForm.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = TMouseButton.mbLeft then
  begin
    IsMouseDown := True;
    model.Zoom.MouseDown(X, Y);
  end;
end;

procedure TOperationForm.Image1MouseLeave(Sender: TObject);
begin

end;

procedure TOperationForm.Image1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if IsMouseDown Then
  begin
   model.Zoom.MouseMove(X,Y);
   UpdateAuto;
  end;

end;

procedure TOperationForm.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if IsMouseDown And (Button = TMouseButton.mbLeft) then
  begin
   IsMouseDown := False;
  end;
end;

procedure TOperationForm.LastPageMenuClick(Sender: TObject);
begin
  LastPageButtonClick(Sender);
end;

procedure TOperationForm.NextPageMenuClick(Sender: TObject);
begin
  NextButtonClick(Sender);
end;

procedure TOperationForm.DisplaySettingMenuClick(Sender: TObject);
begin
  SettingForm.Show();
end;

procedure TOperationForm.FilesListBoxSelectionChange(Sender: TObject;
  User: boolean);
var
  i : Integer;
begin
  If FFilesListBoxLoaded Then
     Exit;

  for i:= 0 to FilesListBox.Count - 1 do
  begin
       model.Repogitory.Selected[i] := FilesListBox.Selected[i];
  end;
  UpdateAuto;

end;

procedure TOperationForm.LastPageButtonClick(Sender: TObject);
begin
  model.LastPage();
  UpdateAuto;
end;

procedure TOperationForm.FirstPageButtonClick(Sender: TObject);
begin
  model.FirstPage();
  UpdateAuto;
end;

procedure TOperationForm.OpenMenuClick(Sender: TObject);
begin
  OpenButtonClick(Sender);
end;

procedure TOperationForm.PageIndexMenuClick(Sender: TObject);
begin
  PageCountLabelClick(Sender);
end;

procedure TOperationForm.PreviewPanelResize(Sender: TObject);
begin
  ThumbnailPanel.Left:=0;
  ThumbnailPanel.Width:=PreviewPanel.Width;
end;

procedure TOperationForm.PreviousPageMenuClick(Sender: TObject);
begin
  PreviousButtonClick(Sender);
end;

procedure TOperationForm.Rotate000MenuClick(Sender: TObject);
begin
    Rotate000ButtonClick(Sender);
end;

procedure TOperationForm.Rotate090MenuClick(Sender: TObject);
begin
  Rotate090ButtonClick(Sender);
end;

procedure TOperationForm.Rotate000ButtonClick(Sender: TObject);
begin
  model.Rotate(0);
  UpdateAuto;
end;

procedure TOperationForm.Rotate180ButtonClick(Sender: TObject);
begin
  model.Rotate(180);
  UpdateAuto;
end;

procedure TOperationForm.Rotate180MenuClick(Sender: TObject);
begin
  Rotate180ButtonClick(Sender);
end;

procedure TOperationForm.Rotate270ButtonClick(Sender: TObject);
begin
  model.Rotate(270);
  UpdateAuto;
end;

procedure TOperationForm.Rotate270MenuClick(Sender: TObject);
begin
  Rotate270ButtonClick(Sender);
end;

procedure TOperationForm.Rotate090ButtonClick(Sender: TObject);
begin
  model.Rotate(90);
  UpdateAuto;
end;

procedure TOperationForm.SelectAllButtonClick(Sender: TObject);
begin
  Repos.SelectAll;
  UpdateAuto();
end;

procedure TOperationForm.ViewAllButtonClick(Sender: TObject);
begin
  model.Zoom.Rate:=1.0;
  UpdateAuto();
end;

procedure TOperationForm.ViewerCloseButtonClick(Sender: TObject);
begin
  ViewerForm.Close();
end;

procedure TOperationForm.ViewerCloseMenuClick(Sender: TObject);
begin
  ViewerCloseButtonClick(Sender);
end;


procedure TOperationForm.ViewerDisplayButtonClick(Sender: TObject);
begin
  ViewerForm.ShowDocument();
end;

procedure TOperationForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SettingLoader.Save;
  model.Free;
end;



end.

