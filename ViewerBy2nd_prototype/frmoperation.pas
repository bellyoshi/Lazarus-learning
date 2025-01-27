unit frmOperation;

// Todo: movie
// Todo: slim normal window size

// Todo: ドラッグ＆ドロップでファイルを登録できるようにする。


interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, frmViewer, ViewerModel, Repogitory,
  PageFormUnit, SettingFormUnit, IViewUnit,  ZoomUnit;

type

  { TOperationForm }

  TOperationForm = class(TForm, IView)
    Rotate0Button: TButton;
    Rotate90Button: TButton;
    Rotate180Button: TButton;
    Rotate270Button: TButton;

    OpenButton: TButton;
    BackGroundDisplayButton: TButton;
    AutoUpdateCheckBox: TCheckBox;
    FitWindowButton: TButton;
    ViewAllButton: TButton;
    ZoomRateLabel: TLabel;
    SelectAllButton: TButton;
    DelteButton: TButton;
    DeselectButton: TButton;
    ZoomInButton: TButton;
    ZoomOutButton: TButton;
    LastPageButton: TButton;
    FirstPageButton: TButton;
    FileInfoLabel: TLabel;
    ViewerDisplayButton: TButton;
    ViewerCloseButton: TButton;
    ViewerGroupBox: TGroupBox;
    Label1: TLabel;
    FilesListBox: TListBox;
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
    Panel1: TPanel;
    procedure BackGroundDisplayButtonClick(Sender: TObject);
    procedure FitWindowButtonClick(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseLeave(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenButtonClick(Sender: TObject);
    procedure DelteButtonClick(Sender: TObject);
    procedure DeselectButtonClick(Sender: TObject);
    procedure DisplaySettingMenuClick(Sender: TObject);
    procedure FilesListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure LastPageButtonClick(Sender: TObject);
    procedure FirstPageButtonClick(Sender: TObject);
    procedure Rotate0ButtonClick(Sender: TObject);
    procedure Rotate180ButtonClick(Sender: TObject);
    procedure Rotate270ButtonClick(Sender: TObject);
    procedure Rotate90ButtonClick(Sender: TObject);
    procedure SelectAllButtonClick(Sender: TObject);
    procedure ViewAllButtonClick(Sender: TObject);
    procedure ViewerCloseButtonClick(Sender: TObject);
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
    procedure ZoomInButtonClick(Sender: TObject);
    procedure ZoomOutButtonClick(Sender: TObject);

  private
    FFilesListBoxLoaded : Boolean;
        IsMouseDown : Boolean;
    procedure LoadBitmap;
    procedure SetCtlEnabled();
    procedure LoadList();
  public

  end;

var
  OperationForm: TOperationForm;

implementation

{$R *.lfm}

{ TOperationForm }

procedure TOperationForm.SetCtlEnabled();
begin
  ViewerDisplayButton.Enabled:=model.HasOperationDocument;
  NextButton.Enabled:= model.CanNext;
  PreviousButton.Enabled := model.CanPrevious;
  LastPageButton.Enabled:=model.CanLast;
  FirstPageButton.Enabled := model.CanFirst;
  PageCountLabel.Caption:= Format('%d / %d', [model.PageIndex + 1, model.PageCount]);
  ZoomInButton.Enabled:= model.CanZoomIn;
  ZoomOutButton.Enabled:=model.CanZoomOut;
  FitWindowButton.Enabled:=model.CanZoom;
  ViewAllButton.Enabled:=model.CanZoom;
  Rotate0Button.Enabled:=model.CanRotate;
  Rotate180Button.Enabled:=model.CanRotate;
  Rotate270Button.Enabled:=model.CanRotate;
  Rotate90Button.Enabled:=model.CanRotate;

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

procedure TOperationForm.UpdateView();
begin
  SetCtlEnabled();
  LoadList();
  LoadBitmap();
  Panel1.Color:=model.Background.Color;
  if Assigned(model.Zoom) then
  begin
    ZoomRateLabel.Caption:= FloatToStr(model.Zoom.Rate * 100) + '%';
  end
  else
  begin
    ZoomRateLabel.Caption:= '';
  end;
  if AutoUpdateCheckBox.Checked then
  begin
    ViewerForm.ShowDocument()
  end;
end;

procedure TOperationForm.ZoomInButtonClick(Sender: TObject);
var
  zoom : TZoom;
begin
  zoom := model.Zoom;
  zoom.ZoomIn();
  UpdateView;
end;

procedure TOperationForm.ZoomOutButtonClick(Sender: TObject);
var
  zoom : TZoom;
begin
  zoom := model.Zoom;
  zoom.ZoomOut();
  UpdateView;

end;

procedure TOperationForm.LoadList();
var
  fileList: TStringList;
  repogitory: TRepogitory;
  i : Integer;
begin
  repogitory := model.Repogitory;
  FFilesListBoxLoaded := True;
  FilesListBox.Items.Clear; // Clear any existing items
  fileList := repogitory.GetFileNames; // Get the filenames from the model
  try
    FilesListBox.Items.Assign(fileList); // Assign the list to ListBox
  finally
    fileList.Free; // Free the TStringList
  end;

  for i := 0 to FilesListBox.Items.Count - 1 do
  begin
       FilesListBox.Selected[i] := repogitory.Selected[i];
  end;

  FFilesListBoxLoaded := False;
end;

procedure TOperationForm.NextButtonClick(Sender: TObject);
begin
  model.Next;
  UpdateView;
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
  UpdateView

end;

procedure TOperationForm.LoadBitmap;
var
  Bitmap : TBitmap;
begin

  //FitImageSize(Image1, Panel1.Width, Panel1.Height, model.ThumbnailRatio);

  try
    // PDFium ページを Delphi ビットマップに描画
    Bitmap := model.GetThumbnailBitmap(Panel1.Width, Panel1.Height);
    Image1.Width := Bitmap.Width;
    Image1.Height := Bitmap.Height;
    Image1.Left := (Panel1.Width - Bitmap.Width) div 2;
    Image1.Top := (Panel1.Height - Bitmap.Height) div 2;

    Image1.Picture.Bitmap.Assign(Bitmap);
  finally
    Bitmap.Free;
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


  UpdateView

end;

procedure TOperationForm.DelteButtonClick(Sender: TObject);
begin
  model.Repogitory.Delete;
  UpdateView();
end;

procedure TOperationForm.DeselectButtonClick(Sender: TObject);
begin
  model.Repogitory.Disselect;
  UpdateView();
end;

procedure TOperationForm.BackGroundDisplayButtonClick(Sender: TObject);
begin
  model.Repogitory.Disselect;
  ViewerForm.ShowDocument() ;
  UpdateView();
end;

procedure TOperationForm.FitWindowButtonClick(Sender: TObject);
begin
    model.Zoom.fitWindow(Panel1.Width, Panel1.Height);
    UpdateView;
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
   UpdateView;
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
  UpdateView;

end;

procedure TOperationForm.LastPageButtonClick(Sender: TObject);
begin
  model.LastPage();
  UpdateView;
end;

procedure TOperationForm.FirstPageButtonClick(Sender: TObject);
begin
  model.FirstPage();
  UpdateView;
end;

procedure TOperationForm.Rotate0ButtonClick(Sender: TObject);
begin
  model.Rotate(0);
    UpdateView      ;
end;

procedure TOperationForm.Rotate180ButtonClick(Sender: TObject);
begin
  model.Rotate(180);    UpdateView  ;
end;

procedure TOperationForm.Rotate270ButtonClick(Sender: TObject);
begin
  model.Rotate(270);     UpdateView   ;
end;

procedure TOperationForm.Rotate90ButtonClick(Sender: TObject);
begin
  model.Rotate(90);    UpdateView ;
end;

procedure TOperationForm.SelectAllButtonClick(Sender: TObject);
begin
  model.Repogitory.SelectAll;
  UpdateView();
end;

procedure TOperationForm.ViewAllButtonClick(Sender: TObject);
begin
  model.Zoom.Rate:=1.0;
  UpdateView();
end;

procedure TOperationForm.ViewerCloseButtonClick(Sender: TObject);
begin
  ViewerForm.Close();
end;


procedure TOperationForm.ViewerDisplayButtonClick(Sender: TObject);
begin
  ViewerForm.ShowDocument();
end;

procedure TOperationForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  model.Free;
end;



end.

