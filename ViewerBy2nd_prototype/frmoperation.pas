unit frmOperation;



interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, frmViewer, ViewerModel, ControlFitter,
  PageFormUnit, SettingFormUnit, IViewUnit, FormSizeCustomizerUnit;

type

  { TOperationForm }

  TOperationForm = class(TForm, IView)
    Button1: TButton;
    BackGroundDisplayButton: TButton;
    AutoUpdateCheckBox: TCheckBox;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
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
    procedure Button1Click(Sender: TObject);
    procedure DisplaySettingMenuClick(Sender: TObject);
    procedure FilesListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure LastPageButtonClick(Sender: TObject);
    procedure FirstPageButtonClick(Sender: TObject);
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

  private
    FFilesListBoxLoaded : Boolean;
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
  if AutoUpdateCheckBox.Checked then
  begin
    model.View();
    SetCtlEnabled();
    ViewerForm.LoadBitmap();
    ViewerForm.Show();
  end;
end;

procedure TOperationForm.LoadList();
var
  fileList: TStringList;
  repogitory: TRepogitory;
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

  if (repogitory.SelectIndex >= 0) and (repogitory.SelectIndex < FilesListBox.Items.Count) then
    FilesListBox.Selected[repogitory.SelectIndex] := True;
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
  PageForm.View := Self;
  PageForm.Show;
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

  FitImageSize(Image1, Panel1.Width, Panel1.Height, model.ThumbnailRatio);

  try
    // PDFium ページを Delphi ビットマップに描画
    Bitmap := model.GetThumbnailBitmap(Image1.Width, Image1.Height);
    Image1.Picture.Bitmap.Assign(Bitmap);
  finally
    Bitmap.Free;
  end;

end;

procedure TOperationForm.Button1Click(Sender: TObject);
begin
  // PDFファイルのみを選択できるようにフィルタを設定
  OpenDialog1.Filter := 'PDF Files|*.pdf';

  if not OpenDialog1.Execute then Exit;


  model.Open(OpenDialog1.FileName);
  UpdateView

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
       if FilesListBox.Selected[i] then
          model.Repogitory.Select(i);
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

procedure TOperationForm.ViewerCloseButtonClick(Sender: TObject);
begin
  ViewerForm.Close();
end;


procedure TOperationForm.ViewerDisplayButtonClick(Sender: TObject);
begin
  model.View();
  SetCtlEnabled();
  ViewerForm.LoadBitmap();
  ViewerForm.Show();
end;

procedure TOperationForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  model.Free;
end;



end.

