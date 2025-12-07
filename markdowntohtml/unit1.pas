unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    ButtonConvert: TButton;
    ButtonClear: TButton;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Label1: TLabel;
    MemoMarkdown: TMemo;
    Panel3: TPanel;
    Label2: TLabel;
    MemoHTML: TMemo;
    procedure ButtonConvertClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
  private
    function MarkdownToHTML(const Markdown: string): string;
    function ProcessHeaders(const Line: string): string;
    function ProcessInline(const InputText: string): string;
    function ProcessLists(const Lines: TStringList): string;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.ButtonConvertClick(Sender: TObject);
begin
  MemoHTML.Text := MarkdownToHTML(MemoMarkdown.Text);
end;

procedure TForm1.ButtonClearClick(Sender: TObject);
begin
  MemoMarkdown.Clear;
  MemoHTML.Clear;
end;

function TForm1.MarkdownToHTML(const Markdown: string): string;
var
  Lines: TStringList;
  i: Integer;
  Line: string;
  InList: Boolean;
  ListType: string;
  HTML: TStringList;
  CurrentList: TStringList;
begin
  HTML := TStringList.Create;
  Lines := TStringList.Create;
  CurrentList := TStringList.Create;
  try
    Lines.Text := Markdown;
    InList := False;
    ListType := '';
    
    HTML.Add('<!DOCTYPE html>');
    HTML.Add('<html>');
    HTML.Add('<head>');
    HTML.Add('<meta charset="UTF-8">');
    HTML.Add('<title>Markdown to HTML</title>');
    HTML.Add('<style>');
    HTML.Add('body { font-family: Arial, sans-serif; max-width: 800px; margin: 40px auto; padding: 20px; line-height: 1.6; }');
    HTML.Add('h1 { border-bottom: 2px solid #333; padding-bottom: 10px; }');
    HTML.Add('h2 { border-bottom: 1px solid #ccc; padding-bottom: 5px; margin-top: 30px; }');
    HTML.Add('code { background-color: #f4f4f4; padding: 2px 4px; border-radius: 3px; }');
    HTML.Add('pre { background-color: #f4f4f4; padding: 10px; border-radius: 5px; overflow-x: auto; }');
    HTML.Add('ul, ol { margin-left: 20px; }');
    HTML.Add('a { color: #0066cc; text-decoration: none; }');
    HTML.Add('a:hover { text-decoration: underline; }');
    HTML.Add('</style>');
    HTML.Add('</head>');
    HTML.Add('<body>');
    
    i := 0;
    while i < Lines.Count do
    begin
      Line := Lines[i];
      
      // 空行の処理
      if Trim(Line) = '' then
      begin
        if InList then
        begin
          if ListType = 'ul' then
            HTML.Add('</ul>')
          else
            HTML.Add('</ol>');
          InList := False;
          ListType := '';
          CurrentList.Clear;
        end;
        HTML.Add('');
        Inc(i);
        Continue;
      end;
      
      // 見出しの処理
      if (Line.StartsWith('# ')) then
      begin
        if InList then
        begin
          if ListType = 'ul' then
            HTML.Add('</ul>')
          else
            HTML.Add('</ol>');
          InList := False;
          ListType := '';
          CurrentList.Clear;
        end;
        HTML.Add('<h1>' + ProcessInline(Copy(Line, 3, Length(Line))) + '</h1>');
        Inc(i);
        Continue;
      end;
      
      if (Line.StartsWith('## ')) then
      begin
        if InList then
        begin
          if ListType = 'ul' then
            HTML.Add('</ul>')
          else
            HTML.Add('</ol>');
          InList := False;
          ListType := '';
          CurrentList.Clear;
        end;
        HTML.Add('<h2>' + ProcessInline(Copy(Line, 4, Length(Line))) + '</h2>');
        Inc(i);
        Continue;
      end;
      
      if (Line.StartsWith('### ')) then
      begin
        if InList then
        begin
          if ListType = 'ul' then
            HTML.Add('</ul>')
          else
            HTML.Add('</ol>');
          InList := False;
          ListType := '';
          CurrentList.Clear;
        end;
        HTML.Add('<h3>' + ProcessInline(Copy(Line, 5, Length(Line))) + '</h3>');
        Inc(i);
        Continue;
      end;
      
      // リストの処理
      if (Line.StartsWith('- ') or Line.StartsWith('* ')) then
      begin
        if not InList or (ListType <> 'ul') then
        begin
          if InList and (ListType = 'ol') then
          begin
            HTML.Add('</ol>');
            CurrentList.Clear;
          end;
          HTML.Add('<ul>');
          InList := True;
          ListType := 'ul';
        end;
        HTML.Add('<li>' + ProcessInline(Copy(Line, 3, Length(Line))) + '</li>');
        Inc(i);
        Continue;
      end;
      
      // 番号付きリストの処理
      if (Length(Line) >= 2) and (Line[1] in ['0'..'9']) and (Line[2] = '.') and 
         ((Length(Line) = 2) or (Line[3] = ' ')) then
      begin
        if not InList or (ListType <> 'ol') then
        begin
          if InList and (ListType = 'ul') then
          begin
            HTML.Add('</ul>');
            CurrentList.Clear;
          end;
          HTML.Add('<ol>');
          InList := True;
          ListType := 'ol';
        end;
        // 番号とピリオドをスキップ
        if Length(Line) > 2 then
          HTML.Add('<li>' + ProcessInline(Trim(Copy(Line, 3, Length(Line)))) + '</li>')
        else
          HTML.Add('<li></li>');
        Inc(i);
        Continue;
      end;
      
      // 通常の段落
      if InList then
      begin
        if ListType = 'ul' then
          HTML.Add('</ul>')
        else
          HTML.Add('</ol>');
        InList := False;
        ListType := '';
        CurrentList.Clear;
      end;
      
      HTML.Add('<p>' + ProcessInline(Line) + '</p>');
      Inc(i);
    end;
    
    // 最後にリストが開いている場合は閉じる
    if InList then
    begin
      if ListType = 'ul' then
        HTML.Add('</ul>')
      else
        HTML.Add('</ol>');
    end;
    
    HTML.Add('</body>');
    HTML.Add('</html>');
    
    Result := HTML.Text;
  finally
    HTML.Free;
    Lines.Free;
    CurrentList.Free;
  end;
end;

function TForm1.ProcessInline(const InputText: string): string;
var
  ResultText: string;
  i: Integer;
  InCode: Boolean;
  CodeStart: Integer;
  LinkStart, BracketEnd, ParenStart, ParenEnd, LinkEnd: Integer;
  LinkText, LinkURL, BeforeLink, AfterLink: string;
  BoldEnd: Integer;
  BoldText, BeforeBold, AfterBold: string;
  ItalicEnd: Integer;
  ItalicText, BeforeItalic, AfterItalic: string;
begin
  ResultText := InputText;
  InCode := False;
  
  // コードブロック（バッククォート）の処理
  i := 1;
  while i <= Length(ResultText) do
  begin
    if ResultText[i] = '`' then
    begin
      if not InCode then
      begin
        CodeStart := i;
        InCode := True;
        // 開始タグを挿入
        ResultText := Copy(ResultText, 1, i - 1) + '<code>' + Copy(ResultText, i + 1, Length(ResultText));
        i := i + 6; // '<code>'の長さ
      end
      else
      begin
        // 終了タグを挿入
        ResultText := Copy(ResultText, 1, i - 1) + '</code>' + Copy(ResultText, i + 1, Length(ResultText));
        i := i + 7; // '</code>'の長さ
        InCode := False;
      end;
    end
    else
      Inc(i);
  end;
  
  // リンクの処理 [text](url)
  i := 1;
  while i <= Length(ResultText) do
  begin
    if (ResultText[i] = '[') and (Pos('](', Copy(ResultText, i, Length(ResultText))) > 0) then
    begin
      LinkStart := i;
      BracketEnd := Pos(']', Copy(ResultText, i, Length(ResultText)));
      if BracketEnd > 0 then
      begin
        LinkText := Copy(ResultText, i + 1, BracketEnd - 2);
        ParenStart := i + BracketEnd;
        if (ParenStart <= Length(ResultText)) and (ResultText[ParenStart] = '(') then
        begin
          ParenEnd := Pos(')', Copy(ResultText, ParenStart, Length(ResultText)));
          if ParenEnd > 0 then
          begin
            LinkURL := Copy(ResultText, ParenStart + 1, ParenEnd - 2);
            LinkEnd := ParenStart + ParenEnd - 1;
            BeforeLink := Copy(ResultText, 1, LinkStart - 1);
            AfterLink := Copy(ResultText, LinkEnd + 1, Length(ResultText));
            ResultText := BeforeLink + '<a href="' + LinkURL + '">' + LinkText + '</a>' + AfterLink;
            i := LinkStart + Length('<a href="' + LinkURL + '">' + LinkText + '</a>');
            Continue;
          end;
        end;
      end;
    end;
    Inc(i);
  end;
  
  // 太字の処理 **text**
  i := 1;
  while i <= Length(ResultText) - 1 do
  begin
    if (ResultText[i] = '*') and (ResultText[i + 1] = '*') then
    begin
      BoldEnd := Pos('**', Copy(ResultText, i + 2, Length(ResultText)));
      if BoldEnd > 0 then
      begin
        BoldText := Copy(ResultText, i + 2, BoldEnd - 1);
        BeforeBold := Copy(ResultText, 1, i - 1);
        AfterBold := Copy(ResultText, i + BoldEnd + 3, Length(ResultText));
        ResultText := BeforeBold + '<strong>' + BoldText + '</strong>' + AfterBold;
        i := i + Length('<strong>' + BoldText + '</strong>');
        Continue;
      end;
    end;
    Inc(i);
  end;
  
  // イタリックの処理 *text*（太字の後に処理）
  i := 1;
  while i <= Length(ResultText) do
  begin
    if (ResultText[i] = '*') and 
       ((i = 1) or (ResultText[i - 1] <> '*')) and
       ((i = Length(ResultText)) or (ResultText[i + 1] <> '*')) then
    begin
      ItalicEnd := Pos('*', Copy(ResultText, i + 1, Length(ResultText)));
      if ItalicEnd > 0 then
      begin
        ItalicText := Copy(ResultText, i + 1, ItalicEnd - 1);
        // <code>タグ内でないことを確認
        if (Pos('<code>', Copy(ResultText, 1, i)) = 0) or 
           (Pos('</code>', Copy(ResultText, 1, i)) > Pos('<code>', Copy(ResultText, 1, i))) then
        begin
          BeforeItalic := Copy(ResultText, 1, i - 1);
          AfterItalic := Copy(ResultText, i + ItalicEnd + 1, Length(ResultText));
          ResultText := BeforeItalic + '<em>' + ItalicText + '</em>' + AfterItalic;
          i := i + Length('<em>' + ItalicText + '</em>');
          Continue;
        end;
      end;
    end;
    Inc(i);
  end;
  
  Result := ResultText;
end;

function TForm1.ProcessHeaders(const Line: string): string;
begin
  Result := Line;
end;

function TForm1.ProcessLists(const Lines: TStringList): string;
begin
  Result := '';
end;

end.

