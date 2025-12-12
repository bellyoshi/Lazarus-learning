unit QuestionData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser;

type
  TQuestion = class
  private
    FQuestion: string;
    FChoices: TStringList;
    FCorrectAnswer: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function ToJSON: TJSONObject;
    procedure FromJSON(JSONObj: TJSONObject);
    property Question: string read FQuestion write FQuestion;
    property Choices: TStringList read FChoices;
    property CorrectAnswer: Integer read FCorrectAnswer write FCorrectAnswer;
  end;

  TQuestionList = class
  private
    FQuestions: TList;
    function GetCount: Integer;
    function GetQuestion(Index: Integer): TQuestion;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Question: TQuestion);
    procedure Delete(Index: Integer);
    procedure Clear;
    function ToJSON: TJSONArray;
    procedure FromJSON(JSONArray: TJSONArray);
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
    property Count: Integer read GetCount;
    property Questions[Index: Integer]: TQuestion read GetQuestion; default;
  end;

implementation

constructor TQuestion.Create;
begin
  inherited Create;
  FChoices := TStringList.Create;
end;

destructor TQuestion.Destroy;
begin
  FChoices.Free;
  inherited Destroy;
end;

function TQuestion.ToJSON: TJSONObject;
var
  JSONArray: TJSONArray;
  i: Integer;
begin
  Result := TJSONObject.Create;
  Result.Add('question', FQuestion);
  Result.Add('correctAnswer', FCorrectAnswer);
  JSONArray := TJSONArray.Create;
  for i := 0 to FChoices.Count - 1 do
    JSONArray.Add(FChoices[i]);
  Result.Add('choices', JSONArray);
end;

procedure TQuestion.FromJSON(JSONObj: TJSONObject);
var
  JSONArray: TJSONArray;
  i: Integer;
begin
  FQuestion := JSONObj.Get('question', '');
  FCorrectAnswer := JSONObj.Get('correctAnswer', 0);
  JSONArray := JSONObj.Get('choices', TJSONArray(nil));
  FChoices.Clear;
  if Assigned(JSONArray) then
    for i := 0 to JSONArray.Count - 1 do
      FChoices.Add(JSONArray.Items[i].AsString);
end;

constructor TQuestionList.Create;
begin
  inherited Create;
  FQuestions := TList.Create;
end;

destructor TQuestionList.Destroy;
begin
  Clear;
  FQuestions.Free;
  inherited Destroy;
end;

function TQuestionList.GetCount: Integer;
begin
  Result := FQuestions.Count;
end;

function TQuestionList.GetQuestion(Index: Integer): TQuestion;
begin
  Result := TQuestion(FQuestions[Index]);
end;

procedure TQuestionList.Add(Question: TQuestion);
begin
  FQuestions.Add(Question);
end;

procedure TQuestionList.Delete(Index: Integer);
begin
  TQuestion(FQuestions[Index]).Free;
  FQuestions.Delete(Index);
end;

procedure TQuestionList.Clear;
var
  i: Integer;
begin
  for i := 0 to FQuestions.Count - 1 do
    TQuestion(FQuestions[i]).Free;
  FQuestions.Clear;
end;

function TQuestionList.ToJSON: TJSONArray;
var
  i: Integer;
begin
  Result := TJSONArray.Create;
  for i := 0 to FQuestions.Count - 1 do
    Result.Add(TQuestion(FQuestions[i]).ToJSON);
end;

procedure TQuestionList.FromJSON(JSONArray: TJSONArray);
var
  i: Integer;
  Question: TQuestion;
begin
  Clear;
  if Assigned(JSONArray) then
    for i := 0 to JSONArray.Count - 1 do
    begin
      Question := TQuestion.Create;
      Question.FromJSON(JSONArray.Items[i] as TJSONObject);
      Add(Question);
    end;
end;

procedure TQuestionList.SaveToFile(const FileName: string);
var
  JSONArray: TJSONArray;
  Stream: TStringList;
  JSONString: string;
begin
  JSONArray := ToJSON;
  try
    JSONString := JSONArray.FormatJSON([], 2);
    Stream := TStringList.Create;
    try
      Stream.Text := JSONString;
      Stream.SaveToFile(FileName);
    finally
      Stream.Free;
    end;
  finally
    JSONArray.Free;
  end;
end;

procedure TQuestionList.LoadFromFile(const FileName: string);
var
  Stream: TStringStream;
  Parser: TJSONParser;
  JSONArray: TJSONArray;
begin
  if not FileExists(FileName) then
    Exit;
  Stream := TStringStream.Create('');
  try
    Stream.LoadFromFile(FileName);
    Parser := TJSONParser.Create(Stream.DataString);
    try
      JSONArray := Parser.Parse as TJSONArray;
      try
        FromJSON(JSONArray);
      finally
        JSONArray.Free;
      end;
    finally
      Parser.Free;
    end;
  finally
    Stream.Free;
  end;
end;

end.

