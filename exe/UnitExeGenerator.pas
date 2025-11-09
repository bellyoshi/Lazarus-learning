unit UnitExeGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, DateUtils;

procedure GenerateHelloWorldExe(const OutputFileName: string);

implementation

const
  ALIGNMENT = $1000;        // ファイルおよびメモリにおけるアライメント
  EXE_HEADER_SIZE = $1000;  // EXEヘッダの総合サイズ（アライメント考慮）
  USE_DLL = 1;              // 使用するDLLの数
  
  // Windows API定数（Free Pascal用）
  IMAGE_FILE_EXECUTABLE_IMAGE = $0002;
  IMAGE_FILE_32BIT_MACHINE = $0100;
  IMAGE_FILE_LINE_NUMS_STRIPPED = $0004;
  IMAGE_FILE_LOCAL_SYMS_STRIPPED = $0008;

// ネイティブコード（コードセクションに保存されるバイナリ）
// push 0
// push dword ptr["exetest"]
// push dword ptr["Hello World!"]
// push 0
// call MessageBoxA
// ret 0
const
  NativeBuffer: array[0..20] of Byte = (
    $6A, $00,                         // push 0
    $68, $0D, $30, $40, $00,         // push dword ptr["exetest"]
    $68, $00, $30, $40, $00,         // push dword ptr["Hello World!"]
    $6A, $00,                         // push 0
    $FF, $15, $4E, $20, $40, $00,    // call MessageBoxA
    $C3                               // ret 0
  );

// インポートセクションに保存されるデータ
// DLL名
const
  szDllName: array[0..15] of Char = 'user32.dll'#0#0#0#0#0#0;

// 初期済みデータ（データセクションに保存されるバイナリ）
const
  InitBuffer: array[0..25] of Byte = (
    $48, $65, $6C, $6C, $6F, $20, $57, $6F, $72, $6C, $64, $21, $00,  // "Hello World!\0"
    $65, $78, $65, $20, $74, $65, $73, $74, $00, $00, $00, $00, $00  // "exe test\0\0\0\0\0"
  );

// PE32形式の実行ファイルを生成（CodeZine記事に基づく実装）
// 実行時にMessageBoxで"Hello World!"を表示
procedure GenerateHelloWorldExe(const OutputFileName: string);
var
  Stream: TFileStream;
  Zero: Byte;
  
  // セクションサイズと位置
  SizeOf_CodeSection: Integer;
  SizeOf_ImportSection: Integer;
  SizeOf_DataSection: Integer;
  Pos_CodeSection: Integer;
  Pos_ImportSection: Integer;
  Pos_DataSection: Integer;
  
  // DOSヘッダー
  ImageDosHeader: IMAGE_DOS_HEADER;
  
  // PEヘッダー
  ImagePeHdr: IMAGE_NT_HEADERS32;
  
  // セクションヘッダー
  CodeSectionHeader: IMAGE_SECTION_HEADER;
  ImportSectionHeader: IMAGE_SECTION_HEADER;
  DataSectionHeader: IMAGE_SECTION_HEADER;
  
  // インポートセクション用のデータ
  ImportDesc: array[0..USE_DLL] of IMAGE_IMPORT_DESCRIPTOR;
  LookupTable: array[0..1] of DWORD;
  HintTable: array[0..13] of Byte;  // WORD(2) + "MessageBoxA\0"(11) = 13バイト
  
  // NULL空間
  szNullSpace: array[0..ALIGNMENT-1] of Byte;
  
  iFileOffset: Integer;
  dwAccessBytes: DWORD;
  
  procedure WriteZero(Count: Integer);
  var
    i: Integer;
  begin
    Zero := 0;
    for i := 1 to Count do
      Stream.Write(Zero, 1);
  end;
  
begin
  // 必要なデータを初期化
  FillChar(szNullSpace, SizeOf(szNullSpace), 0);
  FillChar(ImportDesc, SizeOf(ImportDesc), 0);
  FillChar(LookupTable, SizeOf(LookupTable), 0);
  FillChar(HintTable, SizeOf(HintTable), 0);
  
  // ヒントテーブルを設定（記事のコードに合わせる）
  // ヒントテーブルはWORD(2バイト)のヒント番号 + NULL終端文字列
  HintTable[0] := 0;  // ヒント番号の下位バイト
  HintTable[1] := 0;  // ヒント番号の上位バイト
  Move('MessageBoxA'#0, HintTable[2], 11);  // "MessageBoxA\0" (10文字 + NULL = 11バイト)
  
  // コードセクションのサイズ
  if SizeOf(NativeBuffer) mod ALIGNMENT <> 0 then
    SizeOf_CodeSection := SizeOf(NativeBuffer) + (ALIGNMENT - (SizeOf(NativeBuffer) mod ALIGNMENT))
  else
    SizeOf_CodeSection := SizeOf(NativeBuffer);
  
  // インポートセクションのサイズ
  SizeOf_ImportSection := ALIGNMENT;
  
  // データセクションのサイズ
  if SizeOf(InitBuffer) mod ALIGNMENT <> 0 then
    SizeOf_DataSection := SizeOf(InitBuffer) + (ALIGNMENT - (SizeOf(InitBuffer) mod ALIGNMENT))
  else
    SizeOf_DataSection := SizeOf(InitBuffer);
  
  // コードセクションの開始位置
  Pos_CodeSection := EXE_HEADER_SIZE;
  
  // インポートセクションの開始位置
  Pos_ImportSection := Pos_CodeSection + SizeOf_CodeSection;
  
  // データセクションの開始位置
  Pos_DataSection := Pos_CodeSection + SizeOf_CodeSection + SizeOf_ImportSection;
  
  // インポートディレクトリテーブルを初期化（記事のコードに合わせる）
  ImportDesc[0].OriginalFirstThunk := Pos_ImportSection +
    (USE_DLL + 1) * SizeOf(IMAGE_IMPORT_DESCRIPTOR) +
    SizeOf(szDllName);
  ImportDesc[0].TimeDateStamp := 0;
  ImportDesc[0].ForwarderChain := 0;
  ImportDesc[0].Name := Pos_ImportSection +
    (USE_DLL + 1) * SizeOf(IMAGE_IMPORT_DESCRIPTOR);
  ImportDesc[0].FirstThunk := ImportDesc[0].OriginalFirstThunk +
    SizeOf(LookupTable) + SizeOf(HintTable);
  
  // ルックアップテーブルを初期化（記事のコードに合わせる）
  LookupTable[0] := Pos_ImportSection +
    (USE_DLL + 1) * SizeOf(IMAGE_IMPORT_DESCRIPTOR) +
    SizeOf(szDllName) + SizeOf(LookupTable);
  LookupTable[1] := 0; // 終端
  
  // DOSヘッダーを初期化
  FillChar(ImageDosHeader, SizeOf(ImageDosHeader), 0);
  ImageDosHeader.e_magic := $5A4D;  // 'MZ'
  ImageDosHeader.e_cblp := $0090;
  ImageDosHeader.e_cp := $0003;
  ImageDosHeader.e_crlc := 0;
  ImageDosHeader.e_cparhdr := 4;
  ImageDosHeader.e_minalloc := $0000;
  ImageDosHeader.e_maxalloc := $FFFF;
  ImageDosHeader.e_ss := $0000;
  ImageDosHeader.e_sp := $00B8;
  ImageDosHeader.e_csum := $0000;
  ImageDosHeader.e_ip := $0000;
  ImageDosHeader.e_cs := $0000;
  ImageDosHeader.e_lfarlc := $0040;
  ImageDosHeader.e_ovno := $0000;
  ImageDosHeader.e_oemid := $0000;
  ImageDosHeader.e_oeminfo := $0000;
  ImageDosHeader.e_lfanew := $0100; // PEヘッダの位置
  
  // PEヘッダーを初期化
  FillChar(ImagePeHdr, SizeOf(ImagePeHdr), 0);
  ImagePeHdr.Signature := IMAGE_NT_SIGNATURE;
  ImagePeHdr.FileHeader.Machine := IMAGE_FILE_MACHINE_I386;
  ImagePeHdr.FileHeader.NumberOfSections := 3; // セクション数
  // time(NULL)に相当する処理（記事のコードに合わせる）
  ImagePeHdr.FileHeader.TimeDateStamp := DWORD(DateTimeToUnix(Now));
  ImagePeHdr.FileHeader.PointerToSymbolTable := 0;
  ImagePeHdr.FileHeader.NumberOfSymbols := 0;
  ImagePeHdr.FileHeader.SizeOfOptionalHeader := IMAGE_SIZEOF_NT_OPTIONAL32_HEADER;
  ImagePeHdr.FileHeader.Characteristics := IMAGE_FILE_EXECUTABLE_IMAGE or
    IMAGE_FILE_32BIT_MACHINE or IMAGE_FILE_LINE_NUMS_STRIPPED or
    IMAGE_FILE_LOCAL_SYMS_STRIPPED;
  
  ImagePeHdr.OptionalHeader.Magic := $010B;
  ImagePeHdr.OptionalHeader.MajorLinkerVersion := 1;
  ImagePeHdr.OptionalHeader.MinorLinkerVersion := 0;
  ImagePeHdr.OptionalHeader.SizeOfCode := SizeOf(NativeBuffer);
  ImagePeHdr.OptionalHeader.SizeOfInitializedData := SizeOf(InitBuffer);
  ImagePeHdr.OptionalHeader.SizeOfUninitializedData := 0;
  ImagePeHdr.OptionalHeader.AddressOfEntryPoint := Pos_CodeSection;
  ImagePeHdr.OptionalHeader.BaseOfCode := Pos_CodeSection;
  ImagePeHdr.OptionalHeader.BaseOfData := Pos_DataSection;
  ImagePeHdr.OptionalHeader.ImageBase := $00400000;
  ImagePeHdr.OptionalHeader.SectionAlignment := ALIGNMENT;
  ImagePeHdr.OptionalHeader.FileAlignment := ALIGNMENT;
  ImagePeHdr.OptionalHeader.MajorOperatingSystemVersion := 4;
  ImagePeHdr.OptionalHeader.MinorOperatingSystemVersion := 0;
  ImagePeHdr.OptionalHeader.MajorImageVersion := 0;
  ImagePeHdr.OptionalHeader.MinorImageVersion := 0;
  ImagePeHdr.OptionalHeader.MajorSubsystemVersion := 4;
  ImagePeHdr.OptionalHeader.MinorSubsystemVersion := 0;
  ImagePeHdr.OptionalHeader.Win32VersionValue := 0;
  ImagePeHdr.OptionalHeader.SizeOfImage := EXE_HEADER_SIZE +
    SizeOf_CodeSection + SizeOf_ImportSection + SizeOf_DataSection;
  ImagePeHdr.OptionalHeader.SizeOfHeaders := EXE_HEADER_SIZE;
  ImagePeHdr.OptionalHeader.CheckSum := 0;
  ImagePeHdr.OptionalHeader.Subsystem := IMAGE_SUBSYSTEM_WINDOWS_GUI;
  ImagePeHdr.OptionalHeader.DllCharacteristics := 0;
  ImagePeHdr.OptionalHeader.SizeOfStackReserve := $00100000;
  ImagePeHdr.OptionalHeader.SizeOfStackCommit := $00001000;
  ImagePeHdr.OptionalHeader.SizeOfHeapReserve := $00100000;
  ImagePeHdr.OptionalHeader.SizeOfHeapCommit := $00001000;
  ImagePeHdr.OptionalHeader.LoaderFlags := 0;
  ImagePeHdr.OptionalHeader.NumberOfRvaAndSizes := IMAGE_NUMBEROF_DIRECTORY_ENTRIES;
  
  // データディクショナリ
  ImagePeHdr.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress := Pos_ImportSection;
  ImagePeHdr.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].Size := SizeOf_ImportSection;
  // データディクショナリ（記事のコードに合わせる）
  ImagePeHdr.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IAT].VirtualAddress :=
    Pos_ImportSection +
    (USE_DLL + 1) * SizeOf(IMAGE_IMPORT_DESCRIPTOR) +
    16 * USE_DLL +           // DLL名
    SizeOf(LookupTable) +      // ルックアップテーブル
    SizeOf(HintTable);         // ヒント名（関数名）テーブル
  ImagePeHdr.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IAT].Size := SizeOf(LookupTable);
  
  // コードセクションヘッダー
  FillChar(CodeSectionHeader, SizeOf(CodeSectionHeader), 0);
  Move('.text'#0#0#0, CodeSectionHeader.Name, 8);
  CodeSectionHeader.Misc.VirtualSize := SizeOf_CodeSection;
  CodeSectionHeader.VirtualAddress := Pos_CodeSection;
  CodeSectionHeader.SizeOfRawData := SizeOf(NativeBuffer);
  CodeSectionHeader.PointerToRawData := Pos_CodeSection;
  CodeSectionHeader.PointerToRelocations := 0;
  CodeSectionHeader.PointerToLinenumbers := 0;
  CodeSectionHeader.NumberOfRelocations := 0;
  CodeSectionHeader.NumberOfLinenumbers := 0;
  CodeSectionHeader.Characteristics := IMAGE_SCN_MEM_EXECUTE or
    IMAGE_SCN_MEM_READ or IMAGE_SCN_CNT_CODE;
  
  // インポートセクションヘッダー
  FillChar(ImportSectionHeader, SizeOf(ImportSectionHeader), 0);
  Move('.idata'#0#0, ImportSectionHeader.Name, 8);
  ImportSectionHeader.Misc.VirtualSize := SizeOf_ImportSection;
  ImportSectionHeader.VirtualAddress := Pos_ImportSection;
  ImportSectionHeader.SizeOfRawData := SizeOf_ImportSection;
  ImportSectionHeader.PointerToRawData := Pos_ImportSection;
  ImportSectionHeader.PointerToRelocations := 0;
  ImportSectionHeader.PointerToLinenumbers := 0;
  ImportSectionHeader.NumberOfRelocations := 0;
  ImportSectionHeader.NumberOfLinenumbers := 0;
  ImportSectionHeader.Characteristics := IMAGE_SCN_CNT_INITIALIZED_DATA or
    IMAGE_SCN_MEM_READ;
  
  // データセクションヘッダー
  FillChar(DataSectionHeader, SizeOf(DataSectionHeader), 0);
  Move('.sdata'#0#0, DataSectionHeader.Name, 8);
  DataSectionHeader.Misc.VirtualSize := SizeOf_DataSection;
  DataSectionHeader.VirtualAddress := Pos_DataSection;
  DataSectionHeader.SizeOfRawData := SizeOf_DataSection;
  DataSectionHeader.PointerToRawData := Pos_DataSection;
  DataSectionHeader.PointerToRelocations := 0;
  DataSectionHeader.PointerToLinenumbers := 0;
  DataSectionHeader.NumberOfRelocations := 0;
  DataSectionHeader.NumberOfLinenumbers := 0;
  DataSectionHeader.Characteristics := IMAGE_SCN_CNT_INITIALIZED_DATA or
    IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE;
  
  // ファイルを作成
  Stream := TFileStream.Create(OutputFileName, fmCreate);
  try
    // DOSヘッダー
    Stream.Write(ImageDosHeader, SizeOf(ImageDosHeader));
    iFileOffset := SizeOf(ImageDosHeader);
    
    // MS-DOSスタブは省略、0x0100までNULLを並べる
    WriteZero($0100 - iFileOffset);
    iFileOffset := $0100;
    
    // PEヘッダー
    Stream.Write(ImagePeHdr, SizeOf(ImagePeHdr));
    iFileOffset := iFileOffset + SizeOf(ImagePeHdr);
    
    // コードセクションヘッダー
    Stream.Write(CodeSectionHeader, SizeOf(CodeSectionHeader));
    iFileOffset := iFileOffset + SizeOf(CodeSectionHeader);
    
    // インポートセクションヘッダー
    Stream.Write(ImportSectionHeader, SizeOf(ImportSectionHeader));
    iFileOffset := iFileOffset + SizeOf(ImportSectionHeader);
    
    // データセクションヘッダー
    Stream.Write(DataSectionHeader, SizeOf(DataSectionHeader));
    iFileOffset := iFileOffset + SizeOf(DataSectionHeader);
    
    // EXE_HEADER_SIZEまでNULLを並べる
    WriteZero(EXE_HEADER_SIZE - iFileOffset);
    iFileOffset := EXE_HEADER_SIZE;
    
    // コードセクション
    Stream.Write(NativeBuffer, SizeOf(NativeBuffer));
    iFileOffset := iFileOffset + SizeOf(NativeBuffer);
    
    // Pos_ImportSectionまでNULLを並べる
    WriteZero(Pos_ImportSection - iFileOffset);
    iFileOffset := Pos_ImportSection;
    
    // インポートディレクトリテーブル（Nullディレクトリテーブルを含む）
    Stream.Write(ImportDesc, (USE_DLL + 1) * SizeOf(IMAGE_IMPORT_DESCRIPTOR));
    iFileOffset := iFileOffset + (USE_DLL + 1) * SizeOf(IMAGE_IMPORT_DESCRIPTOR);
    
    // DLL名
    Stream.Write(szDllName, SizeOf(szDllName));
    iFileOffset := iFileOffset + SizeOf(szDllName);
    
    // ルックアップテーブル
    Stream.Write(LookupTable, SizeOf(LookupTable));
    iFileOffset := iFileOffset + SizeOf(LookupTable);
    
    // ヒントテーブル
    Stream.Write(HintTable, SizeOf(HintTable));
    iFileOffset := iFileOffset + SizeOf(HintTable);
    
    // インポートアドレステーブル
    Stream.Write(LookupTable, SizeOf(LookupTable));
    iFileOffset := iFileOffset + SizeOf(LookupTable);
    
    // Pos_DataSectionまでNULLを並べる
    WriteZero(Pos_DataSection - iFileOffset);
    iFileOffset := Pos_DataSection;
    
    // データテーブル
    Stream.Write(InitBuffer, SizeOf(InitBuffer));
    iFileOffset := iFileOffset + SizeOf(InitBuffer);
    
    // ファイルアラインメントを考慮
    if iFileOffset mod ALIGNMENT <> 0 then
    begin
      WriteZero(ALIGNMENT - (iFileOffset mod ALIGNMENT));
    end;
    
  finally
    Stream.Free;
  end;
end;

end.

