import re

# PdfiumCoreで使用されている関数のリスト
used_functions = {
    'FPDF_CloseDocument',
    'FPDF_LoadDocument', 
    'FPDF_GetPageCount',
    'FPDF_LoadPage',
    'FPDF_ClosePage',
    'FPDF_RenderPageBitmap',
    'FPDFBitmap_GetWidth',
    'FPDFBitmap_GetHeight',
    'FPDFBitmap_GetStride',
    'FPDFBitmap_Create',
    'FPDFBitmap_CreateEx',
    'FPDFBitmap_Destroy',
    'FPDFBitmap_GetBuffer',
    'FPDFBitmap_FillRect',
    'InitPDFium',
    'FPDF_InitLibraryWithConfig',
    'FPDF_DestroyLibrary',
    'FPDF_ARGB',
    'FPDF_GetBValue',
    'FPDF_GetGValue',
    'FPDF_GetRValue',
    'FPDF_GetAValue',
    'PDF_USE_XFA',
    'PDF_IsSkiaAvailable',
    'FPDF_InitLibrary',
    'FPDF_InitLibraryWithConfig',
    'FPDF_DestroyLibrary'
}

# PdfiumLib.pasファイルを読み込む
with open('Core/PdfiumLib.pas', 'r', encoding='utf-8') as f:
    lines = f.readlines()

output_lines = []
i = 0

while i < len(lines):
    line = lines[i]
    
    # 関数宣言の開始を検出
    func_match = re.match(r'^(function|procedure)\s+(\w+)\s*\(', line)
    if func_match:
        func_name = func_match.group(2)
        
        # 使用されていない関数の場合、スキップモードを開始
        if func_name not in used_functions:
            # 関数の完全な宣言をスキップ
            # セミコロンが見つかるまでスキップ
            while i < len(lines) and not re.match(r'^.*;\s*$', lines[i]):
                i += 1
            # セミコロン行もスキップ
            if i < len(lines):
                i += 1
            continue
    
    # 通常の行は出力
    output_lines.append(line)
    i += 1

# 結果を書き込む
with open('Core/PdfiumLib.pas', 'w', encoding='utf-8') as f:
    f.writelines(output_lines)

print("使用されていない関数を削除しました")
