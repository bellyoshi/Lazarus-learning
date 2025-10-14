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
    'FPDF_InitLibraryWithConfig',  # InitPDFium内で使用
    'FPDF_DestroyLibrary',         # finalizationで使用
    'FPDF_ARGB',                   # ユーティリティ関数
    'FPDF_GetBValue',              # ユーティリティ関数
    'FPDF_GetGValue',              # ユーティリティ関数
    'FPDF_GetRValue',              # ユーティリティ関数
    'FPDF_GetAValue',              # ユーティリティ関数
    'PDF_USE_XFA',                 # ヘルパー関数
    'PDF_IsSkiaAvailable',         # ヘルパー関数
    'FPDF_InitLibrary'             # InitPDFium内で使用される可能性
}

# PdfiumLib.pasファイルを読み込む
with open('Core/PdfiumLib.pas', 'r', encoding='utf-8') as f:
    lines = f.readlines()

output_lines = []
skip_mode = False
skip_until_end = False
current_function = None

i = 0
while i < len(lines):
    line = lines[i]
    
    # 関数宣言の開始を検出
    func_match = re.match(r'^(function|procedure)\s+(\w+)\s*\(', line)
    if func_match:
        func_name = func_match.group(2)
        current_function = func_name
        
        # 使用されていない関数の場合、スキップモードを開始
        if func_name not in used_functions:
            skip_mode = True
            skip_until_end = True
            i += 1
            continue
    
    # スキップモードの場合
    if skip_mode:
        # 関数の終了を検出（セミコロンで終わる行）
        if re.match(r'^.*;\s*$', line) and not line.strip().startswith('//'):
            skip_mode = False
            skip_until_end = False
            current_function = None
            i += 1
            continue
        # コメント行はスキップ
        if line.strip().startswith('//') or line.strip().startswith('{') or line.strip().startswith('(*'):
            i += 1
            continue
        # 空行はスキップ
        if line.strip() == '':
            i += 1
            continue
        # その他の行はスキップ
        i += 1
        continue
    
    # 通常の行は出力
    output_lines.append(line)
    i += 1

# 結果を書き込む
with open('Core/PdfiumLib.pas', 'w', encoding='utf-8') as f:
    f.writelines(output_lines)

print("使用されていない関数を削除しました")
