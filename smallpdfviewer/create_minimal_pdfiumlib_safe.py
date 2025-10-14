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
    'FPDF_InitLibrary'
}

# 元のファイルを読み込む
with open('Core/PdfiumLib_Original.pas', 'r', encoding='utf-8') as f:
    content = f.read()

# ヘッダー部分を抽出（unit宣言からinterface終了まで）
header_match = re.search(r'(unit PdfiumLib;.*?interface.*?uses.*?end;)\s*type', content, re.DOTALL)
if header_match:
    header = header_match.group(1)
else:
    # フォールバック：最初の500行をヘッダーとして使用
    lines = content.split('\n')
    header = '\n'.join(lines[:500])

# 型定義を抽出
type_match = re.search(r'(type.*?)(const|var|function|procedure)', content, re.DOTALL)
if type_match:
    type_section = type_match.group(1)
else:
    type_section = ""

# 定数定義を抽出
const_match = re.search(r'(const.*?)(var|function|procedure)', content, re.DOTALL)
if const_match:
    const_section = const_match.group(1)
else:
    const_section = ""

# 使用されている関数の宣言を抽出
function_declarations = []
lines = content.split('\n')

for i, line in enumerate(lines):
    # 関数宣言を検索
    func_match = re.match(r'^(function|procedure)\s+(\w+)\s*\(', line)
    if func_match:
        func_name = func_match.group(2)
        if func_name in used_functions:
            # 関数の完全な宣言を取得
            func_lines = [line]
            j = i + 1
            while j < len(lines) and not re.match(r'^.*;\s*$', lines[j]):
                func_lines.append(lines[j])
                j += 1
            if j < len(lines):
                func_lines.append(lines[j])  # セミコロン行を追加
            
            function_declarations.append('\n'.join(func_lines))

# 新しいファイルを作成
new_content = header + '\n\n' + type_section + '\n\n' + const_section + '\n\n' + '\n\n'.join(function_declarations) + '\n\nend.'

# 結果を書き込む
with open('Core/PdfiumLib_Minimal_Safe.pas', 'w', encoding='utf-8') as f:
    f.write(new_content)

print("安全な最小限のPdfiumLibファイルを作成しました")
