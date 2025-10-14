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

# PdfiumLib.pasファイルを読み込む
with open('Core/PdfiumLib.pas', 'r', encoding='utf-8') as f:
    content = f.read()

# 関数宣言を抽出
function_declarations = []
lines = content.split('\n')

for i, line in enumerate(lines):
    # varで始まる行を検出
    if re.match(r'^\s*var\s*$', line):
        # 次の行を確認
        if i + 1 < len(lines):
            next_line = lines[i + 1]
            
            # FPDF_またはFPDFで始まる関数手続き宣言を検出
            match = re.match(r'\s+((?:FPDF_|FPDF)\w+):\s*(procedure|function)', next_line)
            
            if match:
                func_name = match.group(1)
                func_type = match.group(2)
                
                # 完全な宣言を取得（複数行にまたがる可能性がある）
                full_decl = next_line.strip()
                j = i + 2
                
                # セミコロンが見つかるまで行を結合
                while j < len(lines) and ';' not in full_decl:
                    full_decl += ' ' + lines[j].strip()
                    j += 1
                
                function_declarations.append((func_name, func_type, full_decl))

# 使用されていない関数を特定
unused_functions = []
for func_name, func_type, full_decl in function_declarations:
    if func_name not in used_functions:
        unused_functions.append((func_name, func_type, full_decl))

# 構造体/型定義を抽出
type_declarations = []
for i, line in enumerate(lines):
    # typeで始まる行を検出
    if re.match(r'^\s*type\s*$', line):
        # 次の行を確認
        if i + 1 < len(lines):
            next_line = lines[i + 1]
            
            # 構造体や型定義を検出
            if (re.match(r'\s+(\w+)\s*=\s*(record|class|interface)', next_line) or
                re.match(r'\s+(\w+)\s*=\s*\w+', next_line)):
                
                # 完全な宣言を取得
                full_decl = next_line.strip()
                j = i + 2
                
                # セミコロンが見つかるまで行を結合
                while j < len(lines) and ';' not in full_decl and not re.match(r'^\s*end\s*;?\s*$', lines[j]):
                    full_decl += ' ' + lines[j].strip()
                    j += 1
                
                type_name = re.match(r'\s+(\w+)\s*=', next_line).group(1)
                type_declarations.append((type_name, full_decl))

# 使用されている型を特定（PdfiumCoreで使用されている型）
used_types = {
    'FPDF_DOCUMENT', 'FPDF_PAGE', 'FPDF_BITMAP', 'FPDF_DWORD', 'FPDF_FORMFILLINFO',
    'FPDF_LIBRARY_CONFIG', 'PFPDF_LIBRARY_CONFIG', 'FPDF_BOOL', 'FPDF_STRING',
    'FPDF_BYTESTRING', 'FPDF_RENDERERTYPE', 'FPDF_RENDERERTYPE_AGG'
}

# 使用されていない型を特定
unused_types = []
for type_name, full_decl in type_declarations:
    if type_name not in used_types:
        unused_types.append((type_name, full_decl))

# 結果をtxtファイルに保存
with open('unused_functions_and_types.txt', 'w', encoding='utf-8') as f:
    f.write("PdfiumCoreで使用されていない関数と構造体のリスト\n")
    f.write("=" * 60 + "\n\n")
    
    f.write("【使用されていない関数】\n")
    f.write("-" * 30 + "\n")
    for func_name, func_type, full_decl in unused_functions:
        f.write(f"{func_name} ({func_type})\n")
        f.write(f"  {full_decl}\n\n")
    
    f.write(f"\n合計: {len(unused_functions)}個の関数\n\n")
    
    f.write("【使用されていない型/構造体】\n")
    f.write("-" * 30 + "\n")
    for type_name, full_decl in unused_types:
        f.write(f"{type_name}\n")
        f.write(f"  {full_decl}\n\n")
    
    f.write(f"\n合計: {len(unused_types)}個の型/構造体\n\n")
    
    f.write("【使用されている関数】\n")
    f.write("-" * 30 + "\n")
    for func_name in sorted(used_functions):
        f.write(f"{func_name}\n")
    
    f.write(f"\n合計: {len(used_functions)}個の関数\n")

print(f"未使用の関数: {len(unused_functions)}個")
print(f"未使用の型/構造体: {len(unused_types)}個")
print("結果を unused_functions_and_types.txt に保存しました")
