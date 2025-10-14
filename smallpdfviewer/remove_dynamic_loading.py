import re

# PdfiumLib.pasファイルを読み込む
with open('Core/PdfiumLib.pas', 'r', encoding='utf-8') as f:
    lines = f.readlines()

output_lines = []
i = 0

while i < len(lines):
    line = lines[i]
    
    # 動的ローディング関連のコードをスキップ
    if (re.match(r'^\s*procedure NotLoaded', line) or
        re.match(r'^\s*procedure FunctionNotSupported', line) or
        re.match(r'^\s*function PDF_USE_XFA', line) or
        re.match(r'^\s*function PDF_IsSkiaAvailable', line) or
        re.match(r'^\s*procedure InitPDFium', line) or
        re.match(r'^\s*procedure FinalizePDFium', line) or
        re.match(r'^\s*var\s*$', line) and i + 1 < len(lines) and 'PdfiumModule' in lines[i + 1]):
        
        # 関数/手続きの終了までスキップ
        while i < len(lines) and not re.match(r'^end;?\s*$', lines[i]):
            i += 1
        if i < len(lines):
            i += 1
        continue
    
    # 通常の行は出力
    output_lines.append(line)
    i += 1

# 結果を書き込む
with open('Core/PdfiumLib.pas', 'w', encoding='utf-8') as f:
    f.writelines(output_lines)

print("動的ローディング関連のコードを削除しました")
