import re

# PdfiumLib.pasファイルを読み込む
with open('Core/PdfiumLib.pas', 'r', encoding='utf-8') as f:
    lines = f.readlines()

output_lines = []
i = 0

while i < len(lines):
    line = lines[i]
    
    # ImportFuncs配列の開始を検出
    if re.match(r'^\s*ImportFuncs:\s*array', line):
        # 配列の終了までスキップ
        brace_count = 0
        in_array = False
        
        while i < len(lines):
            current_line = lines[i]
            
            # 開き括弧をカウント
            brace_count += current_line.count('(')
            brace_count -= current_line.count(')')
            
            if '(' in current_line:
                in_array = True
            
            # 配列の終了を検出
            if in_array and ');' in current_line and brace_count == 0:
                i += 1
                break
            
            i += 1
        continue
    
    # 通常の行は出力
    output_lines.append(line)
    i += 1

# 結果を書き込む
with open('Core/PdfiumLib.pas', 'w', encoding='utf-8') as f:
    f.writelines(output_lines)

print("ImportFuncs配列を削除しました")
