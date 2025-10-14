import re

# PdfiumLib.pasファイルを読み込む
with open('Core/PdfiumLib.pas', 'r', encoding='utf-8') as f:
    lines = f.readlines()

output_lines = []
i = 0

while i < len(lines):
    line = lines[i]
    
    # varで始まる行を検出
    if re.match(r'^var\s*$', line):
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
                
                # procedureまたはfunctionの宣言を解析
                # パターン: FPDF_XXX: procedure(...); または FPDF_XXX: function(...): ReturnType;
                if func_type == 'procedure':
                    # procedure の場合
                    proc_match = re.match(r'\s*(\w+):\s*procedure\(([^)]*)\);\s*\{\$IFDEF DLLEXPORT\}stdcall\{\$ELSE\}cdecl\{\$ENDIF\};', full_decl)
                    if proc_match:
                        name = proc_match.group(1)
                        params = proc_match.group(2)
                        new_decl = f"procedure {name}({params}); cdecl; external PDFiumDll name '{name}';\n"
                        output_lines.append(new_decl)
                        i = j
                        continue
                else:
                    # function の場合
                    func_match = re.match(r'\s*(\w+):\s*function\(([^)]*)\):\s*([^;]+);\s*\{\$IFDEF DLLEXPORT\}stdcall\{\$ELSE\}cdecl\{\$ENDIF\};', full_decl)
                    if func_match:
                        name = func_match.group(1)
                        params = func_match.group(2)
                        ret_type = func_match.group(3).strip()
                        new_decl = f"function {name}({params}): {ret_type}; cdecl; external PDFiumDll name '{name}';\n"
                        output_lines.append(new_decl)
                        i = j
                        continue
                
                # マッチしなかった場合は元の行を保持
                output_lines.append(line)
            else:
                # FPDF関数ではない場合は元の行を保持
                output_lines.append(line)
        else:
            output_lines.append(line)
    else:
        output_lines.append(line)
    
    i += 1

# 結果を書き込む
with open('Core/PdfiumLib.pas', 'w', encoding='utf-8') as f:
    f.writelines(output_lines)

print("変換完了")
