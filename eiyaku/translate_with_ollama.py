import csv
import re
import sys
import json
import time
from collections import defaultdict
import requests

def extract_japanese_words(line, encoding='shift_jis'):
    """
    CSV行から日本語の単語を抽出する
    """
    try:
        decoded_line = line.decode(encoding) if isinstance(line, bytes) else line
    except:
        try:
            decoded_line = line.decode('utf-8') if isinstance(line, bytes) else line
        except:
            decoded_line = line
    
    fields = decoded_line.strip().split(',')
    japanese_pattern = re.compile(r'[ぁ-ゖァ-ヶ一-龯]+')
    
    japanese_words = []
    for field in fields:
        # 括弧内の読み方を除去（例：「あつい」→「あつい」）
        cleaned_field = re.sub(r'[（(].*?[）)]', '', field)
        
        # 日本語の文字が含まれているかチェック
        if japanese_pattern.search(cleaned_field):
            # 英語や数字のみのフィールドを除外
            if not re.match(r'^[A-Za-z0-9.\-]+$', cleaned_field):
                word = cleaned_field.strip()
                if word and len(word) > 0:
                    japanese_words.append(word)
    
    return japanese_words

def translate_with_ollama(word, model='qwen2.5:0.5b', base_url='http://localhost:11434', max_retries=3):
    """
    Ollamaを使用して日本語を英語に翻訳
    複数の候補を生成
    """
    prompt = f"""以下の日本語の単語を英語に翻訳してください。
単語が複数の意味を持つ場合は、主要な意味を3-5個、カンマ区切りで列挙してください。
回答は英語の翻訳のみを返してください。

日本語単語: {word}
英語翻訳（カンマ区切り）:"""
    
    for attempt in range(max_retries):
        try:
            response = requests.post(
                f'{base_url}/api/generate',
                json={
                    'model': model,
                    'prompt': prompt,
                    'stream': False,
                    'options': {
                        'temperature': 0.7,  # 多様性を持たせる
                        'num_predict': 150
                    }
                },
                timeout=60
            )
            
            if response.status_code == 200:
                result = response.json()
                translation_text = result.get('response', '').strip()
                
                # プロンプトの繰り返しを除去
                if '英語翻訳' in translation_text:
                    translation_text = translation_text.split('英語翻訳')[-1].strip()
                    if ':' in translation_text:
                        translation_text = translation_text.split(':', 1)[-1].strip()
                
                # カンマ区切りの翻訳候補を抽出
                translations = [t.strip() for t in translation_text.split(',')]
                translations = [t for t in translations if t and len(t) > 0]
                
                # 最初の翻訳が長すぎる場合は分割
                if len(translations) == 1 and len(translations[0]) > 50:
                    # スペースやセミコロンで分割を試みる
                    alt_translations = re.split(r'[;,\n]', translations[0])
                    translations = [t.strip() for t in alt_translations if t.strip()]
                
                # 数字や記号のみの候補を除外
                translations = [t for t in translations if not re.match(r'^[0-9.\-]+$', t)]
                
                # 最大5つまで
                if translations:
                    return translations[:5]
                else:
                    return ["translation_failed"]
            else:
                print(f"エラー: HTTP {response.status_code}")
                if attempt < max_retries - 1:
                    time.sleep(1)
                
        except requests.exceptions.RequestException as e:
            print(f"翻訳エラー ({word}): {e}")
            if attempt < max_retries - 1:
                time.sleep(2 ** attempt)  # 指数バックオフ
    
    return ["translation_failed"]

def process_bunruidb(input_file, output_file, encoding='shift_jis', ollama_model='qwen2.5:0.5b'):
    """
    bunruidb.txtを処理して、日本語-英語辞書CSVを作成
    """
    word_dict = defaultdict(set)
    
    print(f"ファイルを読み込んでいます: {input_file}")
    
    # Ollamaが起動しているか確認
    try:
        response = requests.get('http://localhost:11434/api/tags', timeout=5)
        if response.status_code != 200:
            print("警告: Ollamaが起動していない可能性があります")
            print("Ollamaを起動してください: ollama serve")
            return
    except requests.exceptions.RequestException as e:
        print("エラー: Ollamaに接続できません。Ollamaが起動しているか確認してください。")
        print("起動方法: ollama serve")
        print(f"詳細: {e}")
        return
    
    # ファイル読み込み
    try:
        with open(input_file, 'r', encoding=encoding, errors='ignore') as f:
            line_count = 0
            for line in f:
                line_count += 1
                if line_count % 10000 == 0:
                    print(f"処理中: {line_count}行目")
                
                japanese_words = extract_japanese_words(line, encoding)
                for word in japanese_words:
                    if word:
                        word_dict[word].add(word)
    except UnicodeDecodeError:
        print("Shift-JISでの読み込みに失敗。UTF-8で再試行します。")
        with open(input_file, 'r', encoding='utf-8', errors='ignore') as f:
            line_count = 0
            for line in f:
                line_count += 1
                if line_count % 10000 == 0:
                    print(f"処理中: {line_count}行目")
                
                japanese_words = extract_japanese_words(line, 'utf-8')
                for word in japanese_words:
                    if word:
                        word_dict[word].add(word)
    
    print(f"\n抽出された日本語単語数: {len(word_dict)}")
    print(f"Ollamaモデル: {ollama_model} を使用して翻訳を開始します...")
    print("（時間がかかる場合があります）\n")
    
    # CSVに書き込む
    with open(output_file, 'w', encoding='utf-8-sig', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['日本語', '英語候補1', '英語候補2', '英語候補3', '英語候補4', '英語候補5'])
        
        word_count = 0
        total_words = len(word_dict)
        
        for japanese_word in sorted(word_dict.keys()):
            word_count += 1
            if word_count % 10 == 0:
                print(f"翻訳中: {word_count}/{total_words} ({japanese_word})")
            
            translations = translate_with_ollama(japanese_word, model=ollama_model)
            
            # CSV行を作成
            row = [japanese_word] + translations[:5]
            while len(row) < 6:
                row.append('')
            
            writer.writerow(row)
            
            # APIレート制限を避けるため、少し待機
            time.sleep(0.2)
    
    print(f"\n完了しました！出力ファイル: {output_file}")

if __name__ == '__main__':
    input_file = 'bunruidb.txt'
    output_file = 'japanese_english_dictionary.csv'
    ollama_model = 'qwen2.5:0.5b'  # ダウンロード済みのモデル
    
    if len(sys.argv) > 1:
        input_file = sys.argv[1]
    if len(sys.argv) > 2:
        output_file = sys.argv[2]
    if len(sys.argv) > 3:
        ollama_model = sys.argv[3]
    
    print("=" * 60)
    print("日本語-英語辞書生成ツール（Ollama使用）")
    print("=" * 60)
    print(f"入力ファイル: {input_file}")
    print(f"出力ファイル: {output_file}")
    print(f"使用モデル: {ollama_model}")
    print("=" * 60)
    print()
    
    process_bunruidb(input_file, output_file, ollama_model=ollama_model)

