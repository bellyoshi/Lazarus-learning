#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
BMP画像を320x240に縮小するスクリプト
w2560フォルダ内のすべてのBMPファイルを320x240にリサイズし、元のファイルを削除します
"""

import os
from PIL import Image

def resize_bmp_to_320x240(input_dir):
    """
    BMP画像を320x240にリサイズし、元のファイルを削除
    
    Args:
        input_dir: BMP画像が格納されているディレクトリ
    """
    if not os.path.exists(input_dir):
        print(f"エラー: {input_dir} フォルダが見つかりませんでした。")
        return
    
    # BMPファイルを検索
    bmp_files = [f for f in os.listdir(input_dir) if f.lower().endswith('.bmp')]
    
    if not bmp_files:
        print(f"エラー: {input_dir} にBMPファイルが見つかりませんでした。")
        return
    
    print(f"{len(bmp_files)}個のBMPファイルが見つかりました。")
    print("320x240にリサイズを開始します...")
    
    resized = 0
    errors = 0
    
    for bmp_file in bmp_files:
        try:
            # 入力ファイルパス
            input_path = os.path.join(input_dir, bmp_file)
            
            # 一時ファイル名（元のファイルを上書きする前に確認）
            temp_path = os.path.join(input_dir, 'temp_' + bmp_file)
            
            # BMP画像を開く
            img = Image.open(input_path)
            
            # 320x240にリサイズ（アスペクト比を保持してリサイズ）
            img_resized = img.resize((320, 240), Image.Resampling.LANCZOS)
            
            # 一時ファイルに保存
            img_resized.save(temp_path, 'BMP')
            
            # 元のファイルを削除
            os.remove(input_path)
            
            # 一時ファイルを元の名前にリネーム
            os.rename(temp_path, input_path)
            
            resized += 1
            if resized % 10 == 0:
                print(f"  リサイズ済み: {resized}/{len(bmp_files)}")
            
        except Exception as e:
            errors += 1
            print(f"エラー: {bmp_file} のリサイズに失敗しました: {e}")
            # 一時ファイルが残っている場合は削除
            temp_path = os.path.join(input_dir, 'temp_' + bmp_file)
            if os.path.exists(temp_path):
                try:
                    os.remove(temp_path)
                except:
                    pass
    
    print(f"\nリサイズ完了!")
    print(f"  成功: {resized}個")
    print(f"  失敗: {errors}個")

if __name__ == '__main__':
    # w2560フォルダ内のBMPを320x240にリサイズ
    input_directory = 'w2560'
    resize_bmp_to_320x240(input_directory)

