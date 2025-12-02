#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
PNG画像をBMP画像に変換するスクリプト
w2560フォルダ内のすべてのPNGファイルをBMPに変換します
"""

import os
from PIL import Image

def convert_png_to_bmp(input_dir, output_dir=None):
    """
    PNG画像をBMP画像に変換
    
    Args:
        input_dir: PNG画像が格納されているディレクトリ
        output_dir: BMP画像を保存するディレクトリ（Noneの場合はinput_dirと同じ）
    """
    if output_dir is None:
        output_dir = input_dir
    
    # 出力ディレクトリが存在しない場合は作成
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    
    # PNGファイルを検索
    png_files = [f for f in os.listdir(input_dir) if f.lower().endswith('.png')]
    
    if not png_files:
        print(f"エラー: {input_dir} にPNGファイルが見つかりませんでした。")
        return
    
    print(f"{len(png_files)}個のPNGファイルが見つかりました。")
    print("変換を開始します...")
    
    converted = 0
    errors = 0
    
    for png_file in png_files:
        try:
            # 入力ファイルパス
            input_path = os.path.join(input_dir, png_file)
            
            # 出力ファイルパス（拡張子を.bmpに変更）
            bmp_file = os.path.splitext(png_file)[0] + '.bmp'
            output_path = os.path.join(output_dir, bmp_file)
            
            # PNG画像を開く
            img = Image.open(input_path)
            
            # RGBモードに変換（BMPはRGB形式をサポート）
            if img.mode != 'RGB':
                img = img.convert('RGB')
            
            # BMPとして保存
            img.save(output_path, 'BMP')
            
            converted += 1
            if converted % 10 == 0:
                print(f"  変換済み: {converted}/{len(png_files)}")
            
        except Exception as e:
            errors += 1
            print(f"エラー: {png_file} の変換に失敗しました: {e}")
    
    print(f"\n変換完了!")
    print(f"  成功: {converted}個")
    print(f"  失敗: {errors}個")

if __name__ == '__main__':
    # w2560フォルダ内のPNGをBMPに変換
    input_directory = 'w2560'
    
    if os.path.exists(input_directory):
        convert_png_to_bmp(input_directory)
    else:
        print(f"エラー: {input_directory} フォルダが見つかりませんでした。")

