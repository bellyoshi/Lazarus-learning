#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
PNGファイルを削除するスクリプト
w2560フォルダ内のすべてのPNGファイルを削除します
"""

import os

def delete_png_files(directory):
    """
    PNGファイルを削除
    
    Args:
        directory: PNGファイルが格納されているディレクトリ
    """
    if not os.path.exists(directory):
        print(f"エラー: {directory} フォルダが見つかりませんでした。")
        return
    
    # PNGファイルを検索
    png_files = [f for f in os.listdir(directory) if f.lower().endswith('.png')]
    
    if not png_files:
        print(f"{directory} にPNGファイルが見つかりませんでした。")
        return
    
    print(f"{len(png_files)}個のPNGファイルが見つかりました。")
    print("削除を開始します...")
    
    deleted = 0
    errors = 0
    
    for png_file in png_files:
        try:
            file_path = os.path.join(directory, png_file)
            os.remove(file_path)
            deleted += 1
            if deleted % 10 == 0:
                print(f"  削除済み: {deleted}/{len(png_files)}")
        except Exception as e:
            errors += 1
            print(f"エラー: {png_file} の削除に失敗しました: {e}")
    
    print(f"\n削除完了!")
    print(f"  成功: {deleted}個")
    print(f"  失敗: {errors}個")

if __name__ == '__main__':
    # w2560フォルダ内のPNGファイルを削除
    target_directory = 'w2560'
    delete_png_files(target_directory)

