#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
SVGファイルをBMP形式に変換するプログラム
"""

import os
import re
import xml.etree.ElementTree as ET
from pathlib import Path
try:
    from PIL import Image, ImageDraw
except ImportError:
    print("必要なライブラリをインストール中...")
    import subprocess
    import sys
    subprocess.check_call([sys.executable, "-m", "pip", "install", "Pillow"])
    from PIL import Image, ImageDraw


def parse_color(color_str):
    """色文字列をRGBタプルに変換"""
    if color_str.startswith('#'):
        # HEX形式 (#RRGGBB)
        hex_color = color_str[1:]
        if len(hex_color) == 6:
            return tuple(int(hex_color[i:i+2], 16) for i in (0, 2, 4))
    elif color_str.startswith('rgb'):
        # rgb(r, g, b)形式
        match = re.search(r'rgb\((\d+),\s*(\d+),\s*(\d+)\)', color_str)
        if match:
            return tuple(int(x) for x in match.groups())
    # 基本的な色名
    color_map = {
        'red': (231, 76, 60), 'blue': (52, 152, 219),
        'black': (0, 0, 0), 'white': (255, 255, 255)
    }
    return color_map.get(color_str.lower(), (128, 128, 128))


def parse_float(value, default=0.0):
    """文字列を浮動小数点数に変換"""
    try:
        return float(value)
    except (ValueError, TypeError):
        return default


def svg_to_bmp(svg_path, bmp_path, width=None, height=None):
    """
    SVGファイルをBMP形式に変換
    
    Args:
        svg_path: 入力SVGファイルのパス
        bmp_path: 出力BMPファイルのパス
        width: 出力幅（Noneの場合は元のサイズ）
        height: 出力高さ（Noneの場合は元のサイズ）
    """
    try:
        # SVGファイルを読み込む
        tree = ET.parse(svg_path)
        root = tree.getroot()
        
        # 名前空間を処理
        ns = {'svg': 'http://www.w3.org/2000/svg'}
        
        # ビューボックスまたはサイズを取得
        viewbox = root.get('viewBox', '')
        svg_width = parse_float(root.get('width', '200'))
        svg_height = parse_float(root.get('height', '200'))
        
        if viewbox:
            parts = viewbox.split()
            if len(parts) >= 4:
                vb_x, vb_y, vb_w, vb_h = map(float, parts[:4])
                svg_width = vb_w
                svg_height = vb_h
        
        # 出力サイズを決定
        if width is None:
            width = int(svg_width)
        if height is None:
            height = int(svg_height)
        
        # 画像を作成
        img = Image.new('RGB', (width, height), (255, 255, 255))
        draw = ImageDraw.Draw(img)
        
        # スケールファクター
        scale_x = width / svg_width
        scale_y = height / svg_height
        
        # グラデーション定義を取得（簡略化：最初の色を使用）
        gradients = {}
        for defs in root.findall('.//defs', ns):
            for grad in defs.findall('.//linearGradient', ns):
                grad_id = grad.get('id', '')
                stops = grad.findall('.//stop', ns)
                if stops:
                    first_color = stops[0].get('style', '')
                    color_match = re.search(r'stop-color:([^;]+)', first_color)
                    if color_match:
                        gradients[grad_id] = parse_color(color_match.group(1))
        
        # すべての要素を描画
        def draw_element(elem, parent_opacity=1.0):
            tag = elem.tag.split('}')[-1] if '}' in elem.tag else elem.tag
            
            # 色と不透明度を取得
            fill = elem.get('fill', 'black')
            opacity = parse_float(elem.get('opacity', '1.0'), 1.0) * parent_opacity
            
            # グラデーション参照を解決
            if fill.startswith('url(#'):
                grad_id = fill[5:-1]  # url(#grad_id)からgrad_idを抽出
                if grad_id in gradients:
                    fill = gradients[grad_id]
                else:
                    fill = (128, 128, 128)
            
            # 色をRGBに変換
            if isinstance(fill, str):
                fill_color = parse_color(fill)
            else:
                fill_color = fill
            
            # 不透明度を適用（簡略化：アルファチャンネルなし）
            if opacity < 1.0:
                fill_color = tuple(int(c * opacity + 255 * (1 - opacity)) for c in fill_color)
            
            if tag == 'circle':
                cx = parse_float(elem.get('cx', '0')) * scale_x
                cy = parse_float(elem.get('cy', '0')) * scale_y
                r = parse_float(elem.get('r', '0')) * min(scale_x, scale_y)
                draw.ellipse([cx - r, cy - r, cx + r, cy + r], fill=fill_color)
            
            elif tag == 'ellipse':
                cx = parse_float(elem.get('cx', '0')) * scale_x
                cy = parse_float(elem.get('cy', '0')) * scale_y
                rx = parse_float(elem.get('rx', '0')) * scale_x
                ry = parse_float(elem.get('ry', '0')) * scale_y
                draw.ellipse([cx - rx, cy - ry, cx + rx, cy + ry], fill=fill_color)
            
            elif tag == 'rect':
                x = parse_float(elem.get('x', '0')) * scale_x
                y = parse_float(elem.get('y', '0')) * scale_y
                w = parse_float(elem.get('width', '0')) * scale_x
                h = parse_float(elem.get('height', '0')) * scale_y
                draw.rectangle([x, y, x + w, y + h], fill=fill_color)
            
            # 子要素を再帰的に描画
            for child in elem:
                draw_element(child, opacity)
        
        # すべての要素を描画（defs以外）
        for elem in root:
            if elem.tag.split('}')[-1] != 'defs':
                draw_element(elem)
        
        # BMP形式で保存
        img.save(bmp_path, 'BMP')
        
        return True
    except Exception as e:
        print(f"エラー: {svg_path} の変換に失敗しました: {e}")
        import traceback
        traceback.print_exc()
        return False


def main():
    """メイン関数：piecesディレクトリ内のすべてのSVGファイルをBMPに変換"""
    pieces_dir = Path('pieces')
    
    if not pieces_dir.exists():
        print(f"エラー: {pieces_dir} ディレクトリが見つかりません。")
        print("まず generate_pieces.py を実行してSVGファイルを生成してください。")
        return
    
    # 出力ディレクトリを作成
    bmp_dir = Path('pieces_bmp')
    bmp_dir.mkdir(exist_ok=True)
    
    # SVGファイルを検索
    svg_files = list(pieces_dir.glob('*.svg'))
    
    if not svg_files:
        print(f"エラー: {pieces_dir} ディレクトリにSVGファイルが見つかりません。")
        return
    
    print(f"{len(svg_files)}個のSVGファイルをBMPに変換しています...")
    
    success_count = 0
    for svg_file in svg_files:
        # BMPファイル名を生成
        bmp_filename = svg_file.stem + '.bmp'
        bmp_path = bmp_dir / bmp_filename
        
        if svg_to_bmp(svg_file, bmp_path):
            print(f"変換完了: {svg_file.name} -> {bmp_filename}")
            success_count += 1
        else:
            print(f"変換失敗: {svg_file.name}")
    
    print(f"\n完了！{success_count}/{len(svg_files)}個のファイルが変換されました。")
    print(f"出力先: {bmp_dir.absolute()}")


if __name__ == '__main__':
    main()
