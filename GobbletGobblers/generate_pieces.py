#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Gobblet Gobblers の駒をSVG形式で生成するプログラム
大、中、小のサイズと2色（赤、青）の組み合わせで駒を生成します
"""

import os
from pathlib import Path


def create_gobblet_piece(size_name, size_value, color_name, color_value, shadow_color):
    """
    Gobblet Gobblersの駒をSVG形式で生成
    
    Args:
        size_name: サイズ名（'large', 'medium', 'small'）
        size_value: サイズの値（直径）
        color_name: 色名（'red', 'blue'）
        color_value: 色の値（HEXコード）
        shadow_color: 影の色（HEXコード）
    
    Returns:
        SVGコンテンツの文字列
    """
    # サイズに応じた高さを設定（円筒形の高さ）
    height = size_value * 0.8
    
    # SVGのビューボックスサイズ（余白を含む）
    viewbox_size = size_value + 40
    center_x = viewbox_size / 2
    center_y = viewbox_size / 2
    
    # 円筒形の上部円
    top_circle_y = center_y - height / 2
    
    # 円筒形の下部円
    bottom_circle_y = center_y + height / 2
    
    # グラデーションID
    gradient_id = f"grad_{size_name}_{color_name}"
    
    svg_content = f'''<?xml version="1.0" encoding="UTF-8"?>
<svg width="{size_value + 40}" height="{size_value + 40}" 
     viewBox="0 0 {viewbox_size} {viewbox_size}" 
     xmlns="http://www.w3.org/2000/svg">
  <defs>
    <!-- グラデーション定義 -->
    <linearGradient id="{gradient_id}" x1="0%" y1="0%" x2="0%" y2="100%">
      <stop offset="0%" style="stop-color:{color_value};stop-opacity:1" />
      <stop offset="100%" style="stop-color:{shadow_color};stop-opacity:1" />
    </linearGradient>
    
    <!-- 影のフィルター -->
    <filter id="shadow_{size_name}_{color_name}">
      <feGaussianBlur in="SourceAlpha" stdDeviation="3"/>
      <feOffset dx="2" dy="2" result="offsetblur"/>
      <feComponentTransfer>
        <feFuncA type="linear" slope="0.3"/>
      </feComponentTransfer>
      <feMerge>
        <feMergeNode/>
        <feMergeNode in="SourceGraphic"/>
      </feMerge>
    </filter>
  </defs>
  
  <!-- 影 -->
  <ellipse cx="{center_x + 2}" cy="{bottom_circle_y + 2}" 
           rx="{size_value / 2 + 2}" ry="{size_value / 8}" 
           fill="#000000" opacity="0.2"/>
  
  <!-- 円筒形の側面（楕円） -->
  <ellipse cx="{center_x}" cy="{center_y}" 
           rx="{size_value / 2}" ry="{height / 2}" 
           fill="url(#{gradient_id})" 
           filter="url(#shadow_{size_name}_{color_name})"/>
  
  <!-- 円筒形の上部円 -->
  <circle cx="{center_x}" cy="{top_circle_y}" 
          r="{size_value / 2}" 
          fill="url(#{gradient_id})" 
          opacity="0.9"/>
  
  <!-- 上部円のハイライト -->
  <ellipse cx="{center_x - size_value / 6}" cy="{top_circle_y - size_value / 6}" 
           rx="{size_value / 4}" ry="{size_value / 4}" 
           fill="#ffffff" opacity="0.4"/>
  
  <!-- 円筒形の下部円（見える部分） -->
  <ellipse cx="{center_x}" cy="{bottom_circle_y}" 
           rx="{size_value / 2}" ry="{size_value / 8}" 
           fill="{shadow_color}"/>
  
  <!-- 側面のハイライト -->
  <ellipse cx="{center_x - size_value / 3}" cy="{center_y}" 
           rx="{size_value / 6}" ry="{height / 2}" 
           fill="#ffffff" opacity="0.2"/>
</svg>'''
    
    return svg_content


def main():
    """メイン関数：すべての駒を生成"""
    # サイズ定義（直径）
    sizes = {
        'large': 120,
        'medium': 90,
        'small': 60
    }
    
    # 色定義
    colors = {
        'red': {
            'main': '#E74C3C',      # 赤
            'shadow': '#C0392B'      # 暗い赤
        },
        'blue': {
            'main': '#3498DB',      # 青
            'shadow': '#2980B9'     # 暗い青
        }
    }
    
    # 出力ディレクトリを作成
    output_dir = Path('pieces')
    output_dir.mkdir(exist_ok=True)
    
    print("Gobblet Gobblers の駒を生成しています...")
    
    # すべてのサイズと色の組み合わせで駒を生成
    for size_name, size_value in sizes.items():
        for color_name, color_info in colors.items():
            svg_content = create_gobblet_piece(
                size_name, 
                size_value, 
                color_name, 
                color_info['main'], 
                color_info['shadow']
            )
            
            # ファイル名を生成
            filename = f"{size_name}_{color_name}.svg"
            filepath = output_dir / filename
            
            # SVGファイルを保存
            with open(filepath, 'w', encoding='utf-8') as f:
                f.write(svg_content)
            
            print(f"生成: {filepath}")
    
    print(f"\n完了！{len(sizes) * len(colors)}個の駒が生成されました。")
    print(f"出力先: {output_dir.absolute()}")


if __name__ == '__main__':
    main()


