# プロジェクトルール

## ビルドルール
- ソースを変更したときには必ずビルドしてエラーがないか確認する
- Lazbuildのパス: `C:\Lazarus\lazbuild.exe`
- ビルドコマンド: `C:\Lazarus\lazbuild.exe project1.lpi`

## プロジェクト概要
- プロジェクト名: smallpdfviewer
- 開発環境: Lazarus IDE + Free Pascal
- PDF処理ライブラリ: Pdfium
- 目的: LinuxとWindowsの両方でセカンドスクリーンにPDFを表示するGUIアプリ

## ファイル構成
- `PdfiumInitializer.pas` - PDFiumライブラリの初期化
- `PdfDocument.pas` - PDF文書の管理
- `PdfPage.pas` - PDFページの処理
- `PdfRenderer.pas` - PDFレンダリング
- `PdfViewer.pas` - PDFビューアー
- `PdfBitmap.pas` - PDF画像処理
- `PdfiumLib.pas` - PDFiumライブラリのバインディング

## 開発ガイドライン
1. ソースコード変更後は必ずビルドテストを実行
2. エラーや警告が出た場合は適切に修正
3. ユニット名やファイル名の変更時は、すべての参照を更新
4. プロジェクトファイル（.lpi）とメインプログラム（.lpr）の参照も忘れずに更新
