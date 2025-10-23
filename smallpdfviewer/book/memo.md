# LazarusとPdfiumで作るPDFビュワー
## Lazarusとは
イエス​は​そう​言い​終える​と，大声​で​叫ん​だ。「ラザロ，出​て​き​なさい！」 ヨハネ11:43

ラザロは死んだ。しかしイエス・キリストにより4日目になんと墓の中から復活されたのだ。
Lazarus IDEは、Borland Delphiによく似ている。

## なぜLazarusを選んだか
LinuxとWindowsの両方でセカンドスクリーンにPDFを表示するGUIアプリを作成したかった。
Typescript(JavaScript)を試してみたがセカンドスクリーンにウインドウを作成し、操作することがむつかしい。

## PDFとは
PDFDocument

PDFPage

PDFBitmap
  PDFPageをレンダーして得られる画像である。

その他のユニットについて
PdfiumInitializer
  初期化をする
project1とUnit1

Lazbuildについて
  IDEを開くことなくコマンドラインでプロジェクトのビルドができる。
  AIエディタを使っている場合AIに自動的にビルドさせるときに便利
  C:\Lazarusフォルダの中にある。
  AIエディタがLazbuildを実行しようとするがインストールしただけではパスが通っていない。
  AIエディタにLazbuildはC:\Lazarusにある。ビルドせよ。と指示すればコンパイルまでやってくれる。
