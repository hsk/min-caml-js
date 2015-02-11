# mincaml-js

mincaml-jsはocamlのサブセットからJavaScriptへ変換するトランスレータ言語です。
トランスレータにのみ特化することで、シンプルさを維持します。
mincaml-jsはmincamlにいくつかの機能が追加されています。

src,src2,src3,scalaへそれぞれ移動してmakeを実行します。

```
$ make
```

使い方は以下のように`./mincaml [拡張子なしのファイル名]`で、.mlファイルから.jsファイルを生成し、NodeJSで実行します。

```
$ ./mincaml test/fib
$ node test/fib.js
```

mincaml2jsには、4つのバージョンがあります。 

1. ocamlyacc によるバージョン
2. パーサコンビネータによるバージョン
3. scalaのパーサコンビネータによるバージョン
4. より奇麗なJavaScriptの出力バージョン

## テスト

```
$ make do_test
```

## ソースファイル

1. ocamlyaccバージョン
    1. [syntax.ml](src/syntax.ml) 構文木定義
    2. [parser.mly](src/parser.mly) パーサ
    3. [lexer.mll](src/lexer.mll) レキサー
    4. [to_if.ml](src/to_if.ml) match if 変換
    5. [emit.ml](src/emit.ml) JavaScript出力
    6. [main.ml](src/main.ml) メイン
2. パーサコンビネータバージョン
    1. [syntax.ml](src/syntax.ml) 構文木定義
    2. [peg.ml](src2/peg.ml) パーサコンビネータライブラリ
    3. [parser.ml](src2/parser.ml) パーサ
    4. [to_if.ml](src/to_if.ml) match if 変換
    5. [emit.ml](src/emit.ml) JavaScript出力
    6. [main.ml](src2/main.ml) メイン
3. Scalaバージョン
    1. [mincaml2js.scala](scala/mincaml2js.scala) Scalaバージョン(1ソース)
4. より奇麗なJavaScriptの出力バージョン
    1. [syntax.ml](src/syntax.ml) 構文木定義
    2. [peg.ml](src2/peg.ml) パーサコンビネータライブラリ
    3. [parser.ml](src2/parser.ml) パーサ
    4. [to_if.ml](src/to_if.ml) match if 変換
    5. [alpha.ml](src3/alpha.ml) α変換
    6. [inline.ml](src3/inline.ml) インライン展開
    8. [javascript.ml](src3/javascript.ml) javascript変換
    9. [emit.ml](src3/emit.ml) JavaScript出力
    10. [main.ml](src2/main.ml) メイン

## TODO

- ドキュメント
    - 簡単な変換
    - より高度な変換

- 仕様追加
	- [x] begin end
    - [x] パターンマッチング構文
        - [x] match e with | p1 -> e1 | ... | pn -> en
        - [x] _
        - [x] 値バインディング
        - [x] ネストした値バインディング
        - [x] ネストした値の比較
        - [x] whenによるガード
        - [x] asでの値バインディング
    - [ ] リスト
    	- [x] [e1;...;en]でのリストの構築
    	- [x] e::el によるリストへの追加
    	- [x] e @ e でのリストの結合
    	- [ ] Listモジュール
    - [ ] 代数データ型
        - [x] type i = I of int;; I(11)
        - [x] タプル type i = Add of i * i | Int of i
        - [ ] リストの使用 type i = I of int list 
    - [ ] 文字列
        - [x] 文字列リテラル
    - [x] リファレンス
    - [x] レコード
    - [ ] トップレベル
        - [ ] let a = b
        - [ ] let rec a = b
        - [ ] exp
        - [ ] type a = b
        - [ ] open
        - [ ] module = struct ... end
    - [ ] 最適化
        - [x] α変換 alpha
        - [x] javascript変換 javascript
        - [ ] インライン展開 inline
        - [ ] ネストしたletの簡約 assoc
        - [ ] 定数畳み込み constfold
        - [ ] 不要定義削除 elim
