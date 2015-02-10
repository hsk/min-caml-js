# mincaml-js

mincaml-jsはocamlのサブセットからJavaScriptへ変換するトランスレータ言語です。
トランスレータにのみ特化することで、シンプルさを維持します。
mincaml-jsはmincamlにいくつかの機能が追加されています。

srcへ移動し、

```
$ make
```

でビルドします。

```
$ mincaml test/fib
$ node test/fib.js
```

等として使います。

mincaml2jsには、３つのバージョンがあります。 

1. ocamlyacc によるバージョン
2. パーサコンビネータによるバージョン
3. scalaのパーサコンビネータによるバージョン

## テスト

```
$ make do_test
```

## ソースファイル

1. ocamlyaccバージョン
    1. [syntax.ml](src/syntax.ml) 構文木定義
    2. [parser.mly](src/parser.mly) パーサ
    3. [lexer.mll](src/lexer.mll) レキサー
    4. [emit.ml](src/emit.ml) JavaScript出力
    5. [main.ml](src/main.ml) メイン
2. パーサコンビネータバージョン
    1. [syntax.ml](src/syntax.ml) 構文木定義
    2. [peg.ml](src2/peg.ml) パーサコンビネータライブラリ
    3. [parser.ml](src2/parser.ml) パーサ
    4. [to_if.ml](src/to_if.ml) match if 変換
    5. [emit.ml](src/emit.ml) JavaScript出力
    6. [main.ml](src2/main.ml) メイン
3. Scalaバージョン
    1. [mincaml2js.scala](scala/mincaml2js.scala) Scalaバージョン(1ソース)

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
    - [ ] モジュール
    	- [ ] open
    	- [ ] module = struct ... end
    - [ ] トップレベル
    - [ ] 代数データ型
        - [x] type i = I of int;; I(11)
        - [x] タプル type i = Add of i * i | Int of i
        - [ ] リストの使用 type i = I of int list 
    - [ ]文字列
        - [x] 文字列リテラル
        
