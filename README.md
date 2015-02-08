# mincaml-js

mincaml-jsはocamlのサブセットである、mincamlからJavaScriptへのトランスレータです。
トランスレータにのみ特化することで、シンプルさを維持します。
mincaml-jsはmincamlにいくつかの機能が追加されています。

```
$ make
```
でビルドします。

```
$ mincaml test/fib
$ node test/fib.js
```
等として使います。

テストをするには

```
$ make do_test
```

## ソースファイル

1. [type.ml](type.ml) 型定義
2. [id.ml](id.ml) id定義
3. [syntax.ml](syntax.ml) 構文木定義
4. [parser.mly](parser.mly) パーサ
5. [lexer.mll](lexer.mll) レキサー
6. [emit.ml](emit.ml) JavaScript出力
7. [main.ml](main.ml) メイン

## TODO

- ドキュメント
    - 簡単な変換
    - より高度な変換

- 仕様追加
	- [x] begin end
    - [ ] パターンマッチング構文
        - [x] match e with | p1 -> e1 | ... | pn -> en
        - [x] _
        - [x] 値バインディング
        - [x] ネストした値バインディング
        - [x] ネストした値の比較
        - [x] whenによるガード
        - [ ] asでの値バインディング
    - [ ] リスト
    	- [ ] [e1;...;en]でのリストの構築
    	- [ ] e::el によるリストへの追加
    	- [ ] e @ e でのリストの結合
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
