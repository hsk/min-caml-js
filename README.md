# mincaml-js

mincaml-jsはocamlのサブセットである、mincamlからJavaScriptへのトランスレータです。

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


TODO

- ドキュメント
    - 簡単な変換
    - より高度な変換

- 仕様追加
    - パターンマッチング構文
        - match e with | p1 -> e1 | ... | pn -> en
        - _
        - 値バインディング
        - whenによるガード
        - @での値バインディング
    - リスト
    	- [e1;...;en]でのリストの構築
    	- e::el によるリストへの追加
    	- e @ e でのリストの結合
    	- Listモジュール
    - モジュール
    	- open
    	- module = struct ... end
    - トップレベル
    - 代数データ型
    	- type i = I of t
    - 文字列
