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
	- [x] begin end
    - [ ] パターンマッチング構文
        - [x] match e with | p1 -> e1 | ... | pn -> en
        - [x] _
        - [x] 値バインディング
        - [ ] ネストした値バインディング
        - [ ] ネストした値の比較
        - [ ] whenによるガード
        - [ ] @での値バインディング
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
        - [ ]
