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
