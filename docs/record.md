# record

レコードの実装を考えます。

```
type r = {a:int;mutable b:int}
;;
```

型はパーサがスルーして、後続の式を返せばいいでしょう。mutableも使えるようにしましょう。

```
let a ={a=1;b=20} in
```

レコードの初期化は、Recを生成すればOKです。

```
print_int(a.a);print_newline();
print_int(a.b);print_newline();
```

a.aはGet(Var "a",Var "b")と出来ればいいです。

```
let {a=k} = a in
print_int(k);print_newline();
```

letはパターンマッチのMatchにすれば、バックエンドは既にあります。

```
begin match a with
| {b=b} -> print_int(b); print_newline()
end;
```

Matchも同様です。

```
a.b <- 40;
print_int(a.b);print_newline()
```

これは、Putに実装しましょう。

mutableなレコードはおそらく、コンパイラの場合はリファレンスにするのがいいのでしょうけど、コンパイラではないのでそのまま書き換えてよいでしょう。


