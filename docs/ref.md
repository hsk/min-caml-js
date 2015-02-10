# リファレンス

リファレンスの実装を考えます。

```
let a = ref 5 in
a := !a + 1 in
print_int(!a)
```

は

```
var a = makeArray(1,5) in
a[0] = a[0] + 1 in
print_int(a[0])
```

もしくは、

```
var a = ref(5) in
a.ref = a.ref + 1 in
print_int(a.ref)
```

と書けます。