# パターンマッチの変換


パターンマッチは結局一番複雑なデータ構造を扱えるレコードに作り込れば、後のデータは自動的に対応出来ます。
また、パターンマッチはif文に変換してしまえば、JavaScriptの出力は特に変える必要はありません。



1. 形式的に書く
2. 単純なマッチング
3. 複雑なマッチング
4. ガード
5. 複数の条件



```
match 1 with 1 -> 1 | _ -> 2
```

を以下のように変換する事を考えます。

```
(function(e) {
  if (e == 1) { return 1; }
  return 2;
}(1))
```



## 参考URL

- http://qiita.com/h_sakurai/items/9ee6285eb192643f51e5
