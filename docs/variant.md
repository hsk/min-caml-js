# variant

```
type e =
| EUnit
| EInt of int
| EAdd of e * e
;;
let a = EAdd(EInt 1, EInt2) in
let rec eval e =
  match e with
  | EUnit -> 0
  | EInt i -> i
  | EAdd(e1,e2) -> eval e1 + eval e2
in
print_int (eval a);
print_newline()
```

代数データ型は、型情報は消してしまいます。
データコンストラクタは、直接JavaScriptのオブジェクトを生成します。

	EUnit -> {tag:"EUnit", data:undefined}
	EInt 1 -> {tag:"EInt", data:1}
	EAdd(EInt 1,EInt 2) -> {tag:"EAdd", data:{tag:"Tuple", _0:{tag:"EInt", data:1}, _1: {tag:"EInt", data:2} }}

タグ情報を何かしらのデータに付けているだけです。
タプルもまた、レコードにエンコードされるので、結局レコードさえ処理すればよいようにしてあります。
