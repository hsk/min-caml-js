# module

モジュールをJavaScriptで再現する事を考えます。

```ocaml
module A = struct

	let rec f a = print_string(a)
	let rec b a = f a
	let a = "a"
end

let _ =
	A.b "test b";
	A.f "test f";
	A.f A.a
```

変換後のJavaScriptは以下のようにするとよいでしょう。モジュールの実装にはスコープが必要なので、関数を

```ocaml
var A = (function(){
	function f(a) {
		console.log(a);
	}

	function b(a) {
		f(a);
	}
	var a = 1;
	return {f:f,b:b,a:a};
}());

A.b("test b");
A.f("test f");
A.f(A.a)
```

## open

openはjavascriptのwithを使って再現するのが簡単です。

```ocaml
open A

let _ =
	b("test with b");
	f("test with f")
```ocaml

JavaScriptに変換後は以下のようにすると良いでしょう。

```ocaml
with (A) {

	b("test with b");
	f("test with f");
}
```
