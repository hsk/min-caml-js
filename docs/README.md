# λ式からAltJSへの変換向けの正規形

## 1. アブストラクト

現在、AltJSと呼ばれる言語群が存在します。これはJavaScriptへのトランスレータであるプログラミング言語の総称です。
λ計算のAltJSも数多く存在しています。
しかしながら、λ計算ベースの言語からJavaScriptへの変換後のコードは余美しい物ではありません。
様々なプログラムが正確に動作することが重要なので、可読性は犠牲になっているのでしょう。
λ計算の変換過程での正規形にはA正規形や、k正規形、β正規形等があります。
この文章では、JavaScriptへの単純な変換方法について述べ、より発展的なJavaScriptへの変換を提案します。

## 2. イントロダクション

現在、AltJSと呼ばれる言語群が存在します。これはJavaScriptへのトランスレータであるプログラミング言語の総称です。
js\_of\_ocaml、Elm等、λ計算のAltJSも数多く存在しています。
しかしながら、λ計算ベースの言語からJavaScriptへの変換後のコードは美しい物ではないようです。
なぜならば、λ計算の変換過程での正規形にはA正規形や、k正規形、β正規形等があります。
同じようにJavaScriptへの着実で奇麗な変換方法があれば良いでしょう。

## 2.1. 各正規形について

### 2.1.1. A正規形とA正規化

### 2.1.2. K正規形とK正規化

### 2.1.3. β正規形とβ正規化

3章ではJavaScriptへの単純な変換方法について述べます。
4章ではJavaScriptへの変換過程で重要となるであろう正規形のプロトタイプを作成し、形式化します。

## 3. 単純なλ式からJavaScriptへの変換

### 3.1. 変換例

例えば、以下のOCamlのプログラムは

	let rec f x y =
		let a = x + 10 in
		let b = y + 10 in
		a + b

JavaScriptの以下のプログラムに変換できます：

	function f(x,y) {
		var a = x + 10;
		var b = y + 10;
		return a + b;
	}

ところで、以下の式を変換するには:

	let rec f x y =
		let r =
			let a = x + 10 in
			let b = y + 10 in
			a + b
		in r

以下のようにのように変換する必要があります:

	function f(x,y) {
		var r = (function(){
			var a = x + 10;
			var b = y + 10;
			return a + b;
		}());
		return r;
	}

ネストしたletには無名関数を作成する必要がある訳です。

ここで、変換をシンプルにするには全てのletはfunctionを作る事を考えてみると、
最初の例のプログラムは以下のJavaScriptに変換出来ます:

	function f(x, y) {
		return (function(){var a = x + 10;
		return (function(){var b = y + 10;
		return a + b;
		}());
		}());
	}

この章では、以上の変換を考えます。

### 3.2. 形式的な定義

i は識別子 nは数値として

	e ::= n | i | e + e | e e1 ... en | if e then e else e | let i = e in e | let rec i i1 ... in = e in e | ()

というλ計算のN言語から、

	j ::= n | i | j + j | j(j1, ..., jn) | j ? j : j | function (i1, ..., in) { s1 ... sn } | undefined | ( j )
	s ::= var i = j; | function i(i1, ..., in) { s1 ... sn } | return j;
	p ::= s1 ... sn

というM言語への変換を考える。このとき以下の変換で変換可能です。

	                             n -> n
	                             i -> i
	                            () -> undefined
	                             e -> e'
	                       e1 + e2 -> (e1' + e2')
	                   e e1 ... en -> e'(e1', ..., en')
	         if e1 then e2 else e3 -> (e1' ? e2' : e3')
	              let i = e1 in e2 -> (function(){var i = e1'; return e2';}())
	let rec i i1 ... in = e1 in e2 -> (function(){function i(i1,...,in) { return e1'; } return e2'; }())

整数 n、 識別子 i, unit () はそれぞれ、n、i、undefinedに変換されます。
e -> e'は再度変換する事を表している。右側の式のe'はeを変換した結果です。
加算のe1 + e2は e1,e2を変換した結果をそれぞれe1', e2'としたときに、e1' + e2'に変換されます。
関数適用 e e1 ... en は eとe1 ... en がそれぞれ e'、 e1' ... en'に変換されるとき、 e'(e1', ..., en')に変換されます。
条件分岐 if e1 then e2 else e3 は e1, e2, e3がそれぞれ e1', e2', e3'に変換されるとき、三項演算子 ? : を使って、 e1' ? e2' : e3'に変換されます。
変数の束縛と式の結合を表す let i = e1 in e2は、 e1, e2がそれぞれe1'、e2'に変換されるとき、(function(){var i = e1'; return e2';}())に変換されます。
let rec i i1 ... in = e1 in e2 はe1, e2がそれぞれe1'、e2'に変換されるとき、function i(i1,...,in) { return e1'; } e2'に変換されます。

### 3.3. 実装

OCamlで実装します。

```ocaml
type e =
  | EInt of int
  | EVar of string
  | EAdd of e * e
  | EApp of e * e list
  | EIf of e * e * e
  | ELet of string * e * e
  | ELetRec of string * string list * e * e
  | EUnit

type j =
  | JInt of int
  | JVar of string
  | JAdd of j * j
  | JApp of j * j list
  | JIf of j * j * j
  | JFun of string list * s list
  | JUndefined
and s =
  | SVar of string * j
  | SFun of string * string list * s list
  | SRet of j
type p = s list

(*

let rec f x y =
	let a = x + 10 in
	let b = y + 10 in
	a + b
in ()
*)
let src = ELetRec("f", ["x"; "y"],
	ELet("a", EAdd(EVar "x", EInt 10),
	ELet("b", EAdd(EVar "y", EInt 10),
	EAdd(EVar "a", EVar "b")
	)), EUnit)

(*
	(function(){
	  function f(x, y) {
	    return (function(){var a = x * 10;
	    return (function(){var b = y * 10;
	    return a + b;
	    }());
	    }());
	  }
	  return undefined;
	}())
*)

let dst = JApp (JFun ([],
   [SFun ("f", ["x"; "y"],
     [SRet
       (JApp
         (JFun ([],
           [SVar ("a", JAdd (JVar "x", JInt 10));
            SRet
             (JApp
               (JFun ([],
                 [SVar ("b", JAdd (JVar "y", JInt 10));
                  SRet (JAdd (JVar "a", JVar "b"))]),
               []))]),
         []))]);
    SRet JUndefined]), [])

(*
n -> n
i -> i
() -> undefined
e -> e'
e1 + e2 -> (e1' + e2')
e e1 ... en -> e'(e1', ..., en')
if e1 then e2 else e3 -> (e1' ? e2' : e3')
let i = e1 in e2 -> (function(){var i = e1'; return e2';}())
let rec i i1 ... in = e1 in e2 -> (function(){function i(i1,...,in) { return e1'; } return e2'; }())
*)

let rec cnv = function
  | EInt(i) -> JInt(i)
  | EVar(s) -> JVar(s)
  | EUnit -> JUndefined
  | EAdd(e1,e2) -> JAdd(cnv e1, cnv e2)
  | EApp(e,es) -> JApp(cnv e, List.map cnv es)
  | EIf(e1,e2,e3) -> JIf(cnv e1, cnv e2, cnv e3)
  | ELet(s,e1,e2) -> JApp(JFun([],[SVar(s, cnv e1); SRet(cnv e2)]),[])
  | ELetRec(s, ss, e1, e2) -> JApp(JFun([],[SFun(s,ss,[SRet(cnv e1)]); SRet(cnv e2)]),[])

let _ =
	assert ((cnv src) == dst)
```


## 4. より高度なλ式からJavaScriptへの変換

3章では、単純なJavaScriptへの変換を考えました。

しかし、変換結果のプログラムはネストが深く、読み辛い物となってしまいました。
この章で3章の変換をはより発展させることを考えます。
まず、4.1で変換する例を考え、4.2で形式化を行い、4.3でOCamlによる実装を行います。

### 4.1. 変換例

次の例を考えてみましょう:

	let rec f a b =
		let a = a + 10 in
		let b = b + 10 in
		a + b

名前が同じ変数で書き換えられています。

このプログラムは以下のJavaScriptに変換出来ます。

	function f(a,b) {
		var a1 = a + 10;
		var b1 = b + 10;
		return a1 + b1;
	}

この変換はまず、α変換により以下のプログラムに変換します。


	let rec f a b =
		let a1 = a + 10 in
		let b1 = b + 10 in
		a1 + b1

α変換を行うと変数の名前が一意になるので、複数のスコープをスコープ１つにまとめる事が出来るので、let a = b inをvar a = bと書き換える事が出来ます。

	function f(a,b) {
		var a1 = a + 10;
		var b1 = b + 10;
		return a1 + b1;
	}

if式はif文への変換したいので

	let f a =
		a + (if a then 1 else 2)
は

	function f(a) {
		var b;
		if(a) b = 1; else b = 2;
		return a + b;
	}

に変換する必要があります。ここで、bは新しい変数です。ifについては、k正規化で行う、平坦化が必要なわけです。

## 4.2 形式化

<!--
	i は識別子 nは数値として

	e ::= n | i | e + e | e e1 ... en | let i = e in e | if e then e else e | let rec i i1 ... in = e | let rec i () = e | ()

から

	j ::= n | i | j + j | j(j1, ..., jn)
	s ::= var i; | var i = j; | i = j; | j; | if (j) s else s | function i(i1, ..., in) { s1 ... sn } | return j;

な言語へと変換する事を考えます。


	------
	n -> n

	------
	i -> i

	---------------
	() -> undefined

	e1 -> e1'    e2 -> e2'
	----------------------
	e1 + e2 -> (e1' + e2')


	e -> e'   e1 -> e1' ... en -> en'
	--------------------------------
	e e1 ... en -> e'(e1', ..., en')

	e1 -> e1'   e2 -> e2'   e3 -> e3'   i'
	-----------------------------------------------------------------------
	if e1 then e2 else e3 ->
	  (function(){var i'; if (e1') i' = e2'; else i' = e3'; return i'; }())


	e1 -> e1'   e2 -> e2'
	------------------------------------
	let i = e1 in e2 -> var i = e1'; e2'

	e1 -> e1'   e2 -> e2'
	---------------------------------------------------------------------------
	let rec i i1 ... in = e1 in e2 -> function i(i1,...,in) { return e1'; } e2'

-->


	                             Γ |- n -> Γ |- n
	                        Γ,i=i' |- i -> Γ,i=i' |- i'
	                             Γ |- e -> Γ |- e'
	                       Γ |- e1 + e2 -> Γ |- e1' + e2'
	                            Γ |- () -> Γ |- ()
	                   Γ |- e e1 ... en -> Γ |- e' e1' ... en'
	         Γ |- if e1 then e2 else e3 -> Γ |- if e1' then e2' else e3'
	              Γ |- let i = e1 in e2 -> Γ,i=i' |- let i' = e1' in e2'
	Γ |- let rec i i1 ... in = e1 in e2 -> Γ,i=i' |- let rec i' i1' ... in' = e1' in e2'

α変換


## 4.3 OCamlで実装

b.ml,c.mlで実装しました。

b.mlではリストを@演算子を使って結合しています。これでは遅くなるので、
c.mlではSConsとSNilを作り@@関数を作ってリストの代わりに使うようにしました。
SConsには、左側にも、右側にもsを取る事が出来る様に実装したので、高速に結合出来ます。
また、b.mlでの不具合もc.mlでは、対策してあります。

## 5 フューチャーワーク

今後はより高度な変換を行うと良いでしょう。
次の例を考えてみましょう:

	let rec f a b =
		let a = a + 10 in
		let b = b + 10 in
		a + b

名前が同じ変数で書き換えられています。aは後続でもはや使われていないので、以下のように書く事が出来ます。

	function f(a,b) {
		a = a + 10;
		b = b + 10;
		return a + b;
	}

このような書き換えは、後続に変数が出現するかどうかを調べる事で可能になります。
オーナーシップの概念を持ち出す事で、関数的である場合のみ許すような事も出来るでしょう。

また、タプル、配列、レコード、リファレンス、パターンマッチング、があるとよいでしょう。
最終的にささっと、ocamlでプログラムを構築し、javascriptを出力して扱う事が出来れば便利です。

## 6 まとめ

## 7 リファレンス
