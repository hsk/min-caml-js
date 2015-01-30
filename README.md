# λ式からAltJSへの変換向けの正規形

## 1. アブストラクト

現在、AltJSと呼ばれる言語群が存在する。これはJavaScriptへのトランスレータであるプログラミング言語の総称である。
λ計算のAltJSも数多く存在している。
しかしながら、λ計算ベースの言語からJavaScriptへの変換後のコードは余美しい物ではない。
なぜならば、λ計算の変換過程での正規形にはA正規形や、k正規形、β正規形等があるが、広く知られているJavaScriptへの変換用の正規形が知られていない為である。
そこで、この文章では、まず、A正規形、k正規形、β正規形を説明し、JavaScriptへの変換には使い辛い事を述べる。
次にJavaScriptへの通常の変換方法について述べる。
そして、変換過程で重要となるであろう正規形のプロトタイプを作成する。

## 2. イントロダクション

現在、AltJSと呼ばれる言語群が存在する。これはJavaScriptへのトランスレータであるプログラミング言語の総称である。
λ計算のAltJSも数多く存在している。
しかしながら、λ計算ベースの言語からJavaScriptへの変換後のコードは余美しい物ではない。
なぜならば、λ計算の変換過程での正規形にはA正規形や、k正規形、β正規形等があるが、広く知られているJavaScriptへの変換用の正規形が知られていない為である。
そこで、この文章では、JavaScriptへの変換過程で重要となるであろう正規形のプロトタイプを作成する。

## 2.1. 各正規形について

### 2.2. A正規形とA正規化

### 2.3. K正規形とK正規化


### 2.4. β正規形とβ正規化

## 3. 単純なλ式からJavaScriptへの変換

### 3.1. 変換例

例えば、以下のOCamlのプログラムは

	let rec f x y =
		let a = x * 10 in
		let b = y * 10 in
		a + b

JavaScriptの以下のプログラムに変換できます：

	function f(x,y) {
		var a = x * 10;
		var b = y * 10;
		return a + b;
	}

ところで、以下の式を変換するには:

	let rec f x y =
		let r =
			let a = x * 10 in
			let b = y * 10 in
			a + b
		in r

以下のようにのように変換する必要があります:

	function f(x,y) {
		var r = (function(){
			var a = x * 10;
			var b = y * 10;
			return a + b;
		}());
		return r;
	}

ネストしたletには無名関数を作成する必要がある訳です。

ここで、変換をシンプルにするには全てのletはfunctionを作る事を考えてみると、
最初の例のプログラムは以下のJavaScriptに変換出来ます:

	function f(x, y) {
		return (function(){var a = x * 10;
		return (function(){var b = y * 10;
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

というM言語への変換を考える。このとき以下の変換で変換可能である。

	                             n -> n
	                             i -> i
	                            () -> undefined
	                             e -> e'
	                       e1 + e2 -> (e1' + e2')
	                   e e1 ... en -> e'(e1', ..., en')
	         if e1 then e2 else e3 -> (e1' ? e2' : e3')
	              let i = e1 in e2 -> (function(){var i = e1'; return e2';}())
	let rec i i1 ... in = e1 in e2 -> (function(){function i(i1,...,in) { return e1'; } return e2'; }())

整数 n、 識別子 i, unit () はそれぞれ、n、i、undefinedに変換される。
e -> e'は再度変換する事を表している。右側の式のe'はeを変換した結果である。
加算のe1 + e2は e1,e2を変換した結果をそれぞれe1', e2'としたときに、e1' + e2'に変換される。
関数適用 e e1 ... en は eとe1 ... en がそれぞれ e'、 e1' ... en'に変換されるとき、 e'(e1', ..., en')に変換される。
条件分岐 if e1 then e2 else e3 は e1, e2, e3がそれぞれ e1', e2', e3'に変換されるとき、三項演算子 ? : を使って、 e1' ? e2' : e3'に変換される。
変数の束縛と式の結合を表す let i = e1 in e2は、 e1, e2がそれぞれe1'、e2'に変換されるとき、(function(){var i = e1'; return e2';}())に変換される。
let rec i i1 ... in = e1 in e2 はe1, e2がそれぞれe1'、e2'に変換されるとき、function i(i1,...,in) { return e1'; } e2'に変換される。

### 3.3. 実装

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
	  | JAdd of e * e
	  | JApp of e * e list
	  | JIf of e * e * e
	  | JFun of string list * s list
	  | JUndefined
	and s =
	  | SVar of string * j
	  | SFun of string * string list * s list
	  | SRet of j
	type p = s list



## 4. より高度なλ式からJavaScriptへの変換


### 4.1. 変換例

次の例を考えてみましょう:

	let rec f a b =
		let a = a * 10 in
		let b = b * 10 in
		a + b

名前が同じ変数で書き換えられています。

このプログラムは以下のJavaScriptに変換出来ます。

	function f(a,b) {
		var a1 = a * 10;
		var b1 = b * 10;
		return a1 + b1;
	}

この変換はまず、α変換により以下のプログラムに変換します。


	let rec f a b =
		let a1 = a * 10 in
		let b1 = b * 10 in
		a1 + b1

α変換を行うと変数の名前が一意になるので、複数のスコープをスコープ１つにまとめる事が出来るので、let a = b inをvar a = bと書き換える事が出来ます。

	function f(a,b) {
		var a1 = a * 10;
		var b1 = b * 10;
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

	i は識別子 nは数値として

	e ::= n | i | e + e | e e1 ... en | let i = e in e | if e then e else e | let rec i i1 ... in = e | let rec i () = e | ()

から

	e ::= n | i | e + e | e(e1, ..., en)
	s ::= var i; | var i = e; | i = e; | e; | if (e) s else s | function i(i1, ..., in) { s1 ... sn } | return e;
	p ::= s1 ... sn

な言語へと変換する事を考えます。

	                             n -> n
	                             i -> i
	                            () -> undefined
	                             e -> e'
	                       e1 + e2 -> (e1' + e2')
	                   e e1 ... en -> e'(e1', ..., en')
	         if e1 then e2 else e3 -> (function(){var newi; if (e1') newi = e2'; else newi = e3'; return newi; }())
	              let i = e1 in e2 -> var i = e1'; e2'
	let rec i i1 ... in = e1 in e2 -> function i(i1,...,in) { return e1'; } e2'



## 5 オーナーシップによる変数の再利用

### 5.1. 変換例

次の例を考えてみましょう:

	let rec f a b =
		let a = a * 10 in
		let b = b * 10 in
		a + b

名前が同じ変数で書き換えられています。aは後続でもはや使われていないので、以下のように書く事が出来ます。

	function f(a,b) {
		a = a * 10;
		b = b * 10;
		return a + b;
	}

このような書き換えは、オーナーシップの概念を持ち出す事で、関数的である場合のみ許すような事も出来るでしょう。

とりあえず改造方針

JavaScriptをとにかく吐き出すだけ。
いちお型推論はしてもよい。
しなくてもよい。

ここで、問題なのが、正規形と正規化の問題。
広く知られている、形式化の手法はない。

従ってここで、形式化の前に、正規形のプロトタイピングを行う。
