# モジュールシステム

## 1. はじめに

モジュールの機能があれば、ファイルを複数に分けてプログラムを作成出来て便利です。しかし、作るノウハウがまとまっている文章はあまり見た事がありません。そこで、ここでは、OCamlのモジュールシステムをシミュレーションしてみます。シミュレーションするにあたって、データ構造を作成し、ファイルシステムからの読み込みを行えるようにします。出来たら、数字を計算するだけのインタプリタを作成してうまく動いたら、拡張して行きます。そこそこ動くようになったら、ネイティブな関数も呼べるようにして印字機能を作ります。

## 2. データ構造

まず、モジュールシステムを作るにあたってのデータ構造を定義します:

```ocaml
type e =
| Int of int
| Bin of e * string * e
| Var of string list
| Let of string * e * e
| Unit
| LetRec of string * string list * e * e
| Closure of env * string list * e
| App of e * e list
```

整数のみが計算出来て、関数は動き、オープンとモジュールがあります。
変数が文字列ではなく、リストになっている点に注意してください。
モジュールを扱う場合、aという名前だけではなく、Laaaa.Maaaa.Naaaa.aaというような長い名前にも対応する必要があるからです。
このような名前を考える時は、リストにしておくと便利です。

ネイティブの関数も登録出来るようにしましょう:

```ocaml
| NFun of (env * e list -> env * e)
```

オープンと、モジュールも定義しましょう:

```ocaml
| Open of string
| Module of env
| Mod of string * es
```

式以外にも印字処理として使いたいので、env,es,ssを作ります:

```ocaml
and env = (string * e) list
and es = e list
and ss = string list
[@@deriving show]
```

`[@@ deriving show]`を使えば印字も自動で出来るので便利です。

```ocaml
let _ =
  Format.printf "%s\n" (show_e (Bin(Int 1, Int 2)))
```

印字するプログラムを書いて、コンパイルしてみましょう。

```bash
$ ocamlfind ocamlc -package ppx_deriving.show module02.ml -o module02
```

としてコンパイルできます。
実行すると印字されます:

```bash
$ ./module01
Module01.Bin ((Module01.Int 1), "+", (Module01.Int 2))
```

## 3. ファイルシステム

パーサを作って動かすのも良いのですが、複数ファイルを管理するのも大変です。
擬似的に、動作するファイルシステムを作ってみましょう。
以下のように定義しておくと、ファイルがパースされた事にします:

```ocaml
module FileSystem = struct
  let files = [
    "test.ml",[
      Int 1;
      Int 2;
    ];
  ]
```

読み込み関数も作りましょう:

```ocaml
  let read f = List.assoc f files
end
```

簡単ですね。このファイルシステムから読み込むには以下のようにします。


```ocaml
let _ =
  let filename = "test.ml" in
  let es = FileSystem.read filename in
  Format.printf "%s=%s\n" filename (show_es es)
```

簡単です。実行すると:

```bash
ocamlfind ocamlc -package ppx_deriving.show module03.ml -o module03
./module03
test.ml=[(Module03.Int 1); (Module03.Int 2)]
```

動きました。

## 4. 評価器

次に式の評価器を作りましょう。

```ocaml
let rec eval (env:(string * e)list) = function
  | Int i -> (env, Int i)
  | e ->
    assert false
```

intしか動きませんが、動くはずです。次に、全データを実行する関数も書きましょう:

```ocaml
let rec evals env = function
  | [] -> env
  | x::xs ->
    Format.printf "eval : %s\n" (show_e x);
    let (env, x) = eval env x in
    Format.printf "- : %s\n" (show_e x);
    evals env xs
```

この関数は、環境eとeのリストを受け取って、１つずつeを実行します。envはまだ何もありません。
このプログラムは、以下のようにして動かす事が出来ます。

```ocaml
let _ =
  let env = [] in
  let env = evals env (FileSystem.read "test.ml") in
  env
```

実行してみましょう。

```bash
ocamlfind ocamlc -package ppx_deriving.show module04.ml -o module04
./module04
eval : (Module04.Int 1)
- : (Module04.Int 1)
eval : (Module04.Int 2)
- : (Module04.Int 2)
```

何やら動きました。replっぽいのがうれしいですね。



## 5. 足し算

足し算が出来るようにしてみましょう。以下のようにファイルに追加します:

```ocaml
    "test.ml",[
      Int 1;
      Bin (Int 1,"+", Int 2);
    ]
```

評価器の処理も追加しましょう:

```ocaml
let rec eval (env:(string * e)list) = function
  | Int i -> (env, Int i)
  | Bin(a,"+",b) ->
    let (env,a) = eval env a in
    let (env,b) = eval env b in
    begin match (a,b) with
    | Int a, Int b -> (env, Int(a + b))
    | _ -> assert false
    end
  | e ->
    assert false
```

そう、面倒くさいのですが、環境も返すようにしてあります。また、足し算はInt同士のみ行えるようにじっそうしました。

これで２つの式が動くはずです。

```
ocamlfind ocamlc -package ppx_deriving.show module05.ml -o module05
./module05
eval : (Module05.Int 1)
- : (Module05.Int 1)
eval : Module05.Bin ((Module05.Int 1), "+", (Module05.Int 2))
- : (Module05.Int 3)
```


```- : (Module05.Int 3)```のように1+2の結果が3と表示されています。OKですね。


## 6. 変数

Letを導入して変数を定義し、参照してみましょう。

```ocaml
      Let("a",Int 1, Unit);
      Var["a"];
      Let("_",Let("a",Int 11,Var["a"]), Unit);
```

Letの終端にはUnitを追加します。Letは何個でも使う事が出来ます。
１つめの式にLetを更に追加すれば新しいスコープをつくることになります。
特に印字処理は入れていませんが、１つの式を終了する毎に、evalsが表示してくれるので大丈夫です。

評価器には以下の処理を追加します。

```ocaml
  | Unit -> (env, Unit)
  | Let(i,e1,e2) ->
    let (_,e1) = eval env e1 in
    let env = (i,e1)::env in
    eval env e2
  | Var[i] ->
      (env, List.assoc i env)
```

UnitはUnitをそのまま返します。
Letは変数の設定です。
Var[i] は環境を検索して値を返します。

```
ocamlfind ocamlc -package ppx_deriving.show module06.ml -o module06
./module06

:
:

eval : Module06.Let ("a", (Module06.Int 1), Module06.Unit)
- : Module06.Unit
eval : (Module06.Var ["a"])
- : (Module06.Int 1)
eval : Module06.Let ("_",
  Module06.Let ("a", (Module06.Int 11), (Module06.Var ["a"])), Module06.Unit)
- : Module06.Unit
```

変数aに値を保存し、表示して、Letの中味の式で使ったaは中味で使われているため表には現れません。

## 7. 関数

次は関数を追加しましょう。

```ocaml
      LetRec("f",["a";"b"],Bin(Var["a"],"+",Var["b"]), Unit);
      App(Var["f"],[Int 100;Int 23]);
```

LetRecで定義して、Appで実行します。

```ocaml
  | LetRec(a,es,e1,e2) ->
    let env = (a,Closure(env,es,e1))::env in
    eval env e2
  | App(e1,es) ->
    let (env, e1) = eval env e1 in
    let (env, es) = List.fold_left (fun (env,es) e ->
      let (env,e) = eval env e in
      (env, e::es)
    ) (env,[]) es in
    let es = List.rev es in
    begin match e1 with
      | Closure(cenv,is,e) ->
        let cenv = List.fold_left2 (fun env i e ->
          (i,e)::env
        ) cenv is es in
        let (_,e) = eval cenv e in
        (env,e)
      | _ -> assert false
    end
```

実行すると以下のように、動きます:

```ocaml
eval : Module07.LetRec ("f", ["a"; "b"],
  Module07.Bin ((Module07.Var ["a"]), "+", (Module07.Var ["b"])),
  Module07.Unit)
- : Module07.Unit
eval : Module07.App ((Module07.Var ["f"]), [(Module07.Int 100); (Module07.Int 23)])
- : (Module07.Int 123)
```

## 8. ネイティブ関数

以下のようなプログラムを書いておくと、OCamlの関数を呼べます。バインディングを書いた訳ですね。

```ocaml
      LetRec("print_int", ["a"], App(NFun(function
        | (env,[Int a]) ->
          print_int a;
          (env, Unit)
        | _ -> assert false
      ),[Var["a"]]), Unit);
```

この関数を呼び出すには、以下のようにします。

```ocaml
      Let("_",App(Var["print_int"],[Int 1]), Unit);
```

評価器には、App内のClosureの下にNFunを追加し、Appの下に、NFunの生のデータがあった場合はそのまま返すようにします。

```ocaml
  | App(e1,es) ->

    :

    let es = List.rev es in
    begin match e1 with
      | Closure(cenv,is,e) ->
        :
      | NFun(f) -> f (env, es)
      | _ -> assert false
    end
  | NFun(_) as e -> (env,e)

```

実行してみましょう。

```
eval : Module08.LetRec ("print_int", ["a"],
  Module08.App ((Module08.NFun <fun>), [(Module08.Var ["a"])]), Module08.Unit)
- : Module08.Unit
eval : Module08.Let ("_",
  Module08.App ((Module08.Var ["print_int"]), [(Module08.Int 1)]),
  Module08.Unit)
1- : Module08.Unit
```

1と表示されました。改行されてないのが気持悪いですけど、動いてます。
次に改行関数も同じように作って:

```ocaml
      LetRec("print_newline", [], App(NFun(function
        | (env,_) -> print_newline(); (env,Unit)
      ),[]), Unit);

      Let("_",App(Var["print_int"],[Int 1]),
        App(Var["print_newline"],[]));
```

呼び出してみましょう。

```ocaml
eval : Module08.LetRec ("print_newline", [],
  Module08.App ((Module08.NFun <fun>), []), Module08.Unit)
- : Module08.Unit
eval : Module08.Let ("_",
  Module08.App ((Module08.Var ["print_int"]), [(Module08.Int 1)]),
  Module08.App ((Module08.Var ["print_newline"]), []))
1
- : Module08.Unit
```

改行が入って1と表示されています!

## 9. ファイルを分割


先ほど作った、print\_int や print\_newlineをprelude.mlにコピーしてa.mlから呼び出すようにしてみましょう:

```ocaml
    "prelude.ml", [
      LetRec("print_int", ["a"], App(NFun(function
        | (env,[Int a]) ->
          print_int a;
          (env, Unit)
        | _ -> assert false
      ),[Var["a"]]), Unit);
      LetRec("print_newline", [], App(NFun(function
        | (env,_) -> print_newline(); (env,Unit)
      ),[]), Unit);
    ];

    "a.ml",[
      LetRec("print_int_ln", ["a"],
        Let("_",
          App(Var["print_int"], [Var["a"]]),
          App(Var["print_newline"],[])),
      Unit);

      App (Var["print_int_ln"], [Int 1]);
    ];
```

テストを読み込むのをやめて、preludeとaを読み込むように書き換えます：

```ocaml
let _ =
  let env = [] in
  let env = evals env (FileSystem.read "prelude.ml") in
  let env = evals env (FileSystem.read "a.ml") in
  env
```

これで、２つのファイルから読み込んで実行出来るようになりました。

```
ocamlfind ocamlc -package ppx_deriving.show module09.ml -o module09
./module09
eval : Module09.LetRec ("print_int", ["a"],
  Module09.App ((Module09.NFun <fun>), [(Module09.Var ["a"])]), Module09.Unit)
- : Module09.Unit
eval : Module09.LetRec ("print_newline", [],
  Module09.App ((Module09.NFun <fun>), []), Module09.Unit)
- : Module09.Unit
eval : Module09.LetRec ("print_int_ln", ["a"],
  Module09.Let ("_",
    Module09.App ((Module09.Var ["print_int"]), [(Module09.Var ["a"])]),
    Module09.App ((Module09.Var ["print_newline"]), [])), Module09.Unit)
- : Module09.Unit
eval : Module09.App ((Module09.Var ["print_int_ln"]), [(Module09.Int 1)])
1
- : Module09.Unit
```

しかし、preludeの処理も印字されてしまいます。preludeの読み込み時は印字しないようにしましょう。

```ocaml
let rec evals1 env = function
  | [] -> env
  | x::xs ->
    let (env, x) = eval env x in
    evals1 env xs
```

evals1を呼ぶ場合は印字されません:

```ocaml
let _ =
  let env = [] in
  let env = evals1 env (FileSystem.read "prelude.ml") in
  let env = evals env (FileSystem.read "a.ml") in
  env
```

readも同様にしましょう。出来ました。

```
ocamlfind ocamlc -package ppx_deriving.show module09.ml -o module09
./module09
eval : Module09.LetRec ("print_int_ln", ["a"],
  Module09.Let ("_",
    Module09.App ((Module09.Var ["print_int"]), [(Module09.Var ["a"])]),
    Module09.App ((Module09.Var ["print_newline"]), [])), Module09.Unit)
- : Module09.Unit
eval : Module09.App ((Module09.Var ["print_int_ln"]), [(Module09.Int 1)])
1
- : Module09.Unit
```

## 10. Open

いよいよ、Openを実装しましょう。

```ocaml
    "b.ml", [
      Open "A";

      Let("_",
        App(Var["print_int_ln"],[Int 12345]),
      Unit);
    ];
```

これを動かすには、Openの処理にファイルを読み込む処理を書けば良いだけです。
インクルードするだけなら、簡単そうです。

```ocaml
  | Open(x) ->
      let filename = (String.uncapitalize x) ^ ".ml" in
      List.fold_left (fun (env,r) e ->
        eval env e
      ) (env,Unit) (FileSystem.read filename)
```

文字列の最初の文字を小文字にして、".ml"を付けて、読み込み、それぞれを実行するだけです。

"a.ml"ではなく、"b.ml"を読み込むようにして:

```ocaml
let _ =
  let env = [] in
  let env = evals1 env (FileSystem.read "prelude.ml") in
  let env = evals env (FileSystem.read "b.ml") in
  env
```

実行すると:

```
ocamlfind ocamlc -package ppx_deriving.show module10.ml -o module10
./module10
eval : Module10.LetRec ("print_int_ln", ["a"],
  Module10.Let ("_",
    Module10.App ((Module10.Var ["print_int"]), [(Module10.Var ["a"])]),
    Module10.App ((Module10.Var ["print_newline"]), [])), Module10.Unit)
- : Module10.Unit
eval : Module10.App ((Module10.Var ["print_int_ln"]), [(Module10.Int 1)])
1
- : Module10.Unit
```

読み込まれて、実行されました。インクルードがうまく行きました！

## モジュールの自動読み込み

つぎは、Openを書かずに、`A.print_int_ln`と書けば、ファイルを読み込むようにしてみましょう。

```ocaml
    "c.ml", [

      Let("_",
        App(Var["A";"print_int_ln"],[Int 12345]),
      Unit);
      Let("k", Int 55555, Unit);
    ];
```

evalのVarの処理とOpenの処理を以下のように書き換えます:

```ocaml
  | Var i ->
    begin try
      let rec findE (env:(string * e)list) = function
        | [] -> assert false
        | x::xs ->
          begin match List.assoc x env with
          | Module(env) -> findE env xs
          | e -> e
        end
      in
      (env, findE env i)
    with
      | _ ->
        begin match i with
        | x::xs when not (List.mem_assoc x env) ->
          let (env2,_) = eval env (Open x) in
          eval ((x,Module env2)::env) (Var i)
        | _ ->
          Format.printf "not found variable %a\n" pp_ss i;
          assert false
        end
    end
  | Open(x) ->

    if List.mem_assoc x env then (
      match (List.assoc x env) with
      | Module(env2) -> (env2 @ env,Unit)
      | _ -> Format.printf "open error %s\n" x; assert false
    )
    else (
      let filename = (String.uncapitalize x) ^ ".ml" in
      Format.printf "env = %s\n" (show_env env);
      Format.printf "read %s\n" filename;

      List.fold_left (fun (env,r) e ->
        eval env e
      ) (env,Unit) (FileSystem.read filename)
    )
```

どうでしょう:

```
./module11
eval : Module11.Let ("_",
  Module11.App ((Module11.Var ["A"; "print_int_ln"]), [(Module11.Int 12345)]),
  Module11.Unit)
env = [("print_newline",
  Module11.Closure (
    [("print_int",
      Module11.Closure ([], ["a"],
        Module11.App ((Module11.NFun <fun>), [(Module11.Var ["a"])])))], [
    ], Module11.App ((Module11.NFun <fun>), [])));
 ("print_int",
  Module11.Closure ([], ["a"],
    Module11.App ((Module11.NFun <fun>), [(Module11.Var ["a"])])))]
read a.ml
1
12345
- : Module11.Unit
eval : Module11.Let ("k", (Module11.Int 55555), Module11.Unit)
- : Module11.Unit
```

うまくうごいてます。

## 12. ネストしたモジュール読み込み

```ocaml
    "d.ml", [
      Open "C";
      Let("_",
        App(Var["A";"print_int_ln"],[Var["k"]]),
      Unit);

      Let("_",
        App(Var["A";"print_int_ln"],[Var["C";"k"]]),
      Unit);
    ];
```

d.mlを実行すると

```
./module12
eval : (Module12.Open "C")
read c.ml
read a.ml
1
12345
read a.ml
1
55555
read a.ml
1
read c.ml
12345
55555
- : Module12.Unit
```

何度も読み込まれてますが、動いています。最適化が必要ですね。

## 13. 内部モジュール

内部のモジュールも使えると良いです。

```ocaml
    "e.ml", [
      Mod [
        Let("_",
          App(Var["A";"print_int_ln"],[Var["k"]]),
        Unit);
        Let("_",
          App(Var["A";"print_int_ln"],[Var["C";"k"]]),
        Unit);
      ]
    ];
```

使えるようにしましょう。

```
  | Mod(s, es) ->
    let envm = evals env es in
    ((s,Module(envm))::env, Unit) 
```

こうすれば、読み込めるはず。
残す問題は、毎回読み込まれてしまう事です。


```
    "f.ml", [
      Let("_",
        App(Var["A";"print_int_ln"],[Var["E";"Inner";"k"]]),
      Unit);
      Let("_",
        App(Var["A";"print_int_ln"],[Var["E";"Inner";"k"]]),
      Unit);
    ];
```

こうして、Eを読み込んで、印字してみましょう。

## 14. キャッシュする

ファイルを何度もOpenするのでは効率がよくありません。ファイルをキャッシュしましょう。

## 15. 依存フローを作成する

## 16. 依存解析する

## 17. まとめ

ざーっと、最小構成に近い、モジュールシステムを作成してみました。
最初に最小限のデータ構造を定義し、ファイルシステムを作成し、評価器を作りました。
評価器に十分な機能を追加した後、Openしたり、モジュールの読み込みを作成してみました。

今回はインタプリタの作成ですが、このような考えを元にコンパイラへ応用を考えると良いでしょう。
実のところ、ファイルの依存関係はとくに、わざわざグラフを作らなくても済む事が分かったように思います。
コンパイラを作成する場合は、依存元を先にコンパイルすると、
型推論が着実に行えます。とりあえず、メインファイルが依存しているファイルは全てオープンし、そのより上のファイルを読み込んで行く事で、全てのファイルを認識出来ます。
どのファイルがどのファイルを読み込んでいるかを記録しておくと、それはグラフの情報となります。
依存解析をすることで、コンパイル順を決定する事が出来るでしょう。

$a \ne 0$, there are two solutions to \(ax^2 + bx + c = 0\) and they are
$$x = {-b \pm \sqrt{b^2-4ac} \over 2a}.$$


