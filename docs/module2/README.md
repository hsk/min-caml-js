# モジュールシステムと依存解析

これは例によって書き掛けです。最後の詰めがまだ甘い所があります。

## はじめに

  モジュールの機能があれば、ファイルを複数に分けてプログラムを作成出来て便利です。
  そこで、OCamlのモジュールシステムを分析し、最小限のデータ構造でアルゴリズムを考えます。
  まず1章で非常に簡単なOpenがあるだけのデータ構造を考え、2章でトラバースしてみます。
  3章でサブモジュールがある場合の問題について取り上げ、再帰的な読み込みの必要性を述べ、実装してみます。
  4章では結果のデータ構造について考え、実装します。
  5章ではファイルのキャッシュを、６章では再入の防止を考えます。
  7章と8章でそれぞれ、依存グラフの取得と、読み込み順の取得を考えます。
  9章ではサブモジュールについて拡張し、10章で変数について拡張します。
  11章では処理系に組み込んで使う事を考え、最後に12章でまとめます。

## 1. データ構造

  とにかく、簡単なデータ構造でファイルの依存解析を考えましょう。

    type e =
      | Unit
      | Open of string * e

  オープンして、継続し、最後Unitで終わるだけの言語を考えます。

  どんなデータが表現出来るかと言うと、

    module FileSystem = struct
      let files = [
        "A",
          Unit;
        "E",
          Open("A", Unit);
        "F", 
          Open("E", Unit);
        "Inner",
          Open("F", Unit);
        "G",
          Open("Inner", Open("E", Open("F", Unit)));
        "H", Open("I", Unit);
        "I", Open("H", Unit);
        "J", Open("K", Unit);
        "K", Open("L", Unit);
        "L", Open("J", Unit);

      ]
      let read f = List.assoc f files
    end


  こんな感じですのデータ構造を表現出来ます。Aは何もオープンしません。
  EはAをオープンしており、Aに依存しています。
  FはEに依存しています。Gはとにかく色々なプログラムに依存しています。

  HとIは相互依存しています。

  J,K,Lは三つどもえの状態で依存しています。

  これらのデータの依存関係を解析したいわけです。

    let _ =
      Format.printf "%s\n" (show_e (FS.read "A"))

  読み込みはこのように行います。

## 2. トラバース

  トラバースするプログラムを書いてみます。

    type e =
      | Unit
      | Open of string * e
    [@@deriving show]

    module FS = struct
      let files = [
        "A",
          Unit;
        "E",
          Open("A", Unit);
        "F", 
          Open("E", Unit);
        "Inner",
          Open("F", Unit);
        "G",
          Open("Inner", Open("E", Open("F", Unit)));
        "H", Open("I", Unit);
        "I", Open("H", Unit);
        "J", Open("K", Unit);
        "K", Open("L", Unit);
        "L", Open("J", Unit);

      ]
      let read f = List.assoc f files
    end

    type ss = string list
    [@@deriving show]

    let rec eval = function
      | Unit -> []
      | Open(x,e) -> x::(eval e)

    let test file =
      let e = FS.read file in
      Format.printf "## %s\n" file;
      Format.printf "    %s\n" (show_e e);
      let ss = eval e in
      Format.printf "    %s\n" (show_ss ss)

    let _ =
      List.iter(fun (file,_) ->
        test file
      ) FS.files

  実行結果

    ## A
        T02.Unit
        []
    ## E
        T02.Open ("A", T02.Unit)
        ["A"]
    ## F
        T02.Open ("E", T02.Unit)
        ["E"]
    ## Inner
        T02.Open ("F", T02.Unit)
        ["F"]
    ## G
        T02.Open ("Inner", T02.Open ("E", T02.Open ("F", T02.Unit)))
        ["Inner"; "E"; "F"]
    ## H
        T02.Open ("I", T02.Unit)
        ["I"]
    ## I
        T02.Open ("H", T02.Unit)
        ["H"]
    ## J
        T02.Open ("K", T02.Unit)
        ["K"]
    ## K
        T02.Open ("L", T02.Unit)
        ["L"]
    ## L
        T02.Open ("J", T02.Unit)
        ["J"]

  何とも奇麗に依存情報が表示されました。

## 3. 再帰的な読み込み

  ここで、サブモジュールを考えましょう。

    type e =
      | Unit
      | Open of string * e
      | Mod of string * e * e

  こいつがくせ者なのです。たった１行追加しただけです。
  しかしこれが、くせ者なのです。

      "A",
        Mod("B",
          Unit,
        Unit);
      "C",
        Open("A",
        Open("B",
        Unit);

  このプログラムがその問題の本質を捉えています。問題はOpen(B)が、Aのサブモジュールなのか、
  ファイルを開く事なのかは、Aを開いてみないと分からないのです。

  何を言っているんだ？っていますが、以下のプログラムを作ってみてください。

  aa.ml

    module Bb = struct
      let a = "Aa.Bb.a"
    end

  bb.ml

    let a = "Bb.a"

  cc.ml

    open Aa
    let _ =
      Printf.printf "Bb.a=%s\n" Bb.a

  という３つのプログラムを作り以下のようにコンパイル＆実行します。

    ocamlc aa.ml bb.ml cc.ml -o cc
    $ ./cc
    Aa.Bb.a

  aa.mlの中味が表示されています。悪いご冗談をと思うのですが、事実です。

  つまり、openしたモジュールは先に調べなくてはなりません。
  ということは、１つのファイルを調べている間に、他の依存があったら、再入、再入と再帰的に解析して行く必要があるのです。

  さて、再帰的に解析するプログラムを書いてみましょう。

  サブモジュールはとりあえず後回しにしましょう。

    let rec eval = function
      | Unit -> []
      | Open(x,e2) ->
        let e1 = FS.read x in
        let ls = eval e1 in
        ls @ x::(eval e2)

  実行すると

    ## A
        T03.Unit
        []
    ## E
        T03.Open ("A", T03.Unit)
        ["A"]
    ## F
        T03.Open ("E", T03.Unit)
        ["A"; "E"]
    ## Inner
        T03.Open ("F", T03.Unit)
        ["A"; "E"; "F"]
    ## G
        T03.Open ("Inner", T03.Open ("E", T03.Open ("F", T03.Unit)))
        ["A"; "E"; "F"; "Inner"; "A"; "E"; "A"; "E"; "F"]
    ## H
        T03.Open ("I", T03.Unit)
    Fatal error: exception Stack_overflow

  こんな結果になりました。そうご参照していると、HからIを読んで、IからHを読んでと繰り返してエラーです。

  Gを見ると、同じ物が繰り返し表示されてますし、FはEしか見てないのに、Aも表示されています。また、同じファイルを何度も読んでいるのも問題です。

  1. 結果をモジュール毎に分けて残す。
  2. 同じファイルはキャッシュする。
  3. 一度見たファイルは再入しない。

  というような事を考えなくてはなりません。

## 4. 結果のデータ構造

  結果をモジュール毎に分けて残す為に、呼び出した情報をツリー状に返すようにしてみましょう。

### 4.1. 依存情報の為のデータ構造

  一度、より単純なデータ構造で色々実験してみましょう。

    type e =
      | End
      | Let of string * e * e
    [@@deriving show]

  endとletだけがある言語を考えます。
  具体的にデータを書いてみると、以下のようなデータを作る事が出来ます。

    let e =
      Let("O1",
        Let("N1",
          Let("M1", End,
          Let("M2", End,
          End)),
        Let("N2",
          Let("M1", End,
          Let("M2", End,
          End)),
        End)),
      Let("O2",
        Let("N1",
          Let("M1", End,
          Let("M2", End,
          End)),
        Let("N2",
          Let("M1", End,
          Let("M2", End,
          End)),
        End)),
      End))

  これを以下のデータ構造に変換する事を考えましょう。

    type l =
      | Tag of string * l list
    and ls = l list
    [@@deriving show]

  l では以下のように先ほどのデータを表す事が出来ます。

    let l =
      Tag("O1",[
        Tag("N1", [
          Tag("M1", []);
          Tag("M2", []);
        ]);
        Tag("N1", [
          Tag("M1", []);
          Tag("M2", []);
        ]);
      ])

#### 4.1.1. eからlへ変換

  e の利点は１つのデータ構造で表す事が出来る事です。
  lのほうが普通に考えれば分かりやすいのですが、lsとlの２つの処理を作らなくては行けません。
  リスト構造をeのデータに持たせてしまえば、1つの関数で処理出来る訳です。

  まず、このようなXMLのようなタグを持っただけのデータに変換する事を考えましょう。

    let rec e2l (e:e):l list =
      match e with
      | End -> []
      | Let(x,e1,e2) ->
         Tag(x, e2l e1)::(e2l e2)

  これだけで、変換出来ます。
  使ってみましょう。

    let _ =
      let l = e2l e in
      Printf.printf "%s\n" (show_ls l)

  結果として先ほど示したようなデータが出力されます。

    [T04_a.Tag ("O1",
       [T04_a.Tag ("N1", [T04_a.Tag ("M1", []); T04_a.Tag ("M2", [])]);
        T04_a.Tag ("N2", [T04_a.Tag ("M1", []); T04_a.Tag ("M2", [])])]);
     T04_a.Tag ("O2",
       [T04_a.Tag ("N1", [T04_a.Tag ("M1", []); T04_a.Tag ("M2", [])]);
        T04_a.Tag ("N2", [T04_a.Tag ("M1", []); T04_a.Tag ("M2", [])])])]

  ネストが分かりにくいかもしれないので、書き換えると

    [
      T04_a.Tag ("O1", [
        T04_a.Tag ("N1", [
          T04_a.Tag ("M1", []);
          T04_a.Tag ("M2", [])
        ]);
        T04_a.Tag ("N2", [
          T04_a.Tag ("M1", []);
          T04_a.Tag ("M2", [])
        ])
      ]);
      T04_a.Tag ("O2", [
        T04_a.Tag ("N1", [
          T04_a.Tag ("M1", []);
          T04_a.Tag ("M2", [])
        ]);
        T04_a.Tag ("N2", [
          T04_a.Tag ("M1", []);
          T04_a.Tag ("M2", [])
        ])
      ])
    ]

  このように、正しく変換されている事が分かります。

#### 4.1.2. lから親と子のリストの取得

  次に、このタグデータからタグデータ名と子の要素名のリストのリストを作ってみましょう。
  これは、依存情報のリストを作る事に対応します。

    let rec f2 = function
      | Tag(s,ls) ->

  タグ名を子のリストから作成し

        let tagnames = List.map(fun (Tag(n,_)) -> n) ls in

  後続のlsについて再帰的に呼び出します。@つかってますが、まぁ良いでしょう。

        let nexts = List.fold_left (fun l tag ->
          (f2 tag) @ l
        ) [] ls in

  タグ名とタグ名のリストを組にした物と継続のリストを結合して返せば完了です。

        (s,tagnames)::nexts

  印字の為にsssタイプを作って、

    type sss = (string * string list) list
    [@@deriving show]

  使ってみましょう。

    let _ =
      let sss = f2 (Tag("root",l)) in
      Printf.printf "f2 %s\n" (show_sss sss);

  結果は

    f2 [("root", ["O1"; "O2"]); ("O2", ["N1"; "N2"]); ("N2", ["M1"; "M2"]);
     ("M2", []); ("M1", []); ("N1", ["M1"; "M2"]); ("M2", []); ("M1", []);
     ("O1", ["N1"; "N2"]); ("N2", ["M1"; "M2"]); ("M2", []); ("M1", []);
     ("N1", ["M1"; "M2"]); ("M2", []); ("M1", [])]

  できましたね。リストはタグじゃないからrootって付けました。
  XMLを処理する場合もこういうことをする事があったりしません？
  面倒くさいwこのめんどくささをなくしたい訳ですね。

#### 4.1.3. eから親と子のリストの取得

  次はeから同じ物を作ってみましょう。

    let rec f3 (e:e) =
      match e with
      | End -> []
      | Let(x,e1,e2) ->
        let rec names = function
          | End -> []
          | Let(x,_,e2) -> x::(names e2)
        in
        let names = names e1 in
        (x,names)::(f3 e1)@(f3 e2)

  考え方は一緒です最初に子どもの名前のリストを作って、後続のプログラムと結合します。
  使ってみましょう。

    let _ =
      let sss = f3 e in
      Printf.printf "f3 %s\n" (show_sss sss)

  結果は同じように出ました。rootがないですが

    f3 [("O1", ["N1"; "N2"]); ("N1", ["M1"; "M2"]); ("M1", []); ("M2", []);
     ("N2", ["M1"; "M2"]); ("M1", []); ("M2", []); ("O2", ["N1"; "N2"]);
     ("N1", ["M1"; "M2"]); ("M1", []); ("M2", []); ("N2", ["M1"; "M2"]);
     ("M1", []); ("M2", [])]

  いいかんじです。

#### 4.1.4. eから親と子のリストの取得2

  @を使っているのが気持悪い所でしたので、なくして、さらに同じ物が複数現れないようにしてみましょう。

    let rec f4 env e =
      match e with
      | End -> env
      | Let(x,e1,e2) ->
        let rec names = function
          | End -> []
          | Let(x,_,e2) -> x::(names e2)
        in
        if(List.mem_assoc x env) then
          (f4 (f4 env e2) e1)
        else
          (x,names e1)::(f4 (f4 env e2) e1)

    let _ =
      let sss = f4 [] e in
      Printf.printf "f4 %s\n" (show_sss sss)

  できました。

    f4 [("M2", []); ("M1", []); ("N2", ["M1"; "M2"]); ("N1", ["M1"; "M2"]);
     ("O2", ["N1"; "N2"]); ("O1", ["N1"; "N2"])]

  このように、eの構造から、親とこのリストを作る事が出来ます。親とこの関係は、依存リストとして扱う事が出来ます。

### 4.2. 組み込み

  これらのノウハウを元に現状のプログラムの結果を考えましょう。

    type r =
      | RUnit
      | RLet of string * r * r
    and ss = string list
    [@@deriving show]

  型rを用意して、RUnitとRLetがあるようにします。この構造であれば、ツリー状にデータが残せるはずです。

    let rec eval = function
      | Unit -> RUnit
      | Open(x, e2) ->
        let e1 = FS.read x in
        let r1 = eval e1 in
        let r2 = eval e2 in
        RLet(x, r1, r2)
      
    and start x =
      let e = (FS.read x) in
      Format.printf "    %s\n" (show_e e);
      let r = eval e in
      RLet(x, r, RUnit)

    let test file =
      Format.printf "## %s\n" file;
      let r = start file in
      Format.printf "    %s\n" (show_r r)

  結果は以下のようになります。

    ## A
        T04.Unit
        T04.RLet ("A", T04.RUnit, T04.RUnit)
    ## E
        T04.Open ("A", T04.Unit)
        T04.RLet ("E", T04.RLet ("A", T04.RUnit, T04.RUnit), T04.RUnit)
    ## F
        T04.Open ("E", T04.Unit)
        T04.RLet ("F",
      T04.RLet ("E", T04.RLet ("A", T04.RUnit, T04.RUnit), T04.RUnit), T04.RUnit)
    ## Inner
        T04.Open ("F", T04.Unit)
        T04.RLet ("Inner",
      T04.RLet ("F",
        T04.RLet ("E", T04.RLet ("A", T04.RUnit, T04.RUnit), T04.RUnit),
        T04.RUnit), T04.RUnit)
    ## G
        T04.Open ("Inner", T04.Open ("E", T04.Open ("F", T04.Unit)))
        T04.RLet ("G",
      T04.RLet ("Inner",
        T04.RLet ("F",
          T04.RLet ("E", T04.RLet ("A", T04.RUnit, T04.RUnit), T04.RUnit),
          T04.RUnit),
        T04.RLet ("E", T04.RLet ("A", T04.RUnit, T04.RUnit),
          T04.RLet ("F",
            T04.RLet ("E", T04.RLet ("A", T04.RUnit, T04.RUnit), T04.RUnit),
            T04.RUnit))), T04.RUnit)
    ## H
        T04.Open ("I", T04.Unit)
    Fatal error: exception Stack_overflow

  Hはやはりエラーになりますが、ツリー状に情報が残っています。

## 5. ファイルのキャッシュ

  ファイルを何度も読み込むのは処理が遅くなるので高速化の為に１回だけ読み込み後はキャッシュしましょう。
  環境情報を用意して、そこに読み込んだファイルの情報を残すようにします。


    type r =
      | RUnit
      | RLet of string * r * r
      | RRef of r
    and ss = string list
    [@@deriving show]
    type v =
      | VEnv of env * r
    and env = {g:(string * v) list}
    [@@deriving show]

    let empty = {g=[]}
    let rec occur {g=env} x =
      List.mem_assoc x env

    let rec get_cache {g=env} x = 
      match List.assoc x env with
      | VEnv(e,r) -> (e,r)

    let add_cache {g=env} x v = {g=(x,v)::env}

    let rec eval (env:env) = function
      | Unit -> (env, RUnit)
      | Open(x1, e2) ->
        if occur env x1 then
          let (env, r1) = cache env x1 in
          let (env, r2) = eval env e2 in
          (env, RLet(x1, r1, r2))
        else
          let (env, r1) = read env x1 in
          let (env, r2) = eval env e2 in
          (env, RLet(x1, r1, r2))
    and read env x1 =
      let (env1, r1) = eval env (FS.read x1) in
      (add_cache env1 x1 (VEnv (env1, r1)), r1)
    and cache env x1 =
      let (env1, r1) = get_cache env x1 in
      (env, RRef r1)
    and start x =
      let (_,r) = read empty x in
      RLet(x, r, RUnit)

  これで、キャッシュは出来ていますが、やっぱり再入してしまいます。

    ## A
        T05.RLet ("A", T05.RUnit, T05.RUnit)
    ## E
        T05.RLet ("E", T05.RLet ("A", T05.RUnit, T05.RUnit), T05.RUnit)
    ## F
        T05.RLet ("F",
      T05.RLet ("E", T05.RLet ("A", T05.RUnit, T05.RUnit), T05.RUnit), T05.RUnit)
    ## Inner
        T05.RLet ("Inner",
      T05.RLet ("F",
        T05.RLet ("E", T05.RLet ("A", T05.RUnit, T05.RUnit), T05.RUnit),
        T05.RUnit), T05.RUnit)
    ## G
        T05.RLet ("G",
      T05.RLet ("Inner",
        T05.RLet ("F",
          T05.RLet ("E", T05.RLet ("A", T05.RUnit, T05.RUnit), T05.RUnit),
          T05.RUnit),
        T05.RLet ("E", (T05.RRef T05.RLet ("A", T05.RUnit, T05.RUnit)),
          T05.RLet ("F",
            (T05.RRef
               T05.RLet ("E", T05.RLet ("A", T05.RUnit, T05.RUnit), T05.RUnit)),
            T05.RUnit))), T05.RUnit)
    ## H
    Fatal error: exception Stack_overflow

## 6. 再入の防止

  再入を防止するには、再入したことの判定が必要です。
  再入判定の為の目印を用意して、目印が見つかったら再入していると判定しましょう。
  具体的には、ファイルを読み込むときに、キャッシュ情報に読み込み中である情報を書き込みましょう。
  読み込み中に更に読み込もうとしたら、再帰的に読み込んでいる事になるので、エラーです。

  type vに以下のデータ追加します:

      | VCycle

  そして読み込み関数のreadでファイル読み込みの後、評価する前にファイル名にVCycleの目印を入れます。

    and read env x1 =
      Format.printf "read %s\n" x1;
      let (env1, r1) = eval (add_cache env x1 VCycle) (FS.read x1) in
      (add_cache env1 x1 (VEnv (env1, r1)), r1)

  次に結果の type rに以下のデータを追加します:

      | RCycle

  キャッシュの取得関数にVCycleならRCycleを返すようにしましょう:

    let rec get_cache {g=env} x = 
      match List.assoc x env with
      | VEnv(e,r) -> (e,r)
      | VCycle -> (empty,RCycle)

  実行すると以下のようになります。

    ## A
        T06.RLet ("A", T06.RUnit, T06.RUnit)
    ## E
        T06.RLet ("E", T06.RLet ("A", T06.RUnit, T06.RUnit), T06.RUnit)
    ## F
        T06.RLet ("F",
      T06.RLet ("E", T06.RLet ("A", T06.RUnit, T06.RUnit), T06.RUnit), T06.RUnit)
    ## Inner
        T06.RLet ("Inner",
      T06.RLet ("F",
        T06.RLet ("E", T06.RLet ("A", T06.RUnit, T06.RUnit), T06.RUnit),
        T06.RUnit), T06.RUnit)
    ## G
        T06.RLet ("G",
      T06.RLet ("Inner",
        T06.RLet ("F",
          T06.RLet ("E", T06.RLet ("A", T06.RUnit, T06.RUnit), T06.RUnit),
          T06.RUnit),
        T06.RLet ("E", (T06.RRef T06.RLet ("A", T06.RUnit, T06.RUnit)),
          T06.RLet ("F",
            (T06.RRef
               T06.RLet ("E", T06.RLet ("A", T06.RUnit, T06.RUnit), T06.RUnit)),
            T06.RUnit))), T06.RUnit)
    ## H
        T06.RLet ("H",
      T06.RLet ("I", T06.RLet ("H", (T06.RRef T06.RCycle), T06.RUnit), T06.RUnit),
      T06.RUnit)
    ## I
        T06.RLet ("I",
      T06.RLet ("H", T06.RLet ("I", (T06.RRef T06.RCycle), T06.RUnit), T06.RUnit),
      T06.RUnit)
    ## J
        T06.RLet ("J",
      T06.RLet ("K",
        T06.RLet ("L", T06.RLet ("J", (T06.RRef T06.RCycle), T06.RUnit),
          T06.RUnit), T06.RUnit), T06.RUnit)
    ## K
        T06.RLet ("K",
      T06.RLet ("L",
        T06.RLet ("J", T06.RLet ("K", (T06.RRef T06.RCycle), T06.RUnit),
          T06.RUnit), T06.RUnit), T06.RUnit)
    ## L
        T06.RLet ("L",
      T06.RLet ("J",
        T06.RLet ("K", T06.RLet ("L", (T06.RRef T06.RCycle), T06.RUnit),
          T06.RUnit), T06.RUnit), T06.RUnit)


      RLet ("G",
        RLet ("Inner",
          RLet ("E",
            RLet ("F",
              RUnit,
              (RRef
                 RLet ("E",
                   RUnit,
                   RLet ("A",
                     RUnit,
                     RUnit)))),
            (RRef RLet ("A", RUnit, RUnit))),
          RLet ("F",
            RUnit,
            RLet ("E",
              RUnit,
              RLet ("A", RUnit, RUnit)))),
        RUnit)

  再帰的な参照を含んでいるプログラムも全て読み込む事が出来ました。
  再帰的なデータ構造については、再入がある事も分かります。

## 7. 依存グラフの取得

  次は依存グラフを作りましょう。4.1.4で作成したプログラムを改良して組み込めばよいのです。

    type deps = (string * string list) list
    [@@deriving show]

    let rec deps env e =
      match e with
      | RRef e -> deps env e
      | RLet(x,e1,e2) ->
        let rec names = function
          | RRef e -> names e
          | RLet(x,_,e2) -> x::(names e2)
          | _ -> []
        in
        if(List.mem_assoc x env) then
          (deps (deps env e2) e1)
        else
          (x,names e1)::(deps (deps env e2) e1)
      | _ -> env

  P1に関連するファイルの、依存リストは以下のようになります。

    [("P1", ["O1"; "O2"; "O3"]); ("O1", ["N1"; "N2"; "N3"]);
     ("O2", ["N1"; "N2"; "N3"]); ("O3", ["N1"; "N2"; "N3"]);
     ("N1", ["M1"; "M2"; "M3"]); ("N2", ["M1"; "M2"; "M3"]);
     ("N3", ["M1"; "M2"; "M3"]); ("M1", []); ("M2", []); ("M3", [])]

## 8. 読み込み順の取得

  読み込み順を取得するsortという関数を作ってみましょう。

    let rec sort = function
      | RUnit -> []
      | RCycle -> []
      | RRef _ -> []
      | RLet(s,r1,r2) -> sort r1 @ s :: sort r2

  動かすと、

    ## A
        T06.RLet ("A", T06.RUnit, T06.RUnit)
        ["A"]
    ## E
        T06.RLet ("E", T06.RLet ("A", T06.RUnit, T06.RUnit), T06.RUnit)
        ["A"; "E"]
    ## F
        T06.RLet ("F",
          T06.RLet ("E",
            T06.RLet ("A",
              T06.RUnit,
              T06.RUnit
            ),
            T06.RUnit),
          T06.RUnit)

        ["A"; "E"; "F"]
    ## Inner
        T06.RLet ("Inner",
          T06.RLet ("F",
            T06.RLet ("E",
              T06.RLet ("A", T06.RUnit,
                T06.RUnit),
              T06.RUnit),
            T06.RUnit),
          T06.RUnit)

        ["A"; "E"; "F"; "Inner"]
    ## G
        T06.RLet ("G",
      T06.RLet ("Inner",
        T06.RLet ("F",
          T06.RLet ("E", T06.RLet ("A", T06.RUnit, T06.RUnit), T06.RUnit),
          T06.RUnit),
        T06.RLet ("E", (T06.RRef T06.RLet ("A", T06.RUnit, T06.RUnit)),
          T06.RLet ("F",
            (T06.RRef
               T06.RLet ("E", T06.RLet ("A", T06.RUnit, T06.RUnit), T06.RUnit)),
            T06.RUnit))), T06.RUnit)
        ["A"; "E"; "F"; "Inner"; "E"; "F"; "G"]
    ## H
        T06.RLet ("H",
      T06.RLet ("I", T06.RLet ("H", (T06.RRef T06.RCycle), T06.RUnit), T06.RUnit),
      T06.RUnit)
        ["H"; "I"; "H"]
    ## I
        T06.RLet ("I",
      T06.RLet ("H", T06.RLet ("I", (T06.RRef T06.RCycle), T06.RUnit), T06.RUnit),
      T06.RUnit)
        ["I"; "H"; "I"]
    ## J
        T06.RLet ("J",
      T06.RLet ("K",
        T06.RLet ("L", T06.RLet ("J", (T06.RRef T06.RCycle), T06.RUnit),
          T06.RUnit), T06.RUnit), T06.RUnit)
        ["J"; "L"; "K"; "J"]
    ## K
        T06.RLet ("K",
      T06.RLet ("L",
        T06.RLet ("J", T06.RLet ("K", (T06.RRef T06.RCycle), T06.RUnit),
          T06.RUnit), T06.RUnit), T06.RUnit)
        ["K"; "J"; "L"; "K"]
    ## L
        T06.RLet ("L",
      T06.RLet ("J",
        T06.RLet ("K", T06.RLet ("L", (T06.RRef T06.RCycle), T06.RUnit),
          T06.RUnit), T06.RUnit), T06.RUnit)
        ["L"; "K"; "J"; "L"]

  良さそうです。
  ただ読み出し順に同じファイルが含まれてしまってます。

    let rec sort = function
      | RUnit -> []
      | RCycle -> []
      | RRef _ -> []
      | RLet(s,RRef _,r2) -> sort r2
      | RLet(s,r1,r2) -> sort r1 @ s :: sort r2

  と書き換えれば、

    ## A
        ["A"]
    ## E
        ["A"; "E"]
    ## F
        ["A"; "E"; "F"]
    ## Inner
        ["A"; "E"; "F"; "Inner"]
    ## G
        ["A"; "E"; "F"; "Inner"; "G"]
    ## H
        ["I"; "H"]
    ## I
        ["H"; "I"]
    ## J
        ["L"; "K"; "J"]
    ## K
        ["J"; "L"; "K"]
    ## L
        ["K"; "J"; "L"]

  うまく行きました。

## 9. サブモジュールの導入

  サブモジュールを作りましょう。

    type e =
      | Unit
      | Open of string * e
      | Mod of string * e * e


  を追加します。

    | VEnvIn of env * r
    | RCons of r * r


  type vにはVEnvInを追加します。
  type rにはRConsを追加します。

## 10. 変数の導入

  次は、変数を追加しましょう。Openと殆ど同じですが、

    type e =
      | Unit
      | Open of string * e
      | Var of string * e

## 11. 処理系への組み込み

  せっかく考えたプログラムですので、出来ればこのまま使いたい物です。
  そこでこの章では、実際の処理系に組み込む事を考えます。

  実際の処理系は、コンパイラのメイン関数がファイルを読み込み、構文解析をして構文木sを作成するとします。
  その後すぐにコンパイルの処理に入る訳ですが、ここで、構文木sをeに書き換えるとしましょう。
  そうすると、構文木eへの書き換え途中に、別の構文木sの読み込みが必要になってしまいます。

  これではうまく行きません。ファイルシステムとして考えていた部分が通常のコンパイラの構文解析器と考えましょう。
  モジュールシステムを先に読み込みます。モジュールシステムは、ファイルシステムにパーサ結果を求めます。
  ファイルシステム自体はパース結果をキャッシュしておいても良いでしょうし、再読み込みしても良いでしょうが、要するにメモリ容量との相談になるでしょう。
  重要なのは、モジュールシステム主導で動作するようにする事です。
  モジュールシステムがファイルシステムにアクセスして、パース結果を取得し、パース結果を元に、構文木sからeへの変換処理を行って、解析します。
  解析結果が出たら、解析結果に乗っ取って、インタプリタが評価を開始します。

    type s =
      | Unit
      | Var of string list
      | Mod of string * s
      | Open of string * s
      | Let of string * e * e
      | Int of int
      | Bin of e * op * e


## 12. まとめ

  依存情報を調べるアルゴリズムの為の簡単なデータ構造を考え、実際に作成してみました。

  最初に以下のデータ構造を作って依存関係を出力してみました。

    type e =
      | Unit
      | Open of string * e

  3章から6章で、サブモジュールを使うには、再帰的な読み込みが必要である事を見て、再帰的な読み込みを行うようにしました。
  7章と8章では、結果データを解析しました。
  
  9章ではサブモジュールを追加し、解析可能なようにしました。

    type e =
      | Unit
      | Open of string * e
      | Mod of string * e * e

  10章では変数Varを追加しました。

    type e =
      | Unit
      | Open of string * e
      | Mod of string * e * e
      | Var of string * e * e

  11章では以下の言語sから実際のコンパイラへの組み込み方法について考え、シンプルな実装を作成しました。

    type s =
      | Unit
      | Var of string list
      | Mod of string * s
      | Open of string * s
      | Let of string * e * e
      | Int of int
      | Bin of e * op * e

  OCamlの主な依存関係を効率的に計算する手法について考え実装する事が出来ました。
