# mincaml beautifier

これらは、mincamlのソースをパーサコンビネータを使ってネストを奇麗にするプログラム群です。

Scala,JavaScript,Pythonによる実装があります。

パーサコンビネータを作成して、n()でネストしたい文法を括れば、あとはうまい事やってくれます。

文法定義は以下のように非常にシンプルです。

処理速度は、ScalaはJVMの起動が遅い分遅く、JavaScriptが起動が速くて、処理速度も悪くない。Pythonは起動が速いけど処理速度が遅いという感じです。
ただ、文法的にJavaScriptは演算子のオーバーロードが出来ないので分かり辛く、Pythonは演算子の数が少ない。Scalaはより分かりやすいでしょう。

## Scala

    object parse extends PrityPrintParser {

      override protected val whiteSpace = """(?s)(\s|\(\*.*\*\))+""".r

      def keywords  = ( """(let|in|if|else|then|rec|begin|end|match|with|try)\b""".r )
      def id        = ( not(keywords) ~> """[_a-zA-Z0-9]+""".r ).
                    | ( """[+\-*/.<>=:@]+""".r ).
                    | ( """[,!]""".r ).
                    | ( """("(\\.|[^"])*")""".r )
      def exp:Parser[Any]
                    = ( exps ~ rep(";" ~ exps) )
      def exps      = ( rep1(exp1) )
      val exp1      = ( "begin" ~ n(exp) ~ "end" ).
                    | ( ("match" | "try") ~ n(exp) ~ "with" ~ opt("|") ~ n{exp} ~ rep("|" ~ n{exp}) ).
                    | ( "(" ~ n{opt(exp)} ~ ")" ).
                    | ( "{" ~ n{opt(exp)} ~ "}" ).
                    | ( "[" ~ n{opt(exp)} ~ "]" ).
                    | ( "let" ~ n(opt("rec") ~ exp) ~ "in" ~ exp ).
                    | ( "type" ~ n(id ~ "=" ~ exp) ~ ";;" ~ exp ).
                    | ( "type" ~ n{id ~ "=" ~ n(opt("|") ~ exp) ~ rep("|" ~ n(exp))} ~ ";;" ).
                    | ( "if" ~ n{exp} ~ "then" ~ n{exp} ~ "else" ~ exp ).
                    | ( id )

      def apply(str: String):String = apply(exp,str)
    }

## JavaScript

    var keywords  = reg(/^(let|in|if|else|then|rec|begin|end|match|with)\b/);

    var id        = (notp(keywords).next(reg(/^[_a-zA-Z0-9]+/)))
                  .or(reg(/^[+\-*\/.<>=:@]+/))
                  .or(reg(/^[,!]/))
                  .or(reg(/^("(\\.|[^"])*")/));
    var exp1      = p("begin", n(exp), "end")
                  .or(p(p("match").or(p("try")), n(exp), "with", opt("|"), n(exp), rep(p("|", n(exp)))))
                  .or(p("(", n(opt(exp)), ")"))
                  .or(p("{", n(opt(exp)), "}"))
                  .or(p("[", n(opt(exp)), "]"))
                  .or(p("let", n(p(opt("rec"), exp)), "in", exp))
                  .or(p("type", n(p(id, "=", exp)), ";;", exp))
                  .or(p("type", n(id, "=", opt("|"), n(exp), rep(p("|", n(exp)))), ";;"))
                  .or(p("if", n(exp), "then", n(exp), "else", exp))
                  .or(id);
    var exps      = rep1(exp1);
    function exp(i) {
      return p(exps, rep(p(";", exps)))(i);
    }

## Python

    keywords = reg(r"^(let|in|if|else|then|rec|begin|end|match|with|type)\b")

    id = notp(keywords) >> reg(r"^[_a-zA-Z0-9]+") \
        | reg(r'^[+\-*\/.<>:@=][+\-*\/.<>:@=]*') \
        | reg(r'^[,!]') \
        | reg(r'^("(\\.|[^"])*")')

    exp = p(lambda i: (exps + -p(notp(";;") >> p(";"), exps))(i))

    exp1 = p("begin", n(exp), "end") \
        | p("(", n(~exp), ")") \
        | p("{", n(~exp), "}") \
        | p("[", n(~exp), "]") \
        | p("if", n(exp), "then", n(exp), "else", exp) \
        | p("let", n(~p("rec"), exp), "in", exp) \
        | p(p("match") | p("try"), n(exp), "with", ~p("|"), n(exp), -p("|", n(exp))) \
        | p("function", ~p("|"), n(exp), -p("|", n(exp))) \
        | p("type", n(id, "=", ~p("|"), n(exp), -p("|", n(exp))), ~p(";;")) \
        | p("type", n(id, "=", exp), ~p(";;")) \
        | p("open", n(id, -p(".", id)), ~p(";;")) \
        | id
