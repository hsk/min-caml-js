/*
これは、パーサコンビネータでネストを奇麗に付け直してくれる物です。
*/
package mincaml2js

import scala.collection.immutable._
import util.parsing.combinator._

case object NestP
case object NestM

class PrityPrintParser extends RegexParsers {

  override protected def handleWhiteSpace(source: java.lang.CharSequence, offset: Int): Int =
    if (skipWhitespace)
      (whiteSpace findPrefixMatchOf (source.subSequence(offset, source.length))) match {
        case Some(matched) => offset + matched.end
        case None => offset
      }
    else
      offset

 implicit def literal2(s: String): Parser[List[String]] = new Parser[List[String]] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      val start2 = source.subSequence(offset,start).toString
      var i = 0
      var j = start
      while (i < s.length && j < source.length && s.charAt(i) == source.charAt(j)) {
        i += 1
        j += 1
      }
      if (i == s.length) {
        val token = source.subSequence(start, j).toString
        Success(List(start2,token), in.drop(j - offset))
      } else  {
        val found = if (start == source.length()) "end of source" else "`"+source.charAt(start)+"'"
        Failure("`"+s+"' expected but "+found+" found", in.drop(start - offset))
      }
    }
  }

  implicit def regex2(r: scala.util.matching.Regex): Parser[List[String]] = new Parser[List[String]] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      val start2 = source.subSequence(offset,start).toString
      (r findPrefixMatchOf (source.subSequence(start, source.length))) match {
        case Some(matched) =>
          val token = source.subSequence(start, start + matched.end).toString
          Success(List(start2,token),
                  in.drop(start + matched.end - offset))
        case None =>
          val found = if (start == source.length()) "end of source" else "`"+source.charAt(start)+"'"
          Failure("string matching regex `"+r+"' expected but "+found+" found", in.drop(start - offset))
      }
    }
  }

  def n(p:Parser[Any]):Parser[List[Any]] =
    p ^^ {List(NestP,_,NestM)}
    

  def flat(a:Any):List[Any] = {
    a match {
      case List() => List()
      case Some(a) => flat(a)
      case None => List()
      case x::xs => flat(x):::flat(xs)
      case a~b => flat(a):::flat(b)
      case a => List(a)
    }
  }

  def cnv(e:List[Any]):String = {
    val reg = """(^.*\n)(\s+$)""".r
    val reg2 = """(^.*\n.*$)""".r
    val whiteSpace = """(\s*)""".r
    var nest = 0
    var e2 = e.map{
      case s @ reg(a,b) => a
      case s => s
    }
    def loop:List[Any]=>List[Any] = {
      case List() => List()
      case reg2(s)::xs => 
        // xsからnestmを探す
        val pos = xs.indexWhere{
          case whiteSpace(s) => false
          case _ => true
        }
        xs.splitAt(pos) match {
          case (l1,NestM::l2) => NestM::loop(s::l1:::l2)
          case (l1,l2) => s::l1:::loop(l2)
        }
      case x::xs => x :: loop(xs)

    }

    e2 = loop(e2)

    val e3 = e2.map {
      case NestP => nest+=1; ""
      case NestM => nest-=1; ""
      case reg2(s) => s.replaceAll("""\n""", "\n"+("  "*nest) ) 
      case s => s.toString
    }
    e3.fold(""){case (a,b)=>a+b.toString}
  }

  def apply(exp:Parser[Any], str:String):String = {
    parseAll(exp, str) match {
      case Success(a, s) => cnv(flat(a))
      case e => throw new Exception("" + e)
    }
  }
}

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


import java.io._

object test extends App {

  def readAll(s: String): String = {
    val p = new FileInputStream(s)
    def f(s: String, i: BufferedReader): String = {
      i.readLine() match {
        case null => s
        case a => f(s + a + "\n", i)
      }
    }
    f("", new BufferedReader(new InputStreamReader(p)))
  }

  def tes(s:String) {
    println(parse(s.trim))
    println("---")
  }

  def t2() {
    tes("""
    let a = 1 in
    if a then b else c
    """)
    tes("""
    let a = 1 in
    if a
    then b
    else c
    """)
    tes("""
    let a = 1 in
    if a
    then
    b
    else
    c
    """)
    tes("""
    let a =
      let a = 1 in
      if a > 10 
      then
        b
      else
        c
    in
      a + b
    """)

    tes("""
      if a > 10 then
        b
      else if a > 20 then
        c
      else
        d
    """)


    tes("""
      if a > 10 then b else
      if a > 20 then c else
      d
    """)

    tes("""
      let add (a:int) (b:int):int =
        a + b
      in
      add 1 2
    """)

  }
  t()
  t2()
  def t() {
    val tests = List(
      "print","sum-tail", "gcd", "sum", "fib", "ack", "even-odd",
      "adder", "funcomp", "cls-rec", "cls-bug", "cls-bug2",
      "shuffle", "spill", "spill2", "spill3", "join-stack", "join-stack2", "join-stack3",
      "join-reg", "join-reg2", "non-tail-if", "non-tail-if2",
      "inprod", "inprod-rec", "inprod-loop", "matmul", "matmul-flat",
      "ref", "record", "string", "as", "list1", "match", "begin",
      "variant", "when", "list"
      )

    tests.foreach { f =>
      val path = "../test/" + f + ".ml"
      println("test " + path)
      tes(readAll(path))
    }
  }
/*
  Universe.getMethods[parse.type].toList.reverse.foreach(println)

  println("---")

  Universe.getMethods[parse.type].toList.reverse.filter{a=>
    a.matches("""[\w\d]+: .*(Parser\[|scala.util.matching.Regex).*""")
  }.foreach(println)
*/
}

object Universe {

  implicit class RichList[T](self: List[T]) {
    def mkStringIfNonEmpty[T](start: String,
                              seq: String,
                              end: String): String =
      if (self.nonEmpty) self.mkString(start, seq, end)
      else ""
  }

  import scala.reflect.runtime.universe._

  def paramToString(param: Symbol): String =
    (if (param.isImplicit) "implicit " else "") +
      param.name +
      ": " +
      param.typeSignature

  def methodToString(method: MethodSymbol): String =
    method.name.decodedName +
      method.typeParams.map(
        _.name
      ).mkStringIfNonEmpty("[", "][", "]") +
      method.paramLists.map(
        _.map(paramToString).mkString(", ")
      ).mkStringIfNonEmpty("(", ")(", ")") +
      ": " +
      method.returnType

  def printMethods[T: TypeTag] =
    typeOf[T].
      members.
      filter(_.isMethod).
      map(_.asMethod).
      map(methodToString).
      toArray.
      sorted.
      foreach(println)

  def getMethods[T: TypeTag] =
    typeOf[T].
      members.
      filter(_.isMethod).
      map(_.asMethod).
      map(methodToString)

}
