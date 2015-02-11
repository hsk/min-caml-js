package mincaml2js

import scala.collection.immutable._
import util.parsing.combinator._

sealed trait E
case object EUnit extends E
case class EInt(a: Int) extends E
case class EFloat(a: Double) extends E
case class EBin(a: E, op: String, b: E) extends E
case class EPre(op: String, b: E) extends E
case class ELet(a: String, b: E, c: E) extends E
case class ELetRec(a: String, b: E, c: E) extends E
case class EVar(a: String) extends E
case class EBool(a: Boolean) extends E
case class EGet(a: E, b: E) extends E
case class EPut(a: E, b: E, c: E) extends E
case class EIf(a: E, b: E, c: E) extends E
case class ETuple(a: List[E]) extends E
case class EApp(a: E, b: List[E]) extends E
case class EStr(a: String) extends E
case class EFun(ls: List[String], b: E) extends E
case class ERec(ls: List[(String, E)]) extends E
case class ECApp(a: String, b: E) extends E
case object ERaise extends E
case class EMatch(e: E, ls: List[(E, Option[E], E)]) extends E

object parse extends RegexParsers {

  override protected val whiteSpace = """(?s)(\s|\(\*.*\*\))+""".r

  val keywords = List(
    "let", "in", "if", "else", "then", "true", "false",
    "match", "with", "when", "begin", "end", "type", "as","mutable")

  var counter = 0

  def genid(s: String): String = {
    counter += 1
    "__mincaml__" + s + "_" + counter
  }

  def IDENT: Parser[String] =
    """[_a-z][_a-zA-Z0-9]*|([A-Z][_a-zA-Z0-9]*\.)+[_a-z][_a-zA-Z0-9]*""".r ^? {
      case "_" => genid("")
      case a if (!keywords.contains(a)) => a
    }

  def CIDENT: Parser[String] =
    """[A-Z][_a-zA-Z0-9]*""".r ^? {
      case a if (!keywords.contains(a)) => a
    }

  def simple_exp: Parser[E] =
    """-?([1-9][0-9]*\.|0\.)[0-9]*""".r ^^ { a => EFloat(a.toFloat) } |
    """-?([1-9][0-9]*|0)""".r ^^ { a => EInt(a.toInt) } |
    """true""".r ^^ { a => EBool(true) } |
    """false""".r ^^ { a => EBool(false) } |
    "\"([^\"\\\\]|\\\\.)*\"".r ^^ { a => EStr(a.substring(1,a.length-1)) } |
    "(" ~> ")" ^^ { a => EUnit } |
    "[" ~> "]" ^^ { a => ECApp("Nil", EUnit) } |
    ("[" ~> let) ~ (rep(";" ~> let) <~ "]") ^^ {
      case a ~ b => (a :: b).foldRight(ECApp("Nil", EUnit)) {
        case (a, b) =>
          ECApp("Cons", ETuple(List(a, b)))
      }
    } |
    "{" ~> fields <~ "}" ^^ { case a => ERec(a) } |
    IDENT ^^ {
      case a =>
        EVar(a)
    } |
    "begin" ~> exp <~ "end" |
    "!" ~> simple_exp ^^ { a => EBin(a,".", EVar("ref")) } |
    "(" ~> exp <~ ")"

  def exp: Parser[E] =
    let ~ rep(";" ~> let) ^^ {
      case a ~ b => b.foldLeft(a) {
        case (a, b) => ELet(genid(""), a, b)
      }
    }


  def field = (IDENT <~ "=") ~ let ^^  {case a~b => (a,b) }
  def fields = field ~ rep(";" ~> field) ^^ {case a~b => a::b }

  def let: Parser[E] =
    ("let" ~> "rec" ~> IDENT) ~ (("(" ~> ")" ^^ {
      a => List() }) | rep(IDENT)) ~ ("=" ~> exp) ~ ("in" ~> exp) ^^ {
      case a ~ r ~ b ~ c => ELetRec(a, EFun(r, b), c)
    } |
    ("let" ~> IDENT) ~ ("=" ~> exp) ~ ("in" ~> exp) ^^ {
      case a ~ b ~ c => ELet(a, b, c)
    } |
    ("let" ~> "{" ~> fields <~ "}") ~ ("=" ~> exp) ~ ("in" ~> exp) ^^ {
      case a ~ b ~ c => EMatch(b, List((ERec(a), None, c)))
    } |
    ("let" ~> "(" ~> exp <~ ")") ~ ("=" ~> exp) ~ ("in" ~> exp) ^^ {
      case a ~ b ~ c => EMatch(b, List((a, None, c)))
    } |
    cons

  def cons: Parser[E] =
    _if ~ opt(("::" | "@" | "as") ~ cons) ^^ {
      case a ~ Some("::" ~ b) => ECApp("Cons", ETuple(List(a, b)))
      case a ~ Some("@" ~ b) => EApp(EVar("concat"), List(a, b))
      case a ~ Some("as" ~ b) => EBin(a, "as", b)
      case a ~ None => a
    }

  def _if: Parser[E] =
    ("if" ~> exp) ~ ("then" ~> exp) ~ ("else" ~> exp) ^^ {
      case a ~ b ~ c => EIf(a, b, c)
    } |
    ("match" ~> exp <~ "with" <~ opt("|")) ~
      (           exp  ~ opt("when" ~> exp) ~ ("->" ~> exp)) ~
      rep(("|" ~> exp) ~ opt("when" ~> exp) ~ ("->" ~> exp)) ^^ {
      case a ~ b ~ c =>
        EMatch(a, (b :: c).map { case a ~ b ~ c => (a, b, c) })
    } |
    "type" ~> IDENT ~> "=" ~> typ ~> ";;" ~> exp ^^ { a => a } |
    tuple

  def types: Parser[List[String]] =
    IDENT ~ rep("*" ~> IDENT) ^^ { case a ~ b => a :: b }


  def tyrec = opt("mutable")~(IDENT <~ ":") ~ typ ^^ { case a => "" }
  def tyrecs = tyrec ~ rep(";" ~> tyrec) ^^ { case a => "" }

  def typ: Parser[String] =
    opt("|") ~> consts ^^ {case a => ""} |
    types ^^ {case a => ""} |
    "{" ~> tyrecs <~ "}" ^^ { case a => "" }
  def const: Parser[(String,List[String])] =
    CIDENT ~ ("of" ~> types) ^^ { case a ~ b => (a, b) } |
    CIDENT ^^ { a => (a, List()) }

  def consts: Parser[List[(String,List[String])]] =
    const ~ rep("|" ~> const) ^^ { case a ~ b => a :: b }

  def tuple: Parser[E] =
    eq ~ rep("," ~> eq) ^^ {
      case a ~ List() => a
      case a ~ b => ETuple(a :: b)
    }

  def eq: Parser[E] =
    add ~ rep(("=" | ">=" | ">" | "<=" | "<") ~ add) ^^ {
      case a ~ b => b.foldLeft(a) {
        case (a, "=" ~ b) => EBin(a, "==", b)
        case (a, op ~ b) => EBin(a, op, b)
      }
    }

  def add: Parser[E] =
    term ~ rep(("+." | "-." | "+" | "-") ~ term) ^^ {
      case a ~ b => b.foldLeft(a) {
        case (a, "+." ~ b) => EBin(a, "+", b)
        case (a, "-." ~ b) => EBin(a, "-", b)
        case (a, op ~ b) => EBin(a, op, b)
      }
    }

  def term: Parser[E] =
    sub ~ rep(("*." | "/\\.") ~ sub) ^^ {
      case a ~ b => b.foldLeft(a) {
        case (a, "*." ~ b) => EBin(a, "*", b)
        case (a, "/." ~ b) => EBin(a, "/", b)
        case (a, op ~ b) => EBin(a, op, b)
      }
    }

  def sub: Parser[E] =
    "-." ~> app ^^ { a => EPre("-", a) } |
    "-" ~> app ^^ { a => EPre("-", a) } |
    app

  def app: Parser[E] =
    dot ~ rep1(dot) ^^ { case a ~ b => EApp(a, b) } |
    CIDENT ~ exp ^^ { case a ~ b => ECApp(a, b) } |
    CIDENT ^^ { a => ECApp(a, EUnit) } |
    dot

  def dot: Parser[E] =
    simple_exp ~ rep1("." ~> "(" ~> exp <~ ")") ~ opt("<-" ~> let) ^^ {
      case a ~ b ~ Some(c) =>
        b.foldLeft(a) { case (a, b) => EGet(a, b) } match {
          case EGet(a, b) => EPut(a, b, c)
          case _ => throw new Exception("error")
        }
      case a ~ b ~ None => b.foldLeft(a) { case (a, b) => EGet(a, b) }
    } |
    simple_exp ~ rep1("." ~> IDENT) ~ opt("<-" ~> let) ^^ {
      case a ~ b ~ Some(c) =>
        b.foldLeft(a) { case (a, b) => EGet(a, EStr(b)) } match {
          case EGet(a, b) => EPut(a, b, c)
          case _ => throw new Exception("error")
        }
      case a ~ b ~ None => b.foldLeft(a) { case (a, b) => EGet(a, EStr(b)) }
    } |
    simple_exp ~ (":=" ~> let) ^^ {
      case a ~ b => EPut(a, EStr("ref"), b)
    } |
    simple_exp

  def apply(str: String) = {
    parseAll(exp, str) match {
      case Success(tree, _) => tree
      case e => throw new Exception("" + e)
    }
  }
}

import java.io._

object asm {

  var p: PrintStream = System.out

  def open(file: String) {
    p = new PrintStream(new File(file))
  }

  def apply(s: String, n: String = "") {
    p.println("  " + s + (if (n != "") "\t" + n else ""))
  }

  def label(s: String) {
    p.println(s)
  }

  def close() {
    p.close()
    p = System.out
  }
}

object exec {

  def apply(cmd: String): (Int, String, String) = {
    val p = Runtime.getRuntime().exec(cmd)
    val stdin = (readAll(p.getInputStream()))
    val stderr = (readAll(p.getErrorStream()))
    (p.waitFor(), stdin, stderr)
  }

  def readAll(p: InputStream): String = {
    def f(s: String, i: BufferedReader): String = {
      i.readLine() match {
        case null => s
        case a => f(s + a + "\n", i)
      }
    }
    f("", new BufferedReader(new InputStreamReader(p)))
  }

  def readAll(s: String): String = {
    readAll(new FileInputStream(s))
  }
}

object cnv {

  def to_if(e: E): E = {
    e match {
      case EUnit | EVar(_) | EStr(_) | EInt(_) | EFloat(_) | EBool(_) | ERaise => e
      case EPre(op, e1) => EPre(op, to_if(e1))
      case EFun(ss, e) => EFun(ss, to_if(e))
      case ERec(ses) => ERec(ses.map { case (s, e) => (s, to_if(e)) })
      case EApp(e1, e2) => EApp(to_if(e1), e2.map(to_if))
      case EBin(e1, op, e2) => EBin(to_if(e1), op, to_if(e2))
      case ELet(s, e1, e2) => ELet(s, to_if(e1), to_if(e2))
      case ELetRec(s, e1, e2) => ELetRec(s, to_if(e1), to_if(e2))
      case EIf(e1, e2, e3) => EIf(to_if(e1), to_if(e2), to_if(e3))
      case ETuple(es) => to_if(ERec(es.zipWithIndex.map { case (n, i) => ("_" + i, n) }))
      case EGet(e1, e2) => EGet(to_if(e1), to_if(e2))
      case EPut(e1, e2, e3) => EPut(to_if(e1), to_if(e2), to_if(e3))
      case ECApp(e1, e2) => ERec(List(("tag", EStr(e1)), ("data", to_if(e2))))
      case EMatch(e1, ss) =>
        def mat(e: (List[(String, E)], List[E]), me: (E, E)): (List[(String, E)], List[E]) = {
          (e, me) match {
            case ((envs, conds), (EBin(m, "as", EVar(p)), e)) => mat(((p, e) :: envs, conds), (m, e))
            case ((envs, conds), (EVar(p), e)) => ((p, e) :: envs, conds)
            case ((envs, conds), (ERec(ms), e)) =>
              ms.foldLeft(envs, conds) {
                case (env, (i, v)) => mat(env, (v, EBin(e, ".", EVar(i))))
              }
            case ((envs, conds), (m, e)) =>
              (envs, EBin(e, "==", m) :: conds)
          }
        }
        val r = ss.foldRight(ERaise: E) {
          case ((m1, w, f1), str) =>
            val (m, f) = (to_if(m1), to_if(f1))
            val (envs1, conds1) = mat((List(), List()), (m, EVar("_")))
            val envs = envs1.reverse
            val conds = w match {
              case None => conds1
              case Some(e) =>
                EApp(EFun(envs.map(_._1), to_if(e)), envs.map(_._2)) :: conds1
            }
            val ret = EApp(EFun(envs.map { _._1 }, f), envs.map { _._2 })
            if (conds == List()) ret
            else {
              val cond = conds.tail.foldLeft(conds.head) {
                case (tl, e) => EBin(e, "&&", tl)
              }
              EIf(cond, ret, str)
            }
        }
        EApp(EFun(List("_"), r), List(to_if(e1)))
    }
  }

  def show_e(e: E): String = {
    e match {
      case EUnit => "undefined"
      case EVar(s) => s
      case EStr(s) => "\"%s\"".format(s)
      case EInt(s) => "%d".format(s)
      case EFloat(f) => "%f".format(f)
      case EBool(b) => "%b".format(b)
      case EFun(ss, e) =>
        "function(%s){return %s;}".format(ss.mkString(","), show_e(e))
      case ERec(ses) =>
        "{%s}".format(ses.map { case (s, e) => s + ":" + show_e(e) }.mkString(","))
      case EApp(EVar("Array.create"), List(e1, e2)) =>
        "makeArray(%s,%s)".format(show_e(e1), show_e(e2))
      case EApp(e1, e2) =>
        "%s(%s)".format(show_e(e1), e2.map(show_e).mkString(","))
      case EPre(op, e1) =>
        "(%s %s)".format(op, show_e(e1))
      case EBin(e1, op, e2) =>
        "(%s %s %s)".format(show_e(e1), op, show_e(e2))
      case EIf(e1, e2, e3) =>
        "(%s ? %s : %s)".format(show_e(e1), show_e(e2), show_e(e3))
      case ELet(s, e1, e2) =>
        "(function(%s){return %s;}(%s))".format(
          s, show_e(e2), show_e(e1))
      case ELetRec(s, e1, e2) =>
        "(function(){var %s=%s;return %s;}())".format(
          s, show_e(e1), show_e(e2))
      case EGet(e1, e2) =>
        "%s[%s]".format(show_e(e1), show_e(e2))
      case EPut(e1, e2, e3) =>
        "%s[%s] = %s".format(show_e(e1), show_e(e2), show_e(e3))
      case ERaise => "(function(){throw \"error\";}())"
      case EMatch(_, _) | ETuple(_) | ECApp(_, _) =>
        throw new Exception("error")
    }
  }

  def f(oc: PrintStream, ast: E) {

    println("generating javascript...")
    oc.printf("function print_int(n) { console._stdout.write(\"\"+n);}\n")
    oc.printf("var print_string = print_int;\n");
    oc.printf("function makeArray(n,v) { var a = []; for(var i = 0; i < n; i++) a[i] = v; return a; }\n")
    oc.printf("function ref(n) { return {ref:n}; }\n")
    oc.printf("var abs_float = Math.abs;\n")
    oc.printf("var sqrt = Math.sqrt;\n")
    oc.printf("var sin = Math.sin;\n")
    oc.printf("var cos = Math.cos;\n")
    oc.printf("var int_of_float = Math.floor;\n")
    oc.printf("function truncate(a) { return a >= 0 ? Math.floor(a) : -Math.floor(-a); }\n")
    oc.printf("function float_of_int(a){return a+0.0;}\n")
    oc.printf("function print_newline(){console.log(\"\");}\n")
    oc.printf("%s\n", show_e(to_if(ast)))
  }

  def apply(path: String) {
    val e = parse(exec.readAll(path + ".ml"))
    //println(e)
    asm.open(path + ".js")
    cnv.f(asm.p, e)
    asm.close()
  }
}

object main extends App {

  cnv(args(0))
}

object test extends App {

  val tests = List(
    "ref", "record", "string", "as", "list1", "match", "begin", "print", "sum-tail", "gcd", "sum", "fib", "ack", "even-odd",
    "adder", "funcomp", "cls-rec", "cls-bug", "cls-bug2",
    "shuffle", "spill", "spill2", "spill3", "join-stack", "join-stack2", "join-stack3",
    "join-reg", "join-reg2", "non-tail-if", "non-tail-if2",
    "inprod", "inprod-rec", "inprod-loop", "matmul", "matmul-flat", "variant", "when", "list")

  tests.foreach { f =>
    val path = "../test/" + f
    println("test " + path)
    cnv(path)
    println("node run")
    val a = exec("node " + path + ".js")
    print(a)
    println("ocaml run")
    val b = exec("ocaml " + path + ".ml") match { case (a, b, c) => (0, b, "") }
    print(b)
    assert(a == b)
  }
}
