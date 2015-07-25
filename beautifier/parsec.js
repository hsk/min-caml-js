function startsWith(i, param) {
  var ilen = i.length;
  var len = param.length;
  if (len > ilen) return false;
  else return i.substring(0, len) == param;
}

function any_char(i) {
  if(i.length > 0) return [i.substring(0,1), i.substring(1)];
  else return null;
}
var str;

function or (thiz, that) {
  return function(i) {
    var e = thiz(i); if(e === null) return that(i);
    return e;
  };
}
Function.prototype.or=function(that) { return or(this, that); };

function seq() {
  var args = arguments;
  for(var i = 0; i < args.length;i++) {
    if(typeof(args[i])=="string") args[i]=str(args[i]);
  }
  return function(s) {
    var rs = [];
    for(var i = 0; i < args.length; i++) {
      var r = args[i](s);
      if(r === null) return null;
      rs.push(r[0]);
      s = r[1];
    }
    return [rs,s];
  };
}

Function.prototype.seq=function (that) {
  var thiz = this;
  return function(i) {
    var r1 = thiz(i); if(r1 === null) return null;
    var r2 = that(r1[1]); if(r2 === null) return null;
    return [[r1[0], r2[0]], r2[1]];
  };
};

function next (thiz, that) {
  return function(i) {
    var r = thiz.seq(that)(i); if(r === null) return r;
    //console.log("next1111 "+JSON.stringify(r));
    return [r[0][1], r[1]];
  };
}
Function.prototype.next=function(that) { return next(this, that); };

function prev (thiz, that) {
  return function(i) {
    var r = thiz.seq(that)(i); if(r === null) return r;
    return [r[0][0], r[1]];
  };
}
Function.prototype.prev=function(that) { return prev(this, that); };

function action (thiz,f) {
  return function(i) {
    var r = thiz(i); if(r === null) return r;
    return [f(r[0]), r[1]];
  };
}
Function.prototype.action=function(f) { return action(this, f); };

function opt (thiz) {

  if(typeof(thiz)=="string")thiz=str(thiz);
  return function(i) {
    var r = thiz(i); if(r === null) return [null, i];
    return r;
  };
}


function rep (thiz) {
  function parse (i) {
    //console.log("rep "+i)
    var r = thiz(i); if (r === null) return [[], i];
    var r2 = parse(r[1]); if (r2 === null) return [[r[0]],r[1]];
    r2 = r2.concat([]);

    r2[0].unshift(r[0]);
    //console.log("rep "+JSON.stringify(r2));
    return r2;
  }
  return parse;
}

function rep1 (thiz) {
  return thiz.seq(rep(thiz)).action(function (p) {
    var p1 = p[0];
    var p2 = p[1];
    p2 = p2.concat([]);
    p2.unshift(p1);
    //console.log("rep1 "+JSON.stringify(p2));
    return p2;
  });
}

function notp (thiz) {
  return function(i) {
    var r = thiz(i); if(r === null) return ["", i];
    return null;
  };
}

function nstr (param) {
  return function(i) {
    //console.log("nstr "+param+" "+i)
    if(!startsWith (i, param)) return null;
    //console.log("nstr ok");
    return [param, i.substring(param.length)];
  };
}

function range(c1, c2) {
  return function(i) {
    if(i.length <= 0) return null;
    var c = i.charCodeAt(0); if (c1.charCodeAt(0) <= c && c <= c2.charCodeAt(0))
      return [i.substring(0, 1), i.substring(1)];
    return null;
  };
}

function nreg(param) {
  return function(i) {
    var m = i.match(param);
    if(m === null) return null;
    return [m[0],i.substring(m[0].length)];
  };
}
var skip = nreg(/^[ \r\n\t]*/);

function reg (param) {
  return skip.next(nreg(param));
}



function sample() {

  var comment = nstr("/*").next(rep(notp(nstr("*/")).seq(any_char))).next(nstr("*/"));

  var skip =
    rep(nstr(" ").or(nstr("\r")).or(nstr("\n")).or(nstr("\t")).or(comment));

  function str(param) {
    return skip.next(nstr (param));
  }

  var zero = str("0").action(function(_){return 0;});
  var nonzero = range('1', '9').action(function(a){
    return parseInt(a);
  });
  var digit = range('0', '9').action(function(a){
    return parseInt(a);
  });

  var no =
    nonzero.seq(rep(digit)).action(function(res) {
      var l = res[0];
      var ls = res[1];
      for(var i = 0; i < ls.length; i++) l = l * 10 + ls[i];
      return l;
    });

  var int_ = skip.next(no.or(zero));

  var fact = int_.action(function(e){ return ["Int",e]; })
    .or(str ("(").next(function(i){return exp (i);}).prev(str(")")) );

  var term =
    seq(fact, rep(seq(str("*"),fact)) ).action(
    function (res) {
      var t = res[0];
      var ts = res[1];
      for(var i = 0; i < ts.length; i++) {
        t = ["Bin", t, ts[i][0], ts[i][1]];
      }
      return t;
    });

  function exp(i) {

    return term.seq(rep(str("+").seq(term))).action(
    function (res) {
      var t = res[0];
      var ts = res[1];
      for(var i = 0; i < ts.length; i++) {
        t = ["Bin", t, ts[i][0], ts[i][1]];
      }
      return t;
    })(i);
  }

  return exp;
}



function parse(i){
  function NestP(){this.p=true;}
  function NestM(){this.m=true;}


  function n(a,b,c) {
    return seq(a,b,c).action(function(a){
      return [a[0],new NestP(),a[1],new NestM(),a[2]];
    });
  }

  function flat(a) {
    if (a === null) return [];

    if(a instanceof Array) {
      var rc = [];
      for(var i in a) {
        rc = rc.concat(flat(a[i]));
      }
      return rc;
    }
    return [a];
  }

  function cnv(e){
    //console.log(JSON.stringify(e));
    var reg2 = /(^.*\n.*$)/;
    var whiteSpace = /(^\s*$)/;
    var nest = 0;
    var e2 = [];
    for(var i in e) {
      var s = e[i];
      if(typeof(s)=="string"){
        var m = s.match(/(^.*\n)(\s+$)/); if(m) s = m[1];
      }
      e2.push(s);
    }
    //console.log(JSON.stringify(e2));

    var e3 = [];
    for(i = 0; i < e2.length; i++) {
      var s = e2[i];
      if(typeof(s)=="string"){
        var m = s.match(reg2); if(m) {
          e = [];
          for(;i < e2.length; i++) {
            var s2 = e2[i];
            if(s2 instanceof NestM) {
              e3.push(s2);
              continue;
            }
            if(s2.match(whiteSpace)) {e.push(s2); continue; }
            // whitespace以外だ。
            e.push(s2);break;
          }
          //console.log("k "+JSON.stringify(e));

          e3 = e3.concat(e);
          continue;
        }
      }
      e3.push(s);
    }
    //console.log("kore "+JSON.stringify(e3));

    function makeString(s,n) {
      var rs = [];
      for(var i = 0; i < n; i++) {
        rs.push(s);
      }
      return rs.join("");
    }

    e2 = [];
    for(var i in e3) {
      var s = e3[i];

      if(s instanceof NestP) {nest++; continue; }
      if(s instanceof NestM) {nest--; continue; }
      var m = s.match(reg2); if(m) {
          //console.log("add nest "+nest);
          e2.push(m[0].replace(/\n/g, "\n"+makeString("  ",nest) ) );
          continue;
      }
      e2.push(s);
    }
    //console.log(JSON.stringify(e2));
    return e2.join("");
  }

  //skip = nreg(/^(\s|\(\*[\s\S]*\*\))+/);
  var keywords  = reg(/^(let|in|if|else|then|rec|begin|end|match|with)\b/);

  reg = function(s) { return skip.seq(nreg(s));};
  str = function(s) { return skip.seq(nstr(s));};

  var id        = (notp(keywords).next( reg(/^[_a-zA-Z0-9]+/)))
                .or( reg(/^[+\-*\/.<>=:@]+/) )
                .or( reg(/^[,!]/) )
                .or( reg(/^("(\\.|[^"])*")/) );
  var exp1      = n("begin", exp, "end")
  
                .or(seq("match", n("",exp,""), "with", opt("|"), n("",exp,""), rep(seq("|", n("",exp,""))) )  )
                .or( n("(", opt(exp), ")") )
                .or( n("{", opt(exp), "}") )
                .or( n("[", opt(exp), "]") )
                .or( seq(n("let", seq(opt("rec"), exp), "in"), exp) )
                .or( seq(n("type", seq(id, "=", exp), ";;"), exp) )
                .or( n("type", seq(id, "=", n(opt("|"), exp, ""), rep(n("|", exp, ""))), ";;") )
                .or( seq(n("if", exp, ""), n("then", exp, "else"), exp) )
                .or(id);
  var exps      = rep1(exp1);
  function exp(i) {
    return exps.seq(rep(str(";").next(exps)))(i);
  }
  var e = exp(i);
    //console.log(JSON.stringify(e));
  return cnv(flat(e));
}

console.log(JSON.stringify(sample()("11 + 2")));

console.log(parse("begin\n1 + 2\nend"));


console.log(parse(
  "let a = 1 in\n"+
  "if a then b else c\n"+
  ""));

console.log(parse(
  "let a = 1 in\n"+
  "if a\n"+
  "then b\n"+
  "else c\n"+
  ""));

console.log(parse(
  "let a = 1 in\n"+
  "if a\n"+
  "then\n"+
  "b\n"+
  "else\n"+
  "c\n"+
  ""));

console.log(parse(
  "let a =\n"+
  "let a = 1 in\n"+
  "if a > 10 \n"+
  "then\n"+
  "b\n"+
  "else\n"+
  "c\n"+
  "in\n"+
  "a + b\n"+
  ""));

console.log(parse(
  "if a > 10 then\n"+
  "b\n"+
  "else if a > 20 then\n"+
  "c\n"+
  "else\n"+
  "d\n"+
  ""));

console.log(parse(
  "if a > 10 then b else\n"+
  "if a > 20 then c else\n"+
  "d\n"+
  ""));