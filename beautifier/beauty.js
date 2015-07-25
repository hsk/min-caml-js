exports.parse = function(i){
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
  var str = function(s) { return p(skip, nstr(s));};


  Function.prototype.or=function(that) {
    var thiz = this;
    return function(i) {
      var e = thiz(i); if(e === null) return that(i);
      return e;
    };
  };

  function p() {
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

  Function.prototype.next=function(that) {
    var thiz = this;
    return function(i) {
      var r = p(thiz,that)(i); if(r === null) return r;
      return [r[0][1], r[1]];
    };
  };

  Function.prototype.prev=function(that) {
    var thiz = this;
    return function(i) {
      var r = p(thiz,that)(i); if(r === null) return r;
      return [r[0][0], r[1]];
    };
  };

  Function.prototype.action=function(f) {
    var thiz = this;
    return function(i) {
      var r = thiz(i); if(r === null) return r;
      return [f(r[0]), r[1]];
    };
  };

  function opt (thiz) {

    if(typeof(thiz)=="string")thiz=str(thiz);
    return function(i) {
      var r = thiz(i); if(r === null) return [null, i];
      return r;
    };
  }

  function rep (thiz) {
    return function (i) {
      var rs = [];
      while(true){
        var r = thiz(i); if (r === null) return [rs, i];
        rs.push(r[0]);
        i = r[1];
      }
    };
  }

  function rep1 (thiz) {
    return rep(thiz).action(function(p){
      if(p.length < 1) return null;
      return p;
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
      if (m === null) return null;
      return [m[0],i.substring(m[0].length)];
    };
  }

  var skip = nreg(/^(\s|\(\*[\s\S]*\*\))*/);

  var reg = function(s) { return p(skip, nreg(s));};

  function NestP(){this.p=true;}
  function NestM(){this.m=true;}


  function n() {
    return p.apply(this,arguments).action(function(a){
      return [new NestP(),a,new NestM()];
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
    for (var i in e3) {
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
  var e = exp(i);
    //console.log(JSON.stringify(e));
  return cnv(flat(e));
};
