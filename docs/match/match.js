// バインディングの変数
function _(e) {
    return new _P(e);
}
// バインディング変数のコンストラクタ
function _P(e) {
    this.e = e;
}

function cmatch() {
    // 例外文字列
    var t = "   throw ('cannot match '+e);\n"

    // これがコンパイル結果
    var str = "var f = function(e) {\n";

    // 引き数だけコンパイル
    for(var i = 0; i < arguments.length; i++) {
        // 中味取り出し
        var a = arguments[i];
        // 1つめがパターン
        var m = a[0];
        // 2つめが処理する関数
        var f = a[1];
        // mが_なら必ずリターン
        if (m == _) {
            str += "    return ("+f.toString()+"(e));\n"
            t = "";
            break;
        }

        // envsとcondsにmat関数の結果を入れる
        var envs = [];
        var conds = [];
        function mat(m, e) {
            switch (typeof m) {
            case "object":
                // パターン変数なら名前と、eを返す。
                if (m instanceof _P) {
                    envs.push(m.e+":" + e);
                    return;
                }
                // ただのオブジェクトならオブジェクトチェックと
                conds.push("typeof e == 'object'");
                // １こ１この条件を条件に加え
                for(var i in m) {
                    conds.push('"'+i+'" in '+e+'');
                }
                // 更に、再帰的に検索する
                for(var i in m) {
                    mat(m[i], e+"."+i);
                }
                return;

            // 通常データなら、比較条件とする。
            case "string":
            case "number":
                conds.push(e+" == "+JSON.stringify(m));
                return;
            }
        }
        // mについてマッチさせる
        mat(m, "e");

        // 条件があれば条件を出力
        if(conds.length > 0) {
            str += "    if ("+conds.join(" && ")+") {\n";
        }
        // 引数を出力
        str += "        return ("+f+"({"+envs.join(",")+"}));\n";
        if(conds.length > 0) {
            str += "    }\n"
        }
    }
    // 最後に例外と
    str += t;
    // 閉じる
    str += "};\n"
    console.log(str);
    // 実行
    eval(str);
    return f;
}

var evl2 = cmatch(
    [{op:"+",left:_("l"),right:_("r")}, function(e) {
        return evl2(e.l) + evl2(e.r);
    }],
    [{op:"*",left:_("l"),right:_("r")}, function(e) {
        return evl2(e.l) * evl2(e.r);
    }],
    [{op:"a",dt:{op:"b",dt:_("dt")}}, function(e) {
        return evl2(e.dt);
    }],
    [_,function(e){ return e;}]
);
console.log(evl2({op:"+",left:1,right:{op:"*",left:2,right:3}}));
console.log(evl2({op:"a",dt:{op:"b",dt:3}}));

