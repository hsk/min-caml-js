# パターンマッチの変換

```
match 1 with 1 -> 1 | _ -> 2
```

を以下のように変換する事を考えます。

```
(function(e) {
  if (e == 1) { return 1; }
  return 2;
}(1))
```


```
var evl2 = cmatch(
    [["Add",_("l"),_("r")], function(e) {
        return evl2(e[0]) + evl2(e[1]);
    }],
    [["Mul",_("l"),_("r")], function(e) {
        return evl2(e[0]) * evl2(e[1]);
    }],
    [_,function(e){ return e;}]
);
console.log(evl2( match (Add(1{op:"+",left:1,right:{op:"*",left:2,right:3}}));
```

を以下のように変換すると良い。

```
var f = function(e) {
    if (typeof e == 'object' && "op" in e && "left" in e && "right" in e && e.op == "Add") {
        return (function (e) {
        return evl2(e.l) + evl2(e.r);
    }({l:e.left,r:e.right}));
    }
    if (typeof e == 'object' && "op" in e && "left" in e && "right" in e && e.op == "Mul") {
        return (function (e) {
        return evl2(e.l) * evl2(e.r);
    }({l:e.left,r:e.right}));
    }
    return (function (e){ return e;}(e));
};
```


## 参考URL

- http://qiita.com/h_sakurai/items/9ee6285eb192643f51e5
