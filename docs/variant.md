# variant

type e =
| EUnit
| EInt of int
| EAdd of e * e


は
var EUnit = "EUnit";
function EInt(e1) { this.tag = "EInt"; this.e1=e1; }
EInt.prototype.toString = function () { return "EInt("+this.e1+")"; };
function EAdd(e1,e2) { this.tag = "EAdd"; this.e1=e1; this.e2=e2; }
EInt.prototype.toString = function () { return "EAdd("+this.e1+","+this.e2+")"; };
としてしまう。
