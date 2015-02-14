
function print_int(n) {
	console._stdout.write(""+n);
}
var print_string = print_int;
function ref(n) {
	return {ref:n};
}
var abs_float = Math.abs;
var sqrt = Math.sqrt;
var sin = Math.sin;
var cos = Math.cos;
var int_of_float = Math.floor;
function truncate(a) {
	return a >= 0 ? Math.floor(a) : -Math.floor(-a);
}
function float_of_int(a){
	return a+0.0;
}
function print_newline(){
	console.log("");
}

String.sub = function(str,i,len) {
	return str.substring(i,i+len);
};
String.length_ = function(str) {
	return str.length;
};
String.concat = function(sep, ls) {
	if(ls.tag=="Nil") return "";
	if(ls.data._1.tag=="Nil") return ls.data._0;
	return ls.data._0 + sep + String.concat(sep, ls.data._1)
}
Array.create = function (n,v) {
	var a = [];
	for(var i = 0; i < n; i++) a[i] = v;
	return a;
};