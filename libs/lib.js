
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
function int_of_char(a){
	return a;
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

String.uncapitalize = function(str) {
	return str.charAt(0).toLowerCase() + str.slice(1);
}
String.capitalize = function(str) {
	return str.charAt(0).toUpperCase() + str.slice(1);
}

String.get = function(str,pos) {
	return str.charCodeAt(pos);
};

Array.create = function (n,v) {
	var a = [];
	for(var i = 0; i < n; i++) a[i] = v;
	return a;
};

var Format = {
	fprintf: function(fp,s) {
		s = s.replace(/@\./g,"\n").replace(/@?/g,"");
		var n = 2;
		var arg = arguments;
		return fp(s.replace(/%(.)/g, function(a, b) {
			switch (b) {
				case "a":
					var f = arg[n++];
					return f(function(e){return e;},arg[n++]);
				case "f":
					return arg[n++].toFixed(6);
				default: return arg[n++];
			}

		}));
	},
	sprintf: function(s) {
		var args = Array.prototype.slice.call(arguments);
	    args.unshift(function(i){return i;});
		return Format.fprintf.apply(this,args);
	},
	printf: function() {
		var args = Array.prototype.slice.call(arguments);
	    args.unshift(print_string);
		Format.fprintf.apply(this,args);
	},
};

var Printf = {
	sprintf: function(s) {
		var n = 1;
		var arg = arguments;
		return s.replace(/%(.)/g, function(a, b) {
			switch (b) {
				case "f":
					return arg[n++].toFixed(6);
				default: return arg[n++];
			}

		});
	},
	printf: function() {
		print_string(Printf.sprintf.apply(this,arguments));
	},
};
