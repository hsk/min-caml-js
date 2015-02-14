var A = (function(){
	function f(a) {
		console.log(a);
	}

	function b(a) {
		f(a);
	}
	return {f:f,b:b};
}());

A.b("test b");
A.f("test f");

with (A) {

	b("test with b");
	f("test with f");
}
