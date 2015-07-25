default: mincaml2js test
mincaml2js: mincaml2js.scala
	scalac mincaml2js.scala
test:
	time scala mincaml2js.test
js:
	time node test.js
py:
	time python test.py
clean:
	rm -rf mincaml2js
