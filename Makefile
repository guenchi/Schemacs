





install: schemacs.c
	cc -fPIC -shared schemacs.c  -o schemacs.so