





install: emacs.c
	cc -fPIC -shared emacs.c -o emacs.so