## note: this works on my system, you don't need to run it because the
## distribution contains the generated file

SBCL=/usr/bin/sbcl

colornames.lisp: /usr/share/X11/rgb.txt parse-x11-colors.lisp
	rm -f colornames.lisp
	$(SBCL) --load parse-x11-colors.lisp --eval '(quit)'
