README.md: make-readme-markdown.el meson-mode.el
	emacs --script $< < $(word 2,$^) >$@

make-readme-markdown.el:
	wget -q -O $@ https://raw.github.com/mgalgs/make-readme-markdown/master/make-readme-markdown.el
