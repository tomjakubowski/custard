test: test.c foo.h libfoo.a
	gcc -L . test.c -lfoo -o test

foo.h: foo.rs ../target/ironic
	../target/ironic foo.rs > foo.h

libfoo.a: foo.rs
	rustc -g foo.rs
