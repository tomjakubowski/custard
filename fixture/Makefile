test: test.c foo.h libfoo.a
	gcc -std=c99 -Wall -Werror -L . test.c -lfoo -o test

foo.h: foo.rs ../target/ironic
	../target/ironic foo.rs > foo.h

libfoo.a: foo.rs
	rustc foo.rs
