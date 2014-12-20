#include "foo.h"

#include <stdio.h>

int main() {
    struct BigPoint p;
    p.x = 0;
    p.y = 123;
    p.z = 666;
    print_bigpt(p);
}
