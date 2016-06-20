POC parsing C include files to generate @extern definitions
===========================================================

Input

A C file with `#define` and `#include`

cat stdio.c

```C
#define  __USE_GNU

#include <stdio.h>

```

cpp stdio.c | java -jar ./target/scala-2.11/c-include_file-parser-assembly-0.1.jar


Current state
-------------

Not working !!
