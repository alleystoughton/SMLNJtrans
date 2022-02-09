smlnjtrans: A Standard ML program for generating SML/NJ transcript in LaTeX
===========================================================================

[SMLNJtrans](https://alleystoughton.us/smlnjtrans/) is a [Standard
ML](https://smlfamily.github.io) program for generating
[SML/NJ](http://smlnj.org) transcripts in
[LaTeX](https://www.latex-project.org). It makes use of the `Unix`
structure of the [Standard ML Basis
Library](https://smlfamily.github.io/Basis/), and so isn't currently
available for Windows.

This program is configured to be compiled with
[SML/NJ](http://smlnj.org).

To compile the program, change directory to `source`, and then run the
build shell `script`.  This will produce an SML/NJ heap image
called

```
  smlnjtrans-heap.ARCH-OPSYS
```

where `ARCH` is your machine architecture and `OPSYS` is your operating
system.  This heap image should be moved to an approprate directory,
e.g., the `bin/.heap` subdirectory of the SML/NJ distribution.

`smlnjtrans` can be invoked using the shell script of the same name
in the source directory.  This script should be edited as is
explained in the script, and should then be moved to an appropriate
`bin` directory (e.g., `/usr/local/bin`).

See the sub-directory `example` for an example of how `smlnjtrans` can be
used.

Feedback can be emailed to alley.stoughton@icloud.com.
