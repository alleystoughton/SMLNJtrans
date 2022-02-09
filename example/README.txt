In the following, we'll assume that smlnjtrans is on your shell's
search path, and that the main SML/NJ executable is called sml and is
on your search path.

Run the command

  smlnjtrans sml "" foo goo hoo

to generate the files foo.tex, goo.tex, hoo.tex, which are included
by example.tex.

Then run the commands

  latex example.tex
  dvips -Ppdf example.dvi -o example.ps
  ps2pdf example.ps

and view example.pdf.
