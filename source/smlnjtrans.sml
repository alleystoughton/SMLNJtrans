(* smlnj-trans.sml *)

(* A Standard ML program for generating SML/NJ transcripts in LaTeX

   Copyright (C) 2003, 2005, 2013 Alley Stoughton 

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA. *)

(* This program should be invoked with arguments

     SMLNJ PROMPTS FILE...

   where SMLNJ is an SML/NJ executable, PROMPTS is a (possibly empty)
   string of prompt characters, and FILE1 ... FILEn are existing files
   of SML code (optionally annotated with overlay counters -- see
   below).

   SMLNJ need not not be fully qualified; if necessary, it is fully
   qualified using the value of the user's PATH environment variable.
   SMLNJ should be a shell script that invokes an SML/NJ heap image.

   The characters of PROMPTS, which must be printable, non-white space
   characters, plus the characters '-' and '=', must be SMLNJ's prompt
   characters.  Whenever SMLNJ, or the SML code in the files, prompts
   for input, it must do so by printing, at the beginning of a line,
   one of the prompt characters followed by a single blank.
   Furthermore, SMLNJ, or the SML code in the files, must never begin
   a line with a prompt character followed by a blank, when not
   prompting for input.

   smlnj-trans runs SMLNJ as a child process.  It discards the output
   from SMLNJ until it receives SMLNJ's first prompt.  Then,
   smlnj-trans enters its main loop, in which it processes FILE1, ...,
   FILEn, in order.

   When it processes file, FILE, smlnj-trans produces a LaTeX
   transcript, FILE.tex, of the result of feeding the lines of FILE to
   SMLNJ.  The transcript is indented to \leftmargini, and is typeset
   at size \small, in typewriter font.  The text of FILE is set in
   upright shape, whereas the output of SMLNJ is set in slanted shape.
   The transcript may be included in a larger document with the
   command \input{FILE.tex}.

   If file FILE is annotated with an overlay counter, and so has the
   form FIL:CTR where CTR is a numeral, then the LaTeX generated for
   FILE will use the Seminar document class's cumulative overlays
   feature to single-step through the evaluation of FIL.  The overlay
   corresponding to the first input prompt will be CTR. *)

structure Main =
struct

(* val numToInt : string -> int option

   numToInt x returns NONE if x isn't a valid numeral (starting with
   "-" or "~", if negative); otherwise it returns SOME n, where n
   is the integer corresponding to x. *)

fun numToInt x =
      let val x = Substring.full x
      in if case Substring.getc x of
                 NONE       => false
               | SOME(c, _) => Char.isDigit c orelse c = #"-" orelse c = #"~"
         then (case Int.scan StringCvt.DEC Substring.getc x of
                    NONE       => NONE
                  | SOME(n, y) =>
                      if Substring.isEmpty y
                      then SOME n
                      else NONE)
                handle _ => NONE
         else NONE
      end

(* val error : string * string -> 'a *)

fun error(prog, s) =
      (TextIO.output(TextIO.stdErr, prog ^ ": " ^ s ^ "\n");
       OS.Process.exit OS.Process.failure)

(* val usage : string -> 'a *)

fun usage prog = error(prog, "usage: " ^ prog ^ " SMLNJ PROMPTS FILE...")

(* val getPath : unit -> string list *)

fun getPath() =
      case OS.Process.getEnv "PATH" of
           NONE   => nil
         | SOME s => String.fields (fn c => c = #":") s

(* val existsFile : string -> bool *)

fun existsFile s = OS.FileSys.access(s, nil)

(* val fullyQualifyExec : string * string -> string *)

fun fullyQualifyExec(prog, exec) =
      if String.isPrefix "/" exec
      then exec
      else let val path = getPath()

               (* err : unit -> 'a *)

               fun err() =
                     error(prog,
                           "cannot find smlnj executable: \"" ^ exec ^ "\"")

               (* qual : string list -> string *)

               fun qual nil           = err()
                 | qual (dir :: dirs) =
                     let val dirExec = dir ^ "/" ^ exec
                     in if existsFile dirExec then dirExec else qual dirs end
           in qual path end

(* val getPrompts : string * string -> char list *)

fun getPrompts(prog, prompts) =
      let val cs = explode prompts
      in if List.all (fn c => Char.isPrint c andalso not(Char.isSpace c))
                     cs
         then cs @ [#"-", #"="]
         else error(prog,
                    "bad prompt characters: \"" ^
                    String.toString prompts ^ "\"")
      end

(* val memb : ''a * ''a list -> bool *)

fun memb(_, nil)     = false
  | memb(x, y :: ys) = x = y orelse memb(x, ys)

(* val preamble : string

   LaTeX code to make size \small, indent to \leftmargini,
   go into typewriter font, and make LaTeX forget the special
   significance of some characters. *)

val preamble =
       "\\begin{list}{}\n\
       \{\\setlength{\\leftmargin}{\\leftmargini}\n\
       \\\setlength{\\rightmargin}{0cm}\n\
       \\\setlength{\\itemindent}{0cm}\n\
       \\\setlength{\\listparindent}{0cm}\n\
       \\\setlength{\\itemsep}{0cm}\n\
       \\\setlength{\\parsep}{0cm}\n\
       \\\setlength{\\labelsep}{0cm}\n\
       \\\setlength{\\labelwidth}{0cm}\n\
       \\\catcode`\\#=12\n\
       \\\catcode`\\$=12\n\
       \\\catcode`\\%=12\n\
       \\\catcode`\\^=12\n\
       \\\catcode`\\_=12\n\
       \\\catcode`\\.=12\n\
       \\\catcode`\\?=12\n\
       \\\catcode`\\!=12\n\
       \\\catcode`\\&=12\n\
       \\\ttfamily}\n\
       \\\small\n"

(* val postlude : string

   LaTeX code to go back to normal. *)

val postlude = "\\end{list}\n"

(* val trans : char -> string

   Translate a character into LaTeX. *)

fun trans #"\\" = "\\symbol{'134}"
  | trans #"{" = "\\symbol{'173}"
  | trans #"}" = "\\symbol{'175}"
  | trans #" " = "\\ "
  | trans #"~" = "\\symbol{'176}"
  | trans #"[" = "\\symbol{'133}"
  | trans #"]" = "\\symbol{'135}"
  | trans #"*" = "\\symbol{'052}"  (* unclear why needed *)
  | trans c    = if Char.isPrint c then str c else "<bad char>"

(* val transOutput : TextIO.outstream * string -> unit

   Translate a string into LaTeX, and output the result to a stream. *)

fun transOutput(stm, s) =
      let fun outp nil       = ()
            | outp (c :: cs) = (TextIO.output(stm, trans c); outp cs)
      in outp(explode s) end

(* val slantedTransOutput : TextIO.outstream * string -> unit

   Translate a string into LaTeX, giving it slanted shape, and output
   the result to a stream. *)

fun slantedTransOutput(stm, s) =
      (TextIO.output(stm, "\\textsl{");
       transOutput(stm, s);
       TextIO.output(stm, "}"))

(* val ignoreSMLNJOutputUntilPrompt : char list * TextIO.instream -> char

   Ignore SML/NJ process's output until it issues its first prompt,
   which is returned. *)

fun ignoreSMLNJOutputUntilPrompt(prompts, smlnjInStm) =
      let fun lineBegin() =
                let val c = valOf(TextIO.input1 smlnjInStm)
                in if memb(c, prompts)
                   then if valOf(TextIO.input1 smlnjInStm) = #" "
                        then c
                        else lineMiddle()
                   else lineMiddle()
                end

          and lineMiddle() =
                if valOf(TextIO.input1 smlnjInStm) = #"\n"
                then lineBegin()
                else lineMiddle()
      in lineBegin() end

(* val processSMLNJOutputUntilPrompt :
         char list * TextIO.instream * TextIO.outstream -> char

   Handle SMLNJ process's output until it issues its next prompt, which
   is returned. *)

fun processSMLNJOutputUntilPrompt(prompts, smlnjInStm, fileOutStm) =
      let fun lineBegin() =
                let val c = valOf(TextIO.input1 smlnjInStm)
                in if c = #"\n"
                     then (TextIO.output(fileOutStm, "\\item[]\n");
                           lineBegin())
                   else if memb(c, prompts)
                     then let val c' = valOf(TextIO.input1 smlnjInStm)
                          in if c' = #" "
                             then c
                             else lineMiddle(str c ^ str c')
                          end
                   else lineMiddle(str c)
                end

          and lineMiddle s =
                let val c = valOf(TextIO.input1 smlnjInStm)
                in if c = #"\n"
                   then (TextIO.output(fileOutStm, "\\item[]");
                         slantedTransOutput(fileOutStm, s);
                         TextIO.output(fileOutStm, "\n");
                         lineBegin())
                   else lineMiddle(s ^ str c)
                end
      in lineBegin() end

(* val openInAndOutFiles :
         string * string -> TextIO.instream * TextIO.outstream

   Open a file and its transcript file. *)

fun openInAndOutFiles(prog, inFile) =
      let val fileInStm   =
                TextIO.openIn inFile
                  handle _ =>
                           error(prog,
                                 "unable to open file: \"" ^ inFile ^ "\"")
          val outFile     = inFile ^ ".tex"
          val fileOutStm  =
                TextIO.openOut outFile
                  handle _ =>
                           error(prog,
                                 "unable to open file: \"" ^ outFile ^ "\"")
      in (fileInStm, fileOutStm) end

(* val parseFilename : string -> (string, int option)option

   parseFilename x parses an annotated filename;

   if x doesn't include a ":", then parseFilename returns SOME(x, NONE);

   if x has the form "fil:num", where num is a valid numeral, then
   parseFilename returns SOME(fil, SOME n), where n is the integer
   corresponding to num;

   otherwise, parseFilename returns NONE. *)

fun parseFilename x =
      case String.fields (fn c => c = #":") x of
           [fil]      => SOME(fil, NONE)
         | [fil, num] =>
             (case numToInt num of
                   NONE   => NONE
                 | SOME n => SOME(fil, SOME n))
         | _          => NONE

(* val processFile :
         string * char list * TextIO.instream * TextIO.outstream *
         char * string -> char *)

fun processFile(prog, prompts, smlnjInStm, smlnjOutStm, prompt, file) =
      case parseFilename file of
           NONE             =>
             error(prog, "bad annotated file: \"" ^ file ^ "\"")
         | SOME(file, nOpt) =>
             let val (fileInStm, fileOutStm) = openInAndOutFiles(prog, file)

                 (* outputOverlay : int option -> int option *)

                 fun outputOverlay NONE     = NONE
                   | outputOverlay (SOME n) =
                       (TextIO.output
                        (fileOutStm,
                         "\\overlay{" ^ Int.toString n ^ "}");
                        SOME(n + 1))

                 (* procLines : char -> char *)

                 fun procLines(prompt, nOpt) =
                       case TextIO.inputLine fileInStm of
                            NONE      => prompt
                          | SOME line =>
                              let val body   =
                                        String.substring
                                        (line, 0, size line - 1)
                                  val _      =
                                        TextIO.output
                                        (fileOutStm, "\\item[]")
                                  val _      =
                                        slantedTransOutput
                                        (fileOutStm, str prompt ^ " ")
                                  val nOpt   = outputOverlay nOpt
                                  val _      = transOutput(fileOutStm, body)
                                  val nOpt   = outputOverlay nOpt
                                  val _      =
                                        TextIO.output(fileOutStm, "\n")
                                  val _      =
                                        TextIO.output(smlnjOutStm, line)
                                  val _      = TextIO.flushOut smlnjOutStm
                                  val prompt =
                                        processSMLNJOutputUntilPrompt
                                        (prompts, smlnjInStm, fileOutStm)
                              in procLines(prompt, nOpt) end
       
                 val _      = TextIO.output(fileOutStm, preamble)
                 val prompt = procLines(prompt, nOpt)
             in TextIO.output(fileOutStm, postlude);
                TextIO.closeIn fileInStm;
                TextIO.closeOut fileOutStm;
                prompt
             end

(* val processFiles :
         string * char list * TextIO.instream * TextIO.outstream *
         char * string list -> unit *)

fun processFiles(prog, prompts, smlnjInStm, smlnjOutStm, prompt, files) =
      let (* procFiles : char * string list -> unit *) 

          fun procFiles(_,      nil)           = ()
            | procFiles(prompt, file :: files) =
                procFiles
                (processFile
                 (prog, prompts, smlnjInStm, smlnjOutStm, prompt, file),
                 files)
      in procFiles(prompt, files) end

(* val main : string * string list -> OS.Process.status *)

fun main(prog, exec :: prompts :: files) =
      let val exec                      = fullyQualifyExec(prog, exec)
          val prompts                   = getPrompts(prog, prompts)
          val smlnj                     =
                Unix.execute(exec, nil)
                  handle _ =>
                           error(prog,
                                 "unable to execute smlnj executable : \"" ^
                                 exec ^ "\"")
          val (smlnjInStm, smlnjOutStm) = Unix.streamsOf smlnj
          val prompt                    =
                ignoreSMLNJOutputUntilPrompt(prompts, smlnjInStm)
          val _                         =
                processFiles
                (prog, prompts, smlnjInStm, smlnjOutStm, prompt, files)
          val _                         = Unix.reap smlnj
      in OS.Process.success end
  | main(prog, _)                        = usage prog

end;
