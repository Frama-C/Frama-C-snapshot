![Frama-C](share/frama-c.gif?raw=true)

[Frama-C](http://frama-c.com) is a platform dedicated to the analysis of
source code written in C.

## A Collaborative Platform

Frama-C gathers several analysis techniques in a single collaborative
platform, consisting of a **kernel** providing a core set of features
(e.g., a normalized AST for C programs) plus a set of analyzers,
called **plug-ins**. Plug-ins can build upon results computed by other
plug-ins in the platform.

Thanks to this approach, Frama-C provides sophisticated tools, including:

- an analyzer based on abstract interpretation, aimed at verifying
  the absence of run-time errors (**Value**);
- a program proof framework based on weakest precondition calculus (**WP**);
- a program slicer (**Slicing**);
- a tool for verification of temporal (LTL) properties (**Aoraï**);
- several tools for code base exploration and dependency analysis
  (**From**, **Impact**, **Metrics**, **Occurrence**, **Scope**, etc.).

These plug-ins share a common language and can exchange information via
**[ACSL](http://frama-c.com/acsl.html)** (*ANSI/ISO C Specification Language*)
properties. Plug-ins can also collaborate via their APIs.

## Installation

#### Prerequisites

- [OPAM](http://opam.ocaml.org/) (v1.2 or newer; also, an external solver for
                                  OPAM is highly recommended, e.g. aspcud)
- OCaml 4.xx (make sure to use the latest patch release for your minor branch:
  4.00.1, 4.01.0 or 4.02.3)
- A C compiler (gcc and clang are recommended)
- GNU Make

Frama-C is developed mainly in Linux, often tested in Mac OSX
(via Homebrew), and occasionally tested on Windows
(with Cygwin + MinGW).

#### Installing via OPAM

Frama-C is available through [OPAM](http://opam.ocaml.org/), the
OCaml Package Manager.

    opam install frama-c

Please ensure that your OPAM is version 1.2 or newer.

You can install the command-line version only:

    opam install frama-c-base

For more detailed information about installing OPAM/Frama-C,
see [INSTALL.md](INSTALL.md).

## Usage

Frama-C can be run from the command-line, or via its graphical interface.

#### Simple usage

The recommended usage for simple files is one of the following lines:

    frama-c file.c -<plugin> [options]
    frama-c-gui file.c

Where `-<plugin>` is one of the several Frama-C plug-ins,
e.g. `-val`, or `-wp`, or `-metrics`, etc.
Plug-ins can also be run directly from the GUI.

To list all plug-ins, run:

    frama-c -plugins

Each plug-in has a help command
(`-<plugin>-help` or `-<plugin>-h`) that describes its several
options.

#### Complex scenarios

For more complex usage scenarios (lots of files and directories,
with several preprocessing directives), we recommend splitting Frama-C's usage
in two parts:

1. Parsing the input files and saving the result to a file;
2. Loading the parsing results and then running the analyses or the GUI.

Parsing typically involves giving extra arguments to the C preprocessor,
so the `-cpp-extra-args` option is often useful, as in the example below:

    frama-c *.c *.h -cpp-extra-args="-D<define> -I<include>" -save parsed.sav

The results are then loaded into Frama-C for further analyses or for inspection
via the GUI:

    frama-c -load parsed.sav -<plugin> [options]
    frama-c-gui -load parsed.sav -<plugin> [options]

## Further reference

- Links to user and developer manuals, Frama-C archives,
  and plug-in manuals are available at <br> http://frama-c.com/download.html

- [StackOverflow](http://stackoverflow.com/questions/tagged/frama-c) has several
  questions with the `frama-c` tag, which is monitored by several members of the
  Frama-C community.

- The [Frama-c-discuss mailing list](http://lists.gforge.inria.fr/cgi-bin/mailman/listinfo/frama-c-discuss)
  is used for announcements and general discussions.

- The [official bug tracking system](http://bts.frama-c.com/) can be used for
  bug reports.

- The [Frama-C wiki](https://bts.frama-c.com/dokuwiki/doku.php?id=mantis:frama-c:start)
  has some useful information, although it is not entirely up-to-date.

- The [Frama-C blog](http://blog.frama-c.com/) has several posts about
  new developments of Frama-C, as well as general discussions about the C
  language, undefined behavior, floating-point computations, etc.

- The [Github snapshot repository](https://github.com/Frama-C/Frama-C-snapshot)
  contains the .tar.gz archives of stable Frama-C releases, ready to be cloned.
