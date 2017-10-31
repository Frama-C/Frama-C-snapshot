This directory contains a set of a Makefile and several bash scripts which
can be used to simplify non-trivial analyses with Frama-C and some of its
plugins, in particular EVA.
This Makefile can be included in your own Makefile for the following advantages.

1.  It ensures that no unnecessary work is done. If you change the Makefile,
    targets that have their command line affected will be rebuilt, but any
    target for which the command line doesn't change won't be rebuilt.
2.  It provides commonly used default parameters for the analysis. Note that
    you can still append new parameters or completely redefine them.
3.  It splits between parsing and analysis, storing outputs in separate
    repositories: <target>.parse for parsing-related outputs, and
    <target>.eva for EVA-related outputs.
4.  It produces several additional outputs after parsing and after an EVA
    analysis:
    *   `<target>.parse/parse.log`, or `<target>.eva/eva.log`:
        contain the entire output of the parsing/analysis command,
    *   `warnings.log`: only the warnings emitted by Frama-C/EVA,
    *   `alarms.csv`: list of emitted alarms in csv form,
    *   `metrics.log`: various metrics about the analysis,
    *   `stats.txt`: stats about the analysis, such as user time,
        memory consumption, the date of the analysis, coverage of the analysis,
        number of warnings and alarms, and the command line arguments.
5.  It keeps copies of all previous analyses you have done in timestamped
    directories.


Getting started
===============

There is a ready-to-use Makefile skeleton at the end of this section. If you
want explanations about this Makefile, read this entire section.

Other usage examples are available in Frama-C's Github open-source-case-studies
repository: https://github.com/Frama-C/open-source-case-studies

(If you have access to Frama-C's development repositories, you can also use
the examples in `analysis-scripts/examples`.)

Including fcscripts
-------------------

This folder contains several shell scripts and, most importantly,
the `frama-c.mk` file. This file is intended to be included at the top of your
`Makefile`:

````
include fcscripts/frama-c.mk
````

By default, the scripts use the frama-c binaries located in your `$PATH`
environment variable. You may want to specify different binaries, but, if you
want to version your analysis, this path will depend on the computer it is run
on. So, we recommend you use an unversioned file `frama-c-path.mk`. Add this
file to your `.gitignore` and define the `FRAMAC`, `FRAMAC_GUI` and
`FRAMAC_CONFIG` variables there. For instance:

````
FRAMAC_DIR=frama-c/bin
FRAMAC=$(FRAMAC_DIR)/frama-c
FRAMAC_GUI=$(FRAMAC_DIR)/frama-c-gui
FRAMAC_CONFIG=$(FRAMAC_DIR)/frama-c-config
````

And include this file before `frama-c.mk` in your Makefile. As this file
is computer dependent and unversioned, it will not always be present. Prefix
the include command with a minus sign `-` to tell `make` to ignore missing
files:

````
-include frama-c-path.mk
````

Then, to handle both cases when Frama-C is in the path, and when it is not,
use the following conditional definition of `FRAMAC_CONFIG` followed by the
inclusion of `frama-c.mk`:

```
FRAMAC_CONFIG ?= frama-c-config
include $(shell $(FRAMAC_CONFIG) -print-share-path)/analysis-scripts/frama-c.mk
```


Defining analysis global parameters
-----------------------------------

Once `frama-c.mk` is included, you may change default values of variables.
Most usual variables you may want to change are `CPPFLAGS`, `FCFLAGS`
and `EVAFLAGS`. For example:

````
CPPFLAGS  = -D__I586__
FCFLAGS  += -verbose 0
EVAFLAGS += -plevel 100
````

Some arguments are passed to Frama-C from the environment. This is the
case of the `FRAMA_C_MEMORY_FOOTPRINT` variable. You can set it in your
Makefile with the following line:

````
export FRAMA_C_MEMORY_FOOTPRINT = 8
````

The two steps of the analysis
-----------------------------

Parsing might be long on some analyses. The analysis scripts save the result
of the parsing phase so that it is not redone when modifying only analysis
parameters but not parsing parameters.

The parsing result is saved in a `<target>.parse` directory while the result
of the analysis is saved in a `<target>.eva` directory.
The second automatically depends on the first.
Thus, each time you require that make build the `.eva` target,
it will build the `.parse` one first.

````
all: example.eva
````


Defining analysis sources
-------------------------

To define the set of sources to analyze, you must define them as dependencies
of your `.parse` target.

````
example.parse: file1.c file2.c file3.c ...
````

As they are dependencies, parsing will be remade if the sources change.


Defining project-specific parameters
------------------------------------

You can describe several analyses with the same Makefile. We call these
analyses "projects". Projects are not likely to share the exact same
parameters. Thus, it is useful to define these parameters project wise.
`make` allows this by putting the variable definition after the target. For
instance:

````
example.parse: CPPFLAGS += -D__FRAMAC__
example.eva:   FCFLAGS  += -main my_main
example.eva:   EVAFLAGS += -slevel 500
````


Full example
------------

### `Makefile`

````
# optional include, in case frama-c-path.mk does not exist (frama-c in the PATH)
-include frama-c-path.mk
# frama-c-config is used to find the analysis scripts and frama-c.mk
FRAMAC_CONFIG ?= frama-c-config
include $(shell $(FRAMAC_CONFIG) -print-share-path)/analysis-scripts/frama-c.mk

# Global parameters
CPPFLAGS     = -D__I586__
FCFLAGS     += -verbose 0
EVAFLAGS    += -plevel 100

export FRAMA_C_MEMORY_FOOTPRINT = 8

# Default targets
all: example.eva

# Input files
example.parse: example.c

# Project-specific parameters
example.parse: CPPFLAGS += -D__FRAMAC__
example.eva:   FCFLAGS  += -main my_main
example.eva:   EVAFLAGS += -slevel 500
````

### `frama-c-path.mk`

````
FRAMAC_DIR=frama-c/bin
FRAMAC=$(FRAMAC_DIR)/frama-c
FRAMAC_GUI=$(FRAMAC_DIR)/frama-c-gui
FRAMAC_CONFIG=$(FRAMAC_DIR)/frama-c-config
````

### `.gitignore`

````
*.parse*
*.eva*
*.crash
command
parse.log
eva.log
stats.txt
frama-c-path.mk
````
