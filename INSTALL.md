# Installing Frama-C

## Table of Contents

- [Recommended mode: OPAM](#recommended-mode-opam)
  - [Frama-C Requirements](#frama-c-requirements)
  - [Installing OPAM](#installing-opam)
  - [Installing Frama-C from OPAM](#installing-frama-c-from-opam)
  - [Installing Custom Versions of Frama-C via OPAM](#installing-custom-versions-of-frama-c-via-opam)
  - [Installing Frama-C on Windows (via Cygwin + OPAM)](#installing-frama-c-on-windows-via-cygwin--opam)
  - [Installing Frama-C on Mac OS X](#installing-frama-c-on-mac-os-x)
- [Installing Frama-C via your Linux distribution (Debian/Ubuntu/Fedora)](#installing-frama-c-via-your-linux-distribution-debianubuntufedora)
  - [Debian/Ubuntu](#debianubuntu)
  - [Fedora](#fedora)
- [Compiling from source](#compiling-from-source)
  - [Quick Start](#quick-start)
  - [Full Compilation Guide](#full-compilation-guide)
- [Available resources](#available-resources)
- [Installing Additional Plugins](#installing-additional-plugins)

## Recommended mode: OPAM

The preferred method of installation for Frama-C is using
[OPAM](http://opam.ocaml.org/) (v1.2 or newer).

First you need to install OPAM, then you may install Frama-C using OPAM:

    opam install frama-c

**Note:** make sure your OPAM version is >= 1.2.
          Also, it is highly recommended that you install an external solver
          for OPAM, such as `aspcud`, otherwise unexpected dependency errors
          may occur during installation.

### Installing OPAM

Several Linux distributions already include an `opam` package.

OSX has OPAM through Homebrew.

A Windows OPAM is currently being developed, but it is not yet stable.

**Note**: Some distributions include an old version of OPAM (<= 1.1).
          It cannot be used to reliably install Frama-C due to conflicts
          between dependencies.

If your system does not have an OPAM package, you can compile it from source,
or use the provided OPAM binaries available at:

http://opam.ocaml.org/doc/Install.html

### Installing Frama-C from OPAM

There are two Frama-C packages in OPAM:

- `frama-c-base`: minimal Frama-C installation, without GUI; few dependencies
- `frama-c`: includes all GUI-related dependencies, plus other recommended
             packages.

The `frama-c` package recommends the installation of optional packages, e.g.
external provers for WP, such as `why3` and `coq`.

To install `frama-c`, you may need to install Gtk, GtkSourceView and
GnomeCanvas separately.
These are C libraries with OCaml bindings used by the GUI.
To get the exact list of packages that are needed, use:

    opam install depext
    opam depext frama-c

and install the packages listed as missing.

### Installing Custom Versions of Frama-C via OPAM

If you have a **non-standard** version of Frama-C available
(with proprietary extensions, custom plugins, etc.),
you can install it through OPAM using these commands:

    # remove the previous version of frama-c
    opam remove --force frama-c frama-c-base

    # optional packages, but recommended (for efficiency, and for the GUI)
    opam install depext
    opam depext zarith lablgtk conf-gtksourceview conf-gnomecanvas
    opam install zarith lablgtk conf-gtksourceview conf-gnomecanvas

    # install custom version of frama-c
    opam pin add frama-c-base <dir>

where `<dir>` is the root of your unpacked Frama-C archive.

### Installing Frama-C on Windows (via Cygwin + OPAM)

Windows is not officially supported by the Frama-C team
(as in, we may not have the time to fix all issues),
but Frama-C has been succesfully compiled in Windows with the following tools:

- Cygwin (for shell and installation support only;
          the compiled binaries do not depend on Cygwin)
- OPAM for Windows (currently experimental)
- OCaml MinGW-based compiler

Installation instructions are described (and updated continuously) on the
Frama-C wiki:

https://bts.frama-c.com/dokuwiki/doku.php?id=mantis:frama-c:compiling_from_source

(*Note: Your browser may complain about the self-signed certificate.*)

Frama-C Windows releases are periodically made available on the non-official
OPAM MinGW repository:

https://github.com/fdopen/opam-repository-mingw

### Installing Frama-C on Mac OS X

OPAM works perfectly on Mac OS via Homebrew.

Recommended installation:

General Mac OS tools for OCaml:

    xcode-select --install
    open http://brew.sh
    brew install git autoconf meld opam

Graphical User Interface:

    brew install gtk+ --with-jasper
    brew install gtksourceview libgnomecanvas graphviz
    opam install lablgtk ocamlgraph

Recommended for Frama-C:

    brew install gmp
    opam install zarith

Necessary for Frama-C/WP:

    opam install alt-ergo

Also recommended for Frama-C/WP:

    opam install altgr-ergo coq coqide why3


## Installing Frama-C via your Linux distribution (Debian/Ubuntu/Fedora)

**NOTE**: Distribution packages are not as up-to-date as OPAM packages.
          We recommend using OPAM if possible.

### Debian/Ubuntu

If you are using Debian >= Squeeze 6.0 or Ubuntu >= Lucid Lynx 10.04 then
a Frama-C package is provided:

    sudo apt-get install frama-c

or, if you don't want the Gtk-based GUI:

    sudo apt-get install frama-c-base

### Fedora

If you are using Fedora >= 13 then a Frama-C package is provided:

    yum install frama-c


## Compiling from source

**Note**: These instructions are no longer required in the vast majority
          of cases. They are kept here mostly for historical reference.

### Quick Start

1. Install OCaml, OCamlfind and OCamlGraph if not already installed. Note
   that OCaml >= 4.02.3 is needed in order to compile Frama-C

2. (Optional) For the GUI, also install Gtk, GtkSourceView, GnomeCanvas and
   Lablgtk2 if not already installed. If possible, also install Zarith.
   See section 'REQUIREMENTS' below for indications on the names of the
   packages to install, or use 'opam depext' as explained in section 'Opam'
   above.

3. On Linux-like distributions:

        ./configure && make && sudo make install

    See section *Configuration* below for options.

4. On Windows+Cygwin or Windows+MinGW+msys:

        ./configure --prefix C:/windows/path/with/direct/slash/no/space && make && make install

5. The binary `frama-c` (and `frama-c-gui` if you have lablgtk2) is now installed.

6. Optionally, test your installation by running:

        frama-c -val tests/misc/CruiseControl*.c
        frama-c-gui -val tests/misc/CruiseControl*.c (if frama-c-gui is available)


### Full Compilation Guide

#### Requirements

- GNU make version >= 3.81
- OCaml >= 4.02.3
- a C compiler with standard C and POSIX headers and libraries
- [OCamlGraph][OCamlGraph] >= 1.8.5
- [findlib][findlib] >= 1.6.1

The Frama-C GUI also requires:
- Gtk (>= 2.4)
- GtkSourceView 2.x
- GnomeCanvas 2.x
- LablGtk >= 2.18.2

If [Zarith][Zarith] is installed, it will be used by Frama-C.
Otherwise another equivalent but less efficient library will be used.

Plugins may have their own requirements.
Consult their specific documentations for details.

[OCamlGraph]: http://ocamlgraph.lri.fr
[findlib]: http://projects.camlcity.org/projects/findlib.html
[Zarith]: http://forge.ocamlcore.org/projects/zarith


##### Ubuntu

If you are using Ubuntu >= Precise Pangolin 12.04 then an optimal list of
packages is installed by:

    sudo apt-get install ocaml ocaml-native-compilers graphviz \
                 libzarith-ocaml-dev libfindlib-ocaml-dev \
                 liblablgtksourceview2-ocaml-dev liblablgtk2-gnome-ocaml-dev

##### Fedora

If you are using a recent Fedora, an optimal list of packages can be installed
through (replace `dnf` by `yum` in older versions of Fedora):

    sudo dnf install ocaml graphviz \
                 ocaml-zarith-devel ocaml-findlib ocaml \
                 ocaml-lablgtk-devel gtksourceview2-devel libgnomecanvas-devel

##### Other Linux systems

Some other Linux systems provide packages for the required tools and libraries.
Please send us patches to update this section for your favorite distro.


#### Configuration

Frama-C is configured by `./configure [options]`.

`configure` is generated by `autoconf`, so that the standard options for setting
installation directories are available, in particular `--prefix=/path`.

A plugin can be enabled by `--enable-plugin` and disabled by `--disable-plugin`.
By default, all distributed plugins are enabled. Those who defaults to 'no'
are not part of the Frama-C distribution (usually because they are too
experimental to be released as is).

See `./configure --help` for the current list of plugins, and available options.

##### Under Cygwin or MinGW

Use `./configure --prefix C:/windows/path/with/direct/slash`.


#### Compilation

Type `make`.

Some Makefile targets of interest are:
- `doc`      generates the API documentation
- `top`      generates an OCaml toplevel embedding Frama-C as a library.
- `oracles`  sets up the Frama-C test suite oracles for your own configuration.
- `tests`    performs Frama-C's own tests

##### Under Cygwin or MinGW

Use: `make FRAMAC_ROOT_SRCDIR="$(cygpath -a -m $PWD)"`


#### Installation

Type `make install`
(depending on the installation directory, this may require superuser
privileges. The installation directory is chosen through `--prefix`).


#### Testing the Installation

This step is optional.

Test your installation by running:

    frama-c -val tests/misc/CruiseControl*.c
    frama-c-gui -val tests/misc/CruiseControl*.c (if frama-c-gui is available)




#### API Documentation

For plugin developers, the API documentation of the Frama-C kernel and
distributed plugins is available in the file `frama-c-api.tar.gz`, after running
`make doc-distrib`.


#### Uninstallation

Type `make uninstall` to remove Frama-C and all the installed plugins.
(Depending on the installation directory, this may require superuser
privileges.)

## Available resources

Once Frama-C is installed, the following resources should be installed and
available:

### Executables: (in `/INSTALL_DIR/bin`)

- `frama-c`
- `frama-c-gui`       if available
- `frama-c-config`    displays Frama-C configuration paths
- `frama-c.byte`      bytecode version of frama-c
- `frama-c-gui.byte`  bytecode version of frama-c-gui, if available
- `ptests.opt`        testing tools for Frama-c
- `frama-c.toplevel`  if 'make top' previously done

### Shared files: (in `/INSTALL_DIR/share/frama-c` and subdirectories)

- some `.h` and `.c` files used as preludes by Frama-C
- some `Makefiles` used to compile dynamic plugins
- some `.rc` files used to configure Frama-C
- some image files used by the Frama-C GUI

### Documentation files: (in `/INSTALL_DIR/share/frama-c/doc`)

- files used to generate dynamic plugin documentation

### Object files: (in `/INSTALL_DIR/lib/frama-c`)

- object files used to compile dynamic plugins

### Plugin files: (in `/INSTALL_DIR/lib/frama-c/plugins`)

- object files of available dynamic plugins

### Man files: (in `/INSTALL_DIR/man/man1`)

- `man` files for `frama-c` (and `frama-c-gui` if available)


## Installing Additional Plugins

Plugins may be released independently of Frama-C.

The standard way for installing them should be:

    ./configure && make && sudo make install

Plugins may have their own custom installation procedures.
Consult their specific documentations for details.


## HAVE FUN WITH FRAMA-C!
