# Installing Frama-C

## Table of Contents

- [Installing Frama-C](#installing-frama-c)
    - [Table of Contents](#table-of-contents)
    - [Installing Frama-C via opam](#installing-frama-c-via-opam)
        - [Installing opam](#installing-opam)
        - [Installing Custom Versions of Frama-C via opam](#installing-custom-versions-of-frama-c-via-opam)
        - [Installing Frama-C on Windows (via Cygwin + opam)](#installing-frama-c-on-windows-via-cygwin--opam)
        - [Installing Frama-C on macOS](#installing-frama-c-on-macos)
    - [Installing Frama-C via your Linux distribution (Debian/Ubuntu/Fedora)](#installing-frama-c-via-your-linux-distribution-debianubuntufedora)
    - [Compiling from source](#compiling-from-source)
        - [Quick Start](#quick-start)
        - [Full Compilation Guide](#full-compilation-guide)
    - [Available resources](#available-resources)
        - [Executables: (in `/INSTALL_DIR/bin`)](#executables-in-install_dirbin)
        - [Shared files: (in `/INSTALL_DIR/share/frama-c` and subdirectories)](#shared-files-in-install_dirshareframa-c-and-subdirectories)
        - [Documentation files: (in `/INSTALL_DIR/share/frama-c/doc`)](#documentation-files-in-install_dirshareframa-cdoc)
        - [Object files: (in `/INSTALL_DIR/lib/frama-c`)](#object-files-in-install_dirlibframa-c)
        - [Plugin files: (in `/INSTALL_DIR/lib/frama-c/plugins`)](#plugin-files-in-install_dirlibframa-cplugins)
        - [Man files: (in `/INSTALL_DIR/man/man1`)](#man-files-in-install_dirmanman1)
    - [Installing Additional Plugins](#installing-additional-plugins)
    - [HAVE FUN WITH FRAMA-C!](#have-fun-with-frama-c)

## Installing Frama-C via opam

[opam](http://opam.ocaml.org/) is the OCaml package manager. Every Frama-C
release is made available via an opam package.

First you need to install opam, then you may install Frama-C using opam:

    opam install frama-c

**Note:** make sure your opam version is >= 1.2.2.
          Also, it is highly recommended that you install an external solver
          for opam, such as `aspcud`, otherwise unexpected dependency errors
          may occur during installation.

### Installing opam

Several Linux distributions already include an `opam` package.

OSX has opam through Homebrew.

A [Cygwin-based opam](https://fdopen.github.io/opam-repository-mingw/installation)
is available on Windows. It is less stable than it is for the other OSes, but should work.

If your system does not have an opam package >= 1.2.2 you can compile it from source,
or use the provided opam binaries available at:

http://opam.ocaml.org/doc/Install.html

### Installing Frama-C from opam

The Frama-C package in opam is called `frama-c`, which includes both the
command-line `frama-c` executable and the graphical interface `frama-c-gui`.

(Note: before version 16 Sulfur, there were two packages, `frama-c-base` and
`frama-c`, which were merged together.)

`frama-c` includes non-OCaml dependencies, such as Gtk and GMP. In most
systems, opam can take care of these external dependencies through
its `depext` plug-in: issuing the two commands

    opam install depext
    opam depext frama-c

will install the appropriate system packages (this of course requires
administrator rights on the system).

If your system is not supported by `depext`, you will need to install
Gtk, GtkSourceView, GnomeCanvas and GMP, including development libraries,
separately. If you do so, please consider providing the system name and list of
packages (e.g. via a [Github issue](https://github.com/Frama-C/Frama-C-snapshot/issues/new))
so that we can add it to the Frama-C `depext` package.

### Known working configuration

The following set of packages is known to be a working configuration for
Frama-C 18 (Argon):

- OCaml 4.05.0
- alt-ergo.1.30 or, under a non-commercial license, alt-ergo.2.0.0 (pin recommended)
- apron.20160125 (optional)
- coq.8.7.2 (optional; pin recommended)
- lablgtk.2.18.5
- mlgmpidl.1.2.7 (optional)
- ocamlgraph.1.8.8
- why3.0.88.3
- yojson.1.4.1 (optional)
- zarith.1.7

Note: *pin recommended* indicates packages likely to become incompatible in
      future releases; `opam pin` is recommended to prevent them from breaking.

### Installing Custom Versions of Frama-C via opam

If you have a **non-standard** version of Frama-C available
(with proprietary extensions, custom plugins, etc.),
you can use opam to install Frama-C's dependencies and compile your
own sources directly:

    # optional: remove the standard frama-c package if it was installed
    opam remove --force frama-c frama-c-base

    # install Frama-C's dependencies
    opam install depext
    opam depext frama-c
    opam install --deps-only frama-c

    # install custom version of frama-c
    opam pin add --kind=path frama-c <dir>

where `<dir>` is the root of your unpacked Frama-C archive.
See `opam pin` for more details.

If your extensions require other libraries than the ones already used
by Frama-C, they must of course be installed as well.

### Installing Frama-C on Windows (via Cygwin + opam)

Windows is not officially supported by the Frama-C team
(as in, we may not have the time to fix all issues),
but Frama-C has been successfully compiled in Windows with the following tools:

- Cygwin (for shell and installation support only;
          the compiled binaries do not depend on Cygwin)
- opam for Windows (currently experimental)
- OCaml MinGW-based compiler

You may follow these instructions for installing OCaml for Windows:

https://fdopen.github.io/opam-repository-mingw/installation/

Note that `lablgtk` (used by Frama-C) requires installing `depext` and
`depext-cygwinports`, as indicated in the page.

Once the Windows-based opam repository is configured, simply run:

    opam install frama-c

Some (now obsoleted) compilation instructions for older versions of Frama-C on
Windows are available on the Frama-C wiki:

https://bts.frama-c.com/dokuwiki/doku.php?id=mantis:frama-c:compiling_from_source

### Installing Frama-C on macOS

[opam](https://opam.ocaml.org) works perfectly on macOS via
[Homebrew](https://brew.sh).
We recommend to rely on it for the installation of Frama-C.

1. Install *required* general macOS tools for OCaml:

    ```shell
    brew install autoconf pkg-config opam
    ```

   Do not forget to `opam init` and ``eval `opam config env` `` for a proper
   opam installation (if not already done before on your machine).

2. Install *required* dependencies for Frama-C:

    ```shell
    brew install gmp gtk+ gtksourceview libgnomecanvas
    ```

3. Install *recommended* dependencies for Frama-C:

    ```shell
    brew install graphviz
    opam install why3
    ```

4. Install *optional* dependencies for Frama-C/WP:

    ```shell
    opam install coq coqide
    ```

5. Install Frama-C:

    ```shell
    opam install frama-c
    ```

## Installing Frama-C via your Linux distribution (Debian/Ubuntu/Fedora)

**NOTE**: Distribution packages are updated later than opam packages,
          so if you want access to the most recent versions of Frama-C,
          opam is currently the recommended approach.

Also note that it is **not** recommended to mix OCaml packages installed by
your distribution with packages installed via opam. When using opam,
we recommend uninstalling all `ocaml-*` packages from your distribution, and
then installing, exclusively via opam, an OCaml compiler and all the OCaml
packages you need. This ensures that only those versions will be in the PATH.

The advantage of using distribution packages is that dependencies are almost
always handled by the distribution's package manager. The disadvantage is that,
if you need some optional OCaml package that has not been packaged in your
distribution (e.g. `landmarks`, which is distributed via opam), it may be very
hard to install it, since mixing opam and non-opam packages often fails
(and is **strongly** discouraged).

Debian/Ubuntu: `apt-get install frama-c`

Fedora: `dnf install frama-c`

Arch Linux: `yaourt -S frama-c`

## Compiling from source

**Note**: These instructions are no longer required in the vast majority
          of cases. They are kept here mostly for historical reference.

### Quick Start

1. Install OCaml, OCamlfind, OCamlGraph and Zarith if not already installed.
   Note that OCaml >= 4.02.3 is needed in order to compile Frama-C.

2. (Optional) For the GUI, also install Gtk, GtkSourceView, GnomeCanvas and
   Lablgtk2 if not already installed.
   See section 'REQUIREMENTS' below for indications on the names of the
   packages to install, or use 'opam depext' as explained in section 'Opam'
   above.

3. On Linux-like distributions:

        ./configure && make && sudo make install

    See section *Configuration* below for options.

4. On Windows+Cygwin:

        ./configure --prefix="$(cygpath -a -m <installation path>)" && make && make install

5. The binary `frama-c` (and `frama-c-gui` if you have lablgtk2) is now installed.

6. Optionally, test your installation by running:

        frama-c -val tests/misc/CruiseControl*.c
        frama-c-gui -val tests/misc/CruiseControl*.c # if frama-c-gui is available

### Full Compilation Guide

#### Frama-C Requirements

- GNU make version >= 3.81
- OCaml >= 4.02.3
- a C compiler with standard C and POSIX headers and libraries
- [OCamlGraph][OCamlGraph] >= 1.8.8
- [findlib][findlib] >= 1.6.1
- [Zarith][Zarith]

The Frama-C GUI also requires:
- Gtk (>= 2.4)
- GtkSourceView 2.x
- GnomeCanvas 2.x
- LablGtk >= 2.18.5

Plugins may have their own requirements.
Consult their specific documentations for details.

[OCamlGraph]: http://ocamlgraph.lri.fr
[findlib]: http://projects.camlcity.org/projects/findlib.html
[Zarith]: http://github.com/ocaml/Zarith


#### Configuration

Frama-C is configured by `./configure [options]`.

`configure` is generated by `autoconf`, so that the standard options for setting
installation directories are available, in particular `--prefix=/path`.

A plugin can be enabled by `--enable-plugin` and disabled by `--disable-plugin`.
By default, all distributed plugins are enabled. Those who default to 'no'
are not part of the Frama-C distribution (usually because they are too
experimental to be released as is).

See `./configure --help` for the current list of plugins, and available options.

##### Under Cygwin

Use `./configure --prefix="$(cygpath -a -m <installation path>)"`.

(using Unix-style paths without the drive letter will probably not work)


#### Compilation

Type `make`.

Some Makefile targets of interest are:
- `doc`      generates the API documentation.
- `top`      generates an OCaml toplevel embedding Frama-C as a library.
- `oracles`  sets up the Frama-C test suite oracles for your own configuration.
- `tests`    performs Frama-C's own tests.


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
- `ptests.opt`        testing tool for Frama-c
- `frama-c.toplevel`  if 'make top' previously done

### Shared files: (in `/INSTALL_DIR/share/frama-c` and subdirectories)

- some `.h` and `.c` files used as preludes by Frama-C
- some `Makefiles` used to compile dynamic plugins
- some `.rc` files used to configure Frama-C
- some image files used by the Frama-C GUI
- some files for Frama-C/plug-in development (autocomplete scripts,
  Emacs settings, scripts for running Eva, ...)

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

    ./configure && make && make install

Plugins may have their own custom installation procedures.
Consult their specific documentation for details.


## HAVE FUN WITH FRAMA-C!
