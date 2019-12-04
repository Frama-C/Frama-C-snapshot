# Installing Frama-C

- [Installing Frama-C](#installing-frama-c)
    - [Table of Contents](#table-of-contents)
    - [Installing Frama-C via opam](#installing-frama-c-via-opam)
        - [Installing opam](#installing-opam)
        - [Installing Frama-C from opam repository](#installing-frama-c-from-opam-repository)
        - [Installing Custom Versions of Frama-C](#installing-custom-versions-of-frama-c)
        - [Installing Frama-C on Windows via WSL](#installing-frama-c-on-windows-via-wsl)
        - [Installing Frama-C on macOS](#installing-frama-c-on-mac-os)
    - [Installing Frama-C via your Linux distribution (Debian/Ubuntu/Fedora)](#installing-frama-c-via-your-linux-distribution-debianubuntufedora)
    - [Compiling from source](#compiling-from-source)
        - [Quick Start](#quick-start)
        - [Full Compilation Guide](#full-compilation-guide)
- [Testing the Installation](#testing-the-installation)
    - [Available resources](#available-resources)
        - [Executables: (in `/INSTALL_DIR/bin`)](#executables-in-install_dirbin)
        - [Shared files: (in `/INSTALL_DIR/share/frama-c` and subdirectories)](#shared-files-in-install_dirshareframa-c-and-subdirectories)
        - [Documentation files: (in `/INSTALL_DIR/share/frama-c/doc`)](#documentation-files-in-install_dirshareframa-cdoc)
        - [Object files: (in `/INSTALL_DIR/lib/frama-c`)](#object-files-in-install_dirlibframa-c)
        - [Plugin files: (in `/INSTALL_DIR/lib/frama-c/plugins`)](#plugin-files-in-install_dirlibframa-cplugins)
        - [Man files: (in `/INSTALL_DIR/man/man1`)](#man-files-in-install_dirmanman1)
- [Installing Additional Frama-C Plugins](#installing-additional-frama-c-plugins)
    - [HAVE FUN WITH FRAMA-C!](#have-fun-with-frama-c)

## Installing Frama-C via opam

[opam](http://opam.ocaml.org/) is the OCaml package manager. Every Frama-C
release is made available via an opam package.

First you need to install opam, then you may install Frama-C using opam.

### Installing opam

Several Linux distributions already include an `opam` package.

**Note:** make sure your opam version is >= 2.0.0.

macOS has opam through Homebrew.

Windows users can install opam via WSL (Windows Subsystem for Linux).

If your system does not have an opam package >= 2.0.0 you can compile it from source,
or use the provided opam binaries available at:

http://opam.ocaml.org/doc/Install.html

### Installing Frama-C from opam repository

The Frama-C package in opam is called `frama-c`, which includes both the
command-line `frama-c` executable and the graphical interface `frama-c-gui`.

`frama-c` has some non-OCaml dependencies, such as Gtk and GMP. In most
systems, opam can take care of these external dependencies through
its `depext` plug-in: issuing the two commands

    # install Frama-C's dependencies
    opam install depext
    opam depext frama-c

will install the appropriate system packages (this of course requires
administrator rights on the system).

If your system is not supported by `depext`, you will need to install
Gtk, GtkSourceView, GnomeCanvas and GMP, including development libraries,
separately. If you do so, please consider providing the system name and list of
packages (e.g. via a [Github issue](https://github.com/Frama-C/Frama-C-snapshot/issues/new))
so that we can add it to the Frama-C `depext` package.

    # install Frama-C
    opam install frama-c

### Configuring provers for Frama-C/WP

Frama-C/WP uses the [Why3](http://why3.lri.fr/) platform to run external provers for proving ACSL annotations.
The Why3 platform and the Alt-Ergo prover are automatically installed _via_ opam
when installing Frama-C.

Other recommended, efficient provers are CVC4 and Z3.
They can be used as replacement or combined with Alt-Ergo.
Actually, you can use any prover supported by Why3 in combination with Frama-C/WP.

Most provers are available on all platforms. After their installation,
Why3 must be configured to make them available for Frama-C/WP:

    ```shell
    why3 config --detect
    ```

### Known working configuration

The following set of packages is known to be a working configuration for
Frama-C 20 (Calcium):

- OCaml 4.05.0
- ocamlfind.1.8.0
- apron.20160125 (optional)
- lablgtk.2.18.8 | lablgtk3.3.0.beta6 + lablgtk3-sourceview3.3.0.beta6
- mlgmpidl.1.2.11 (optional)
- ocamlgraph.1.8.8
- why3.1.2.0
- alt-ergo.2.0.0 (for wp, optional)
- yojson.1.7.0
- zarith.1.9.1

### Installing Custom Versions of Frama-C

If you have a **non-standard** version of Frama-C available
(with proprietary extensions, custom plugins, etc.),
you can use opam to install Frama-C's dependencies and compile your
own sources directly:

    # optional: remove the standard frama-c package if it was installed
    opam remove --force frama-c

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

### Installing Frama-C on Windows via WSL

Frama-C is developed on Linux, but it can be installed on Windows using the
following tools:

- Windows Subsystem for Linux (Ubuntu 18.04)
- VcXsrv (X server for Windows)

For enabling WSL on Windows, you may follow these instructions:

https://docs.microsoft.com/en-us/windows/wsl/install-win10

As a quick guide, the following instructions should work. First, start
PowerShell with administrator rights and run the following command to activate
Windows Subsystem for Linux:

```
Enable-WindowsOptionalFeature -Online -FeatureName Microsoft-Windows-Subsystem-Linux
```

Then, reboot the operating system. After rebooting, run again the PowerShell
terminal with administrator rights. Move to your user directory, download the
distribution and install it:

```
cd C:\Users\<Your User Directory>
Invoke-WebRequest -Uri https://aka.ms/wsl-ubuntu-1804 -OutFile Ubuntu.appx -UseBasicParsing
Add-AppxPackage .\Ubuntu.appx
```

Ubuntu should now be available in the Windows menu. Run it and follow the
instructions to create a user.

For installing opam, some packages are required. The following commands can be
run to update the system and install those packages:

```
sudo add-apt-repository -y ppa:avsm/ppa
sudo apt update
sudo apt upgrade
sudo apt install make m4 gcc opam
```

Then opam can be set up using these commands:

```
opam init --disable-sandboxing -c 4.05.0 --shell-setup
eval $(opam env)
opam install -y depext
```

Now, for installing Frama-C, run the following commands that will use `apt` to
install the dependencies of the opam packages and then install them:

```
opam depext --install -y lablgtk3 lablgtk3-sourceview3
opam depext --install -y frama-c
```

Microsoft WSL does not support graphical user interfaces directly. If you want
to run Frama-C's GUI, you need to install an X server, such as VcXsrv or
Cygwin/X. We present below how to install VcXsrv.

First, install VcXsrv from:

https://sourceforge.net/projects/vcxsrv/

The default installation settings should work.
Now run it from the Windows menu (it is named XLaunch).
On the first configuration screen, select "Multiple Windows". On the
second, keep "Start no client" selected. On the third configuration step, add an
additional parameter `-nocursor` in the field "Additional parameters for
VcXsrv". You can save this configuration at the last step if you want, before
clicking "Finish".

Once it is done, the Xserver is ready. From WSL, run:

```
export DISPLAY=:0
frama-c-gui
```

### Installing Frama-C on macOS

[opam](https://opam.ocaml.org) works perfectly on macOS via
[Homebrew](https://brew.sh).
We highly recommend to rely on it for the installation of Frama-C.

1. Install *required* general macOS tools for OCaml:

    ```shell
    brew install autoconf pkg-config opam
    ```

   Do not forget to `opam init` and ``eval `opam config env` `` for a proper
   opam installation (if not already done before).

2. Set up a compatible OCaml version (replace `<version>` with the version
   indicated in the 'recommended working configuration' section):

    ```shell
    opam switch create <version>
    ```

3. Install *required* dependencies for Frama-C:

    ```shell
    brew install gmp gtk+ gtksourceview libgnomecanvas
    ```

    The graphical libraries require additional manual configuration of your
    bash profile. Consult this [issue](https://github.com/ocaml/opam-repository/issues/13709) on opam
    for details. A known working configuration is:

    ```shell
    export PKG_CONFIG_PATH=/usr/local/opt/libffi/lib/pkgconfig:/usr/local/opt/libxml2/lib/pkgconfig:/usr/local/lib/pkgconfig
    ```

4. Install *recommended* dependencies for Frama-C:

    ```shell
    brew install graphviz
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

Arch Linux: `pikaur -S frama-c`

## Compiling from source

**Note**: These instructions are no longer required in the vast majority
          of cases. They are kept here mostly for historical reference.

### Quick Start

1. Install OCaml, OCamlfind, OCamlGraph and Zarith if not already installed.
   Note that OCaml >= 4.05.0 is needed in order to compile Frama-C.

2. (Optional) For the GUI, also install Gtk, GtkSourceView, GnomeCanvas and
   Lablgtk2 or Lablgtk3 + Lablgtksourceview3 if not already installed.
   See section 'REQUIREMENTS' below for indications on the names of the
   packages to install, or use 'opam depext' as explained in section 'Opam'
   above.

3. On Linux-like distributions:

        ./configure && make && sudo make install

    See section *Configuration* below for options.

4. On Windows+Cygwin:

        ./configure --prefix="$(cygpath -a -m <installation path>)" && make && make install

5. The binary `frama-c` (and `frama-c-gui` if you have lablgtk2) is now installed.

### Full Compilation Guide

#### Frama-C Requirements

- GNU make version >= 3.81
- OCaml >= 4.05.0
- a C compiler with standard C and POSIX headers and libraries
- [OCamlGraph][OCamlGraph] >= 1.8.8
- [findlib][findlib] >= 1.6.1
- [Zarith][Zarith]

The Frama-C GUI also requires:
- Gtk (>= 2.4)
- GtkSourceView 2.x or 3.x (compatible with your Gtk version)
- GnomeCanvas 2.x (only for Gtk 2.x)
- LablGtk >= 2.18.5 or Lablgtk3 >= beta5 + corresponding Lablgtksourceview3

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
- `oracles`  sets up the Frama-C test suite oracles for your own configuration.
- `tests`    performs Frama-C's own tests.


#### Installation

Type `make install`
(depending on the installation directory, this may require superuser
privileges. The installation directory is chosen through `--prefix`).


#### API Documentation

For plugin developers, the API documentation of the Frama-C kernel and
distributed plugins is available in the file `frama-c-api.tar.gz`, after running
`make doc-distrib`.


#### Uninstallation

Type `make uninstall` to remove Frama-C and all the installed plugins.
(Depending on the installation directory, this may require superuser
privileges.)

# Testing the Installation

This step is optional.

Download some test files:

    export PREFIX_URL="https://raw.githubusercontent.com/Frama-C/Frama-C-snapshot/master/tests/value/"
    wget -P test ${PREFIX_URL}/CruiseControl.c
    wget -P test ${PREFIX_URL}/CruiseControl_const.c
    wget -P test ${PREFIX_URL}/CruiseControl.h
    wget -P test ${PREFIX_URL}/CruiseControl_extern.h
    wget -P test ${PREFIX_URL}/scade_types.h
    wget -P test ${PREFIX_URL}/config_types.h
    wget -P test ${PREFIX_URL}/definitions.h

Then test your installation by running:

    frama-c -eva test/CruiseControl*.c
    # or (if frama-c-gui is available)
    frama-c-gui -eva test/CruiseControl*.c

# Available resources

Once Frama-C is installed, the following resources should be installed and
available:

## Executables: (in `/INSTALL_DIR/bin`)

- `frama-c`
- `frama-c-gui`       if available
- `frama-c-config`    displays Frama-C configuration paths
- `frama-c.byte`      bytecode version of frama-c
- `frama-c-gui.byte`  bytecode version of frama-c-gui, if available
- `ptests.opt`        testing tool for Frama-c
- `frama-c-script`    utilities related to analysis parametrization

## Shared files: (in `/INSTALL_DIR/share/frama-c` and subdirectories)

- some `.h` and `.c` files used as preludes by Frama-C
- some `Makefiles` used to compile dynamic plugins
- some `.rc` files used to configure Frama-C
- some image files used by the Frama-C GUI
- some files for Frama-C/plug-in development (autocomplete scripts,
  Emacs settings, scripts for running Eva, ...)

## Documentation files: (in `/INSTALL_DIR/share/frama-c/doc`)

- files used to generate dynamic plugin documentation

## Object files: (in `/INSTALL_DIR/lib/frama-c`)

- object files used to compile dynamic plugins

## Plugin files: (in `/INSTALL_DIR/lib/frama-c/plugins`)

- object files of available dynamic plugins

## Man files: (in `/INSTALL_DIR/man/man1`)

- `man` files for `frama-c` (and `frama-c-gui` if available)


# Installing Additional Frama-C Plugins

Plugins may be released independently of Frama-C.

The standard way for installing them should be:

    ./configure && make && make install

Plugins may have their own custom installation procedures.
Consult their specific documentation for details.


# HAVE FUN WITH FRAMA-C!
