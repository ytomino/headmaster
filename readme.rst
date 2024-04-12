"headmaster"
============

What's this?
------------

This is a translator from C header files to another language.

Depending tools and libraries
-----------------------------

GMP
 http://gmplib.org/
MPFR
 http://www.mpfr.org/
Objective Caml >= 4.11
 http://caml.inria.fr/
OCamlbuild
 https://github.com/ocaml/ocamlbuild
Or ocamlmake
 https://github.com/ytomino/ocamlmake
Docutils (optional, only used for the manual page)
 http://docutils.sourceforge.net/

Download
--------

Please clone from github with ``--recursive`` option
because some submodules exists in this repository.

``git clone --recursive git://github.com/ytomino/headmaster.git``

Install
-------

"headmaster" command
++++++++++++++++++++

"headmaster" command is a translator.

Run ``make`` to install the command and the manual page: ::

 make -C main install PREFIX=$HOME/opt/headmaster

Or, to install only the command: ::

 make -C main install-bin BINDIR=$HOME/bin

These destination paths are only instances.

Makefile variables to specify the destination directory
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``PREFIX=...``
 Specify the toplevel installation directory.

``BINDIR=...`` (the default is ``$PREFIX/bin``)
 Specify the directory that the executable file should be copied to.

``MANDIR=...`` (the default is ``$PREFIX/share/man``)
 Specify the directory that the manual page should be copied to.

Makefile variables for the library locations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``WITH_GMP=...``
 Specify GMP directory. (e.g. ``WITH_GMP=/opt/gmp``,
 then ``/opt/gmp/include`` and ``/opt/gmp/lib`` would be used.)
``WITH_MPFR=...``
 Specify MPFR directory.

"hmcpp" command
+++++++++++++++

"hmcpp" command is a simple C preprocessor based on "headmaster"
for purpose to check myself.
It is NOT satisfied all ANSI-C specifications.
Please do NOT use it (except searching bugs of "headmaster").

Run ``make`` to install the command: ::

 make -C examples/hmcpp install-bin BINDIR=$HOME/bin
