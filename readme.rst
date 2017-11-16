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
Objective Caml
 http://caml.inria.fr/

Download
--------

Please clone from github with ``--recursive`` option
because some submodules exists in this repository.

``git clone --recursive git://github.com/ytomino/headmaster.git``

Install
-------

"headmaster" command
++++++++++++++++++++

``make -C main install BINDIR=$HOME/bin``

"headmaster" command is a translator.

Makefile variables to specify the destination directory
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``BINDIR=...``
 Specify the directory that an executable file should be copied to.

Makefile variables for the library locations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``WITH_GMP=...``
 Specify GMP directory. (ex. ``WITH_GMP=/opt/gmp``,
 then ``/opt/gmp/include`` and ``/opt/gmp/lib`` would be used.)
``WITH_MPFR=...``
 Specify MPFR directory.

"hmcpp" command
+++++++++++++++

``make -C examples/hmcpp install BINDIR=$HOME/bin``

"hmcpp" command is a simple C preprocessor based on "headmaster"
for purpose to check myself.
It is NOT satisfied all ANSI-C specifications.
Please do NOT use it (except searching bugs of "headmaster").
