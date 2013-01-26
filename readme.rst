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

License
-------

**license of "headmaster"** ::

 Copyright 2010-2013 YT. All rights reserved.
 
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 
 THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
