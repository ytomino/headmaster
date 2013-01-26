==========
headmaster
==========

------------------------
translate C header files
------------------------

:Author: Yuta Tomino
:Date: 2010-2013
:Version: 0.23
:Manual section: 1
:Manual group: preprocessor

SYNOPSIS
--------

``headmaster [options] <input-file>``

DESCRIPTION
-----------

"headmaster" is a translator from C header files to another language.

OPTIONS
-------

-D DIR, --destdir DIR    | Specify destination directory
-f LANG, --from LANG     | Specify source language. (default: c)
                         | Currently, the choices are "c", "c++", "objc" or "objc++",
                         | but it is only affected reserved words.
--gcc COMMAND            | Specify using gcc (default) and command location.
                         | This option is usable for cross-compiling.
                         | (ex. --gcc ~/bin/i686-w64-mingw32-gcc)
-h, --help               | Display the usage.
-I PATH, --include PATH  | Add PATH to user header search path list.
--isystem PATH           | Add PATH to system header search path list.
-p                       | Create missing destination directory
--tab N                  | Count tab as N characters. (default: 1)
                         | This option is only affected to error message.
-t LANG, --to LANG       | Specify destination language. (default: ada)
                         | Currently, it only supports "ada".
-v, --version            | Print the version number.

ENVIRONMENT
-----------

| "headmaster" does not directly use environment variables.
| Please set environment variables for gcc.

SEE ALSO
--------

gcc(1)
