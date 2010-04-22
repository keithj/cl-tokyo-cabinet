
Introduction

The cl-tokyo-cabinet system provides access to the Tokyo Cabinet DBM
library by Mikio Hirabayashi, via CFFI. See http://1978th.net/tokyocabinet/

This project started as an experiment to see what Tokyo Cabinet
performance would be like when used from Common Lisp. Most of the BDB
and hash database functionality are implemented. The API is divided
into two parts; a low-level FFI package which is a direct CFFI
translation of the Tokyo Cabinet functions and a higher level, more
Lisp-centric generic function API on top of that.

Having now benchmarked insertion speed against equivalent code using
the C BDB API, the results are encouraging (see the perf directory for
results). The Lisp API produced a byte-identical database in a
wall-time of between 1.14x and 1.62x that taken by the C API (median
of 7 runs each).


Installation

cl-tokyo-cabinet uses ASDF for system definition. Copy or symlink
cl-tokyo-cabinet.asd (and optionally cl-tokyo-cabinet-test.asd) to
your asdf:*central-registry* and load cl-tokyo-cabinet with the
asdf:operate function:

 (asdf:operate 'asdf:load-op :cl-tokyo-cabinet)

or with the equivalent deoxybyte-systems:load-system function:
 
 (dxs:load-system :cl-tokyo-cabinet)


Tests

To run the unit and regression tests you need to have LIFT
installed. Run the tests with the asdf:operate function:

 (asdf:operate 'asdf:test-op :cl-tokyo-cabinet)

or with the equivalent deoxybyte-systems:test-system function:

 (dxs:test-system :cl-tokyo-cabinet)


Documentation

See the Lisp docstrings, particularly the package docstrings for an
overview. HTML documentation may be generated with the command:

 (dxs:document-system :cl-tokyo-cabinet)

at the REPL, provided that CLDOC is installed.


Dependencies

deoxybyte-systems       git://github.com/keithj/deoxybyte-systems.git

CFFI                    http://common-lisp.net/project/cffi/


Optional dependencies

LIFT                    http://common-lisp.net/project/lift/
CLDOC                   http://common-lisp.net/project/cldoc/
