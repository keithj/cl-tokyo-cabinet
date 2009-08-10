
Introduction

The cl-tokyo-cabinet system provides access to the Tokyo Cabinet DBM
library by Mikio Hirabayashi, via CFFI. See
http://tokyocabinet.sourceforge.net

This project is an experiment to see what Tokyo Cabinet performance
would be like, used from Common Lisp. So far, I'm not entirely happy
with the result, which I'm sure is down to inadequacies in my code.

I'm still working on this when I have the time, but it will remain
lower priority than my other projects, such as cl-genomic and cl-sam.

- The tests should all pass
- The performance is poor
- In retrospect, I think a full generic function interface was
  unnecessary
- The docstrings are incomplete

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
