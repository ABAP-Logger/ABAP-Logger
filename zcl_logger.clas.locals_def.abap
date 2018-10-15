*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

types: begin of ty_exception,
         level     type i,
         exception type ref to cx_root,
       end of ty_exception,
       tty_exception type standard table of ty_exception.

types: tty_exception_data    type standard table of bal_s_exc with default key.
