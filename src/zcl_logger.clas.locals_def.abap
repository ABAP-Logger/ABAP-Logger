*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

TYPES: BEGIN OF ty_exception,
         level     TYPE i,
         exception TYPE REF TO cx_root,
       END OF ty_exception,
       tty_exception TYPE STANDARD TABLE OF ty_exception.

TYPES tty_exception_data TYPE STANDARD TABLE OF bal_s_exc WITH DEFAULT KEY.
