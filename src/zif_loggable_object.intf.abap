INTERFACE zif_loggable_object
  PUBLIC .
  TYPES:
    BEGIN OF ty_message,
      symsg     TYPE symsg,
      exception TYPE REF TO cx_root,
      string    TYPE string,
    END OF ty_message,
    tty_messages TYPE STANDARD TABLE OF ty_message WITH DEFAULT KEY.

  METHODS get_message_table RETURNING VALUE(r_result) TYPE tty_messages.
ENDINTERFACE.
