INTERFACE zif_loggable_object
  PUBLIC .
  TYPES:
    BEGIN OF ty_symsg,
      msgid TYPE symsgid,
      msgno TYPE symsgno,
      msgv1 TYPE symsgv,
      msgv2 TYPE symsgv,
      msgv3 TYPE symsgv,
      msgv4 TYPE symsgv,
    END OF ty_symsg.
  TYPES:
    BEGIN OF ty_message,
      type      TYPE symsgty,
      symsg     TYPE ty_symsg,
      exception TYPE REF TO cx_root,
      string    TYPE string,
    END OF ty_message,
    tty_messages TYPE STANDARD TABLE OF ty_message WITH DEFAULT KEY.

  METHODS get_message_table RETURNING VALUE(r_result) TYPE tty_messages.
ENDINTERFACE.
