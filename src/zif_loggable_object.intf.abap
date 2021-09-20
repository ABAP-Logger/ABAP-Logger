interface ZIF_LOGGABLE_OBJECT
  public .
    TYPES: begin of ty_message,
            SYMSG type SYMSG,
            exception type ref to cx_root,
            string type string,
*            context       type simple,
           end of ty_message,
           tty_message type standard table of ty_message with empty key.

    METHODS get_message_table RETURNING VALUE(r_result) TYPE tty_message.
endinterface.
