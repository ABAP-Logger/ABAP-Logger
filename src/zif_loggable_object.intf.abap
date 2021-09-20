interface ZIF_LOGGABLE_OBJECT
  public .
    TYPES: begin of ty_message,
            SYMSG type SYMSG, 
            exception type ref to cx_root, 
            string type string,
            context       type simple optional,
           end of ty_message,
           tty_message type standard table ty_message empty key.
  
    METHODS get_message_table RETURNING VALUE(r_result) TYPE tty_message.
endinterface.
