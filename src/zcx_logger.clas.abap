CLASS zcx_logger DEFINITION
  PUBLIC
  INHERITING FROM cx_no_check
  CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS zcx_logger TYPE sotr_conc VALUE 'B9D98DB24EAF1EDD8ED3241224D60A6A' ##NO_TEXT.
    DATA info TYPE string.

    METHODS constructor
    IMPORTING
      textid LIKE textid OPTIONAL
      previous LIKE previous OPTIONAL
      info TYPE string OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_logger IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        textid = textid
        previous = previous.
    IF textid IS INITIAL.
      me->textid = zcx_logger.
    ENDIF.
    me->info = info.
  ENDMETHOD.

ENDCLASS.
