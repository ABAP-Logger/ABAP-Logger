CLASS zcx_logger_display_profile DEFINITION
  PUBLIC
  INHERITING FROM zcx_logger
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS zcx_logger_display_profile TYPE sotr_conc VALUE 'B9D98DB24EAF1EDD8ED3241224D60A6A' ##NO_TEXT.

    METHODS constructor
    IMPORTING
      textid LIKE textid OPTIONAL
      previous LIKE previous OPTIONAL
      info TYPE string OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_logger_display_profile IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        textid = textid
        previous = previous
        info = info.
    IF textid IS INITIAL.
      me->textid = zcx_logger_display_profile.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
