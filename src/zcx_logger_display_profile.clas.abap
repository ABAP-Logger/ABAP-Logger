class ZCX_LOGGER_DISPLAY_PROFILE definition
  public
  inheriting from ZCX_LOGGER
  create public .

public section.

  constants ZCX_LOGGER_DISPLAY_PROFILE type SOTR_CONC value 'B9D98DB24EAF1EDD8ED3241224D60A6A' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !INFO type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_LOGGER_DISPLAY_PROFILE IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
INFO = INFO
.
 IF textid IS INITIAL.
   me->textid = ZCX_LOGGER_DISPLAY_PROFILE .
 ENDIF.
  endmethod.
ENDCLASS.
