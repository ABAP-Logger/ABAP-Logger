class ZCX_LOGGER definition
  public
  inheriting from CX_NO_CHECK
  create public .

public section.

  constants ZCX_LOGGER type SOTR_CONC value 'B9D98DB24EAF1EDD8ED3241224D60A6A' ##NO_TEXT.
  data INFO type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !INFO type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_LOGGER IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = ZCX_LOGGER .
 ENDIF.
me->INFO = INFO .
  endmethod.
ENDCLASS.
