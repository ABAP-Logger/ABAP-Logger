class ZCX_STATIC_CHECK_WITH_LOG definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  data LOGGER type ref to ZIF_LOGGER read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !LOGGER type ref to ZIF_LOGGER .
protected section.
private section.
ENDCLASS.



CLASS ZCX_STATIC_CHECK_WITH_LOG IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
me->LOGGER = LOGGER .
  endmethod.
ENDCLASS.
