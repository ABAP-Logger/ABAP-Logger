*&---------------------------------------------------------------------*
*& Report ZDEMO_LOGGER04
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdemo_logger04.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-001.
PARAMETERS:
  p_single RADIOBUTTON GROUP sel DEFAULT 'X',
  p_stndrd RADIOBUTTON GROUP sel,
  p_notree RADIOBUTTON GROUP sel,
  p_self   RADIOBUTTON GROUP sel.
SELECTION-SCREEN END OF BLOCK b01.


END-OF-SELECTION.

* show a single log file
  IF NOT p_single IS INITIAL.
    SUBMIT zdemo_logger04_single
           AND RETURN.
  ENDIF.

* show many log files
  IF NOT p_stndrd IS INITIAL.
    SUBMIT zdemo_logger04_standard
           AND RETURN.
  ENDIF.

* show one log file and no tree next to it
  IF NOT p_notree IS INITIAL.
    SUBMIT zdemo_logger04_no_tree
           AND RETURN.
  ENDIF.

  IF NOT p_self IS INITIAL.
    SUBMIT zdemo_logger04_self
      AND RETURN.
  ENDIF.
