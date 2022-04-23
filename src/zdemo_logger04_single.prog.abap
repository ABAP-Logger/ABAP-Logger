*&---------------------------------------------------------------------*
*& Report zdemo_logger04_single
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdemo_logger04_single MESSAGE-ID bl.
DATA :
  logger          TYPE REF TO zif_logger.

START-OF-SELECTION.

  PERFORM logs_create.

END-OF-SELECTION.
  IF logger->is_empty( ) EQ abap_false.
    logger->fullscreen( ).
  ENDIF.

FORM logs_create.

  logger = zcl_logger_factory=>create_log(
            object = ''
            subobject = ''
            desc = 'Application Log Demo'
            settings  = zcl_logger_factory=>create_settings(
*               )->set_expiry_date( lv_expire
               )->set_autosave( abap_false
               )->set_must_be_kept_until_expiry( abap_true
               )->set_display_profile( EXPORTING
*                 i_display_profile = g_s_display_profile
                 i_profile_name = zcl_logger_settings=>display_profile_names-single
*                 i_context = ls_context
               ) ).

  DATA :
    importance TYPE balprobcl,
    l_s_msg    TYPE bal_s_msg,
    l_msgno    TYPE symsgno.

  l_msgno = 301.
  DO.
    l_s_msg-msgid = 'BL'.
    l_s_msg-msgno = l_msgno.

*   derive message type
    IF l_msgno CP '*4'.
      l_s_msg-msgty = 'E'.
    ELSEIF l_msgno CP '*2*'.
      l_s_msg-msgty = 'W'.
    ELSE.
      l_s_msg-msgty = 'S'.
    ENDIF.

*   derive message type
    IF l_msgno CP '*2'.
      importance = '1'.
    ELSEIF l_msgno CP '*5*'.
      importance = '2'.
    ELSE.
      importance = '3'.
    ENDIF.

    MESSAGE ID l_s_msg-msgid TYPE l_s_msg-msgty NUMBER l_s_msg-msgno
             INTO DATA(lv_msg).


*    ls_context-carrid = 'SF'. "Airline
*    ls_context-connid = 3. "Connection number
*    ls_context-fldate = sy-datum + l_msgno. "Flight Date
*    ls_context-id = l_msgno + 1000 ."customer
*    logger->set_default_context( ls_context ).

    logger->add(
*  EXPORTING
*    obj_to_log    =
*    context       = ls_context***
*    callback_form =
*    callback_prog =
*    callback_fm   =
*    type          =
        importance    = importance
*  RECEIVING
*    self          =
    ).

*   exit when end number is reached
    ADD 1 TO l_msgno.
    IF l_msgno >= 332.
      EXIT.
    ENDIF.

  ENDDO.


ENDFORM.
