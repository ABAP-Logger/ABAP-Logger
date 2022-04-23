*&---------------------------------------------------------------------*
*& Report zdemo_logger04_self
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdemo_logger04_self MESSAGE-ID bl.
DATA :
  logger          TYPE REF TO zif_logger.

PARAMETERS:
  p_grid   AS CHECKBOX DEFAULT space.

START-OF-SELECTION.

  PERFORM logs_create.

END-OF-SELECTION.
  IF logger->is_empty( ) EQ abap_false.
    logger->fullscreen( ).
  ENDIF.

FORM logs_create.
  DATA:
    ls_context           TYPE bal_s_ex01,
    g_s_display_profile  TYPE bal_s_prof,
    l_s_fcat TYPE bal_s_fcat.

  g_s_display_profile-title     = 'Application Log:Self defined display profile'.
  g_s_display_profile-use_grid = P_GRID.
  g_s_display_profile-head_text = 'Application.Log.Demo'.
  g_s_display_profile-head_size = 47.
  g_s_display_profile-tree_size = 28.
  g_s_display_profile-disvariant-report = sy-repid.
  g_s_display_profile-disvariant-handle = 'LOG'.
  g_s_display_profile-show_all = 'X'.

  "Level 1 can create method to added level1 fields
  CLEAR l_s_fcat.
  l_s_fcat-ref_table = 'BAL_S_SHOW'.
  l_s_fcat-ref_field = 'EXTNUMBER'.
  l_s_fcat-outputlen  = 40.
  APPEND l_s_fcat TO g_s_display_profile-lev1_fcat.

  CLEAR l_s_fcat.
  l_s_fcat-ref_table = 'BAL_S_EX01'.
  l_s_fcat-ref_field = 'CARRID'.
  l_s_fcat-outputlen  = 3.
  APPEND l_s_fcat TO g_s_display_profile-lev2_fcat.

  CLEAR l_s_fcat.
  l_s_fcat-ref_table = 'BAL_S_EX01'.
  l_s_fcat-ref_field = 'CONNID'.
  l_s_fcat-outputlen  = 4.
  APPEND l_s_fcat TO g_s_display_profile-lev2_fcat.

  CLEAR l_s_fcat.
  l_s_fcat-ref_table = 'BAL_S_EX01'.
  l_s_fcat-ref_field = 'ID'.
  l_s_fcat-outputlen  = 8.
  APPEND l_s_fcat TO g_s_display_profile-lev3_fcat.

  logger = zcl_logger_factory=>create_log(
            object = ''
            subobject = ''
            desc = 'Application Log Demo'
            settings  = zcl_logger_factory=>create_settings(
*               )->set_expiry_date( lv_expire
               )->set_autosave( abap_false
               )->set_must_be_kept_until_expiry( abap_true
               )->set_display_profile( EXPORTING
                 i_display_profile = g_s_display_profile
                 i_profile_name = zcl_logger_settings=>display_profile_names-self_defined
                 "i_context = ls_context
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


    data(rem) = ( l_msgno mod 2 ).
    IF   rem  = 0.
    ls_context-carrid = 'SF'. "Airline
    ELSE.
    ls_context-carrid = 'AI'.
    ENDIF.

    ls_context-connid = importance. "Connection number
    ls_context-fldate = sy-datum + importance. "Flight Date
    ls_context-id = importance + 1000 ."customer
    logger->set_default_context( ls_context ).

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
