CLASS zcl_logger_collection DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_logger_factory.

  PUBLIC SECTION.
    INTERFACES: zif_logger_collection.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      loggers TYPE STANDARD TABLE OF REF TO zif_logger WITH DEFAULT KEY.
    METHODS get_log_handles
      RETURNING
        VALUE(r_return) TYPE bal_t_logh.
    METHODS get_display_profile
      IMPORTING
        display_profile_head_size TYPE i
        display_profile_tree_size TYPE i
      RETURNING
        VALUE(r_return)           TYPE bal_s_prof.

ENDCLASS.

CLASS zcl_logger_collection IMPLEMENTATION.

  METHOD zif_logger_collection~add_logger.
    APPEND logger TO loggers.
  ENDMETHOD.

  METHOD zif_logger_collection~display_logs.
    DATA display_profile TYPE bal_s_prof.
    display_profile = get_display_profile(
      display_profile_head_size = display_profile_head_size
      display_profile_tree_size = display_profile_tree_size ).

    zif_logger_collection~display_logs_using_profile( display_profile ).
  ENDMETHOD.

  METHOD zif_logger_collection~display_logs_using_profile.
    DATA log_handles TYPE bal_t_logh.
    log_handles = get_log_handles( ).

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile  = display_profile
        i_t_log_handle       = log_handles
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
      "Todo "Raise Exception Error?
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE sy-msgty.
    ENDIF.
  ENDMETHOD.

  METHOD get_log_handles.
    DATA logger TYPE REF TO zif_logger.
    LOOP AT loggers INTO logger.
      INSERT logger->handle INTO TABLE r_return.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_display_profile.
    CALL FUNCTION 'BAL_DSP_PROFILE_STANDARD_GET'
      IMPORTING
        e_s_display_profile = r_return.

    r_return-head_size = display_profile_head_size.
    r_return-tree_size = display_profile_tree_size.
    "interesting fact - I can't remember why I needed to move the hidden columns....
    IF r_return-mess_fcat IS NOT INITIAL.
      SORT r_return-mess_fcat BY no_out ASCENDING col_pos DESCENDING.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
