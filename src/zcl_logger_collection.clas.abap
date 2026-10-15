CLASS zcl_logger_collection DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_logger_factory.

  PUBLIC SECTION.

    INTERFACES zif_logger_collection.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA loggers TYPE STANDARD TABLE OF REF TO zif_logger WITH DEFAULT KEY.

    METHODS get_log_handles
      RETURNING
        VALUE(result) TYPE bal_t_logh.

    METHODS get_display_profile
      IMPORTING
        display_profile_head_size TYPE i
        display_profile_tree_size TYPE i
      RETURNING
        VALUE(result)             TYPE bal_s_prof.

ENDCLASS.



CLASS zcl_logger_collection IMPLEMENTATION.


  METHOD get_display_profile.
    CALL FUNCTION 'BAL_DSP_PROFILE_STANDARD_GET'
      IMPORTING
        e_s_display_profile = result
      EXCEPTIONS
        OTHERS              = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_logger.
    ENDIF.

    result-head_size = display_profile_head_size.
    result-tree_size = display_profile_tree_size.
    "interesting fact - I can't remember why I needed to move the hidden columns....
    IF result-mess_fcat IS NOT INITIAL.
      SORT result-mess_fcat BY no_out ASCENDING col_pos DESCENDING.
    ENDIF.
  ENDMETHOD.


  METHOD get_log_handles.
    DATA logger TYPE REF TO zif_logger.
    LOOP AT loggers INTO logger.
      INSERT logger->get_handle( ) INTO TABLE result.
    ENDLOOP.
  ENDMETHOD.


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
      RAISE EXCEPTION TYPE zcx_logger.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
