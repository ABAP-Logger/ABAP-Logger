CLASS zcl_logger_ui DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_logger_ui.

    CLASS-METHODS create_display_profile
      IMPORTING
        i_detlevel    TYPE clike OPTIONAL
        i_no_tree     TYPE clike OPTIONAL
        i_popup       TYPE clike OPTIONAL
        i_single_log  TYPE clike OPTIONAL
        i_standard    TYPE clike DEFAULT abap_true
      RETURNING
        VALUE(result) TYPE REF TO zif_logger_display_profile.

    CLASS-METHODS display_fullscreen
      IMPORTING
        logger        TYPE REF TO zif_logger
        profile       TYPE REF TO zif_logger_display_profile OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zif_logger_ui.

    CLASS-METHODS display_as_popup
      IMPORTING
        logger        TYPE REF TO zif_logger
        profile       TYPE REF TO zif_logger_display_profile OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zif_logger_ui.

    CLASS-METHODS display_in_container
      IMPORTING
        container     TYPE REF TO cl_gui_container
        logger        TYPE REF TO zif_logger
        profile       TYPE REF TO zif_logger_display_profile OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zif_logger_ui.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA control_handle TYPE balcnthndl.

ENDCLASS.



CLASS zcl_logger_ui IMPLEMENTATION.


  METHOD create_display_profile.

    CREATE OBJECT result TYPE zcl_logger_display_profile.

    result->set(
      i_detlevel   = i_detlevel
      i_no_tree    = i_no_tree
      i_popup      = i_popup
      i_single_log = i_single_log
      i_standard   = i_standard ).

  ENDMETHOD.


  METHOD display_as_popup.

    CREATE OBJECT result TYPE zcl_logger_ui.

    result->display_as_popup(
      logger  = logger
      profile = profile ).

  ENDMETHOD.


  METHOD display_fullscreen.

    CREATE OBJECT result TYPE zcl_logger_ui.

    result->display_fullscreen(
      logger  = logger
      profile = profile ).

  ENDMETHOD.


  METHOD display_in_container.

    CREATE OBJECT result TYPE zcl_logger_ui.

    result->display_in_container(
      container = container
      logger    = logger
      profile   = profile ).

  ENDMETHOD.


  METHOD zif_logger_ui~display_as_popup.

    DATA display_profile TYPE bal_s_prof.
    DATA log_handles     TYPE bal_t_logh.

    INSERT logger->get_handle( ) INTO TABLE log_handles.

    IF profile IS NOT INITIAL.
      display_profile = profile->get( ).
    ELSE.
      CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
        IMPORTING
          e_s_display_profile = display_profile
        EXCEPTIONS
          OTHERS              = 1.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING
            info = zcl_logger=>get_system_message( ).
      ENDIF.
    ENDIF.

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
      RAISE EXCEPTION TYPE zcx_logger
        EXPORTING
          info = zcl_logger=>get_system_message( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_logger_ui~display_fullscreen.

    DATA display_profile TYPE bal_s_prof.
    DATA log_handles     TYPE bal_t_logh.

    INSERT logger->get_handle( ) INTO TABLE log_handles.

    IF profile IS NOT INITIAL.
      display_profile = profile->get( ).
    ELSE.
      CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
        IMPORTING
          e_s_display_profile = display_profile
        EXCEPTIONS
          OTHERS              = 1.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING
            info = zcl_logger=>get_system_message( ).
      ENDIF.
    ENDIF.

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
      RAISE EXCEPTION TYPE zcx_logger
        EXPORTING
          info = zcl_logger=>get_system_message( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_logger_ui~display_in_container.

    DATA display_profile TYPE bal_s_prof.
    DATA log_handles     TYPE bal_t_logh.

    INSERT logger->get_handle( ) INTO TABLE log_handles.

    IF control_handle IS INITIAL.
      IF profile IS NOT INITIAL.
        display_profile = profile->get( ).
      ELSE.
        CALL FUNCTION 'BAL_DSP_PROFILE_NO_TREE_GET'
          IMPORTING
            e_s_display_profile = display_profile
          EXCEPTIONS
            OTHERS              = 1.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_logger
            EXPORTING
              info = zcl_logger=>get_system_message( ).
        ENDIF.
      ENDIF.
      CALL FUNCTION 'BAL_CNTL_CREATE'
        EXPORTING
          i_container          = container
          i_s_display_profile  = display_profile
          i_t_log_handle       = log_handles
        IMPORTING
          e_control_handle     = control_handle
        EXCEPTIONS
          profile_inconsistent = 1
          internal_error       = 2
          OTHERS               = 3.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING
            info = zcl_logger=>get_system_message( ).
      ENDIF.
    ELSE.
      CALL FUNCTION 'BAL_CNTL_REFRESH'
        EXPORTING
          i_control_handle  = control_handle
          i_t_log_handle    = log_handles
        EXCEPTIONS
          control_not_found = 1
          internal_error    = 2
          OTHERS            = 3.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING
            info = zcl_logger=>get_system_message( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
