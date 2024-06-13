CLASS zcl_logger_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_logger_injector.

  PUBLIC SECTION.

    "! Starts a new log.
    CLASS-METHODS create_log
      IMPORTING
        object       TYPE csequence OPTIONAL
        subobject    TYPE csequence OPTIONAL
        desc         TYPE csequence OPTIONAL
        context      TYPE any OPTIONAL
        settings     TYPE REF TO zif_logger_settings OPTIONAL
      RETURNING
        VALUE(r_log) TYPE REF TO zif_logger.

    "! Reopens an already existing log.
    CLASS-METHODS open_log
      IMPORTING
        object                   TYPE csequence
        subobject                TYPE csequence
        desc                     TYPE csequence OPTIONAL
        create_if_does_not_exist TYPE abap_bool DEFAULT abap_false
        settings                 TYPE REF TO zif_logger_settings OPTIONAL
      RETURNING
        VALUE(r_log)             TYPE REF TO zif_logger.

    "! Creates a settings object which can be modified. It can be pass on
    "! the creation of the logger to change its behavior.
    CLASS-METHODS create_settings
      RETURNING
        VALUE(r_settings) TYPE REF TO zif_logger_settings.

    CLASS-METHODS create_collection
      RETURNING
        VALUE(r_collection) TYPE REF TO zif_logger_collection.

    CLASS-METHODS create_display_profile
      IMPORTING
        i_detlevel               TYPE clike OPTIONAL
        i_no_tree                TYPE clike OPTIONAL
        i_popup                  TYPE clike OPTIONAL
        i_single_log             TYPE clike OPTIONAL
        i_standard               TYPE clike DEFAULT abap_true
      RETURNING
        VALUE(r_display_profile) TYPE REF TO zif_logger_display_profile.


    "! Reopens specific log instance.
    CLASS-METHODS open_log_by_db_number
      IMPORTING
        db_number        TYPE balognr
        settings         TYPE REF TO zif_logger_settings OPTIONAL
      RETURNING
        VALUE(logger_if) TYPE REF TO zif_logger.


  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA:
      log_logger          TYPE REF TO zif_logger,
      log_settings        TYPE REF TO zif_logger_settings,
      log_collection      TYPE REF TO zif_logger_collection,
      log_display_profile TYPE REF TO zif_logger_display_profile.

ENDCLASS.



CLASS zcl_logger_factory IMPLEMENTATION.


  METHOD create_collection.
    IF log_collection IS INITIAL.
      CREATE OBJECT r_collection TYPE zcl_logger_collection.
    ELSE.
      r_collection = log_collection.
    ENDIF.
  ENDMETHOD.


  METHOD create_display_profile.
    IF log_display_profile IS INITIAL.
      CREATE OBJECT r_display_profile TYPE zcl_logger_display_profile.
    ELSE.
      r_display_profile = log_display_profile.
    ENDIF.

    r_display_profile->set(
      i_detlevel    = i_detlevel
      i_no_tree     = i_no_tree
      i_popup       = i_popup
      i_single_log  = i_single_log
      i_standard    = i_standard ).
  ENDMETHOD.


  METHOD create_log.
    FIELD-SYMBOLS <context_val> TYPE c.

    DATA lo_log TYPE REF TO zcl_logger.

    IF log_logger IS INITIAL.
      CREATE OBJECT lo_log TYPE zcl_logger.
    ELSE.
      lo_log ?= log_logger.
    ENDIF.

    lo_log->header-object    = object.
    lo_log->header-subobject = subobject.
    lo_log->header-extnumber = desc.

    IF settings IS BOUND.
      lo_log->settings = settings.
    ELSE.
      lo_log->settings = create_settings( ).
    ENDIF.

    " Special case: Logger can work without object - but then the data cannot be written to the database.
    IF object IS INITIAL.
      lo_log->settings->set_autosave( abap_false ).
    ENDIF.

    " Set deletion date and set if log can be deleted before deletion date is reached.
    lo_log->header-aldate_del = lo_log->settings->get_expiry_date( ).
    lo_log->header-del_before = lo_log->settings->get_must_be_kept_until_expiry( ).

    IF context IS SUPPLIED AND context IS NOT INITIAL.
      lo_log->header-context-tabname =
        cl_abap_typedescr=>describe_by_data( context )->get_ddic_header( )-tabname.
      ASSIGN context TO <context_val> CASTING.
      lo_log->header-context-value = <context_val>.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = lo_log->header
      IMPORTING
        e_log_handle = lo_log->handle.

    " BAL_LOG_CREATE will fill in some additional header data.
    " This FM updates our instance attribute to reflect that.
    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING
        i_log_handle = lo_log->handle
      IMPORTING
        e_s_log      = lo_log->header.

    r_log = lo_log.
  ENDMETHOD.


  METHOD create_settings.
    IF log_settings IS INITIAL.
      CREATE OBJECT r_settings TYPE zcl_logger_settings.
    ELSE.
      r_settings = log_settings.
    ENDIF.
  ENDMETHOD.


  METHOD open_log.
    DATA: filter             TYPE bal_s_lfil,
          l_object           TYPE balobj_d,
          l_subobject        TYPE balsubobj,
          extnumber          TYPE balnrext,
          found_headers      TYPE balhdr_t,
          most_recent_header TYPE balhdr.

    l_object    = object.
    l_subobject = subobject.
    extnumber   = desc.

    CALL FUNCTION 'BAL_FILTER_CREATE'
      EXPORTING
        i_object       = l_object
        i_subobject    = l_subobject
        i_extnumber    = extnumber
      IMPORTING
        e_s_log_filter = filter.

    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_s_log_filter = filter
      IMPORTING
        e_t_log_header = found_headers
      EXCEPTIONS
        log_not_found  = 1.

    IF sy-subrc = 1.
      IF create_if_does_not_exist = abap_true.
        r_log = create_log( object    = object
                            subobject = subobject
                            desc      = desc
                            settings  = settings ).
      ENDIF.
      RETURN.
    ENDIF.

    " Delete all but the last row.  Keep the found_headers table this way so we can pass it to BAL_DB_LOAD.
    IF lines( found_headers ) > 1.
      DELETE found_headers TO ( lines( found_headers ) - 1 ).
    ENDIF.
    READ TABLE found_headers INDEX 1 INTO most_recent_header.

    DATA lo_log TYPE REF TO zcl_logger.

    IF log_logger IS INITIAL.
      CREATE OBJECT lo_log TYPE zcl_logger.
    ELSE.
      lo_log ?= log_logger.
    ENDIF.

    lo_log->db_number = most_recent_header-lognumber.
    lo_log->handle    = most_recent_header-log_handle.

    IF settings IS BOUND.
      lo_log->settings = settings.
    ELSE.
      lo_log->settings = create_settings( ).
    ENDIF.

    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header = found_headers.

    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING
        i_log_handle = lo_log->handle
      IMPORTING
        e_s_log      = lo_log->header.

    r_log = lo_log.
  ENDMETHOD.

  METHOD open_log_by_db_number.
    "Load log into SAP and get it's handle
    DATA: log_numbers TYPE bal_t_logn,
          log_handles TYPE bal_t_logh.
    INSERT db_number INTO TABLE log_numbers.

    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_lognumber      = log_numbers
      IMPORTING
        e_t_log_handle     = log_handles
      EXCEPTIONS
        no_logs_specified  = 1                " No logs specified
        log_not_found      = 2                " Log not found
        log_already_loaded = 3                " Log is already loaded
        OTHERS             = 4.
    IF sy-subrc <> 0 OR lines( log_handles ) <> 1.
      "^Should find exactly one log, otherwise it's either wrong number or some internal SAP problem
      RAISE EXCEPTION TYPE zcx_logger.
    ENDIF.

    "Create log object
    DATA logger TYPE REF TO zcl_logger.
    IF log_logger IS INITIAL.
      CREATE OBJECT logger TYPE zcl_logger.
    ELSE.
      logger ?= log_logger.
    ENDIF.

    DATA handle TYPE balloghndl.
    READ TABLE log_handles INDEX 1 INTO handle.
    logger->handle = handle.
    logger->db_number = db_number.

    IF settings IS BOUND.
      logger->settings = settings.
    ELSE.
      logger->settings = create_settings( ).
    ENDIF.

    "Get log header
    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING
        i_log_handle = logger->handle
      IMPORTING
        e_s_log      = logger->header.

    logger_if = logger.
  ENDMETHOD.

ENDCLASS.
