CLASS zcl_logger_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_logger_injector.

  PUBLIC SECTION.

    "! Starts a new log.
    CLASS-METHODS create_log
      IMPORTING
        object        TYPE csequence OPTIONAL
        subobject     TYPE csequence OPTIONAL
        extnumber     TYPE csequence OPTIONAL
        context       TYPE any OPTIONAL
        settings      TYPE REF TO zif_logger_settings OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zif_logger.

    "! Reopens an already existing log.
    CLASS-METHODS open_log
      IMPORTING
        object                   TYPE csequence
        subobject                TYPE csequence
        extnumber                TYPE csequence OPTIONAL
        create_if_does_not_exist TYPE abap_bool DEFAULT abap_false
        settings                 TYPE REF TO zif_logger_settings OPTIONAL
      RETURNING
        VALUE(result)            TYPE REF TO zif_logger.

    "! Creates a settings object which can be modified. It can be pass on
    "! the creation of the logger to change its behavior.
    CLASS-METHODS create_settings
      RETURNING
        VALUE(result) TYPE REF TO zif_logger_settings.

    CLASS-METHODS create_collection
      RETURNING
        VALUE(result) TYPE REF TO zif_logger_collection.

    "! Reopens specific log instance.
    CLASS-METHODS open_log_by_db_number
      IMPORTING
        db_number     TYPE balognr
        settings      TYPE REF TO zif_logger_settings OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zif_logger.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA:
      log_logger     TYPE REF TO zif_logger,
      log_settings   TYPE REF TO zif_logger_settings,
      log_collection TYPE REF TO zif_logger_collection.

    CLASS-METHODS find_log_headers
      IMPORTING
        object        TYPE csequence OPTIONAL
        subobject     TYPE csequence OPTIONAL
        extnumber     TYPE csequence OPTIONAL
        db_number     TYPE balognr OPTIONAL
      RETURNING
        VALUE(result) TYPE balhdr_t.

    CLASS-METHODS open_log_by_header
      IMPORTING
        header        TYPE balhdr
        settings      TYPE REF TO zif_logger_settings OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zif_logger.

ENDCLASS.



CLASS zcl_logger_factory IMPLEMENTATION.


  METHOD create_collection.
    IF log_collection IS INITIAL.
      CREATE OBJECT result TYPE zcl_logger_collection.
    ELSE.
      result = log_collection.
    ENDIF.
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
    lo_log->header-extnumber = extnumber.

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
        e_log_handle = lo_log->handle
      EXCEPTIONS
        OTHERS       = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_logger.
    ENDIF.

    " BAL_LOG_CREATE will fill in some additional header data.
    " This FM updates our instance attribute to reflect that.
    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING
        i_log_handle = lo_log->handle
      IMPORTING
        e_s_log      = lo_log->header
      EXCEPTIONS
        OTHERS       = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_logger.
    ENDIF.

    result = lo_log.
  ENDMETHOD.


  METHOD create_settings.
    IF log_settings IS INITIAL.
      CREATE OBJECT result TYPE zcl_logger_settings.
    ELSE.
      result = log_settings.
    ENDIF.
  ENDMETHOD.


  METHOD find_log_headers.
    DATA: filter      TYPE bal_s_lfil,
          l_object    TYPE balobj_d,
          l_subobject TYPE balsubobj,
          l_extnumber TYPE balnrext,
          log_numbers TYPE bal_t_logn.

    l_object    = object.
    l_subobject = subobject.
    l_extnumber = extnumber.
    IF db_number IS SUPPLIED.
      INSERT db_number INTO TABLE log_numbers.
    ENDIF.

    CALL FUNCTION 'BAL_FILTER_CREATE'
      EXPORTING
        i_object       = l_object
        i_subobject    = l_subobject
        i_extnumber    = l_extnumber
        i_t_lognumber  = log_numbers
      IMPORTING
        e_s_log_filter = filter
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_logger.
    ENDIF.

    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_s_log_filter = filter
      IMPORTING
        e_t_log_header = result
      EXCEPTIONS
        log_not_found  = 0
        OTHERS         = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_logger.
    ENDIF.
  ENDMETHOD.


  METHOD open_log.
    DATA: found_headers      TYPE balhdr_t,
          most_recent_header TYPE balhdr.

    found_headers = find_log_headers( object = object subobject = subobject extnumber = extnumber ).

    IF lines( found_headers ) = 0 .
      IF create_if_does_not_exist = abap_true.
        result = create_log( object    = object
                             subobject = subobject
                             extnumber = extnumber
                             settings  = settings ).
      ENDIF.
      RETURN.
    ENDIF.

    " Delete all but the last row.
    IF lines( found_headers ) > 1.
      DELETE found_headers TO ( lines( found_headers ) - 1 ).
    ENDIF.
    READ TABLE found_headers INDEX 1 INTO most_recent_header.

    result = open_log_by_header( header = most_recent_header settings = settings ).
  ENDMETHOD.


  METHOD open_log_by_db_number.
    DATA: header      TYPE balhdr,
          log_headers TYPE balhdr_t.

    log_headers = find_log_headers( db_number = db_number ).
    IF lines( log_headers ) <> 1.
      "^Should find exactly one log since db_number is unique identifier
      RAISE EXCEPTION TYPE zcx_logger.
    ENDIF.

    READ TABLE log_headers INDEX 1 INTO header.
    result = open_log_by_header( header = header settings = settings ).
  ENDMETHOD.


  METHOD open_log_by_header.
    DATA:   log_headers  TYPE balhdr_t.
    INSERT header INTO TABLE log_headers.

    "If you call BAL_DB_LOAD for a log that is already loaded, it doesn't return its handle, so don't rely on returned data
    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header     = log_headers
      EXCEPTIONS
        no_logs_specified  = 1                " No logs specified
        log_not_found      = 2                " Log not found
        log_already_loaded = 3                " Log is already loaded
        OTHERS             = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_logger.
    ENDIF.

    DATA logger TYPE REF TO zcl_logger.
    IF log_logger IS INITIAL.
      CREATE OBJECT logger TYPE zcl_logger.
    ELSE.
      logger ?= log_logger.
    ENDIF.

    logger->handle = header-log_handle.
    logger->db_number = header-lognumber.

    IF settings IS BOUND.
      logger->settings = settings.
    ELSE.
      logger->settings = create_settings( ).
    ENDIF.

    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING
        i_log_handle = logger->handle
      IMPORTING
        e_s_log      = logger->header
      EXCEPTIONS
        OTHERS       = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_logger.
    ENDIF.

    result = logger.
  ENDMETHOD.
ENDCLASS.
