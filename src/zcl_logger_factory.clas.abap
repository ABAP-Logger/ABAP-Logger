class zcl_logger_factory definition
  public
  final
  create private .

  public section.

    "! Starts a new log.
    class-methods create_log
      importing
        object       type csequence optional
        subobject    type csequence optional
        desc         type csequence optional
        context      type simple optional
        settings     type ref to zif_logger_settings optional
      returning
        value(r_log) type ref to zif_logger .

    "! Reopens an already existing log.
    class-methods open_log
      importing
        object                   type csequence
        subobject                type csequence
        desc                     type csequence optional
        create_if_does_not_exist type abap_bool default abap_false
        settings                 type ref to zif_logger_settings optional
      returning
        value(r_log)             type ref to zif_logger .

    "! Creates a settings object which can be modified. It can be pass on
    "! the creation of the logger to change its behavior.
    class-methods create_settings
      returning
        value(r_settings) type ref to zif_logger_settings.

    class-methods create_collection
      returning
        value(r_collection) type ref to zif_logger_collection .
  protected section.
  private section.
ENDCLASS.



CLASS ZCL_LOGGER_FACTORY IMPLEMENTATION.


  method create_log.

    data: lo_log type ref to zcl_logger.
    field-symbols <context_val> type c.

    create object lo_log.
    lo_log->header-object    = object.
    lo_log->header-subobject = subobject.
    lo_log->header-extnumber = desc.

    if settings is bound.
      lo_log->settings = settings.
    else.
      create object lo_log->settings type zcl_logger_settings.
    endif.

* Special case: Logger can work without object - but then
* the data cannot be written to the database.
    if object is initial.
      lo_log->settings->set_autosave( abap_false ).
    endif.

* Use secondary database connection to write data to database even if
* main program does a rollback (e. g. during a dump).
    if lo_log->settings->get_usage_of_secondary_db_conn( ) = abap_true.
      lo_log->sec_connection     = abap_true.
      lo_log->sec_connect_commit = abap_true.
    endif.

* Set deletion date and set if log can be deleted before deletion date is reached.
    lo_log->header-aldate_del = lo_log->settings->get_expiry_date( ).
    lo_log->header-del_before = lo_log->settings->get_must_be_kept_until_expiry( ).

    if context is supplied and context is not initial.
      lo_log->header-context-tabname =
        cl_abap_typedescr=>describe_by_data( context )->get_ddic_header( )-tabname.
      assign context to <context_val> casting.
      lo_log->header-context-value = <context_val>.
    endif.

    call function 'BAL_LOG_CREATE'
      exporting
        i_s_log      = lo_log->header
      importing
        e_log_handle = lo_log->handle.

* BAL_LOG_CREATE will fill in some additional header data.
* This FM updates our instance attribute to reflect that.
    call function 'BAL_LOG_HDR_READ'
      exporting
        i_log_handle = lo_log->handle
      importing
        e_s_log      = lo_log->header.

    r_log = lo_log.

  endmethod.


  method create_settings.

    create object r_settings type zcl_logger_settings.

  endmethod.


  method open_log.

    data: filter             type bal_s_lfil,
          l_object           type balobj_d,
          l_subobject        type balsubobj,
          extnumber          type balnrext,
          found_headers      type balhdr_t,
          most_recent_header type balhdr.
    data: lo_log             type ref to zcl_logger.

    l_object = object.
    l_subobject = subobject.
    extnumber = desc.

    call function 'BAL_FILTER_CREATE'
      EXPORTING
        i_object       = l_object
        i_subobject    = l_subobject
        i_extnumber    = extnumber
      IMPORTING
        e_s_log_filter = filter.

    call function 'BAL_DB_SEARCH'
      exporting
        i_s_log_filter = filter
      importing
        e_t_log_header = found_headers
      exceptions
        log_not_found  = 1.

    if sy-subrc = 1.
      if create_if_does_not_exist = abap_true.
        r_log = zcl_logger=>new( object    = object
                                 subobject = subobject
                                 desc      = desc ).
      endif.
      return.
    endif.

* Delete all but the last row.  Keep the found_headers table this way
* so we can pass it to BAL_DB_LOAD.
    if lines( found_headers ) > 1.
      delete found_headers to ( lines( found_headers ) - 1 ).
    endif.
    read table found_headers index 1 into most_recent_header.

    create object lo_log.
    lo_log->db_number = most_recent_header-lognumber.
    lo_log->handle    = most_recent_header-log_handle.

    if settings is bound.
      lo_log->settings = settings.
    else.
      create object lo_log->settings type zcl_logger_settings.
    endif.

    call function 'BAL_DB_LOAD'
      exporting
        i_t_log_header = found_headers.

    call function 'BAL_LOG_HDR_READ'
      exporting
        i_log_handle = lo_log->handle
      importing
        e_s_log      = lo_log->header.

    r_log = lo_log.

  endmethod.

  METHOD CREATE_COLLECTION.
    CREATE OBJECT r_collection TYPE zcl_logger_collection.
  ENDMETHOD.

ENDCLASS.
