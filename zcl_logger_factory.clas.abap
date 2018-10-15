class zcl_logger_factory definition
  public
  final
  create private .

  public section.

    class-methods create_log
      importing
        !object                  type csequence optional
        !subobject               type csequence optional
        !desc                    type csequence optional
        !context                 type simple optional
        !auto_save               type abap_bool optional
        !second_db_conn          type abap_bool default abap_true
        max_exception_drill_down type i default 10
      returning
        value(r_log)             type ref to zif_logger .

    class-methods open_log
      importing
        !object                   type csequence
        !subobject                type csequence
        !desc                     type csequence optional
        !create_if_does_not_exist type abap_bool default abap_false
        !auto_save                type abap_bool optional
      returning
        value(r_log)              type ref to zif_logger .

  protected section.
  private section.
endclass.



class zcl_logger_factory implementation.

  method create_log.

*-- Added AUTO_SAVE as a parameter.  There are times when
*-- you do not want to save the log unless certain kinds
*-- of messages are put in the log.  By allowing the explicit
*-- setting of the AUTO_SAVE value, this can be done
*-- The SAVE method must be called at the end processing
*-- to save all of the log data

    data: lo_log type ref to zcl_logger.
    field-symbols <context_val> type c.

    create object lo_log.
    lo_log->header-object    = object.
    lo_log->header-subobject = subobject.
    lo_log->header-extnumber = desc.

*-- If AUTO_SAVE is not passed in, then use the old logic
*-- This is to ensure backwards compatiblilty
    if auto_save is supplied.
      lo_log->auto_save = auto_save.
    else.
      if object is not initial and subobject is not initial.
        lo_log->auto_save = abap_true.
      endif.
    endif.

* Use secondary database connection to write data to database even if
* main program does a rollback (e. g. during a dump).
    if second_db_conn = abap_true.
      lo_log->sec_connection     = abap_true.
      lo_log->sec_connect_commit = abap_true.
    endif.

* Safety limit for previous exception drill down
    lo_log->max_exception_drill_down = max_exception_drill_down.

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


  method open_log.

*-- Added AUTO_SAVE as a parameter.  There are times when
*-- you do not want to save the log unless certain kinds
*-- of messages are put in the log.  By allowing the explicit
*-- setting of the AUTO_SAVE value, this can be done
*-- The SAVE method must be called at the end processing
*-- to save all of the log data

    data: filter        type bal_s_lfil,
          desc_filter   type bal_s_extn,
          obj_filter    type bal_s_obj,
          subobj_filter type bal_s_sub,

          found_headers      type balhdr_t,
          most_recent_header type balhdr,
          handles_loaded     type bal_t_logh.
    data: lo_log             type ref to zcl_logger.

    desc_filter-option = subobj_filter-option = obj_filter-option = 'EQ'.
    desc_filter-sign   = subobj_filter-sign = obj_filter-sign = 'I'.

    obj_filter-low = object.
    append obj_filter to filter-object.
    subobj_filter-low = subobject.
    append subobj_filter to filter-subobject.
    if desc is supplied.
      desc_filter-low = desc.
      append desc_filter to filter-extnumber.
    endif.

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
*-- If AUTO_SAVE is not passed in, then use the old logic
*-- This is to ensure backwards compatiblilty
    if auto_save is not supplied.
      lo_log->auto_save = abap_true.
    else.
      lo_log->auto_save = auto_save.
    endif.

    lo_log->db_number = most_recent_header-lognumber.
    lo_log->handle    = most_recent_header-log_handle.

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

endclass.
