*----------------------------------------------------------------------*
*       CLASS ZCL_LOGGER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class zcl_logger definition
  public
  create private .

  public section.
    type-pools abap .

*"* public components of class ZCL_LOGGER
*"* do not include other source files here!!!
    data header type bal_s_log read-only .
    data handle type balloghndl read-only .
    data db_number type balognr read-only .

    class-methods new
      importing
        !object         type csequence optional
        !subobject      type csequence optional
        !desc           type csequence optional
        !context        type simple optional
        !auto_save      type abap_bool optional
        !second_db_conn type abap_bool default abap_true
      returning
        value(r_log)    type ref to zcl_logger .
    class-methods open
      importing
        !object                   type csequence
        !subobject                type csequence
        !desc                     type csequence optional
        !create_if_does_not_exist type abap_bool default abap_false
        !auto_save                type abap_bool optional
      returning
        value(r_log)              type ref to zcl_logger .
    methods add
      importing
        !obj_to_log    type any optional
        !context       type simple optional
        !callback_form type csequence optional
        !callback_prog type csequence optional
        !callback_fm   type csequence optional
        !type          type symsgty optional
        !importance    type balprobcl optional
          preferred parameter obj_to_log
      returning
        value(self)    type ref to zcl_logger .
    methods a
      importing
        !obj_to_log    type any optional
        !context       type simple optional
        !callback_form type csequence optional
        !callback_prog type csequence optional
        !callback_fm   type csequence optional
        !importance    type balprobcl optional
          preferred parameter obj_to_log
      returning
        value(self)    type ref to zcl_logger .
    methods e
      importing
        !obj_to_log    type any optional
        !context       type simple optional
        !callback_form type csequence optional
        !callback_prog type csequence optional
        !callback_fm   type csequence optional
        !importance    type balprobcl optional
          preferred parameter obj_to_log
      returning
        value(self)    type ref to zcl_logger .
    methods w
      importing
        !obj_to_log    type any optional
        !context       type simple optional
        !callback_form type csequence optional
        !callback_prog type csequence optional
        !callback_fm   type csequence optional
        !importance    type balprobcl optional
          preferred parameter obj_to_log
      returning
        value(self)    type ref to zcl_logger .
    methods i
      importing
        !obj_to_log    type any optional
        !context       type simple optional
        !callback_form type csequence optional
        !callback_prog type csequence optional
        !callback_fm   type csequence optional
        !importance    type balprobcl optional
          preferred parameter obj_to_log
      returning
        value(self)    type ref to zcl_logger .
    methods s
      importing
        !obj_to_log    type any optional
        !context       type simple optional
        !callback_form type csequence optional
        !callback_prog type csequence optional
        !callback_fm   type csequence optional
        !importance    type balprobcl optional
          preferred parameter obj_to_log
      returning
        value(self)    type ref to zcl_logger .
    methods popup .
    methods fullscreen .
    methods export_to_table
      returning
        value(rt_bapiret) type bapirettab .
    methods get_autosave
      returning
        value(auto_save) type abap_bool .
    methods set_autosave
      importing
        !auto_save type abap_bool .
    methods save .
  protected section.
*"* protected components of class ZCL_LOGGER
*"* do not include other source files here!!!
  private section.

* Local type for hrpad_message as it is not available in an ABAP Development System
    types: begin of hrpad_message_field_list_alike,
             scrrprfd type scrrprfd.
    types: end of hrpad_message_field_list_alike.

    types: begin of hrpad_message_alike,
             cause(32)    type c,              "original: hrpad_message_cause
             detail_level type ballevel.
        include type symsg .
    types: field_list type standard table of hrpad_message_field_list_alike
           with non-unique key scrrprfd.
    types: end of hrpad_message_alike.

*"* private components of class ZCL_LOGGER
*"* do not include other source files here!!!
    data auto_save type abap_bool .
    data sec_connection type abap_bool .
    data sec_connect_commit type abap_bool .
ENDCLASS.



CLASS ZCL_LOGGER IMPLEMENTATION.


  method a.
    self = add(
      obj_to_log    = obj_to_log
      context       = context
      callback_form = callback_form
      callback_prog = callback_prog
      callback_fm   = callback_fm
      type          = 'A'
      importance    = importance ).
  endmethod.


  method add.

    data: detailed_msg      type bal_s_msg,
          free_text_msg     type char200,
          ctx_type          type ref to cl_abap_typedescr,
          ctx_ddic_header   type x030l,
          msg_type          type ref to cl_abap_typedescr,
          msg_table_type    type ref to cl_abap_tabledescr,
          exception_data    type bal_s_exc,
          log_numbers       type bal_t_lgnm,
          log_handles       type bal_t_logh,
          log_number        type bal_s_lgnm,
          formatted_context type bal_s_cont,
          formatted_params  type bal_s_parm.

    field-symbols: <table_of_messages> type any table,
                   <message_line>      type any,
                   <bapi_msg>          type bapiret2,
                   <bdc_msg>           type bdcmsgcoll,
                   <hrpad_msg>         type hrpad_message_alike,
                   <context_val>       type any.

    if context is not initial.
      assign context to <context_val>.
      formatted_context-value = <context_val>.
      ctx_type = cl_abap_typedescr=>describe_by_data( context ).

      call method ctx_type->get_ddic_header
        receiving
          p_header     = ctx_ddic_header
        exceptions
          not_found    = 1
          no_ddic_type = 2
          others       = 3.
      if sy-subrc = 0.
        formatted_context-tabname = ctx_ddic_header-tabname.
      endif.
    endif.

    if callback_fm is not initial.
      formatted_params-callback-userexitf = callback_fm.
      formatted_params-callback-userexitp = callback_prog.
      formatted_params-callback-userexitt = 'F'.
    elseif callback_form is not initial.
      formatted_params-callback-userexitf = callback_form.
      formatted_params-callback-userexitp = callback_prog.
      formatted_params-callback-userexitt = ' '.
    endif.

    msg_type = cl_abap_typedescr=>describe_by_data( obj_to_log ).

    if obj_to_log is initial.
      detailed_msg-msgty = sy-msgty.
      detailed_msg-msgid = sy-msgid.
      detailed_msg-msgno = sy-msgno.
      detailed_msg-msgv1 = sy-msgv1.
      detailed_msg-msgv2 = sy-msgv2.
      detailed_msg-msgv3 = sy-msgv3.
      detailed_msg-msgv4 = sy-msgv4.
    elseif msg_type->type_kind = cl_abap_typedescr=>typekind_oref.
      exception_data-exception = obj_to_log.
      exception_data-msgty = type.
      exception_data-probclass = importance.
    elseif msg_type->type_kind = cl_abap_typedescr=>typekind_table.
      assign obj_to_log to <table_of_messages>.
      loop at <table_of_messages> assigning <message_line>.
        add( <message_line> ).
      endloop.
      return.
    elseif msg_type->absolute_name = '\TYPE=BAPIRET2'.
      assign obj_to_log to <bapi_msg>.
      detailed_msg-msgty = <bapi_msg>-type.
      detailed_msg-msgid = <bapi_msg>-id.
      detailed_msg-msgno = <bapi_msg>-number.
      detailed_msg-msgv1 = <bapi_msg>-message_v1.
      detailed_msg-msgv2 = <bapi_msg>-message_v2.
      detailed_msg-msgv3 = <bapi_msg>-message_v3.
      detailed_msg-msgv4 = <bapi_msg>-message_v4.
    elseif msg_type->absolute_name = '\TYPE=BDCMSGCOLL'.
      assign obj_to_log to <bdc_msg>.
      detailed_msg-msgty = <bdc_msg>-msgtyp.
      detailed_msg-msgid = <bdc_msg>-msgid.
      detailed_msg-msgno = <bdc_msg>-msgnr.
      detailed_msg-msgv1 = <bdc_msg>-msgv1.
      detailed_msg-msgv2 = <bdc_msg>-msgv2.
      detailed_msg-msgv3 = <bdc_msg>-msgv3.
      detailed_msg-msgv4 = <bdc_msg>-msgv4.
    elseif msg_type->absolute_name = '\TYPE=HRPAD_MESSAGE'.
      assign obj_to_log to <hrpad_msg>.
      detailed_msg-msgty = <hrpad_msg>-msgty.
      detailed_msg-msgid = <hrpad_msg>-msgid.
      detailed_msg-msgno = <hrpad_msg>-msgno.
      detailed_msg-msgv1 = <hrpad_msg>-msgv1.
      detailed_msg-msgv2 = <hrpad_msg>-msgv2.
      detailed_msg-msgv3 = <hrpad_msg>-msgv3.
      detailed_msg-msgv4 = <hrpad_msg>-msgv4.
    else.
      free_text_msg = obj_to_log.
    endif.

    if free_text_msg is not initial.
      call function 'BAL_LOG_MSG_ADD_FREE_TEXT'
        exporting
          i_log_handle = me->handle
          i_msgty      = type
          i_probclass  = importance
          i_text       = free_text_msg
          i_s_context  = formatted_context
          i_s_params   = formatted_params.
    elseif exception_data is not initial.
      call function 'BAL_LOG_EXCEPTION_ADD'
        exporting
          i_log_handle = me->handle
          i_s_exc      = exception_data.
    elseif detailed_msg is not initial.
      detailed_msg-context = formatted_context.
      detailed_msg-params = formatted_params.
      detailed_msg-probclass = importance.
      call function 'BAL_LOG_MSG_ADD'
        exporting
          i_log_handle = me->handle
          i_s_msg      = detailed_msg.
    endif.

    if auto_save = abap_true.
      append me->handle to log_handles.
      call function 'BAL_DB_SAVE'
        exporting
          i_t_log_handle       = log_handles
          i_2th_connection     = me->sec_connection
          i_2th_connect_commit = me->sec_connect_commit
        importing
          e_new_lognumbers     = log_numbers.
      if me->db_number is initial.
        read table log_numbers index 1 into log_number.
        me->db_number = log_number-lognumber.
      endif.
    endif.

    self = me.
  endmethod.


  method e.
    self = add(
      obj_to_log    = obj_to_log
      context       = context
      callback_form = callback_form
      callback_prog = callback_prog
      callback_fm   = callback_fm
      type          = 'E'
      importance    = importance ).
  endmethod.


  method export_to_table.
    data: log_handle      type bal_t_logh,
          message_handles type bal_t_msgh,
          message         type bal_s_msg,
          bapiret2        type bapiret2.

    field-symbols <msg_handle> type balmsghndl.

    insert handle into table log_handle.

    call function 'BAL_GLB_SEARCH_MSG'
      exporting
        i_t_log_handle = log_handle
      importing
        e_t_msg_handle = message_handles
      exceptions
        msg_not_found  = 0.

    loop at message_handles assigning <msg_handle>.
      call function 'BAL_LOG_MSG_READ'
        exporting
          i_s_msg_handle = <msg_handle>
        importing
          e_s_msg        = message
        exceptions
          others         = 3.
      if sy-subrc is initial.
        message id message-msgid
                type message-msgty
                number message-msgno
                into bapiret2-message
                with message-msgv1 message-msgv2 message-msgv3 message-msgv4.

        bapiret2-type          = message-msgty.
        bapiret2-id            = message-msgid.
        bapiret2-number        = message-msgno.
        bapiret2-log_no        = <msg_handle>-log_handle. "last 2 chars missing!!
        bapiret2-log_msg_no    = <msg_handle>-msgnumber.
        bapiret2-message_v1    = message-msgv1.
        bapiret2-message_v2    = message-msgv2.
        bapiret2-message_v3    = message-msgv3.
        bapiret2-message_v4    = message-msgv4.
        bapiret2-system        = sy-sysid.
        append bapiret2 to rt_bapiret.
      endif.
    endloop.

  endmethod.


  method fullscreen.

    data: profile        type bal_s_prof,
          lt_log_handles type bal_t_logh.

    append me->handle to lt_log_handles.

    call function 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
      importing
        e_s_display_profile = profile.

    call function 'BAL_DSP_LOG_DISPLAY'
      exporting
        i_s_display_profile = profile
        i_t_log_handle      = lt_log_handles.

  endmethod.


  method get_autosave.

    auto_save = me->auto_save.

  endmethod.


  method i.
    self = add(
      obj_to_log    = obj_to_log
      context       = context
      callback_form = callback_form
      callback_prog = callback_prog
      callback_fm   = callback_fm
      type          = 'I'
      importance    = importance ).
  endmethod.


  method new.

*-- Added AUTO_SAVE as a parameter.  There are times when
*-- you do not want to save the log unless certain kinds
*-- of messages are put in the log.  By allowing the explicit
*-- setting of the AUTO_SAVE value, this can be done
*-- The SAVE method must be called at the end processing
*-- to save all of the log data

    field-symbols <context_val> type c.

    create object r_log.
    r_log->header-object = object.
    r_log->header-subobject = subobject.
    r_log->header-extnumber = desc.

*-- If AUTO_SAVE is not passed in, then use the old logic
*-- This is to ensure backwards compatiblilty
    if auto_save is supplied.
      r_log->auto_save = auto_save.
    else.
      if object is not initial and subobject is not initial.
        r_log->auto_save = abap_true.
      endif.
    endif.

* Use secondary database connection to write data to database even if
* main program does a rollback (e. g. during a dump).
    if second_db_conn = abap_true.
      r_log->sec_connection = abap_true.
      r_log->sec_connect_commit = abap_true.
    endif.

    if context is supplied and context is not initial.
      r_log->header-context-tabname =
        cl_abap_typedescr=>describe_by_data( context )->get_ddic_header( )-tabname.
      assign context to <context_val> casting.
      r_log->header-context-value = <context_val>.
    endif.

    call function 'BAL_LOG_CREATE'
      exporting
        i_s_log      = r_log->header
      importing
        e_log_handle = r_log->handle.

* BAL_LOG_CREATE will fill in some additional header data.
* This FM updates our instance attribute to reflect that.
    call function 'BAL_LOG_HDR_READ'
      exporting
        i_log_handle = r_log->handle
      importing
        e_s_log      = r_log->header.

  endmethod.


  method open.

*-- Added AUTO_SAVE as a parameter.  There are times when
*-- you do not want to save the log unless certain kinds
*-- of messages are put in the log.  By allowing the explicit
*-- setting of the AUTO_SAVE value, this can be done
*-- The SAVE method must be called at the end processing
*-- to save all of the log data

    data: filter             type bal_s_lfil,
          desc_filter        type bal_s_extn,
          obj_filter         type bal_s_obj,
          subobj_filter      type bal_s_sub,

          found_headers      type balhdr_t,
          most_recent_header type balhdr,
          handles_loaded     type bal_t_logh.

    desc_filter-option = subobj_filter-option = obj_filter-option = 'EQ'.
    desc_filter-sign = subobj_filter-sign = obj_filter-sign = 'I'.

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

    create object r_log.
*-- If AUTO_SAVE is not passed in, then use the old logic
*-- This is to ensure backwards compatiblilty
    if auto_save is not supplied.
      r_log->auto_save = abap_true.
    else.
      r_log->auto_save = auto_save.
    endif.

    r_log->db_number = most_recent_header-lognumber.
    r_log->handle = most_recent_header-log_handle.

    call function 'BAL_DB_LOAD'
      exporting
        i_t_log_header = found_headers.

    call function 'BAL_LOG_HDR_READ'
      exporting
        i_log_handle = r_log->handle
      importing
        e_s_log      = r_log->header.

  endmethod.


  method popup.
* See SBAL_DEMO_04_POPUP for ideas

    data: profile type bal_s_prof.
    call function 'BAL_DSP_PROFILE_POPUP_GET'
      importing
        e_s_display_profile = profile.

    call function 'BAL_DSP_LOG_DISPLAY'
      exporting
        i_s_display_profile = profile
        i_t_log_handle      = me->handle.

  endmethod.


  method s.
    self = add(
      obj_to_log    = obj_to_log
      context       = context
      callback_form = callback_form
      callback_prog = callback_prog
      callback_fm   = callback_fm
      type          = 'S'
      importance    = importance ).
  endmethod.


  method save.
*--------------------------------------------------------------------*
* Method to save the log on demand.  Intended to be called at the    *
*  end of the log processing so that logs can be saved depending     *
*  on other criteria, like the existance of error messages.          *
*  If there are no error messages, it may not be desireable to save  *
*  a log                                                             *
*--------------------------------------------------------------------*


    data:
      log_handles type bal_t_logh,
      log_numbers type bal_t_lgnm,
      log_number  type bal_s_lgnm.

    check auto_save = abap_false.

    append me->handle to log_handles.
    call function 'BAL_DB_SAVE'
      exporting
        i_t_log_handle       = log_handles
        i_2th_connection     = me->sec_connection
        i_2th_connect_commit = me->sec_connect_commit
      importing
        e_new_lognumbers     = log_numbers.
    if me->db_number is initial.
      read table log_numbers index 1 into log_number.
      me->db_number = log_number-lognumber.
    endif.

  endmethod.


  method set_autosave.

    me->auto_save = auto_save.

  endmethod.


  method w.
    self = add(
      obj_to_log    = obj_to_log
      context       = context
      callback_form = callback_form
      callback_prog = callback_prog
      callback_fm   = callback_fm
      type          = 'W'
      importance    = importance ).
  endmethod.
ENDCLASS.
