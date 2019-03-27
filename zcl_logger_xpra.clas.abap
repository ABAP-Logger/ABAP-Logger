"#autoformat (Pretty Print class automatically with abapCI plug-in (indent + all lower case)
"! <p class="shorttext synchronized" lang="en">Logger for XPRA reports - add messages to transport log</p>
class zcl_logger_xpra definition
  public
  create private
  global friends zcl_logger_factory .

  public section.
    interfaces zif_logger_xpra.

  protected section.
* Local type for hrpad_message as it is not available in an ABAP Development System
    types: begin of hrpad_message_field_list_alike,
             scrrprfd type scrrprfd.
    types: end of hrpad_message_field_list_alike.

    types: begin of hrpad_message_alike,
             cause(32)    type c,                          "original: hrpad_message_cause
             detail_level type ballevel.
        include type symsg .
    types: field_list type standard table of hrpad_message_field_list_alike
           with non-unique key scrrprfd.
    types: end of hrpad_message_alike.

    "! <p class="shorttext synchronized" lang="en">Create message from text string</p>
    "!
    "! @parameter message_text | <p class="shorttext synchronized" lang="en">Text string</p>
    "! @parameter rs_message   | <p class="shorttext synchronized" lang="en">Message for log</p>
    methods
      message_from_string importing !i_message_text   type csequence
                          returning value(rs_message) type sprot_u.
    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter is_message | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rs_message | <p class="shorttext synchronized" lang="en"></p>
    methods
      message_from_bal_s_msg importing !is_message       type bal_s_msg
                             returning value(rs_message) type sprot_u.

  private section.
    constants c_new_object type sprot_u-newobj value 'X'.

    data  auto_save                type abap_bool .
    data  sec_connection           type abap_bool .
    data  sec_connect_commit       type abap_bool .
    data: max_exception_drill_down type i.

    methods:
      drill_down_into_exception importing exception                      type ref to cx_root
                                          type                           type symsgty optional
                                          importance                     type balprobcl optional
                                returning value(rt_exception_data_table) type tty_exception_data.


    data handle type balloghndl.
    data db_number type balognr.
    data header type bal_s_log.

    data mt_messages  type standard table of sprot_u with default key.
    data m_new_object type sprot_u-newobj.
endclass.



class zcl_logger_xpra implementation.
  method zif_logger_xpra~start_new_section.
    m_new_object = c_new_object.
  endmethod.


  method zif_logger~a.
    self = zif_logger~add( obj_to_log    = obj_to_log
                           context       = context
                           callback_form = callback_form
                           callback_prog = callback_prog
                           callback_fm   = callback_fm
                           type          = 'A'
                           importance    = importance ).
  endmethod.


  method zif_logger~e.
    self = zif_logger~add( obj_to_log    = obj_to_log
                           context       = context
                           callback_form = callback_form
                           callback_prog = callback_prog
                           callback_fm   = callback_fm
                           type          = 'E'
                           importance    = importance ).
  endmethod.


  method zif_logger~w.
    self = zif_logger~add( obj_to_log    = obj_to_log
                           context       = context
                           callback_form = callback_form
                           callback_prog = callback_prog
                           callback_fm   = callback_fm
                           type          = 'W'
                           importance    = importance ).
  endmethod.


  method zif_logger~i.
    self = zif_logger~add( obj_to_log    = obj_to_log
                           context       = context
                           callback_form = callback_form
                           callback_prog = callback_prog
                           callback_fm   = callback_fm
                           type          = 'I'
                           importance    = importance ).
  endmethod.


  method zif_logger~s.
    self = zif_logger~add( obj_to_log    = obj_to_log
                           context       = context
                           callback_form = callback_form
                           callback_prog = callback_prog
                           callback_fm   = callback_fm
                           type          = 'S'
                           importance    = importance ).
  endmethod.


  method zif_logger~add.
    data: detailed_msg         type bal_s_msg,
          exception_data_table type tty_exception_data,
          free_text_msg        type char200,
          ctx_type             type ref to cl_abap_typedescr,
          ctx_ddic_header      type x030l,
          msg_type             type ref to cl_abap_typedescr,
          msg_table_type       type ref to cl_abap_tabledescr,
          log_numbers          type bal_t_lgnm,
          log_handles          type bal_t_logh,
          log_number           type bal_s_lgnm,
          formatted_context    type bal_s_cont,
          formatted_params     type bal_s_parm.

    field-symbols: <table_of_messages> type any table,
                   <message_line>      type any,
                   <bapiret1_msg>      type bapiret1,
                   <bapi_msg>          type bapiret2,
                   <bapi_coru_msg>     type bapi_coru_return,
                   <bapi_order_msg>    type bapi_order_return,
                   <bdc_msg>           type bdcmsgcoll,
                   <hrpad_msg>         type hrpad_message_alike,
                   <rcomp_msg>         type rcomp,
                   <context_val>       type any.

    if context is not initial.
      assign context to <context_val>.
      formatted_context-value = <context_val>.
      ctx_type                = cl_abap_typedescr=>describe_by_data( context ).

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

    if obj_to_log is not supplied.
      detailed_msg-msgty = sy-msgty.
      detailed_msg-msgid = sy-msgid.
      detailed_msg-msgno = sy-msgno.
      detailed_msg-msgv1 = sy-msgv1.
      detailed_msg-msgv2 = sy-msgv2.
      detailed_msg-msgv3 = sy-msgv3.
      detailed_msg-msgv4 = sy-msgv4.
    elseif msg_type->type_kind = cl_abap_typedescr=>typekind_oref.
      exception_data_table = me->drill_down_into_exception(
          exception   = obj_to_log
          type        = type
          importance  = importance
          ).
    elseif msg_type->type_kind = cl_abap_typedescr=>typekind_table.
      assign obj_to_log to <table_of_messages>.
      loop at <table_of_messages> assigning <message_line>.
        zif_logger~add( <message_line> ).
      endloop.
      return.
    elseif msg_type->absolute_name = '\TYPE=BAPIRET1'.
      assign obj_to_log to <bapiret1_msg>.
      detailed_msg-msgty = <bapiret1_msg>-type.
      detailed_msg-msgid = <bapiret1_msg>-id.
      detailed_msg-msgno = <bapiret1_msg>-number.
      detailed_msg-msgv1 = <bapiret1_msg>-message_v1.
      detailed_msg-msgv2 = <bapiret1_msg>-message_v2.
      detailed_msg-msgv3 = <bapiret1_msg>-message_v3.
      detailed_msg-msgv4 = <bapiret1_msg>-message_v4.
    elseif msg_type->absolute_name = '\TYPE=BAPIRET2'.
      assign obj_to_log to <bapi_msg>.
      detailed_msg-msgty = <bapi_msg>-type.
      detailed_msg-msgid = <bapi_msg>-id.
      detailed_msg-msgno = <bapi_msg>-number.
      detailed_msg-msgv1 = <bapi_msg>-message_v1.
      detailed_msg-msgv2 = <bapi_msg>-message_v2.
      detailed_msg-msgv3 = <bapi_msg>-message_v3.
      detailed_msg-msgv4 = <bapi_msg>-message_v4.
    elseif msg_type->absolute_name = '\TYPE=BAPI_CORU_RETURN'.
      assign obj_to_log to <bapi_coru_msg>.
      detailed_msg-msgty = <bapi_coru_msg>-type.
      detailed_msg-msgid = <bapi_coru_msg>-id.
      detailed_msg-msgno = <bapi_coru_msg>-number.
      detailed_msg-msgv1 = <bapi_coru_msg>-message_v1.
      detailed_msg-msgv2 = <bapi_coru_msg>-message_v2.
      detailed_msg-msgv3 = <bapi_coru_msg>-message_v3.
      detailed_msg-msgv4 = <bapi_coru_msg>-message_v4.
    elseif msg_type->absolute_name = '\TYPE=BAPI_ORDER_RETURN'.
      assign obj_to_log to <bapi_order_msg>.
      detailed_msg-msgty = <bapi_order_msg>-type.
      detailed_msg-msgid = <bapi_order_msg>-id.
      detailed_msg-msgno = <bapi_order_msg>-number.
      detailed_msg-msgv1 = <bapi_order_msg>-message_v1.
      detailed_msg-msgv2 = <bapi_order_msg>-message_v2.
      detailed_msg-msgv3 = <bapi_order_msg>-message_v3.
      detailed_msg-msgv4 = <bapi_order_msg>-message_v4.
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
    elseif msg_type->absolute_name = '\TYPE=RCOMP'.
      assign obj_to_log to <rcomp_msg>.
      detailed_msg-msgty = <rcomp_msg>-msgty.
      detailed_msg-msgid = <rcomp_msg>-msgid.
      detailed_msg-msgno = <rcomp_msg>-msgno.
      detailed_msg-msgv1 = <rcomp_msg>-msgv1.
      detailed_msg-msgv2 = <rcomp_msg>-msgv2.
      detailed_msg-msgv3 = <rcomp_msg>-msgv3.
      detailed_msg-msgv4 = <rcomp_msg>-msgv4.
    else.
      free_text_msg = obj_to_log.
    endif.

    if free_text_msg is not initial.
      append message_from_string( free_text_msg ) to mt_messages.
    elseif exception_data_table is not initial.
      field-symbols <exception_data> like line of exception_data_table.
      loop at exception_data_table assigning <exception_data>.
        perform exception_into_msg in program saplsbal
              using <exception_data>
              changing detailed_msg.
        append message_from_bal_s_msg( detailed_msg ) to mt_messages.
      endloop.
    elseif detailed_msg is not initial.
      append message_from_bal_s_msg( detailed_msg ) to mt_messages.
    endif.

    if auto_save = abap_true.
      zif_logger~save( ).
    endif.

    clear m_new_object.
    self = me.
  endmethod.


  method drill_down_into_exception.
    data: i                  type i value 2,
          previous_exception type ref to cx_root,
          exceptions         type tty_exception.

    field-symbols <ex> like line of exceptions.
    append initial line to exceptions assigning <ex>.
    <ex>-level = 1.
    <ex>-exception = exception.

    previous_exception = exception.

    while i <= max_exception_drill_down.
      if previous_exception->previous is not bound.
        exit.
      endif.

      previous_exception ?= previous_exception->previous.

      append initial line to exceptions assigning <ex>.
      <ex>-level = i.
      <ex>-exception = previous_exception.
      i = i + 1.
    endwhile.

    field-symbols <ret> like line of rt_exception_data_table.
    sort exceptions by level descending.                   "Display the deepest exception first
    loop at exceptions assigning <ex>.
      append initial line to rt_exception_data_table assigning <ret>.
      <ret>-exception = <ex>-exception.
      <ret>-msgty     = type.
      <ret>-probclass = importance.
    endloop.
  endmethod.


  method zif_logger~export_to_table.
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

        bapiret2-type       = message-msgty.
        bapiret2-id         = message-msgid.
        bapiret2-number     = message-msgno.
        bapiret2-log_no     = <msg_handle>-log_handle.     "last 2 chars missing!!
        bapiret2-log_msg_no = <msg_handle>-msgnumber.
        bapiret2-message_v1 = message-msgv1.
        bapiret2-message_v2 = message-msgv2.
        bapiret2-message_v3 = message-msgv3.
        bapiret2-message_v4 = message-msgv4.
        bapiret2-system     = sy-sysid.
        append bapiret2 to rt_bapiret.
      endif.
    endloop.
  endmethod.


  method zif_logger~fullscreen.
    call function 'TR_FLUSH_LOG'.
  endmethod.


  method zif_logger~get_autosave.
    auto_save = me->auto_save.
  endmethod.


  method zif_logger~popup.
    call function 'TR_FLUSH_LOG'.
  endmethod.


  method zif_logger~save.
    call function 'TR_APPEND_LOG'
      tables
        xmsg           = mt_messages
      exceptions
        file_not_found = 1
        wrong_call     = 2.
    if sy-subrc = 0.
      clear mt_messages.
    endif.
  endmethod.


  method zif_logger~set_autosave.
    me->auto_save = auto_save.
  endmethod.


  method message_from_string.
    data:
      begin of ls_msg,
        v1 type symsgv,
        v2 type symsgv,
        v3 type symsgv,
        v4 type symsgv,
      end of ls_msg.

    ls_msg = i_message_text.

    "rs_message-level    =
    "rs_message-severity =
    rs_message-langu    = sy-langu.
    rs_message-ag       = 'BL'.
    rs_message-msgnr    = '001'.
    rs_message-newobj   = m_new_object.
    rs_message-var1     = ls_msg-v1.
    rs_message-var2     = ls_msg-v2.
    rs_message-var3     = ls_msg-v3.
    rs_message-var4     = ls_msg-v4.
  endmethod.


  method message_from_bal_s_msg.
    "rs_message-level    =
    "rs_message-severity =
    rs_message-langu    = sy-langu.
    rs_message-ag       = is_message-msgid.
    rs_message-msgnr    = is_message-msgno.
    rs_message-newobj   = m_new_object.
    rs_message-var1     = is_message-msgv1.
    rs_message-var2     = is_message-msgv2.
    rs_message-var3     = is_message-msgv3.
    rs_message-var4     = is_message-msgv4.
  endmethod.

endclass.
