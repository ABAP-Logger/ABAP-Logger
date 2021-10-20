*----------------------------------------------------------------------*
*       CLASS ZCL_LOGGER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class zcl_logger definition
  public
  create private
  global friends zcl_logger_factory.

  public section.
*"* public components of class ZCL_LOGGER
*"* do not include other source files here!!!
    type-pools abap .

    interfaces zif_logger.
    aliases: add for zif_logger~add,
             a for zif_logger~a,
             e for zif_logger~e,
             w for zif_logger~w,
             i for zif_logger~i,
             s for zif_logger~s,
             has_errors for zif_logger~has_errors,
             has_warnings for zif_logger~has_warnings,
             is_empty for zif_logger~is_empty,
             length for zif_logger~length,
             save for zif_logger~save,
             export_to_table for zif_logger~export_to_table,
             fullscreen for zif_logger~fullscreen,
             popup for zif_logger~popup,
             handle for zif_logger~handle,
             db_number for zif_logger~db_number,
             header for zif_logger~header.

    "! Starts a new log.
    "! For backwards compatibility only! Use ZCL_LOGGER_FACTORY instead.
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

    "! Reopens an already existing log.
    "! For backwards compatibility only! Use ZCL_LOGGER_FACTORY instead.
    class-methods open
      importing
        !object                   type csequence
        !subobject                type csequence
        !desc                     type csequence optional
        !create_if_does_not_exist type abap_bool default abap_false
        !auto_save                type abap_bool optional
      returning
        value(r_log)              type ref to zcl_logger .

  protected section.
*"* protected components of class ZCL_LOGGER
*"* do not include other source files here!!!
  private section.

  constants:
    begin of c_struct_kind,
      syst  type i value 1,
      bapi  type i value 2,
      bdc   type i value 3,
      sprot type i value 4,
    end of c_struct_kind .

*"* private components of class ZCL_LOGGER
*"* do not include other source files here!!!
    data  sec_connection           type abap_bool .
    data  sec_connect_commit       type abap_bool .
    data  settings                 type ref to zif_logger_settings.

    methods:
      "! Safety limit for previous exception drill down
      drill_down_into_exception
        importing
          exception                      type ref to cx_root
          type                           type symsgty optional
          importance                     type balprobcl optional
        returning
          value(rt_exception_data_table) type tty_exception_data,

      get_message_handles
        importing
          msgtype                   type symsgty optional
        returning
          value(rt_message_handles) type bal_t_msgh ,

      add_structure
        importing
          obj_to_log    type any optional
          context       type simple optional
          callback_form type csequence optional
          callback_prog type csequence optional
          callback_fm   type csequence optional
          type          type symsgty optional
          importance    type balprobcl optional
            preferred parameter obj_to_log
        returning
          value(self)   type ref to zif_logger .

    methods save_log.
    methods get_struct_kind
      importing
        !msg_type     type ref to cl_abap_typedescr
      returning
        value(result) type string .
    methods add_syst_msg
      importing
        !obj_to_log         type any
      returning
        value(detailed_msg) type bal_s_msg .
    methods add_bapi_msg
      importing
        !obj_to_log         type any
      returning
        value(detailed_msg) type bal_s_msg .
    methods add_bdc_msg
      importing
        !obj_to_log         type any
      returning
        value(detailed_msg) type bal_s_msg .
    methods add_sprot_msg
      importing
        !obj_to_log         type any
      returning
        value(detailed_msg) type bal_s_msg .
endclass.



class zcl_logger implementation.


  method add_bapi_msg.
    data bapi_message type bapiret1.
    move-corresponding obj_to_log to bapi_message.
    detailed_msg-msgty = bapi_message-type.
    detailed_msg-msgid = bapi_message-id.
    detailed_msg-msgno = bapi_message-number.
    detailed_msg-msgv1 = bapi_message-message_v1.
    detailed_msg-msgv2 = bapi_message-message_v2.
    detailed_msg-msgv3 = bapi_message-message_v3.
    detailed_msg-msgv4 = bapi_message-message_v4.
  endmethod.


  method add_bdc_msg.
    data bdc_message type bdcmsgcoll.
    move-corresponding obj_to_log to bdc_message.
    detailed_msg-msgty = bdc_message-msgtyp. "!
    detailed_msg-msgid = bdc_message-msgid.
    detailed_msg-msgno = bdc_message-msgnr. "!
    detailed_msg-msgv1 = bdc_message-msgv1.
    detailed_msg-msgv2 = bdc_message-msgv2.
    detailed_msg-msgv3 = bdc_message-msgv3.
    detailed_msg-msgv4 = bdc_message-msgv4.
  endmethod.  


  method add_sprot_msg.
    data sprot_message type sprot_u.
    move-corresponding obj_to_log to sprot_message.
    detailed_msg-msgty = sprot_message-severity.
    detailed_msg-msgid = sprot_message-ag.
    detailed_msg-msgno = sprot_message-msgnr. "!
    detailed_msg-msgv1 = sprot_message-var1.
    detailed_msg-msgv2 = sprot_message-var2.
    detailed_msg-msgv3 = sprot_message-var3.
    detailed_msg-msgv4 = sprot_message-var4.
  endmethod.


  method add_syst_msg.
    data syst_message type symsg.
    move-corresponding obj_to_log to syst_message.
    move-corresponding syst_message to detailed_msg.
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

    while i <= settings->get_max_exception_drill_down( ).
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


  method get_message_handles.

    data: log_handle type bal_t_logh,
          filter     type bal_s_mfil.

    field-symbols <f> like line of filter-msgty.

    insert handle into table log_handle.

    if msgtype is not initial.
      append initial line to filter-msgty assigning <f>.
      <f>-sign   = 'I'.
      <f>-option = 'EQ'.
      <f>-low    = msgtype.
    endif.

    call function 'BAL_GLB_SEARCH_MSG'
      exporting
        i_t_log_handle = log_handle
        i_s_msg_filter = filter
      importing
        e_t_msg_handle = rt_message_handles
      exceptions
        msg_not_found  = 0.

  endmethod.


  method get_struct_kind.

    data: msg_struct_kind type ref to cl_abap_structdescr,
          components      type abap_compdescr_tab,
          component       like line of components,
          syst_count      type i,
          bapi_count      type i,
          bdc_count       type i,
          sprot_count     type i.

    if msg_type->type_kind = cl_abap_typedescr=>typekind_struct1
      or msg_type->type_kind = cl_abap_typedescr=>typekind_struct2.

      msg_struct_kind ?= msg_type.
      components = msg_struct_kind->components.

      " Count number of fields expected for each supported type of message structure
      loop at components into component.
        if 'MSGTY,MSGID,MSGNO,MSGV1,MSGV2,MSGV3,MSGV4,' cs |{ component-name },|.
          syst_count = syst_count + 1.
        endif.
        if 'TYPE,NUMBER,ID,MESSAGE_V1,MESSAGE_V2,MESSAGE_V3,MESSAGE_V4,' cs |{ component-name },|.
          bapi_count = bapi_count + 1.
        endif.
        if 'MSGTYP,MSGID,MSGNR,MSGV1,MSGV2,MSGV3,MSGV4,' cs |{ component-name },|.
          bdc_count = bdc_count + 1.
        endif.
        if 'SEVERITY,AG,MSGNR,VAR1,VAR2,VAR3,VAR4,' cs |{ component-name },|.
          sprot_count = sprot_count + 1.
        endif.
      endloop.

      " Set message type if all expected fields are present
      if syst_count = 7.
        result = c_struct_kind-syst.
      elseif bapi_count = 7.
        result = c_struct_kind-bapi.
      elseif bdc_count = 7.
        result = c_struct_kind-bdc.
      elseif sprot_count = 7.
        result = c_struct_kind-sprot.
      endif.
    endif.

  endmethod.


  method new.

    if auto_save is supplied.
      r_log ?= zcl_logger_factory=>create_log(
        object = object
        subobject = subobject
        desc = desc
        context = context
        settings = zcl_logger_factory=>create_settings(
          )->set_usage_of_secondary_db_conn( second_db_conn
          )->set_autosave( auto_save )
      ).
    else.
      r_log ?= zcl_logger_factory=>create_log(
        object = object
        subobject = subobject
        desc = desc
        context = context
        settings = zcl_logger_factory=>create_settings(
          )->set_usage_of_secondary_db_conn( second_db_conn )
      ).
    endif.

  endmethod.


  method open.

    if auto_save is supplied.
      r_log ?= zcl_logger_factory=>open_log(
        object = object
        subobject = subobject
        desc = desc
        create_if_does_not_exist = create_if_does_not_exist
        settings = zcl_logger_factory=>create_settings(
          )->set_autosave( auto_save )
      ).
    else.
      r_log ?= zcl_logger_factory=>open_log(
        object = object
        subobject = subobject
        desc = desc
        create_if_does_not_exist = create_if_does_not_exist
      ).
    endif.

  endmethod.


  method zif_logger~a.
    self = add(
      obj_to_log    = obj_to_log
      context       = context
      callback_form = callback_form
      callback_prog = callback_prog
      callback_fm   = callback_fm
      type          = 'A'
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
          struct_kind          type i,
          log_numbers          type bal_t_lgnm,
          log_handles          type bal_t_logh,
          log_number           type bal_s_lgnm,
          formatted_context    type bal_s_cont,
          formatted_params     type bal_s_parm,
          message_type         type symsgty,
          "these objects could be moved into their own method
          "see adt://***/sap/bc/adt/oo/classes/zcl_logger/source/main#start=391,10;end=415,61
          symsg                type symsg,
          loggable             TYPE REF TO zif_loggable_object,
          loggable_object_messages TYPE zif_loggable_object=>tty_messages.

    field-symbols: <table_of_messages> type any table,
                   <message_line>      type any,
                   <context_val>       type any,
                   <loggable_object_message> TYPE zif_loggable_object=>ty_message.

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
    struct_kind = get_struct_kind( msg_type ).

    if obj_to_log is not supplied.
      detailed_msg = add_syst_msg( syst ).
    elseif struct_kind = c_struct_kind-syst.
      detailed_msg = add_syst_msg( obj_to_log ).
    elseif struct_kind = c_struct_kind-bapi.
      detailed_msg = add_bapi_msg( obj_to_log ).
    elseif struct_kind = c_struct_kind-bdc.
      detailed_msg = add_bdc_msg( obj_to_log ).
    elseif msg_type->type_kind = cl_abap_typedescr=>typekind_oref.
         TRY.
          "BEGIN this could/should be moved into its own method
          loggable ?= obj_to_log.
          loggable_object_messages = loggable->get_message_table( ) .
          LOOP AT loggable_object_messages ASSIGNING <loggable_object_message>.
            IF <loggable_object_message>-symsg IS NOT INITIAL.
              MOVE-CORRESPONDING <loggable_object_message>-symsg TO symsg.
              symsg-msgty = <loggable_object_message>-type.
              zif_logger~add(
                  obj_to_log    = symsg
                  context       = context ).
            ENDIF.
            IF <loggable_object_message>-exception IS BOUND.
              zif_logger~add(
                  type          = <loggable_object_message>-type
                  obj_to_log    = <loggable_object_message>-exception
                  context       = context ).
            ENDIF.
            IF <loggable_object_message>-string IS NOT INITIAL.
              zif_logger~add(
                  type          = <loggable_object_message>-type
                  obj_to_log    = <loggable_object_message>-string
                  context       = context ).
            ENDIF.
          ENDLOOP.
          "END this could/should be moved into its own method

        CATCH cx_sy_move_cast_error.
          IF type IS INITIAL.
            message_type = if_msg_output=>msgtype_error.
          ELSE.
            message_type = type.
          ENDIF.
          exception_data_table = me->drill_down_into_exception(
              exception   = obj_to_log
              type        = message_type
              importance  = importance
              ).

      ENDTRY.
    elseif msg_type->type_kind = cl_abap_typedescr=>typekind_table.
      assign obj_to_log to <table_of_messages>.
      loop at <table_of_messages> assigning <message_line>.
        if sy-tabix = 1.
          zif_logger~add(
              obj_to_log    = <message_line>
              context       = context ).
        else.
          zif_logger~add( obj_to_log = <message_line> ).
        endif.
      endloop.
    elseif msg_type->type_kind = cl_abap_typedescr=>typekind_struct1   "flat structure
        or msg_type->type_kind = cl_abap_typedescr=>typekind_struct2.  "deep structure (already when string is used)
      add_structure(
        exporting
          obj_to_log    = obj_to_log
          context       = context
          callback_form = callback_form
          callback_prog = callback_prog
          callback_fm   = callback_fm
          type          = type
          importance    = importance
        receiving
          self          = self
      ).
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
    elseif exception_data_table is not initial.
      field-symbols <exception_data> like line of exception_data_table.
      loop at exception_data_table assigning <exception_data>.
        call function 'BAL_LOG_EXCEPTION_ADD'
          exporting
            i_log_handle = me->handle
            i_s_exc      = <exception_data>.
      endloop.
    elseif detailed_msg is not initial.
      detailed_msg-context   = formatted_context.
      detailed_msg-params    = formatted_params.
      detailed_msg-probclass = importance.
      call function 'BAL_LOG_MSG_ADD'
        exporting
          i_log_handle = me->handle
          i_s_msg      = detailed_msg.
    endif.

    if me->settings->get_autosave( ) = abap_true.
      save_log( ).
    endif.
    self = me.
  endmethod.


  method add_structure.
    data: msg_type        type ref to cl_abap_typedescr,
          msg_struct_type type ref to cl_abap_structdescr,
          components      type abap_compdescr_tab,
          component       like line of components,
          string_to_log   type string.
    field-symbols: <component>   type any.

    msg_struct_type ?= cl_abap_typedescr=>describe_by_data( obj_to_log ).
    components = msg_struct_type->components.
    add( '--- Begin of structure ---' ).
    loop at components into component.
      assign component component-name of structure obj_to_log to <component>.
      if sy-subrc = 0.
        msg_type = cl_abap_typedescr=>describe_by_data( <component> ).
        if msg_type->kind = cl_abap_typedescr=>kind_elem.
          string_to_log = |{ to_lower( component-name ) } = { <component> }|.
          add( string_to_log ).
        elseif msg_type->kind = cl_abap_typedescr=>kind_struct.
          add_structure(
            exporting
              obj_to_log    = <component>
              context       = context
              callback_form = callback_form
              callback_prog = callback_prog
              callback_fm   = callback_fm
              type          = type
              importance    = importance
            receiving
              self          = self
          ).
        endif.
      endif.
    endloop.
    add( '--- End of structure ---' ).
  endmethod.


  method zif_logger~e.
    self = add(
      obj_to_log    = obj_to_log
      context       = context
      callback_form = callback_form
      callback_prog = callback_prog
      callback_fm   = callback_fm
      type          = 'E'
      importance    = importance ).
  endmethod.


  method zif_logger~export_to_table.
    data: message_handles type bal_t_msgh,
          message         type bal_s_msg,
          bapiret2        type bapiret2.

    field-symbols <msg_handle> type balmsghndl.

    message_handles = get_message_handles( ).

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

    data:
      profile        type bal_s_prof,
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


  method zif_logger~has_errors.

    rv_yes = boolc( lines( get_message_handles( msgtype = 'E' ) ) > 0 ).

  endmethod.


  method zif_logger~has_warnings.

    rv_yes = boolc( lines( get_message_handles( msgtype = 'W' ) ) > 0 ).

  endmethod.


  method zif_logger~i.
    self = add(
      obj_to_log    = obj_to_log
      context       = context
      callback_form = callback_form
      callback_prog = callback_prog
      callback_fm   = callback_fm
      type          = 'I'
      importance    = importance ).
  endmethod.


  method zif_logger~is_empty.

    rv_yes = boolc( length( ) = 0 ).

  endmethod.


  method zif_logger~length.

    rv_length = lines( get_message_handles( ) ).

  endmethod.


  method zif_logger~popup.
* See SBAL_DEMO_04_POPUP for ideas
    data relevant_profile type bal_s_prof.
    data lt_log_handles type bal_t_logh.

    append me->handle to lt_log_handles.

    if profile is supplied and profile is not initial.
      relevant_profile = profile.
    else.
      call function 'BAL_DSP_PROFILE_POPUP_GET'
        importing
          e_s_display_profile = relevant_profile.
    endif.

    call function 'BAL_DSP_LOG_DISPLAY'
      exporting
        i_s_display_profile = relevant_profile
        i_t_log_handle      = lt_log_handles.
  endmethod.


  method zif_logger~s.
    self = add(
      obj_to_log    = obj_to_log
      context       = context
      callback_form = callback_form
      callback_prog = callback_prog
      callback_fm   = callback_fm
      type          = 'S'
      importance    = importance ).
  endmethod.


  method zif_logger~save.
    check me->settings->get_autosave( ) = abap_false.
    save_log( ).
  endmethod.


  method zif_logger~w.
    self = add(
      obj_to_log    = obj_to_log
      context       = context
      callback_form = callback_form
      callback_prog = callback_prog
      callback_fm   = callback_fm
      type          = 'W'
      importance    = importance ).
  endmethod.


  method save_log.
    data log_handles type bal_t_logh.
    data log_numbers type bal_t_lgnm.
    data log_number type bal_s_lgnm.

    insert me->handle into table log_handles.
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
endclass.
