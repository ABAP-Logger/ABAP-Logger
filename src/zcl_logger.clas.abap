CLASS zcl_logger DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_logger_factory.

  PUBLIC SECTION.
*"* public components of class ZCL_LOGGER
*"* do not include other source files here!!!
    TYPE-POOLS abap.

    INTERFACES zif_logger.
    ALIASES: add FOR zif_logger~add,
             a FOR zif_logger~a,
             e FOR zif_logger~e,
             w FOR zif_logger~w,
             i FOR zif_logger~i,
             s FOR zif_logger~s,
             has_errors FOR zif_logger~has_errors,
             has_warnings FOR zif_logger~has_warnings,
             is_empty FOR zif_logger~is_empty,
             length FOR zif_logger~length,
             save FOR zif_logger~save,
             export_to_table FOR zif_logger~export_to_table,
             fullscreen FOR zif_logger~fullscreen,
             popup FOR zif_logger~popup,
             handle FOR zif_logger~handle,
             db_number FOR zif_logger~db_number,
             header FOR zif_logger~header.

    "! Starts a new log.
    "! For backwards compatibility only! Use ZCL_LOGGER_FACTORY instead.
    CLASS-METHODS new
      IMPORTING
        !object         TYPE csequence OPTIONAL
        !subobject      TYPE csequence OPTIONAL
        !desc           TYPE csequence OPTIONAL
        !context        TYPE simple OPTIONAL
        !auto_save      TYPE abap_bool OPTIONAL
        !second_db_conn TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(r_log)    TYPE REF TO zcl_logger.

    "! Reopens an already existing log.
    "! For backwards compatibility only! Use ZCL_LOGGER_FACTORY instead.
    CLASS-METHODS open
      IMPORTING
        !object                   TYPE csequence
        !subobject                TYPE csequence
        !desc                     TYPE csequence OPTIONAL
        !create_if_does_not_exist TYPE abap_bool DEFAULT abap_false
        !auto_save                TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(r_log)              TYPE REF TO zcl_logger.

  PROTECTED SECTION.
*"* protected components of class ZCL_LOGGER
*"* do not include other source files here!!!
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_struct_kind,
        syst  TYPE i VALUE 1,
        bapi  TYPE i VALUE 2,
        bdc   TYPE i VALUE 3,
        sprot TYPE i VALUE 4,
      END OF c_struct_kind.

*"* private components of class ZCL_LOGGER
*"* do not include other source files here!!!
    DATA sec_connection     TYPE abap_bool.
    DATA sec_connect_commit TYPE abap_bool.
    DATA settings           TYPE REF TO zif_logger_settings.

    METHODS:
      "! Safety limit for previous exception drill down
      drill_down_into_exception
        IMPORTING
          exception                      TYPE REF TO cx_root
          type                           TYPE symsgty OPTIONAL
          importance                     TYPE balprobcl OPTIONAL
        RETURNING
          VALUE(rt_exception_data_table) TYPE tty_exception_data,

      get_message_handles
        IMPORTING
          msgtype                   TYPE symsgty OPTIONAL
        RETURNING
          VALUE(rt_message_handles) TYPE bal_t_msgh,

      add_structure
        IMPORTING
          obj_to_log    TYPE any OPTIONAL
          context       TYPE simple OPTIONAL
          callback_form TYPE csequence OPTIONAL
          callback_prog TYPE csequence OPTIONAL
          callback_fm   TYPE csequence OPTIONAL
          type          TYPE symsgty OPTIONAL
          importance    TYPE balprobcl OPTIONAL
            PREFERRED PARAMETER obj_to_log
        RETURNING
          VALUE(self)   TYPE REF TO zif_logger.

    METHODS save_log.
    METHODS get_struct_kind
      IMPORTING
        !msg_type     TYPE REF TO cl_abap_typedescr
      RETURNING
        VALUE(result) TYPE string.
    METHODS add_syst_msg
      IMPORTING
        !obj_to_log         TYPE any
      RETURNING
        VALUE(detailed_msg) TYPE bal_s_msg.
    METHODS add_bapi_msg
      IMPORTING
        !obj_to_log         TYPE any
      RETURNING
        VALUE(detailed_msg) TYPE bal_s_msg.
    METHODS add_bdc_msg
      IMPORTING
        !obj_to_log         TYPE any
      RETURNING
        VALUE(detailed_msg) TYPE bal_s_msg.
    METHODS add_sprot_msg
      IMPORTING
        !obj_to_log         TYPE any
      RETURNING
        VALUE(detailed_msg) TYPE bal_s_msg.
ENDCLASS.



CLASS zcl_logger IMPLEMENTATION.

  METHOD add_bapi_msg.
    DATA bapi_message TYPE bapiret1.
    MOVE-CORRESPONDING obj_to_log TO bapi_message.
    detailed_msg-msgty = bapi_message-type.
    detailed_msg-msgid = bapi_message-id.
    detailed_msg-msgno = bapi_message-number.
    detailed_msg-msgv1 = bapi_message-message_v1.
    detailed_msg-msgv2 = bapi_message-message_v2.
    detailed_msg-msgv3 = bapi_message-message_v3.
    detailed_msg-msgv4 = bapi_message-message_v4.
  ENDMETHOD.

  METHOD add_bdc_msg.
    DATA bdc_message TYPE bdcmsgcoll.
    MOVE-CORRESPONDING obj_to_log TO bdc_message.
    detailed_msg-msgty = bdc_message-msgtyp.
    detailed_msg-msgid = bdc_message-msgid.
    detailed_msg-msgno = bdc_message-msgnr.
    detailed_msg-msgv1 = bdc_message-msgv1.
    detailed_msg-msgv2 = bdc_message-msgv2.
    detailed_msg-msgv3 = bdc_message-msgv3.
    detailed_msg-msgv4 = bdc_message-msgv4.
  ENDMETHOD.

  METHOD add_sprot_msg.
    DATA sprot_message TYPE sprot_u.
    MOVE-CORRESPONDING obj_to_log TO sprot_message.
    detailed_msg-msgty = sprot_message-severity.
    detailed_msg-msgid = sprot_message-ag.
    detailed_msg-msgno = sprot_message-msgnr.
    detailed_msg-msgv1 = sprot_message-var1.
    detailed_msg-msgv2 = sprot_message-var2.
    detailed_msg-msgv3 = sprot_message-var3.
    detailed_msg-msgv4 = sprot_message-var4.
  ENDMETHOD.

  METHOD add_syst_msg.
    DATA syst_message TYPE symsg.
    MOVE-CORRESPONDING obj_to_log TO syst_message.
    MOVE-CORRESPONDING syst_message TO detailed_msg.
  ENDMETHOD.


  METHOD drill_down_into_exception.
    DATA: i                  TYPE i VALUE 2,
          previous_exception TYPE REF TO cx_root,
          exceptions         TYPE tty_exception.

    FIELD-SYMBOLS <ex> LIKE LINE OF exceptions.
    APPEND INITIAL LINE TO exceptions ASSIGNING <ex>.
    <ex>-level     = 1.
    <ex>-exception = exception.

    previous_exception = exception.

    WHILE i <= settings->get_max_exception_drill_down( ).
      IF previous_exception->previous IS NOT BOUND.
        EXIT.
      ENDIF.

      previous_exception ?= previous_exception->previous.

      APPEND INITIAL LINE TO exceptions ASSIGNING <ex>.
      <ex>-level     = i.
      <ex>-exception = previous_exception.
      i              = i + 1.
    ENDWHILE.

    FIELD-SYMBOLS <ret> LIKE LINE OF rt_exception_data_table.

    "Display the deepest exception first
    SORT exceptions BY level DESCENDING.
    LOOP AT exceptions ASSIGNING <ex>.
      APPEND INITIAL LINE TO rt_exception_data_table ASSIGNING <ret>.
      <ret>-exception = <ex>-exception.
      <ret>-msgty     = type.
      <ret>-probclass = importance.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_message_handles.
    DATA: log_handle TYPE bal_t_logh,
          filter     TYPE bal_s_mfil.

    FIELD-SYMBOLS <f> LIKE LINE OF filter-msgty.

    INSERT handle INTO TABLE log_handle.

    IF msgtype IS NOT INITIAL.
      APPEND INITIAL LINE TO filter-msgty ASSIGNING <f>.
      <f>-sign   = 'I'.
      <f>-option = 'EQ'.
      <f>-low    = msgtype.
    ENDIF.

    CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
      EXPORTING
        i_t_log_handle = log_handle
        i_s_msg_filter = filter
      IMPORTING
        e_t_msg_handle = rt_message_handles
      EXCEPTIONS
        msg_not_found  = 0.
  ENDMETHOD.

  METHOD get_struct_kind.
    DATA: msg_struct_kind TYPE REF TO cl_abap_structdescr,
          components      TYPE abap_compdescr_tab,
          component       LIKE LINE OF components,
          syst_count      TYPE i,
          bapi_count      TYPE i,
          bdc_count       TYPE i,
          sprot_count     TYPE i.

    IF msg_type->type_kind = cl_abap_typedescr=>typekind_struct1
        OR msg_type->type_kind = cl_abap_typedescr=>typekind_struct2.

      msg_struct_kind ?= msg_type.
      components = msg_struct_kind->components.

      " Count number of fields expected for each supported type of message structure
      LOOP AT components INTO component.
        IF 'MSGTY,MSGID,MSGNO,MSGV1,MSGV2,MSGV3,MSGV4,' CS |{ component-name },|.
          syst_count = syst_count + 1.
        ENDIF.
        IF 'TYPE,NUMBER,ID,MESSAGE_V1,MESSAGE_V2,MESSAGE_V3,MESSAGE_V4,' CS |{ component-name },|.
          bapi_count = bapi_count + 1.
        ENDIF.
        IF 'MSGTYP,MSGID,MSGNR,MSGV1,MSGV2,MSGV3,MSGV4,' CS |{ component-name },|.
          bdc_count = bdc_count + 1.
        ENDIF.
        IF 'SEVERITY,AG,MSGNR,VAR1,VAR2,VAR3,VAR4,' CS |{ component-name },|.
          sprot_count = sprot_count + 1.
        ENDIF.
      ENDLOOP.

      " Set message type if all expected fields are present
      IF syst_count = 7.
        result = c_struct_kind-syst.
      ELSEIF bapi_count = 7.
        result = c_struct_kind-bapi.
      ELSEIF bdc_count = 7.
        result = c_struct_kind-bdc.
      ELSEIF sprot_count = 7.
        result = c_struct_kind-sprot.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD new.
    IF auto_save IS SUPPLIED.
      r_log ?= zcl_logger_factory=>create_log(
        object = object
        subobject = subobject
        desc = desc
        context = context
        settings = zcl_logger_factory=>create_settings(
          )->set_usage_of_secondary_db_conn( second_db_conn
          )->set_autosave( auto_save ) ).
    ELSE.
      r_log ?= zcl_logger_factory=>create_log(
        object = object
        subobject = subobject
        desc = desc
        context = context
        settings = zcl_logger_factory=>create_settings(
          )->set_usage_of_secondary_db_conn( second_db_conn ) ).
    ENDIF.
  ENDMETHOD.

  METHOD open.
    IF auto_save IS SUPPLIED.
      r_log ?= zcl_logger_factory=>open_log(
        object = object
        subobject = subobject
        desc = desc
        create_if_does_not_exist = create_if_does_not_exist
        settings = zcl_logger_factory=>create_settings(
          )->set_autosave( auto_save ) ).
    ELSE.
      r_log ?= zcl_logger_factory=>open_log(
        object = object
        subobject = subobject
        desc = desc
        create_if_does_not_exist = create_if_does_not_exist ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_logger~a.
    self = add(
      obj_to_log          = obj_to_log
      context             = context
      callback_form       = callback_form
      callback_prog       = callback_prog
      callback_fm         = callback_fm
      callback_parameters = callback_parameters
      type                = 'A'
      importance          = importance ).
  ENDMETHOD.

  METHOD zif_logger~add.
    DATA: detailed_msg             TYPE bal_s_msg,
          exception_data_table     TYPE tty_exception_data,
          free_text_msg            TYPE char200,
          ctx_type                 TYPE REF TO cl_abap_typedescr,
          ctx_ddic_header          TYPE x030l,
          msg_type                 TYPE REF TO cl_abap_typedescr,
          struct_kind              TYPE i,
          formatted_context        TYPE bal_s_cont,
          formatted_params         TYPE bal_s_parm,
          message_type             TYPE symsgty,
          "these objects could be moved into their own method
          "see adt://***/sap/bc/adt/oo/classes/zcl_logger/source/main#start=391,10;end=415,61
          symsg                    TYPE symsg,
          syst_buffer              TYPE syst,
          loggable                 TYPE REF TO zif_loggable_object,
          loggable_object_messages TYPE zif_loggable_object=>tty_messages.

    FIELD-SYMBOLS: <table_of_messages>       TYPE ANY TABLE,
                   <message_line>            TYPE any,
                   <context_val>             TYPE any,
                   <loggable_object_message> TYPE zif_loggable_object=>ty_message.

    " Remember system message since it might get changed inadvertently
    syst_buffer = syst.
    IF context IS NOT INITIAL.
      ASSIGN context TO <context_val>.
      formatted_context-value = <context_val>.
      ctx_type                = cl_abap_typedescr=>describe_by_data( context ).

      ctx_type->get_ddic_header(
        RECEIVING
          p_header     = ctx_ddic_header
        EXCEPTIONS
          not_found    = 1
          no_ddic_type = 2
          OTHERS       = 3 ).
      IF sy-subrc = 0.
        formatted_context-tabname = ctx_ddic_header-tabname.
      ENDIF.
    ENDIF.

    IF callback_fm IS NOT INITIAL.
      formatted_params-callback-userexitf = callback_fm.
      formatted_params-callback-userexitp = callback_prog.
      formatted_params-callback-userexitt = 'F'.
      formatted_params-t_par              = callback_parameters.
    ELSEIF callback_form IS NOT INITIAL.
      formatted_params-callback-userexitf = callback_form.
      formatted_params-callback-userexitp = callback_prog.
      formatted_params-callback-userexitt = ' '.
      formatted_params-t_par              = callback_parameters.
    ENDIF.

    msg_type    = cl_abap_typedescr=>describe_by_data( obj_to_log ).
    struct_kind = get_struct_kind( msg_type ).

    IF obj_to_log IS INITIAL.
      detailed_msg = add_syst_msg( syst_buffer ).
    ELSEIF struct_kind = c_struct_kind-syst.
      detailed_msg = add_syst_msg( obj_to_log ).
    ELSEIF struct_kind = c_struct_kind-bapi.
      detailed_msg = add_bapi_msg( obj_to_log ).
    ELSEIF struct_kind = c_struct_kind-bdc.
      detailed_msg = add_bdc_msg( obj_to_log ).
    ELSEIF struct_kind = c_struct_kind-sprot.
      detailed_msg = add_sprot_msg( obj_to_log ).
    ELSEIF msg_type->type_kind = cl_abap_typedescr=>typekind_oref.
      TRY.
          "BEGIN this could/should be moved into its own method
          loggable ?= obj_to_log.
          loggable_object_messages = loggable->get_message_table( ).
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
          exception_data_table = drill_down_into_exception(
              exception   = obj_to_log
              type        = message_type
              importance  = importance
              ).

      ENDTRY.
    ELSEIF msg_type->type_kind = cl_abap_typedescr=>typekind_table.
      ASSIGN obj_to_log TO <table_of_messages>.
      LOOP AT <table_of_messages> ASSIGNING <message_line>.
        IF sy-tabix = 1.
          zif_logger~add(
              obj_to_log    = <message_line>
              context       = context
              importance    = importance
              type          = type ).
        ELSE.
          zif_logger~add(
              obj_to_log    = <message_line>
              importance    = importance
              type          = type ).
        ENDIF.
      ENDLOOP.
    ELSEIF msg_type->type_kind = cl_abap_typedescr=>typekind_struct1     "flat structure
        OR msg_type->type_kind = cl_abap_typedescr=>typekind_struct2.    "deep structure (already when string is used)
      self = add_structure(
          obj_to_log    = obj_to_log
          context       = context
          callback_form = callback_form
          callback_prog = callback_prog
          callback_fm   = callback_fm
          type          = type
          importance    = importance ).
    ELSE.
      free_text_msg = obj_to_log.
    ENDIF.

    IF free_text_msg IS NOT INITIAL.
      message_type = type.
      IF message_type IS INITIAL.
        message_type = if_msg_output=>msgtype_success.
      ENDIF.

      CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
        EXPORTING
          i_log_handle = me->handle
          i_msgty      = message_type
          i_probclass  = importance
          i_text       = free_text_msg
          i_s_context  = formatted_context
          i_s_params   = formatted_params.
    ELSEIF exception_data_table IS NOT INITIAL.
      FIELD-SYMBOLS <exception_data> LIKE LINE OF exception_data_table.
      LOOP AT exception_data_table ASSIGNING <exception_data>.
        CALL FUNCTION 'BAL_LOG_EXCEPTION_ADD'
          EXPORTING
            i_log_handle = me->handle
            i_s_exc      = <exception_data>.
      ENDLOOP.
    ELSEIF detailed_msg IS NOT INITIAL.
      detailed_msg-context   = formatted_context.
      detailed_msg-params    = formatted_params.
      detailed_msg-probclass = importance.
      IF type IS NOT INITIAL.
        detailed_msg-msgty   = type.
      ENDIF.

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = me->handle
          i_s_msg      = detailed_msg.
    ENDIF.

    IF me->settings->get_autosave( ) = abap_true.
      save_log( ).
    ENDIF.
    self = me.
  ENDMETHOD.


  METHOD add_structure.
    DATA: msg_type        TYPE REF TO cl_abap_typedescr,
          msg_struct_type TYPE REF TO cl_abap_structdescr,
          components      TYPE abap_compdescr_tab,
          component       LIKE LINE OF components,
          string_to_log   TYPE string.
    FIELD-SYMBOLS: <component>   TYPE any.

    msg_struct_type ?= cl_abap_typedescr=>describe_by_data( obj_to_log ).
    components = msg_struct_type->components.
    add( '--- Begin of structure ---' ).
    LOOP AT components INTO component.
      ASSIGN COMPONENT component-name OF STRUCTURE obj_to_log TO <component>.
      IF sy-subrc = 0.
        msg_type = cl_abap_typedescr=>describe_by_data( <component> ).
        IF msg_type->kind = cl_abap_typedescr=>kind_elem.
          string_to_log = |{ to_lower( component-name ) } = { <component> }|.
          add( string_to_log ).
        ELSEIF msg_type->kind = cl_abap_typedescr=>kind_struct.
          self = add_structure(
              obj_to_log    = <component>
              context       = context
              callback_form = callback_form
              callback_prog = callback_prog
              callback_fm   = callback_fm
              type          = type
              importance    = importance ).
        ENDIF.
      ENDIF.
    ENDLOOP.
    add( '--- End of structure ---' ).
  ENDMETHOD.

  METHOD zif_logger~e.
    self                  = add(
      obj_to_log          = obj_to_log
      context             = context
      callback_form       = callback_form
      callback_prog       = callback_prog
      callback_fm         = callback_fm
      callback_parameters = callback_parameters
      type                = 'E'
      importance          = importance ).
  ENDMETHOD.

  METHOD zif_logger~export_to_table.
    DATA: message_handles TYPE bal_t_msgh,
          message         TYPE bal_s_msg,
          bapiret2        TYPE bapiret2.

    FIELD-SYMBOLS <msg_handle> TYPE balmsghndl.

    message_handles = get_message_handles( ).

    LOOP AT message_handles ASSIGNING <msg_handle>.
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = <msg_handle>
        IMPORTING
          e_s_msg        = message
        EXCEPTIONS
          OTHERS         = 3.
      IF sy-subrc IS INITIAL.
        MESSAGE ID message-msgid
                TYPE message-msgty
                NUMBER message-msgno
                INTO bapiret2-message
                WITH message-msgv1 message-msgv2 message-msgv3 message-msgv4.

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
        APPEND bapiret2 TO rt_bapiret.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_logger~fullscreen.
    DATA:
      profile        TYPE bal_s_prof,
      lt_log_handles TYPE bal_t_logh.

    INSERT me->handle INTO TABLE lt_log_handles.

    CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
      IMPORTING
        e_s_display_profile = profile.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = profile
        i_t_log_handle      = lt_log_handles.
  ENDMETHOD.

  METHOD zif_logger~has_errors.
    rv_yes = boolc( lines( get_message_handles( msgtype = 'E' ) ) > 0 ).
  ENDMETHOD.

  METHOD zif_logger~has_warnings.
    rv_yes = boolc( lines( get_message_handles( msgtype = 'W' ) ) > 0 ).
  ENDMETHOD.

  METHOD zif_logger~i.
    self = add(
      obj_to_log          = obj_to_log
      context             = context
      callback_form       = callback_form
      callback_prog       = callback_prog
      callback_fm         = callback_fm
      callback_parameters = callback_parameters
      type                = 'I'
      importance          = importance ).
  ENDMETHOD.

  METHOD zif_logger~is_empty.
    rv_yes = boolc( length( ) = 0 ).
  ENDMETHOD.

  METHOD zif_logger~length.
    rv_length = lines( get_message_handles( ) ).
  ENDMETHOD.

  METHOD zif_logger~popup.
* See SBAL_DEMO_04_POPUP for ideas
    DATA relevant_profile TYPE bal_s_prof.
    DATA lt_log_handles   TYPE bal_t_logh.

    INSERT me->handle INTO TABLE lt_log_handles.

    IF profile IS SUPPLIED AND profile IS NOT INITIAL.
      relevant_profile = profile.
    ELSE.
      CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
        IMPORTING
          e_s_display_profile = relevant_profile.
    ENDIF.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = relevant_profile
        i_t_log_handle      = lt_log_handles.
  ENDMETHOD.

  METHOD zif_logger~s.
    self = add(
      obj_to_log          = obj_to_log
      context             = context
      callback_form       = callback_form
      callback_prog       = callback_prog
      callback_fm         = callback_fm
      callback_parameters = callback_parameters
      type                = 'S'
      importance          = importance ).
  ENDMETHOD.

  METHOD zif_logger~save.
    CHECK settings->get_autosave( ) = abap_false.
    save_log( ).
  ENDMETHOD.

  METHOD zif_logger~w.
    self = add(
      obj_to_log          = obj_to_log
      context             = context
      callback_form       = callback_form
      callback_prog       = callback_prog
      callback_fm         = callback_fm
      callback_parameters = callback_parameters
      type                = 'W'
      importance          = importance ).
  ENDMETHOD.

  METHOD save_log.
    DATA log_handles TYPE bal_t_logh.
    DATA log_numbers TYPE bal_t_lgnm.
    DATA log_number  TYPE bal_s_lgnm.

    INSERT me->handle INTO TABLE log_handles.
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle       = log_handles
        i_2th_connection     = me->sec_connection
        i_2th_connect_commit = me->sec_connect_commit
      IMPORTING
        e_new_lognumbers     = log_numbers.
    IF me->db_number IS INITIAL.
      READ TABLE log_numbers INDEX 1 INTO log_number.
      me->db_number = log_number-lognumber.
    ENDIF.
  ENDMETHOD.

  METHOD zif_logger~set_header.
    me->header-extnumber = description.

    CALL FUNCTION 'BAL_LOG_HDR_CHANGE'
      EXPORTING
        i_log_handle            = me->handle
        i_s_log                 = header
      EXCEPTIONS
        log_not_found           = 1
        log_header_inconsistent = 2
        OTHERS                  = 3.
    ASSERT sy-subrc = 0.

    self = me.
  ENDMETHOD.

ENDCLASS.
