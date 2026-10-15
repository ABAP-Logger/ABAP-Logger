CLASS zcl_logger DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_logger_factory.

  PUBLIC SECTION.

    INTERFACES zif_logger.
    INTERFACES zif_logger_log_object.

    DATA handle    TYPE balloghndl READ-ONLY.
    DATA db_number TYPE balognr    READ-ONLY.
    DATA header    TYPE bal_s_log  READ-ONLY.

    CLASS-METHODS get_system_message
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      ty_free_text TYPE c LENGTH 200,
      BEGIN OF ty_exception,
        level     TYPE i,
        exception TYPE REF TO cx_root,
      END OF ty_exception,
      tty_exception TYPE STANDARD TABLE OF ty_exception.

    TYPES tty_exception_data TYPE STANDARD TABLE OF bal_s_exc WITH DEFAULT KEY.

    DATA settings TYPE REF TO zif_logger_settings.

    METHODS get_context
      IMPORTING
        context       TYPE any
      RETURNING
        VALUE(result) TYPE bal_s_cont.

    METHODS get_parameters
      IMPORTING
        callback_form       TYPE csequence
        callback_prog       TYPE csequence
        callback_fm         TYPE csequence
        callback_parameters TYPE bal_t_par
      RETURNING
        VALUE(result)       TYPE bal_s_parm.

    METHODS get_message_handles
      IMPORTING
        msgtype       TYPE symsgty OPTIONAL
      RETURNING
        VALUE(result) TYPE bal_t_msgh.

    METHODS add_message
      IMPORTING
        message           TYPE bal_s_msg
        formatted_context TYPE bal_s_cont
        formatted_params  TYPE bal_s_parm
        !type             TYPE symsgty
        importance        TYPE balprobcl
        detlevel          TYPE ballevel.

    METHODS add_free_text
      IMPORTING
        free_text         TYPE ty_free_text
        formatted_context TYPE bal_s_cont
        formatted_params  TYPE bal_s_parm
        !type             TYPE symsgty
        importance        TYPE balprobcl
        detlevel          TYPE ballevel.

    METHODS add_structure
      IMPORTING
        obj_to_log          TYPE any
        !context            TYPE any OPTIONAL
        callback_form       TYPE csequence
        callback_prog       TYPE csequence
        callback_fm         TYPE csequence
        callback_parameters TYPE bal_t_par
        !type               TYPE symsgty
        importance          TYPE balprobcl
        detlevel            TYPE ballevel.

    METHODS add_table
      IMPORTING
        obj_to_log          TYPE any
        !context            TYPE any
        callback_form       TYPE csequence
        callback_prog       TYPE csequence
        callback_fm         TYPE csequence
        callback_parameters TYPE bal_t_par
        !type               TYPE symsgty
        importance          TYPE balprobcl
        detlevel            TYPE ballevel.

    METHODS add_object
      IMPORTING
        obj_to_log          TYPE any
        !context            TYPE any
        callback_form       TYPE csequence
        callback_prog       TYPE csequence
        callback_fm         TYPE csequence
        callback_parameters TYPE bal_t_par
        !type               TYPE symsgty
        importance          TYPE balprobcl
        detlevel            TYPE ballevel
      RETURNING
        VALUE(result)       TYPE tty_exception_data.

    METHODS add_exception
      IMPORTING
        exception_data    TYPE bal_s_exc
        formatted_context TYPE bal_s_cont
        formatted_params  TYPE bal_s_parm.

    METHODS drill_down_into_exception
      IMPORTING
        exception     TYPE REF TO cx_root
        type          TYPE symsgty
        importance    TYPE balprobcl
        detlevel      TYPE ballevel
      RETURNING
        VALUE(result) TYPE tty_exception_data.

    METHODS save_log.

ENDCLASS.



CLASS zcl_logger IMPLEMENTATION.


  METHOD add_exception.

    DATA message            TYPE bal_s_msg.
    DATA text_key           TYPE scx_t100key.
    DATA index              TYPE i.
    DATA text_id            TYPE sotr_conc.
    DATA substitution_table TYPE sotr_params.

    FIELD-SYMBOLS <attribute>     TYPE scx_t100key-attr1.
    FIELD-SYMBOLS <message_value> TYPE bal_s_msg-msgv1.
    FIELD-SYMBOLS <substitution>  TYPE sotr_param.

    " exception -> type OTR-message or T100-message?
    cl_message_helper=>check_msg_kind(
      EXPORTING
        msg     = exception_data-exception
      IMPORTING
        t100key = text_key
        textid  = text_id ).

    IF text_id IS NOT INITIAL.
      " If it is a OTR-message
      CALL FUNCTION 'BAL_LOG_EXCEPTION_ADD'
        EXPORTING
          i_log_handle     = handle
          i_s_exc          = exception_data
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING
            info = get_system_message( ).
      ENDIF.
      RETURN.
    ENDIF.

    " get the parameter for text switching
    cl_message_helper=>get_text_params(
      EXPORTING
        obj    = exception_data-exception
      IMPORTING
        params = substitution_table ).

    " exception with T100 message
    message-msgid = text_key-msgid.
    message-msgno = text_key-msgno.

    DO 4 TIMES.
      index = sy-index - 1.
      ASSIGN text_key-attr1 INCREMENT index TO <attribute> RANGE text_key.
      IF sy-subrc = 0 AND <attribute> IS NOT INITIAL.
        READ TABLE substitution_table ASSIGNING <substitution> WITH KEY param = <attribute>.
        IF sy-subrc = 0 AND <substitution>-value IS NOT INITIAL.
          ASSIGN message-msgv1 INCREMENT index TO <message_value> RANGE message.
          IF sy-subrc = 0.
            <message_value> = <substitution>-value.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDDO.

    message-msgty     = exception_data-msgty.
    message-probclass = exception_data-probclass.
    message-detlevel  = exception_data-detlevel.
    message-time_stmp = exception_data-time_stmp.
    message-alsort    = exception_data-alsort.
    message-context   = formatted_context.
    message-params    = formatted_params.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = handle
        i_s_msg          = message
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_logger
        EXPORTING
          info = get_system_message( ).
    ENDIF.

  ENDMETHOD.


  METHOD add_free_text.

    DATA message_type TYPE sy-msgty.

    message_type = type.
    IF message_type IS INITIAL.
      message_type = if_msg_output=>msgtype_success.
    ENDIF.

    TRY.
        CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
          EXPORTING
            i_log_handle     = handle
            i_msgty          = message_type
            i_probclass      = importance
            i_text           = free_text
            i_s_context      = formatted_context
            i_s_params       = formatted_params
            i_detlevel       = detlevel
          EXCEPTIONS
            log_not_found    = 1
            msg_inconsistent = 2
            log_is_full      = 3
            OTHERS           = 4.
      CATCH cx_sy_dyn_call_param_not_found.
        CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
          EXPORTING
            i_log_handle     = handle
            i_msgty          = message_type
            i_probclass      = importance
            i_text           = free_text
            i_s_context      = formatted_context
            i_s_params       = formatted_params
          EXCEPTIONS
            log_not_found    = 1
            msg_inconsistent = 2
            log_is_full      = 3
            OTHERS           = 4.
    ENDTRY.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_logger
        EXPORTING
          info = get_system_message( ).
    ENDIF.

  ENDMETHOD.


  METHOD add_message.

    DATA detailed_msg TYPE bal_s_msg.

    detailed_msg           = message.
    detailed_msg-context   = formatted_context.
    detailed_msg-params    = formatted_params.
    detailed_msg-probclass = importance.
    detailed_msg-detlevel  = detlevel.

    IF type IS NOT INITIAL.
      detailed_msg-msgty = type.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = handle
        i_s_msg          = detailed_msg
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_logger
        EXPORTING
          info = get_system_message( ).
    ENDIF.

  ENDMETHOD.


  METHOD add_object.

    DATA loggable                 TYPE REF TO zif_logger_log_object.
    DATA loggable_object_messages TYPE zif_logger_log_object=>tty_messages.
    DATA symsg                    TYPE symsg.
    DATA message_type             TYPE symsgty.

    FIELD-SYMBOLS <loggable_object_message> TYPE zif_logger_log_object=>ty_message.

    TRY.
        loggable ?= obj_to_log.
        loggable_object_messages = loggable->get_message_table( ).

        " TODO: Context is passed to all messages. Maybe it should be only the first one (see add_table).
        LOOP AT loggable_object_messages ASSIGNING <loggable_object_message>.
          IF <loggable_object_message>-symsg IS NOT INITIAL.
            MOVE-CORRESPONDING <loggable_object_message>-symsg TO symsg.
            symsg-msgty = <loggable_object_message>-type.

            zif_logger~add(
              obj_to_log          = symsg
              context             = context
              callback_form       = callback_form
              callback_prog       = callback_prog
              callback_fm         = callback_fm
              callback_parameters = callback_parameters
              importance          = importance
              detlevel            = detlevel ).
          ENDIF.

          IF <loggable_object_message>-exception IS BOUND.
            zif_logger~add(
              type                = <loggable_object_message>-type
              obj_to_log          = <loggable_object_message>-exception
              context             = context
              callback_form       = callback_form
              callback_prog       = callback_prog
              callback_fm         = callback_fm
              callback_parameters = callback_parameters
              importance          = importance
              detlevel            = detlevel ).
          ENDIF.

          IF <loggable_object_message>-string IS NOT INITIAL.
            zif_logger~add(
              type                = <loggable_object_message>-type
              obj_to_log          = <loggable_object_message>-string
              context             = context
              callback_form       = callback_form
              callback_prog       = callback_prog
              callback_fm         = callback_fm
              callback_parameters = callback_parameters
              importance          = importance
              detlevel            = detlevel ).
          ENDIF.
        ENDLOOP.

      CATCH cx_sy_move_cast_error.
        IF type IS INITIAL.
          message_type = if_msg_output=>msgtype_error.
        ELSE.
          message_type = type.
        ENDIF.

        " Return exceptions to log
        result = drill_down_into_exception(
          exception  = obj_to_log
          type       = message_type
          importance = importance
          detlevel   = detlevel ).
    ENDTRY.

  ENDMETHOD.


  METHOD add_structure.

    DATA component_type  TYPE REF TO cl_abap_typedescr.
    DATA struct_type     TYPE REF TO cl_abap_structdescr.
    DATA components      TYPE abap_compdescr_tab.
    DATA component       LIKE LINE OF components.
    DATA component_name  LIKE component-name.
    DATA string_to_log   TYPE string.

    FIELD-SYMBOLS <component> TYPE any.

    zif_logger~add(
      obj_to_log          = '--- Begin of structure ---'
      context             = context
      callback_form       = callback_form
      callback_prog       = callback_prog
      callback_fm         = callback_fm
      callback_parameters = callback_parameters ).

    struct_type ?= cl_abap_typedescr=>describe_by_data( obj_to_log ).
    components   = struct_type->components.

    LOOP AT components INTO component.
      component_name = component-name.
      ASSIGN COMPONENT component_name OF STRUCTURE obj_to_log TO <component>.
      IF sy-subrc <> 0.
        " It might be an unnamed component like .INCLUDE
        component_name = |Include { sy-tabix }|.
        ASSIGN COMPONENT sy-tabix OF STRUCTURE obj_to_log TO <component>.
      ENDIF.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      component_type = cl_abap_typedescr=>describe_by_data( <component> ).

      IF component_type->kind = cl_abap_typedescr=>kind_elem.
        string_to_log = |{ to_lower( component_name ) } = { <component> }|.
        zif_logger~add(
          obj_to_log          = string_to_log
          callback_form       = callback_form
          callback_prog       = callback_prog
          callback_fm         = callback_fm
          callback_parameters = callback_parameters
          type                = type
          importance          = importance
          detlevel            = detlevel ).
      ELSEIF component_type->kind = cl_abap_typedescr=>kind_struct.
        add_structure(
          obj_to_log          = <component>
          callback_form       = callback_form
          callback_prog       = callback_prog
          callback_fm         = callback_fm
          callback_parameters = callback_parameters
          type                = type
          importance          = importance
          detlevel            = detlevel ).
      ENDIF.
    ENDLOOP.

    zif_logger~add( '--- End of structure ---' ).

  ENDMETHOD.


  METHOD add_table.

    FIELD-SYMBOLS <table_of_messages> TYPE ANY TABLE.
    FIELD-SYMBOLS <message_line>      TYPE any.

    ASSIGN obj_to_log TO <table_of_messages>.

    LOOP AT <table_of_messages> ASSIGNING <message_line>.
      " Context only on first message
      IF sy-tabix = 1.
        zif_logger~add(
          obj_to_log          = <message_line>
          context             = context
          callback_form       = callback_form
          callback_prog       = callback_prog
          callback_fm         = callback_fm
          callback_parameters = callback_parameters
          importance          = importance
          type                = type
          detlevel            = detlevel ).
      ELSE.
        zif_logger~add(
          obj_to_log          = <message_line>
          callback_form       = callback_form
          callback_prog       = callback_prog
          callback_fm         = callback_fm
          callback_parameters = callback_parameters
          importance          = importance
          type                = type
          detlevel            = detlevel ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD drill_down_into_exception.

    DATA i                  TYPE i VALUE 2.
    DATA previous_exception TYPE REF TO cx_root.
    DATA exceptions         TYPE tty_exception.

    FIELD-SYMBOLS <ex>  LIKE LINE OF exceptions.
    FIELD-SYMBOLS <ret> LIKE LINE OF result.

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

    " Display the deepest exception first
    SORT exceptions BY level DESCENDING.
    LOOP AT exceptions ASSIGNING <ex>.
      APPEND INITIAL LINE TO result ASSIGNING <ret>.
      <ret>-exception = <ex>-exception.
      <ret>-msgty     = type.
      <ret>-probclass = importance.
      <ret>-detlevel  = detlevel.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_context.

    DATA ctx_type        TYPE REF TO cl_abap_typedescr.
    DATA ctx_ddic_header TYPE x030l.

    FIELD-SYMBOLS <context_val> TYPE any.

    CHECK context IS NOT INITIAL.

    ASSIGN context TO <context_val>.
    result-value = <context_val>.

    ctx_type = cl_abap_typedescr=>describe_by_data( context ).

    ctx_type->get_ddic_header(
      RECEIVING
        p_header     = ctx_ddic_header
      EXCEPTIONS
        not_found    = 1
        no_ddic_type = 2
        OTHERS       = 3 ).
    IF sy-subrc = 0.
      result-tabname = ctx_ddic_header-tabname.
    ENDIF.

  ENDMETHOD.


  METHOD get_message_handles.

    DATA log_handle TYPE bal_t_logh.
    DATA filter     TYPE bal_s_mfil.

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
        e_t_msg_handle = result
      EXCEPTIONS
        msg_not_found  = 0
        OTHERS         = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_logger
        EXPORTING
          info = get_system_message( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_parameters.

    IF callback_fm IS NOT INITIAL.
      result-callback-userexitf = callback_fm.
      result-callback-userexitp = callback_prog.
      result-callback-userexitt = 'F'.
      result-t_par              = callback_parameters.
    ELSEIF callback_form IS NOT INITIAL.
      result-callback-userexitf = callback_form.
      result-callback-userexitp = callback_prog.
      result-callback-userexitt = ' '.
      result-t_par              = callback_parameters.
    ENDIF.

  ENDMETHOD.


  METHOD get_system_message.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO result.

  ENDMETHOD.


  METHOD save_log.

    DATA log_handles       TYPE bal_t_logh.
    DATA log_numbers       TYPE bal_t_lgnm.
    DATA log_number        TYPE bal_s_lgnm.
    DATA secondary_db_conn TYPE flag.

    secondary_db_conn = settings->get_usage_of_secondary_db_conn( ).

    INSERT handle INTO TABLE log_handles.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle       = log_handles
        i_2th_connection     = secondary_db_conn
        i_2th_connect_commit = secondary_db_conn
      IMPORTING
        e_new_lognumbers     = log_numbers
      EXCEPTIONS
        log_not_found        = 1
        save_not_allowed     = 2
        numbering_error      = 3
        OTHERS               = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_logger
        EXPORTING
          info = get_system_message( ).
    ENDIF.

    IF db_number IS INITIAL.
      READ TABLE log_numbers INDEX 1 INTO log_number.
      db_number = log_number-lognumber.
    ENDIF.

    IF sy-batch = abap_true.
      CALL FUNCTION 'BP_ADD_APPL_LOG_HANDLE'
        EXPORTING
          loghandle = handle
        EXCEPTIONS
          OTHERS    = 0 ##FM_SUBRC_OK.
    ENDIF.

  ENDMETHOD.


  METHOD zif_logger_log_object~get_message_table.

    DATA message_handles TYPE bal_t_msgh.
    DATA message         TYPE bal_s_msg.
    DATA message_result  TYPE zif_logger_log_object~ty_message.

    FIELD-SYMBOLS <msg_handle> TYPE balmsghndl.

    message_handles = get_message_handles( ).

    LOOP AT message_handles ASSIGNING <msg_handle>.
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = <msg_handle>
        IMPORTING
          e_s_msg        = message
        EXCEPTIONS
          log_not_found  = 1
          msg_not_found  = 0
          OTHERS         = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING
            info = get_system_message( ).
      ENDIF.

      IF message IS NOT INITIAL.
        message_result-type        = message-msgty.
        message_result-symsg-msgid = message-msgid.
        message_result-symsg-msgno = message-msgno.
        message_result-symsg-msgv1 = message-msgv1.
        message_result-symsg-msgv2 = message-msgv2.
        message_result-symsg-msgv3 = message-msgv3.
        message_result-symsg-msgv4 = message-msgv4.
        APPEND message_result TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_logger~abend.
    result = zif_logger~add(
      obj_to_log          = obj_to_log
      context             = context
      callback_form       = callback_form
      callback_prog       = callback_prog
      callback_fm         = callback_fm
      callback_parameters = callback_parameters
      type                = 'A'
      importance          = importance
      detlevel            = detlevel ).
  ENDMETHOD.


  METHOD zif_logger~add.

    DATA data_type         TYPE REF TO cl_abap_typedescr.
    DATA message           TYPE bal_s_msg.
    DATA formatted_context TYPE bal_s_cont.
    DATA formatted_params  TYPE bal_s_parm.
    DATA exceptions        TYPE tty_exception_data.

    FIELD-SYMBOLS <exception> LIKE LINE OF exceptions.

    result = me.

    CHECK obj_to_log IS NOT INITIAL.

    formatted_context = get_context( context ).

    formatted_params = get_parameters(
      callback_fm         = callback_fm
      callback_prog       = callback_prog
      callback_form       = callback_form
      callback_parameters = callback_parameters ).

    data_type = cl_abap_typedescr=>describe_by_data( obj_to_log ).

    CASE data_type->type_kind.
      WHEN cl_abap_typedescr=>typekind_oref.

        " Any objects including exceptions
        exceptions = add_object(
          obj_to_log          = obj_to_log
          context             = context
          callback_form       = callback_form
          callback_prog       = callback_prog
          callback_fm         = callback_fm
          callback_parameters = callback_parameters
          type                = type
          importance          = importance
          detlevel            = detlevel ).

        LOOP AT exceptions ASSIGNING <exception>.
          add_exception(
            exception_data    = <exception>
            formatted_context = formatted_context
            formatted_params  = formatted_params ).
        ENDLOOP.

      WHEN cl_abap_typedescr=>typekind_table.

        " Internal tables
        add_table(
          obj_to_log          = obj_to_log
          context             = context
          callback_form       = callback_form
          callback_prog       = callback_prog
          callback_fm         = callback_fm
          callback_parameters = callback_parameters
          type                = type
          importance          = importance
          detlevel            = detlevel ).

      WHEN cl_abap_typedescr=>typekind_struct1 OR cl_abap_typedescr=>typekind_struct2.
        " flat or deep structure (already when string is used)

        " Predefined or other structures
        message = lcl_helper=>get_bal_message( obj_to_log ).

        IF message IS NOT INITIAL.
          add_message(
            message           = message
            formatted_context = formatted_context
            formatted_params  = formatted_params
            type              = type
            importance        = importance
            detlevel          = detlevel ).
        ELSE.
          add_structure(
            obj_to_log          = obj_to_log
            context             = context
            callback_form       = callback_form
            callback_prog       = callback_prog
            callback_fm         = callback_fm
            callback_parameters = callback_parameters
            type                = type
            importance          = importance
            detlevel            = detlevel ).
        ENDIF.

      WHEN OTHERS.

        " Anything else treat as text
        add_free_text(
          free_text         = |{ obj_to_log }|
          formatted_context = formatted_context
          formatted_params  = formatted_params
          type              = type
          importance        = importance
          detlevel          = detlevel ).

    ENDCASE.

    IF settings->get_autosave( ) = abap_true.
      save_log( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_logger~error.
    result = zif_logger~add(
      obj_to_log          = obj_to_log
      context             = context
      callback_form       = callback_form
      callback_prog       = callback_prog
      callback_fm         = callback_fm
      callback_parameters = callback_parameters
      type                = if_msg_output=>msgtype_error
      importance          = importance
      detlevel            = detlevel ).
  ENDMETHOD.


  METHOD zif_logger~exit.
    result = zif_logger~add(
      obj_to_log          = obj_to_log
      context             = context
      callback_form       = callback_form
      callback_prog       = callback_prog
      callback_fm         = callback_fm
      callback_parameters = callback_parameters
      type                = 'X'
      importance          = importance
      detlevel            = detlevel ).
  ENDMETHOD.


  METHOD zif_logger~export_to_table.

    DATA message_handles TYPE bal_t_msgh.
    DATA message         TYPE bal_s_msg.
    DATA bapiret2        TYPE bapiret2.
    DATA exception_msg   TYPE c LENGTH 255.

    FIELD-SYMBOLS <msg_handle> TYPE balmsghndl.

    message_handles = get_message_handles( ).

    LOOP AT message_handles ASSIGNING <msg_handle>.
      CLEAR bapiret2.
      CLEAR message.

      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = <msg_handle>
        IMPORTING
          e_s_msg        = message
        EXCEPTIONS
          log_not_found  = 1
          msg_not_found  = 0
          OTHERS         = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_logger
          EXPORTING
            info = get_system_message( ).
      ENDIF.

      IF message IS NOT INITIAL.
        MESSAGE ID message-msgid TYPE message-msgty NUMBER message-msgno
          WITH message-msgv1 message-msgv2 message-msgv3 message-msgv4 INTO bapiret2-message.

        bapiret2-type       = message-msgty.
        bapiret2-id         = message-msgid.
        bapiret2-number     = message-msgno.
        bapiret2-log_no     = <msg_handle>-log_handle.     " last 2 chars missing!!
        bapiret2-log_msg_no = <msg_handle>-msgnumber.
        bapiret2-message_v1 = message-msgv1.
        bapiret2-message_v2 = message-msgv2.
        bapiret2-message_v3 = message-msgv3.
        bapiret2-message_v4 = message-msgv4.
        bapiret2-system     = sy-sysid.
        APPEND bapiret2 TO result.
      ELSE.
        CALL FUNCTION 'BAL_LOG_EXCEPTION_READ'
          EXPORTING
            i_s_msg_handle = <msg_handle>
            i_langu        = sy-langu
          IMPORTING
            e_txt_msg      = exception_msg
          EXCEPTIONS
            log_not_found  = 1
            msg_not_found  = 2
            OTHERS         = 3.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_logger
            EXPORTING
              info = get_system_message( ).
        ENDIF.

        bapiret2-type       = message-msgty.
        bapiret2-log_no     = <msg_handle>-log_handle.
        bapiret2-log_msg_no = <msg_handle>-msgnumber.
        bapiret2-message    = exception_msg.
        bapiret2-system     = sy-sysid.
        APPEND bapiret2 TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_logger~free.

    " Save any messages (safety) only if an object has been defined
    IF header-object IS NOT INITIAL.
      zif_logger~save( ).
    ENDIF.

    " Clear log from memory
    CALL FUNCTION 'BAL_LOG_REFRESH'
      EXPORTING
        i_log_handle  = handle
      EXCEPTIONS
        log_not_found = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_logger
        EXPORTING
          info = get_system_message( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_logger~get_db_number.
    result = db_number.
  ENDMETHOD.


  METHOD zif_logger~get_handle.
    result = handle.
  ENDMETHOD.


  METHOD zif_logger~get_header.
    result = header.
  ENDMETHOD.


  METHOD zif_logger~has_errors.
    result = boolc( lines( get_message_handles( msgtype = 'E' ) ) > 0 ).
  ENDMETHOD.


  METHOD zif_logger~has_warnings.
    result = boolc( lines( get_message_handles( msgtype = 'W' ) ) > 0 ).
  ENDMETHOD.


  METHOD zif_logger~info.
    result = zif_logger~add(
      obj_to_log          = obj_to_log
      context             = context
      callback_form       = callback_form
      callback_prog       = callback_prog
      callback_fm         = callback_fm
      callback_parameters = callback_parameters
      type                = if_msg_output=>msgtype_info
      importance          = importance
      detlevel            = detlevel ).
  ENDMETHOD.


  METHOD zif_logger~is_empty.
    result = boolc( zif_logger~length( ) = 0 ).
  ENDMETHOD.


  METHOD zif_logger~length.
    result = lines( get_message_handles( ) ).
  ENDMETHOD.


  METHOD zif_logger~save.
    CHECK settings->get_autosave( ) = abap_false.
    save_log( ).
  ENDMETHOD.


  METHOD zif_logger~set_header.

    header-extnumber = description.

    CALL FUNCTION 'BAL_LOG_HDR_CHANGE'
      EXPORTING
        i_log_handle            = handle
        i_s_log                 = header
      EXCEPTIONS
        log_not_found           = 1
        log_header_inconsistent = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_logger
        EXPORTING
          info = get_system_message( ).
    ENDIF.

    result = me.

  ENDMETHOD.


  METHOD zif_logger~success.
    result = zif_logger~add(
      obj_to_log          = obj_to_log
      context             = context
      callback_form       = callback_form
      callback_prog       = callback_prog
      callback_fm         = callback_fm
      callback_parameters = callback_parameters
      type                = if_msg_output=>msgtype_success
      importance          = importance
      detlevel            = detlevel ).
  ENDMETHOD.


  METHOD zif_logger~trace.
    result = zif_logger~add(
      obj_to_log          = obj_to_log
      context             = context
      callback_form       = callback_form
      callback_prog       = callback_prog
      callback_fm         = callback_fm
      callback_parameters = callback_parameters
      type                = ' '
      importance          = importance
      detlevel            = detlevel ).
  ENDMETHOD.


  METHOD zif_logger~warning.
    result = zif_logger~add(
      obj_to_log          = obj_to_log
      context             = context
      callback_form       = callback_form
      callback_prog       = callback_prog
      callback_fm         = callback_fm
      callback_parameters = callback_parameters
      type                = if_msg_output=>msgtype_warning
      importance          = importance
      detlevel            = detlevel ).
  ENDMETHOD.
ENDCLASS.
