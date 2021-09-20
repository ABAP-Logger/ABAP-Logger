class ltd_loggable_object definition create public FOR TESTING.

  public section.
  DATA messages TYPE zif_loggable_object=>tty_message .
  INTERFACES zif_loggable_object.
  protected section.
  private section.

endclass.

class ltd_loggable_object implementation.

  method zif_loggable_object~get_message_table.
    r_result = messages.
  endmethod.

endclass.

class lcl_test definition for testing
  duration short
  risk level harmless.
  private section.

    data:
      anon_log     type ref to zif_logger,
      named_log    type ref to zif_logger,
      reopened_log type ref to zif_logger.

    class-methods:
      class_setup.

    methods:
      setup,
      teardown,
      get_first_message
        importing log_handle type balloghndl
        returning value(msg) type char255,
      get_messages
        importing
          log_handle  type balloghndl
        exporting
          texts       type table_of_strings
          msg_details type bal_tt_msg,

      format_message
        importing id         like sy-msgid default sy-msgid
                  lang       type langu default '-'
                  no         like sy-msgno default sy-msgno
                  v1         like sy-msgv1 default sy-msgv1
                  v2         like sy-msgv2 default sy-msgv2
                  v3         like sy-msgv3 default sy-msgv3
                  v4         like sy-msgv4 default sy-msgv4
        returning value(msg) type string,

      can_create_anon_log for testing,
      can_create_named_log for testing,
      can_reopen_log for testing,
      can_create_expiring_log_days for testing,
      can_create_expiring_log_date for testing,
      can_open_or_create for testing,

      can_add_log_context for testing,

      can_add_to_log for testing,
      can_add_to_named_log for testing,

      auto_saves_named_log for testing,
      auto_saves_reopened_log for testing,

      can_log_string for testing,
      can_log_char   for testing,
      can_log_symsg for testing,
      can_log_bapiret1  for testing,
      can_log_bapiret2  for testing,
      can_log_bapi_coru_return for testing,
      can_log_bapi_order_return for testing,
      can_log_rcomp     for testing,
      can_log_prott for testing,
      can_log_bapirettab for testing,
      can_log_err for testing,
      can_log_chained_exceptions for testing,
      can_log_batch_msgs for testing,
      can_log_any_simple_structure for testing,
      can_log_any_deep_structure for testing,
      can_log_loggable_object for testing,

      can_add_msg_context for testing,
      can_add_callback_sub for testing,
      can_add_callback_fm  for testing,

      must_use_factory for testing,

      can_use_and_chain_aliases for testing,

      return_proper_status for testing,
      return_proper_length for testing,
      can_add_table_msg_context FOR TESTING RAISING cx_static_check.

endclass.       "lcl_Test

class lcl_test implementation.

  method class_setup.
    zcl_logger=>new(
      object = 'ABAPUNIT'
      subobject = 'LOGGER'
      desc = 'Log saved in database' )->add( 'This message is in the database' ).
  endmethod.

  method setup.
    anon_log  = zcl_logger=>new( ).
    named_log = zcl_logger=>new( object = 'ABAPUNIT'
                                 subobject = 'LOGGER'
                                 desc = `Hey it's a log` ).
    reopened_log = zcl_logger=>open( object = 'ABAPUNIT'
                                     subobject = 'LOGGER'
                                     desc = 'Log saved in database' ).
  endmethod.

  method can_create_anon_log.
    cl_aunit_assert=>assert_bound(
      act = anon_log
      msg = 'Cannot Instantiate Anonymous Log' ).
  endmethod.

  method can_create_named_log.
    cl_aunit_assert=>assert_bound(
      act = named_log
      msg = 'Cannot Instantiate Named Log' ).
  endmethod.

  method can_create_expiring_log_days.
    data expiring_log type ref to zif_logger.
    data act_header type bal_s_log.
    constants days_until_log_can_be_deleted type i value 365.

    expiring_log = zcl_logger_factory=>create_log(
      object    = 'ABAPUNIT'
      subobject = 'LOGGER'
      desc      = 'Log that is not deletable and expiring'
      settings  = zcl_logger_factory=>create_settings(
        )->set_expiry_in_days( days_until_log_can_be_deleted
        )->set_must_be_kept_until_expiry( abap_true )
    ).

    cl_aunit_assert=>assert_bound(
      act = expiring_log
      msg = 'Cannot Instantiate Expiring Log' ).

    call function 'BAL_LOG_HDR_READ'
      exporting
        i_log_handle = expiring_log->handle
      importing
        e_s_log      = act_header.

    data lv_exp type d.
    lv_exp = sy-datum + days_until_log_can_be_deleted.

    cl_aunit_assert=>assert_equals(
      exporting
        exp     = lv_exp
        act     = act_header-aldate_del
        msg     = 'Log is not expiring in correct amount of days'
    ).

    cl_aunit_assert=>assert_equals(
      exporting
        exp     = abap_true
        act     = act_header-del_before
        msg     = 'Log should not be deletable before expiry date'
    ).
  endmethod.

  method can_create_expiring_log_date.
    data expiring_log type ref to zif_logger.
    data act_header type bal_s_log.
    constants days_until_log_can_be_deleted type i value 365.

    data lv_expire type d.
    lv_expire = sy-datum + days_until_log_can_be_deleted.

    expiring_log = zcl_logger_factory=>create_log(
      object    = 'ABAPUNIT'
      subobject = 'LOGGER'
      desc      = 'Log that is not deletable and expiring'
      settings  = zcl_logger_factory=>create_settings(
        )->set_expiry_date( lv_expire
        )->set_must_be_kept_until_expiry( abap_true )
    ).

    cl_aunit_assert=>assert_bound(
      act = expiring_log
      msg = 'Cannot Instantiate Expiring Log' ).

    call function 'BAL_LOG_HDR_READ'
      exporting
        i_log_handle = expiring_log->handle
      importing
        e_s_log      = act_header.

    cl_aunit_assert=>assert_equals(
      exporting
        exp     = lv_expire
        act     = act_header-aldate_del
        msg     = 'Log is not expiring on correct date'
    ).

    cl_aunit_assert=>assert_equals(
      exporting
        exp     = abap_true
        act     = act_header-del_before
        msg     = 'Log should not be deletable before expiry date'
    ).
  endmethod.

  method can_reopen_log.
    cl_aunit_assert=>assert_bound(
      act = reopened_log
      msg = 'Cannot Reopen Log from DB' ).
  endmethod.

  method can_open_or_create.
    data: created_log type ref to zif_logger,
          handles     type bal_t_logh.
    call function 'BAL_GLB_MEMORY_REFRESH'. "Close Logs
    reopened_log = zcl_logger=>open( object = 'ABAPUNIT'
                                     subobject = 'LOGGER'
                                     desc = 'Log saved in database'
                                     create_if_does_not_exist = abap_true ).
    created_log = zcl_logger=>open( object = 'ABAPUNIT'
                                    subobject = 'LOGGER'
                                    desc = 'Log not in database'
                                    create_if_does_not_exist = abap_true ).
    call function 'BAL_GLB_SEARCH_LOG'
      importing
        e_t_log_handle = handles.

    cl_aunit_assert=>assert_equals(
      exp = 2
      act = lines( handles )
      msg = 'Did not create nonexistent log from OPEN' ).

  endmethod.

  method can_add_log_context.

    data: log                 type ref to zif_logger,
          random_country_data type t005t,
          act_header          type bal_s_log.

    random_country_data-mandt = sy-mandt.
    random_country_data-spras = 'D'.
    random_country_data-land1 = 'DE'.

    log = zcl_logger=>new( context = random_country_data ).

    call function 'BAL_LOG_HDR_READ'
      exporting
        i_log_handle = log->handle
      importing
        e_s_log      = act_header.

    cl_aunit_assert=>assert_equals(
      exp = 'T005T'
      act = act_header-context-tabname
      msg = 'Did not add context to log' ).

    cl_aunit_assert=>assert_equals(
      exp = random_country_data
      act = act_header-context-value
      msg = 'Did not add context to log' ).

  endmethod.

  method can_add_to_log.
    data: dummy type c.

    message s001(00) with 'I' 'test' 'the' 'logger.' into dummy.
    anon_log->add( ).

    cl_aunit_assert=>assert_equals(
      exp = 'Itestthelogger.'
      act = get_first_message( anon_log->handle )
      msg = 'Did not log system message properly' ).

  endmethod.

  method can_add_to_named_log.
    data: dummy type c.

    message s001(00) with 'Testing' 'a' 'named' 'logger.' into dummy.
    named_log->add( ).

    cl_aunit_assert=>assert_equals(
      exp = 'Testinganamedlogger.'
      act = get_first_message( named_log->handle )
      msg = 'Did not write to named log' ).
  endmethod.


  method auto_saves_named_log.
    data: dummy       type c,
          log_numbers type bal_t_logn,
          msg         type string.

    message s000(sabp_unit) with 'Testing' 'logger' 'that' 'saves.' into dummy.
    named_log->add( ).
    msg = format_message( ).

    call function 'BAL_GLB_MEMORY_REFRESH'.

    append named_log->db_number to log_numbers.
    call function 'BAL_DB_LOAD'
      exporting
        i_t_lognumber = log_numbers.

    cl_aunit_assert=>assert_equals(
      exp = msg
      act = get_first_message( named_log->handle )
      msg = 'Did not write to named log' ).

  endmethod.

  method auto_saves_reopened_log.
    data: log_numbers type bal_t_logn,
          act_texts   type table_of_strings,
          act_text    type string.
    reopened_log->add( 'This is another message in the database' ).
    call function 'BAL_GLB_MEMORY_REFRESH'.

    append reopened_log->db_number to log_numbers.
    call function 'BAL_DB_LOAD'
      exporting
        i_t_lognumber = log_numbers.

    get_messages( exporting log_handle  = reopened_log->handle
                  importing texts       = act_texts ).

    read table act_texts index 1 into act_text.
    cl_aunit_assert=>assert_equals(
      exp = 'This message is in the database'
      act = act_text
      msg = 'Did not autosave to reopened log' ).

    read table act_texts index 2 into act_text.
    cl_aunit_assert=>assert_equals(
      exp = 'This is another message in the database'
      act = act_text
      msg = 'Did not autosave to reopened log' ).

  endmethod.

  method can_log_string.
    data: stringmessage type string value `Logging a string, guys!`.
    anon_log->add( stringmessage ).

    cl_aunit_assert=>assert_equals(
      exp = stringmessage
      act = get_first_message( anon_log->handle )
      msg = 'Did not log system message properly' ).

  endmethod.

  method can_log_char.
    data: charmessage type char70 value 'Logging a char sequence!'.
    anon_log->add( charmessage ).

    cl_aunit_assert=>assert_equals(
      exp = charmessage
      act = get_first_message( anon_log->handle )
      msg = 'Did not log system message properly' ).
  endmethod.

  method can_log_symsg.
    data: symsg            type symsg,
          msg_handle       type balmsghndl,
          expected_details type bal_s_msg,
          actual_details   type bal_s_msg,
          actual_text      type char200.

    expected_details-msgty = symsg-msgty = 'W'.
    expected_details-msgid = symsg-msgid = 'BL'.
    expected_details-msgno = symsg-msgno = '001'.
    expected_details-msgv1 = symsg-msgv1 = 'This'.
    expected_details-msgv2 = symsg-msgv2 = 'is'.
    expected_details-msgv3 = symsg-msgv3 = 'a'.
    expected_details-msgv4 = symsg-msgv4 = 'test'.

    anon_log->add( symsg ).

    msg_handle-log_handle = anon_log->handle.
    msg_handle-msgnumber  = '000001'.

    call function 'BAL_LOG_MSG_READ'
      exporting
        i_s_msg_handle = msg_handle
      importing
        e_s_msg        = actual_details
        e_txt_msg      = actual_text.

    cl_aunit_assert=>assert_not_initial(
      act = actual_details-time_stmp
      msg = 'Did not log system message properly' ).

    expected_details-msg_count = 1.
    clear actual_details-time_stmp.

    cl_aunit_assert=>assert_equals(
      exp = expected_details
      act = actual_details
      msg = 'Did not log system message properly' ).

    cl_aunit_assert=>assert_equals(
      exp = 'This is a test'
      act = condense( actual_text )
      msg = 'Did not log system message properly' ).

    cl_aunit_assert=>assert_equals(
      exp = abap_true
      act = anon_log->has_warnings( )
      msg = 'Did not log or fetch system message properly'
    ).

  endmethod.

  method can_log_bapiret1.
    data: bapi_msg         type bapiret1,
          msg_handle       type balmsghndl,
          expected_details type bal_s_msg,
          actual_details   type bal_s_msg,
          actual_text      type char200.

    expected_details-msgty = bapi_msg-type = 'W'.
    expected_details-msgid = bapi_msg-id = 'BL'.
    expected_details-msgno = bapi_msg-number = '001'.
    expected_details-msgv1 = bapi_msg-message_v1 = 'This'.
    expected_details-msgv2 = bapi_msg-message_v2 = 'is'.
    expected_details-msgv3 = bapi_msg-message_v3 = 'a'.
    expected_details-msgv4 = bapi_msg-message_v4 = 'test'.

    anon_log->add( bapi_msg ).

    msg_handle-log_handle = anon_log->handle.
    msg_handle-msgnumber  = '000001'.

    call function 'BAL_LOG_MSG_READ'
      exporting
        i_s_msg_handle = msg_handle
      importing
        e_s_msg        = actual_details
        e_txt_msg      = actual_text.

    cl_aunit_assert=>assert_not_initial(
      act = actual_details-time_stmp
      msg = 'Did not log system message properly' ).

    expected_details-msg_count = 1.
    clear actual_details-time_stmp.

    cl_aunit_assert=>assert_equals(
      exp = expected_details
      act = actual_details
      msg = 'Did not log system message properly' ).

    cl_aunit_assert=>assert_equals(
      exp = 'This is a test'
      act = condense( actual_text )
      msg = 'Did not log system message properly' ).

    cl_aunit_assert=>assert_equals(
      exp = abap_true
      act = anon_log->has_warnings( )
      msg = 'Did not log or fetch system message properly'
    ).

  endmethod.

  method can_log_bapiret2.
    data: bapi_msg         type bapiret2,
          msg_handle       type balmsghndl,
          expected_details type bal_s_msg,
          actual_details   type bal_s_msg,
          actual_text      type char200.

    expected_details-msgty = bapi_msg-type = 'W'.
    expected_details-msgid = bapi_msg-id = 'BL'.
    expected_details-msgno = bapi_msg-number = '001'.
    expected_details-msgv1 = bapi_msg-message_v1 = 'This'.
    expected_details-msgv2 = bapi_msg-message_v2 = 'is'.
    expected_details-msgv3 = bapi_msg-message_v3 = 'a'.
    expected_details-msgv4 = bapi_msg-message_v4 = 'test'.

    anon_log->add( bapi_msg ).

    msg_handle-log_handle = anon_log->handle.
    msg_handle-msgnumber  = '000001'.

    call function 'BAL_LOG_MSG_READ'
      exporting
        i_s_msg_handle = msg_handle
      importing
        e_s_msg        = actual_details
        e_txt_msg      = actual_text.

    cl_aunit_assert=>assert_not_initial(
      act = actual_details-time_stmp
      msg = 'Did not log system message properly' ).

    expected_details-msg_count = 1.
    clear actual_details-time_stmp.

    cl_aunit_assert=>assert_equals(
      exp = expected_details
      act = actual_details
      msg = 'Did not log system message properly' ).

    cl_aunit_assert=>assert_equals(
      exp = 'This is a test'
      act = condense( actual_text )
      msg = 'Did not log system message properly' ).

    cl_aunit_assert=>assert_equals(
      exp = abap_true
      act = anon_log->has_warnings( )
      msg = 'Did not log or fetch system message properly'
    ).
  endmethod.

  method can_log_bapi_coru_return.
    data: bapi_msg         type bapi_coru_return,
          msg_handle       type balmsghndl,
          expected_details type bal_s_msg,
          actual_details   type bal_s_msg,
          actual_text      type char200.

    expected_details-msgty = bapi_msg-type = 'W'.
    expected_details-msgid = bapi_msg-id = 'BL'.
    expected_details-msgno = bapi_msg-number = '001'.
    expected_details-msgv1 = bapi_msg-message_v1 = 'This'.
    expected_details-msgv2 = bapi_msg-message_v2 = 'is'.
    expected_details-msgv3 = bapi_msg-message_v3 = 'a'.
    expected_details-msgv4 = bapi_msg-message_v4 = 'test'.

    anon_log->add( bapi_msg ).

    msg_handle-log_handle = anon_log->handle.
    msg_handle-msgnumber  = '000001'.

    call function 'BAL_LOG_MSG_READ'
      exporting
        i_s_msg_handle = msg_handle
      importing
        e_s_msg        = actual_details
        e_txt_msg      = actual_text.

    cl_aunit_assert=>assert_not_initial(
      act = actual_details-time_stmp
      msg = 'Did not log system message properly' ).

    expected_details-msg_count = 1.
    clear actual_details-time_stmp.

    cl_aunit_assert=>assert_equals(
      exp = expected_details
      act = actual_details
      msg = 'Did not log system message properly' ).

    cl_aunit_assert=>assert_equals(
      exp = 'This is a test'
      act = condense( actual_text )
      msg = 'Did not log system message properly' ).

    cl_aunit_assert=>assert_equals(
      exp = abap_true
      act = anon_log->has_warnings( )
      msg = 'Did not log or fetch system message properly'
    ).
  endmethod.

  method can_log_bapi_order_return.
    data: msg_handle       type balmsghndl,
          expected_details type bal_s_msg,
          actual_details   type bal_s_msg,
          actual_text      type char200.

"Solution manager doens't have BAPI_ORDER_RETURN. Therefore avoid using the concrete type
    DATA bapi_order_return_data_ref TYPE REF TO DATA.
    DATA bapi_return_temp TYPE bapiret2."these fields have the same name as BAPI_ORDER_RETURN
    FIELD-SYMBOLS <bapi_order_return_structure> TYPE ANY.
    TRY.
        CREATE DATA bapi_order_return_data_ref TYPE ('BAPI_ORDER_RETURN').
    CATCH CX_SY_CREATE_DATA_ERROR.
      RETURN."Non ECC System such as SolutionManager
    ENDTRY.
    ASSIGN bapi_order_return_data_ref->* TO <bapi_order_return_structure>.

    expected_details-msgty = bapi_return_temp-type = 'E'.
    expected_details-msgid = bapi_return_temp-id = 'BL'.
    expected_details-msgno = bapi_return_temp-number = '001'.
    expected_details-msgv1 = bapi_return_temp-message_v1 = 'This'.
    expected_details-msgv2 = bapi_return_temp-message_v2 = 'is'.
    expected_details-msgv3 = bapi_return_temp-message_v3 = 'a'.
    expected_details-msgv4 = bapi_return_temp-message_v4 = 'test'.
    MOVE-CORRESPONDING bapi_return_temp TO <bapi_order_return_structure>.
    anon_log->add( <bapi_order_return_structure> ).

    msg_handle-log_handle = anon_log->handle.
    msg_handle-msgnumber  = '000001'.

    call function 'BAL_LOG_MSG_READ'
      exporting
        i_s_msg_handle = msg_handle
      importing
        e_s_msg        = actual_details
        e_txt_msg      = actual_text.

    cl_aunit_assert=>assert_not_initial(
      act = actual_details-time_stmp
      msg = 'Did not log system message properly' ).

    expected_details-msg_count = 1.
    clear actual_details-time_stmp.

    cl_aunit_assert=>assert_equals(
      exp = expected_details
      act = actual_details
      msg = 'Did not log system message properly' ).

    cl_aunit_assert=>assert_equals(
      exp = 'This is a test'
      act = condense( actual_text )
      msg = 'Did not log system message properly' ).

    cl_aunit_assert=>assert_equals(
      exp = abap_true
      act = anon_log->has_errors( )
      msg = 'Did not log or fetch system message properly'
    ).
  endmethod.

  method can_log_rcomp.
    data:
          msg_handle       type balmsghndl,
          expected_details type bal_s_msg,
          actual_details   type bal_s_msg,
          actual_text      type char200.

"Solution manager doens't have PROTT. Therefore avoid using the concrete type
    DATA rcomp_data_ref TYPE REF TO DATA.
    FIELD-SYMBOLS <rcomp_structure> TYPE ANY.
    TRY.
        CREATE DATA rcomp_data_ref TYPE ('RCOMP').
    CATCH CX_SY_CREATE_DATA_ERROR.
      RETURN."Non ECC System such as SolutionManager
    ENDTRY.
    ASSIGN rcomp_data_ref->* TO <rcomp_structure>.

    expected_details-msgty = 'E'.
    expected_details-msgid = 'BL'.
    expected_details-msgno = '001'.
    expected_details-msgv1 = 'This'.
    expected_details-msgv2 = 'is'.
    expected_details-msgv3 = 'a'.
    expected_details-msgv4 = 'test'.

    MOVE-CORRESPONDING expected_details TO <rcomp_structure>.

    anon_log->add( <rcomp_structure> ).

    msg_handle-log_handle = anon_log->handle.
    msg_handle-msgnumber  = '000001'.

    call function 'BAL_LOG_MSG_READ'
      exporting
        i_s_msg_handle = msg_handle
      importing
        e_s_msg        = actual_details
        e_txt_msg      = actual_text.

    cl_aunit_assert=>assert_not_initial(
      act = actual_details-time_stmp
      msg = 'Did not log system message properly' ).

    expected_details-msg_count = 1.
    clear actual_details-time_stmp.

    cl_aunit_assert=>assert_equals(
      exp = expected_details
      act = actual_details
      msg = 'Did not log system message properly' ).

    cl_aunit_assert=>assert_equals(
      exp = 'This is a test'
      act = condense( actual_text )
      msg = 'Did not log system message properly' ).

    cl_aunit_assert=>assert_equals(
      exp = abap_true
      act = anon_log->has_errors( )
      msg = 'Did not log or fetch system message properly'
    ).
  endmethod.

 METHOD can_log_prott.

    data: msg_handle       type balmsghndl,
          expected_details type bal_s_msg,
          actual_details   type bal_s_msg,
          actual_text      type char200.

"Solution manager doens't have PROTT. Therefore avoid using the concrete type
    DATA prott_data_ref TYPE REF TO DATA.
    FIELD-SYMBOLS <prott_structure> TYPE ANY.
    TRY.
        CREATE DATA prott_data_ref TYPE ('PROTT').
    CATCH CX_SY_CREATE_DATA_ERROR.
      RETURN."Non ECC System such as SolutionManager
    ENDTRY.
    ASSIGN prott_data_ref->* TO <prott_structure>.

    expected_details-msgty = 'W'.
    expected_details-msgid = 'BL'.
    expected_details-msgno = '001'.
    expected_details-msgv1 = 'This'.
    expected_details-msgv2 = 'is'.
    expected_details-msgv3 = 'a'.
    expected_details-msgv4 = 'test'.

    MOVE-CORRESPONDING expected_details TO <prott_structure>.

    anon_log->add( <prott_structure> ).

    msg_handle-log_handle = anon_log->handle.
    msg_handle-msgnumber  = '000001'.

    call function 'BAL_LOG_MSG_READ'
      exporting
        i_s_msg_handle = msg_handle
      importing
        e_s_msg        = actual_details
        e_txt_msg      = actual_text.

    cl_aunit_assert=>assert_not_initial(
      act = actual_details-time_stmp
      msg = 'Did not log system message properly' ).

    expected_details-msg_count = 1.
    clear actual_details-time_stmp.

    cl_aunit_assert=>assert_equals(
      exp = expected_details
      act = actual_details
      msg = 'Did not log system message properly' ).

    cl_aunit_assert=>assert_equals(
      exp = 'This is a test'
      act = condense( actual_text )
      msg = 'Did not log system message properly' ).

    cl_aunit_assert=>assert_equals(
      exp = abap_true
      act = anon_log->has_warnings( )
      msg = 'Did not log or fetch system message properly'
    ).
  ENDMETHOD.

  method can_log_bapirettab.
    data: bapi_messages type bapirettab,
          bapi_msg      type bapiret2,
          exp_texts     type table_of_strings,
          exp_text      type string,
          exp_details   type bal_tt_msg,
          exp_detail    type bal_s_msg,
          act_texts     type table_of_strings,
          act_text      type string,
          act_details   type bal_tt_msg,
          act_detail    type bal_s_msg.

    define bapiret_messages_are.
      exp_detail-msgty = bapi_msg-type = &1.
      exp_detail-msgid = bapi_msg-id   = &2.
      exp_detail-msgno = bapi_msg-number = &3.
      exp_detail-msgv1 = bapi_msg-message_v1 = &4.
      exp_detail-msgv2 = bapi_msg-message_v2 = &5.
      exp_detail-msgv3 = bapi_msg-message_v3 = &6.
      exp_detail-msgv4 = bapi_msg-message_v4 = &7.
      exp_text = |{ exp_detail-msgv1 } { exp_detail-msgv2 } {
                    exp_detail-msgv3 } { exp_detail-msgv4 }|.
      APPEND bapi_msg TO bapi_messages.
      APPEND exp_detail TO exp_details.
      APPEND exp_text TO exp_texts.
    end-of-definition.

    bapiret_messages_are: 'S' 'BL' '001' 'This' 'is' 'happy' 'message',
                  'W' 'BL' '001' 'This' 'is' 'warning' 'message',
                  'E' 'BL' '001' 'This' 'is' 'angry' 'message'.

    anon_log->add( bapi_messages ).

    get_messages( exporting log_handle  = anon_log->handle
                  importing texts       = act_texts
                            msg_details = act_details ).

    do 3 times.
      read table act_details into act_detail index sy-index.
      read table exp_details into exp_detail index sy-index.

      cl_aunit_assert=>assert_not_initial(
        act = act_detail-time_stmp
        msg = 'Did not log system message properly' ).

      exp_detail-msg_count = 1.
      clear act_detail-time_stmp.

      cl_aunit_assert=>assert_equals(
        exp = exp_detail
        act = act_detail
        msg = 'Did not log bapirettab properly' ).

      read table act_texts into act_text index sy-index.
      read table exp_texts into exp_text index sy-index.
      cl_aunit_assert=>assert_equals(
        exp = exp_text
        act = condense( act_text )
        msg = 'Did not log bapirettab properly' ).
    enddo.

  endmethod.

  method can_log_err.
    data: impossible_int type i,
          err            type ref to cx_sy_zerodivide,
          act_txt        type char255,
          exp_txt        type char255,
          long_text      type string,
          msg_handle     type balmsghndl.

    try.
        impossible_int = 1 / 0.                            "Make an error!
      catch cx_sy_zerodivide into err.
        anon_log->add( err ).
        exp_txt         = err->if_message~get_text( ).
        long_text       = err->if_message~get_longtext( ).
    endtry.

    msg_handle-log_handle = anon_log->handle.
    msg_handle-msgnumber  = '000001'.
    call function 'BAL_LOG_EXCEPTION_READ'
      exporting
        i_s_msg_handle = msg_handle
      importing
        e_txt_msg      = act_txt.

    cl_aunit_assert=>assert_equals(
      exp = exp_txt "'Division by zero'
      act = act_txt
      msg = 'Did not log throwable correctly' ).

  endmethod.

  method can_log_chained_exceptions.
    data: main_exception     type ref to lcx_t100,
          previous_exception type ref to lcx_t100,
          catched_exception  type ref to lcx_t100,
          msg_handle         type balmsghndl,
          act_texts          type table_of_strings,
          act_text           type string.

    define exceptions_are.
      create object main_exception
        exporting
          previous = previous_exception
          id       = &1
          no       = &2
          msgv1    = &3
          msgv2    = &4
          msgv3    = &5
          msgv4    = &6.
      previous_exception = main_exception.
    end-of-definition.

    "Given
    exceptions_are:
      'SABP_UNIT' '010' ''     ''   ''     '',
      'SABP_UNIT' '030' ''     ''   ''     '',
      'SABP_UNIT' '000' 'This' 'is' 'test' 'message'.


    "When
    try.
        raise exception main_exception.
      catch lcx_t100 into catched_exception.
        anon_log->add( catched_exception ).
    endtry.

    "Then
    get_messages( exporting log_handle = anon_log->handle
                  importing texts      = act_texts ).

    read table act_texts index 1 into act_text.
    cl_aunit_assert=>assert_equals(
      exp = format_message( id = 'SABP_UNIT' no = 010 )    " 'Message 1'
      act = act_text
      msg = 'Did not log chained exception correctly' ).

    read table act_texts index 2 into act_text.
    cl_aunit_assert=>assert_equals(
      exp = format_message( id = 'SABP_UNIT' no = 030 )    " 'Message 2'
      act = act_text
      msg = 'Did not log chained exception correctly' ).

    read table act_texts index 3 into act_text.
    cl_aunit_assert=>assert_equals(
      exp = format_message( id = 'SABP_UNIT' no = 000 v1 = 'This' v2 = 'is' v3 = 'test' v4 = 'message' )       " 'Message: This is test message'
      act = act_text
      msg = 'Did not log chained exception correctly' ).
  endmethod.

  method can_log_batch_msgs.
    data: batch_msgs type table of bdcmsgcoll,
          batch_msg  type bdcmsgcoll,
          act_texts  type table_of_strings,
          act_text   type string.

    define batch_messages_are.
      batch_msg-msgtyp = &1.
      batch_msg-msgid = &2.
      batch_msg-msgnr = &3.
      batch_msg-msgv1 = &4.
      batch_msg-msgv2 = &5.
      batch_msg-msgv3 = &6.
      batch_msg-msgv4 = &7.
      APPEND batch_msg TO batch_msgs.
    end-of-definition.

    batch_messages_are:
      'S' 'SABP_UNIT' '010' ''     ''   ''     '',
      'S' 'SABP_UNIT' '030' ''     ''   ''     '',
      'S' 'SABP_UNIT' '000' 'This' 'is' 'test' 'message'.

    anon_log->add( batch_msgs ).

    get_messages( exporting log_handle = anon_log->handle
                  importing texts      = act_texts ).

    read table act_texts index 1 into act_text.
    cl_aunit_assert=>assert_equals(
      exp = format_message( id = 'SABP_UNIT' no = 010 )    " 'Message 1'
      act = act_text
      msg = 'Did not log BDC return messages correctly' ).

    read table act_texts index 2 into act_text.
    cl_aunit_assert=>assert_equals(
      exp = format_message( id = 'SABP_UNIT' no = 030 )    " 'Message 2'
      act = act_text
      msg = 'Did not log BDC return messages correctly' ).

    read table act_texts index 3 into act_text.
    cl_aunit_assert=>assert_equals(
      exp = format_message( id = 'SABP_UNIT' no = 000 v1 = 'This' v2 = 'is' v3 = 'test' v4 = 'message' )        " 'Message: This is test message'
      act = act_text
      msg = 'Did not log BDC return messages correctly' ).

  endmethod.

  method can_log_any_simple_structure.
    types: begin of ty_struct,
             comp1 type string,
             comp2 type i,
           end of ty_struct.
    data: struct      type ty_struct,
          act_table   type table_of_strings,
          exp_table   type table_of_strings,
          exp_line    like line of exp_table,
          msg_details type bal_tt_msg.

    struct-comp1 = 'Demo'.
    struct-comp2 = 5.
    anon_log->e( struct ).

    get_messages( exporting log_handle  = anon_log->handle
                  importing texts       = act_table
                            msg_details = msg_details ).

    exp_line = '--- Begin of structure ---'.
    append exp_line to exp_table.
    exp_line = 'comp1 = Demo'.
    append exp_line to exp_table.
    exp_line = 'comp2 = 5'.
    append exp_line to exp_table.
    exp_line = '--- End of structure ---'.
    append exp_line to exp_table.

    cl_aunit_assert=>assert_equals(
      exp = exp_table
      act = act_table
      msg = 'Simple structure was not logged correctly'
    ).
  endmethod.


  method can_log_any_deep_structure.
    types: begin of ty_struct,
             comp1 type string,
             comp2 type i,
           end of ty_struct,
           begin of ty_deep_struct,
             comp1 type string,
             deep  type ty_struct,
           end of ty_deep_struct.
    data: struct      type ty_deep_struct,
          act_table   type table_of_strings,
          exp_table   type table_of_strings,
          exp_line    like line of exp_table,
          msg_details type bal_tt_msg.

    struct-comp1 = 'Demo'.
    struct-deep-comp1 = 'Inner component'.
    struct-deep-comp2 = 10.
    anon_log->e( struct ).

    get_messages( exporting log_handle  = anon_log->handle
                  importing texts       = act_table
                            msg_details = msg_details ).

    exp_line = '--- Begin of structure ---'.
    append exp_line to exp_table.
    exp_line = 'comp1 = Demo'.
    append exp_line to exp_table.
    exp_line = '--- Begin of structure ---'.
    append exp_line to exp_table.
    exp_line = 'comp1 = Inner component'.
    append exp_line to exp_table.
    exp_line = 'comp2 = 10'.
    append exp_line to exp_table.
    exp_line = '--- End of structure ---'.
    append exp_line to exp_table.
    exp_line = '--- End of structure ---'.
    append exp_line to exp_table.

    cl_aunit_assert=>assert_equals(
      exp = exp_table
      act = act_table
      msg = 'Deep structure was not logged correctly'
    ).
  endmethod.


  method can_add_msg_context.
    data: addl_context type bezei20 value 'Berlin',        "data element from dictionary!
          msg_handle   type balmsghndl,
          act_details  type bal_s_msg.

    anon_log->add( obj_to_log = 'Here is some text'
                   context = addl_context ).

    msg_handle-log_handle = anon_log->handle.
    msg_handle-msgnumber  = '000001'.
    call function 'BAL_LOG_MSG_READ'
      exporting
        i_s_msg_handle = msg_handle
      importing
        e_s_msg        = act_details.

    cl_aunit_assert=>assert_equals(
      exp = addl_context
      act = act_details-context-value
      msg = 'Did not add context correctly' ).
    cl_aunit_assert=>assert_equals(
      exp = 'BEZEI20'
      act = act_details-context-tabname
      msg = 'Did not add context correctly' ).

  endmethod.


  method can_log_loggable_object.
    "given
    DATA loggable_message type zif_loggable_object=>ty_message.
    DATA loggable TYPE REF TO ltd_loggable_object.
    DATA dummy type string.
    CREATE OBJECT loggable TYPE ltd_loggable_object.

    message s001(00) with 'I' 'test' 'the' 'logger.' into dummy.
    MOVE-CORRESPONDING sy to loggable_message-symsg.
    APPEND loggable_message TO loggable->messages.

    "when
    named_log->add( obj_to_log = loggable ).

    "then
    cl_aunit_assert=>assert_equals(
      exp = 'Itestthelogger.'
      act = get_first_message( named_log->handle )
      msg = 'Did not add loggable message correctly' ).
  endmethod.


  method can_add_table_msg_context.
    data: addl_context type bezei20 value 'Berlin',        "data element from dictionary!
          msg_handle   type balmsghndl,
          act_details  type bal_s_msg.
    data msg_table type table_of_strings.

    append `Here is some text` to msg_table.
    append `Here is some other text` to msg_table.


    anon_log->add( obj_to_log = msg_table
                   context    = addl_context ).

    msg_handle-log_handle = anon_log->handle.
    msg_handle-msgnumber  = '000001'.
    call function 'BAL_LOG_MSG_READ'
      exporting
        i_s_msg_handle = msg_handle
      importing
        e_s_msg        = act_details.

    cl_aunit_assert=>assert_equals(
      exp = addl_context
      act = act_details-context-value
      msg = 'Did not add context correctly' ).
    cl_aunit_assert=>assert_equals(
      exp = 'BEZEI20'
      act = act_details-context-tabname
      msg = 'Did not add context correctly' ).


    msg_handle-msgnumber  = '000002'.
    call function 'BAL_LOG_MSG_READ'
      exporting
        i_s_msg_handle = msg_handle
      importing
        e_s_msg        = act_details.

    cl_aunit_assert=>assert_initial(
        act = act_details-context-value
        msg = 'Context should only be added to first line'  ).

    cl_aunit_assert=>assert_initial(
        act = act_details-context-tabname
        msg = 'Context should only be added to first line'  ).
  endmethod.

  method can_add_callback_sub.
    data: msg_handle   type balmsghndl,
          msg_detail   type bal_s_msg,
          exp_callback type bal_s_clbk.

    anon_log->add( obj_to_log = 'Message with Callback'
                   callback_form = 'FORM'
                   callback_prog = 'PROGRAM' ).

    msg_handle-log_handle = anon_log->handle.
    msg_handle-msgnumber  = '000001'.

    call function 'BAL_LOG_MSG_READ'
      exporting
        i_s_msg_handle = msg_handle
      importing
        e_s_msg        = msg_detail.

    exp_callback-userexitf = 'FORM'.
    exp_callback-userexitp = 'PROGRAM'.
    exp_callback-userexitt = ' '.

    cl_aunit_assert=>assert_equals(
      exp = exp_callback
      act = msg_detail-params-callback
      msg = 'Did not add callback correctly' ).

  endmethod.

  method can_add_callback_fm.
    data: msg_handle   type balmsghndl,
          msg_detail   type bal_s_msg,
          exp_callback type bal_s_clbk.

    anon_log->add( obj_to_log = 'Message with Callback'
                   callback_fm = 'FUNCTION' ).

    msg_handle-log_handle = anon_log->handle.
    msg_handle-msgnumber  = '000001'.

    call function 'BAL_LOG_MSG_READ'
      exporting
        i_s_msg_handle = msg_handle
      importing
        e_s_msg        = msg_detail.

    exp_callback-userexitf = 'FUNCTION'.
    exp_callback-userexitp = ' '.
    exp_callback-userexitt = 'F'.

    cl_aunit_assert=>assert_equals(
      exp = exp_callback
      act = msg_detail-params-callback
      msg = 'Did not add callback correctly' ).
  endmethod.

  method must_use_factory.
    data: log type ref to object.
    try.
        create object log type ('ZCL_LOGGER').
        cl_aunit_assert=>fail( 'Did not force creation via factory' ).
      catch cx_sy_create_object_error.
        "PASSED
    endtry.
  endmethod.

  method can_use_and_chain_aliases.
    data: texts       type table_of_strings,
          text        type string,
          msg_details type bal_tt_msg,
          msg_detail  type bal_s_msg.

    anon_log->a( 'Severe Abort Error!' )->e( |Here's an error!| ).
    anon_log->w( 'This is a warning' )->i( `Helpful Information` ).
    anon_log->s( 'Great' && 'Success' ).

    get_messages( exporting log_handle  = anon_log->handle
                  importing texts       = texts
                            msg_details = msg_details ).
    read table texts index 1 into text.
    read table msg_details index 1 into msg_detail.
    cl_aunit_assert=>assert_equals(
      exp = 'A'
      act = msg_detail-msgty
      msg = 'Didn''t log by alias' ).
    cl_aunit_assert=>assert_equals(
      exp = 'Severe Abort Error!'
      act = text
      msg = 'Didn''t log by alias' ).
    read table texts index 2 into text.
    read table msg_details index 2 into msg_detail.
    cl_aunit_assert=>assert_equals(
      exp = 'E'
      act = msg_detail-msgty
      msg = 'Didn''t log by alias' ).
    cl_aunit_assert=>assert_equals(
      exp = 'Here''s an error!'
      act = text
      msg = 'Didn''t log by alias' ).
    read table texts index 3 into text.
    read table msg_details index 3 into msg_detail.
    cl_aunit_assert=>assert_equals(
      exp = 'W'
      act = msg_detail-msgty
      msg = 'Didn''t log by alias' ).
    cl_aunit_assert=>assert_equals(
      exp = 'This is a warning'
      act = text
      msg = 'Didn''t log by alias' ).
    read table texts index 4 into text.
    read table msg_details index 4 into msg_detail.
    cl_aunit_assert=>assert_equals(
      exp = 'I'
      act = msg_detail-msgty
      msg = 'Didn''t log by alias' ).
    cl_aunit_assert=>assert_equals(
      exp = 'Helpful Information'
      act = text
      msg = 'Didn''t log by alias' ).
    read table texts index 5 into text.
    read table msg_details index 5 into msg_detail.
    cl_aunit_assert=>assert_equals(
      exp = 'S'
      act = msg_detail-msgty
      msg = 'Didn''t log by alias' ).
    cl_aunit_assert=>assert_equals(
      exp = 'GreatSuccess'
      act = text
      msg = 'Didn''t log by alias' ).

  endmethod.

  method get_first_message.
    data: msg_handle type balmsghndl.
    msg_handle-log_handle = log_handle.
    msg_handle-msgnumber  = '000001'.

    call function 'BAL_LOG_MSG_READ'
      exporting
        i_s_msg_handle = msg_handle
      importing
        e_txt_msg      = msg.
  endmethod.

  method get_messages.

    data: handle_as_table type bal_t_logh,
          message_handles type bal_t_msgh,
          msg_handle      type balmsghndl,
          msg_detail      type bal_s_msg,
          msg_text        type char255.

    append log_handle to handle_as_table.
    call function 'BAL_GLB_SEARCH_MSG'
      exporting
        i_t_log_handle = handle_as_table
      importing
        e_t_msg_handle = message_handles.

    loop at message_handles into msg_handle.
      call function 'BAL_LOG_MSG_READ'
        exporting
          i_s_msg_handle = msg_handle
        importing
          e_s_msg        = msg_detail
          e_txt_msg      = msg_text.
      append msg_detail to msg_details.
      append msg_text to texts.
    endloop.

  endmethod.

  method format_message.
    call function 'FORMAT_MESSAGE'
      exporting
        id        = id
        lang      = sy-langu
        no        = no
        v1        = v1
        v2        = v2
        v3        = v3
        v4        = v4
      importing
        msg       = msg
      exceptions
        not_found = 1
        others    = 2.
*      TODO: raise abap unit
  endmethod.

  method teardown.
    call function 'BAL_GLB_MEMORY_REFRESH'.
  endmethod.

  method return_proper_status.

    cl_aunit_assert=>assert_not_initial(
      act = anon_log->is_empty( )
      msg = 'Not empty at start' ).

    anon_log->s( 'success' ).
    anon_log->i( 'info' ).

    cl_aunit_assert=>assert_initial(
      act = anon_log->is_empty( )
      msg = 'Empty after add' ).

    cl_aunit_assert=>assert_initial(
      act = anon_log->has_errors( )
      msg = 'Has errors when there were no errors' ).

    cl_aunit_assert=>assert_initial(
      act = anon_log->has_warnings( )
      msg = 'Has warnings when there were no warnings' ).

    anon_log->e( 'error' ).
    anon_log->w( 'warning' ).

    cl_aunit_assert=>assert_not_initial(
      act = anon_log->has_errors( )
      msg = 'Has no errors when there were errors' ).

    cl_aunit_assert=>assert_not_initial(
      act = anon_log->has_warnings( )
      msg = 'Has no warnings when there were warnings' ).

  endmethod.

  method return_proper_length.

    cl_aunit_assert=>assert_equals(
      exp = 0
      act = anon_log->length( )
      msg = 'Did not return 0 length at start' ).

    anon_log->s( 'success' ).
    anon_log->i( 'info' ).
    anon_log->w( 'warning' ).

    cl_aunit_assert=>assert_equals(
      exp = 3
      act = anon_log->length( )
      msg = 'Did not return right length after add' ).

  endmethod.

endclass.       "lcl_Test
