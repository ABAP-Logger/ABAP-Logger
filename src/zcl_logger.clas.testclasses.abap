CLASS ltd_loggable_object DEFINITION CREATE PUBLIC FOR TESTING.

  PUBLIC SECTION.
    DATA messages TYPE zif_loggable_object=>tty_messages .
    INTERFACES zif_loggable_object.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS ltd_loggable_object IMPLEMENTATION.

  METHOD zif_loggable_object~get_message_table.
    r_result = messages.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    DATA:
          anon_log     TYPE REF TO zif_logger,
          named_log    TYPE REF TO zif_logger,
          reopened_log TYPE REF TO zif_logger.

    CLASS-METHODS:
      class_setup.

    METHODS:
      setup,
      teardown,
      get_first_message
        IMPORTING log_handle TYPE balloghndl
        RETURNING VALUE(msg) TYPE char255,
      get_messages
        IMPORTING
          log_handle  TYPE balloghndl
        EXPORTING
          texts       TYPE table_of_strings
          msg_details TYPE bal_tt_msg,

      format_message
        IMPORTING id         LIKE sy-msgid DEFAULT sy-msgid
                  no         LIKE sy-msgno DEFAULT sy-msgno
                  v1         LIKE sy-msgv1 DEFAULT sy-msgv1
                  v2         LIKE sy-msgv2 DEFAULT sy-msgv2
                  v3         LIKE sy-msgv3 DEFAULT sy-msgv3
                  v4         LIKE sy-msgv4 DEFAULT sy-msgv4
        RETURNING VALUE(msg) TYPE string,

      can_create_anon_log FOR TESTING,
      can_create_named_log FOR TESTING,
      can_reopen_log FOR TESTING,
      can_create_expiring_log_days FOR TESTING,
      can_create_expiring_log_date FOR TESTING,
      can_open_or_create FOR TESTING,

      can_add_log_context FOR TESTING,

      can_add_to_log FOR TESTING,
      can_add_to_named_log FOR TESTING,

      auto_saves_named_log FOR TESTING,
      auto_saves_reopened_log FOR TESTING,

      can_log_string FOR TESTING,
      can_log_char   FOR TESTING,
      can_log_symsg FOR TESTING,
      can_log_bapiret1  FOR TESTING,
      can_log_bapiret2  FOR TESTING,
      can_log_bapi_coru_return FOR TESTING,
      can_log_bapi_order_return FOR TESTING,
      can_log_rcomp     FOR TESTING,
      can_log_prott FOR TESTING,
      can_log_sprot FOR TESTING,
      can_log_bapirettab FOR TESTING,
      can_log_err FOR TESTING,
      can_log_chained_exceptions FOR TESTING,
      can_log_batch_msgs FOR TESTING,
      can_log_any_simple_structure FOR TESTING,
      can_log_any_deep_structure FOR TESTING,
      can_log_loggable_object FOR TESTING,

      can_add_msg_context FOR TESTING,
      can_add_callback_sub FOR TESTING,
      can_add_callback_fm  FOR TESTING,

      must_use_factory FOR TESTING,

      can_use_and_chain_aliases FOR TESTING,

      return_proper_status FOR TESTING,
      return_proper_length FOR TESTING,
      can_add_table_msg_context FOR TESTING RAISING cx_static_check.

ENDCLASS.       "lcl_Test

CLASS lcl_test IMPLEMENTATION.

  METHOD class_setup.
    zcl_logger=>new(
      object = 'ABAPUNIT'
      subobject = 'LOGGER'
      desc = 'Log saved in database' )->add( 'This message is in the database' ).
  ENDMETHOD.

  METHOD setup.
    anon_log  = zcl_logger=>new( ).
    named_log = zcl_logger=>new( object = 'ABAPUNIT'
                                 subobject = 'LOGGER'
                                 desc = `Hey it's a log` ).
    reopened_log = zcl_logger=>open( object = 'ABAPUNIT'
                                     subobject = 'LOGGER'
                                     desc = 'Log saved in database' ).
  ENDMETHOD.

  METHOD can_create_anon_log.
    cl_aunit_assert=>assert_bound(
      act = anon_log
      msg = 'Cannot Instantiate Anonymous Log' ).
  ENDMETHOD.

  METHOD can_create_named_log.
    cl_aunit_assert=>assert_bound(
      act = named_log
      msg = 'Cannot Instantiate Named Log' ).
  ENDMETHOD.

  METHOD can_create_expiring_log_days.
    DATA      expiring_log                  TYPE REF TO zif_logger.
    DATA      act_header                    TYPE bal_s_log.
    CONSTANTS days_until_log_can_be_deleted TYPE i VALUE 365.

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

    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING
        i_log_handle = expiring_log->handle
      IMPORTING
        e_s_log      = act_header.

    DATA lv_exp TYPE d.
    lv_exp = sy-datum + days_until_log_can_be_deleted.

    cl_aunit_assert=>assert_equals(
      EXPORTING
        exp     = lv_exp
        act     = act_header-aldate_del
        msg     = 'Log is not expiring in correct amount of days'
    ).

    cl_aunit_assert=>assert_equals(
      EXPORTING
        exp     = abap_true
        act     = act_header-del_before
        msg     = 'Log should not be deletable before expiry date'
    ).
  ENDMETHOD.

  METHOD can_create_expiring_log_date.
    DATA      expiring_log                  TYPE REF TO zif_logger.
    DATA      act_header                    TYPE bal_s_log.
    CONSTANTS days_until_log_can_be_deleted TYPE i VALUE 365.

    DATA lv_expire TYPE d.
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

    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING
        i_log_handle = expiring_log->handle
      IMPORTING
        e_s_log      = act_header.

    cl_aunit_assert=>assert_equals(
      EXPORTING
        exp     = lv_expire
        act     = act_header-aldate_del
        msg     = 'Log is not expiring on correct date'
    ).

    cl_aunit_assert=>assert_equals(
      EXPORTING
        exp     = abap_true
        act     = act_header-del_before
        msg     = 'Log should not be deletable before expiry date'
    ).
  ENDMETHOD.

  METHOD can_reopen_log.
    cl_aunit_assert=>assert_bound(
      act = reopened_log
      msg = 'Cannot Reopen Log from DB' ).
  ENDMETHOD.

  METHOD can_open_or_create.
    DATA: created_log TYPE REF TO zif_logger,
          handles     TYPE bal_t_logh.
    CALL FUNCTION 'BAL_GLB_MEMORY_REFRESH'.                "Close Logs
    reopened_log = zcl_logger=>open( object = 'ABAPUNIT'
                                     subobject = 'LOGGER'
                                     desc = 'Log saved in database'
                                     create_if_does_not_exist = abap_true ).
    created_log = zcl_logger=>open( object = 'ABAPUNIT'
                                    subobject = 'LOGGER'
                                    desc = 'Log not in database'
                                    create_if_does_not_exist = abap_true ).
    CALL FUNCTION 'BAL_GLB_SEARCH_LOG'
      IMPORTING
        e_t_log_handle = handles.

    cl_aunit_assert=>assert_equals(
      exp = 2
      act = lines( handles )
      msg = 'Did not create nonexistent log from OPEN' ).

  ENDMETHOD.

  METHOD can_add_log_context.

    DATA: log                 TYPE REF TO zif_logger,
          random_country_data TYPE t005t,
          act_header          TYPE bal_s_log.

    random_country_data-mandt = sy-mandt.
    random_country_data-spras = 'D'.
    random_country_data-land1 = 'DE'.

    log = zcl_logger=>new( context = random_country_data ).

    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING
        i_log_handle = log->handle
      IMPORTING
        e_s_log      = act_header.

    cl_aunit_assert=>assert_equals(
      exp = 'T005T'
      act = act_header-context-tabname
      msg = 'Did not add context to log' ).

    cl_aunit_assert=>assert_equals(
      exp = random_country_data
      act = act_header-context-value
      msg = 'Did not add context to log' ).

  ENDMETHOD.

  METHOD can_add_to_log.
    DATA: dummy TYPE c.

    MESSAGE s001(00) WITH 'I' 'test' 'the' 'logger.' INTO dummy.
    anon_log->add( ).

    cl_aunit_assert=>assert_equals(
      exp = 'Itestthelogger.'
      act = get_first_message( anon_log->handle )
      msg = 'Did not log system message properly' ).

  ENDMETHOD.

  METHOD can_add_to_named_log.
    DATA: dummy TYPE c.

    MESSAGE s001(00) WITH 'Testing' 'a' 'named' 'logger.' INTO dummy.
    named_log->add( ).

    cl_aunit_assert=>assert_equals(
      exp = 'Testinganamedlogger.'
      act = get_first_message( named_log->handle )
      msg = 'Did not write to named log' ).
  ENDMETHOD.


  METHOD auto_saves_named_log.
    DATA: dummy       TYPE c,
          log_numbers TYPE bal_t_logn,
          msg         TYPE string.

    MESSAGE s000(sabp_unit) WITH 'Testing' 'logger' 'that' 'saves.' INTO dummy.
    named_log->add( ).
    msg = format_message( ).

    CALL FUNCTION 'BAL_GLB_MEMORY_REFRESH'.

    APPEND named_log->db_number TO log_numbers.
    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_lognumber = log_numbers.

    cl_aunit_assert=>assert_equals(
      exp = msg
      act = get_first_message( named_log->handle )
      msg = 'Did not write to named log' ).

  ENDMETHOD.

  METHOD auto_saves_reopened_log.
    DATA: log_numbers TYPE bal_t_logn,
          act_texts   TYPE table_of_strings,
          act_text    TYPE string.
    reopened_log->add( 'This is another message in the database' ).
    CALL FUNCTION 'BAL_GLB_MEMORY_REFRESH'.

    APPEND reopened_log->db_number TO log_numbers.
    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_lognumber = log_numbers.

    get_messages( EXPORTING log_handle  = reopened_log->handle
                  IMPORTING texts       = act_texts ).

    READ TABLE act_texts INDEX 1 INTO act_text.
    cl_aunit_assert=>assert_equals(
      exp = 'This message is in the database'
      act = act_text
      msg = 'Did not autosave to reopened log' ).

    READ TABLE act_texts INDEX 2 INTO act_text.
    cl_aunit_assert=>assert_equals(
      exp = 'This is another message in the database'
      act = act_text
      msg = 'Did not autosave to reopened log' ).

  ENDMETHOD.

  METHOD can_log_string.
    DATA: stringmessage TYPE string VALUE `Logging a string, guys!`.
    anon_log->add( stringmessage ).

    cl_aunit_assert=>assert_equals(
      exp = stringmessage
      act = get_first_message( anon_log->handle )
      msg = 'Did not log system message properly' ).

  ENDMETHOD.

  METHOD can_log_char.
    DATA: charmessage TYPE char70 VALUE 'Logging a char sequence!'.
    anon_log->add( charmessage ).

    cl_aunit_assert=>assert_equals(
      exp = charmessage
      act = get_first_message( anon_log->handle )
      msg = 'Did not log system message properly' ).
  ENDMETHOD.

  METHOD can_log_symsg.
    DATA: symsg            TYPE symsg,
          msg_handle       TYPE balmsghndl,
          expected_details TYPE bal_s_msg,
          actual_details   TYPE bal_s_msg,
          actual_text      TYPE char200.

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

    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = msg_handle
      IMPORTING
        e_s_msg        = actual_details
        e_txt_msg      = actual_text.

    cl_aunit_assert=>assert_not_initial(
      act = actual_details-time_stmp
      msg = 'Did not log system message properly' ).

    expected_details-msg_count = 1.
    CLEAR actual_details-time_stmp.

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

  METHOD can_log_bapiret1.
    DATA: bapi_msg         TYPE bapiret1,
          msg_handle       TYPE balmsghndl,
          expected_details TYPE bal_s_msg,
          actual_details   TYPE bal_s_msg,
          actual_text      TYPE char200.

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

    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = msg_handle
      IMPORTING
        e_s_msg        = actual_details
        e_txt_msg      = actual_text.

    cl_aunit_assert=>assert_not_initial(
      act = actual_details-time_stmp
      msg = 'Did not log system message properly' ).

    expected_details-msg_count = 1.
    CLEAR actual_details-time_stmp.

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

  METHOD can_log_bapiret2.
    DATA: bapi_msg         TYPE bapiret2,
          msg_handle       TYPE balmsghndl,
          expected_details TYPE bal_s_msg,
          actual_details   TYPE bal_s_msg,
          actual_text      TYPE char200.

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

    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = msg_handle
      IMPORTING
        e_s_msg        = actual_details
        e_txt_msg      = actual_text.

    cl_aunit_assert=>assert_not_initial(
      act = actual_details-time_stmp
      msg = 'Did not log system message properly' ).

    expected_details-msg_count = 1.
    CLEAR actual_details-time_stmp.

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

  METHOD can_log_bapi_coru_return.
    DATA: bapi_msg         TYPE bapi_coru_return,
          msg_handle       TYPE balmsghndl,
          expected_details TYPE bal_s_msg,
          actual_details   TYPE bal_s_msg,
          actual_text      TYPE char200.

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

    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = msg_handle
      IMPORTING
        e_s_msg        = actual_details
        e_txt_msg      = actual_text.

    cl_aunit_assert=>assert_not_initial(
      act = actual_details-time_stmp
      msg = 'Did not log system message properly' ).

    expected_details-msg_count = 1.
    CLEAR actual_details-time_stmp.

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

  METHOD can_log_bapi_order_return.
    DATA: msg_handle       TYPE balmsghndl,
          expected_details TYPE bal_s_msg,
          actual_details   TYPE bal_s_msg,
          actual_text      TYPE char200.

    "Solution manager doens't have BAPI_ORDER_RETURN. Therefore avoid using the concrete type
    DATA bapi_order_return_data_ref TYPE REF TO data.
    DATA bapi_return_temp           TYPE bapiret2.         "these fields have the same name as BAPI_ORDER_RETURN
    FIELD-SYMBOLS <bapi_order_return_structure> TYPE any.
    TRY.
        CREATE DATA bapi_order_return_data_ref TYPE ('BAPI_ORDER_RETURN').
      CATCH cx_sy_create_data_error.
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

    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = msg_handle
      IMPORTING
        e_s_msg        = actual_details
        e_txt_msg      = actual_text.

    cl_aunit_assert=>assert_not_initial(
      act = actual_details-time_stmp
      msg = 'Did not log system message properly' ).

    expected_details-msg_count = 1.
    CLEAR actual_details-time_stmp.

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
  ENDMETHOD.

  METHOD can_log_rcomp.
    DATA:
          msg_handle       TYPE balmsghndl,
          expected_details TYPE bal_s_msg,
          actual_details   TYPE bal_s_msg,
          actual_text      TYPE char200.

    "Solution manager doens't have PROTT. Therefore avoid using the concrete type
    DATA rcomp_data_ref TYPE REF TO data.
    FIELD-SYMBOLS <rcomp_structure> TYPE any.
    TRY.
        CREATE DATA rcomp_data_ref TYPE ('RCOMP').
      CATCH cx_sy_create_data_error.
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

    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = msg_handle
      IMPORTING
        e_s_msg        = actual_details
        e_txt_msg      = actual_text.

    cl_aunit_assert=>assert_not_initial(
      act = actual_details-time_stmp
      msg = 'Did not log system message properly' ).

    expected_details-msg_count = 1.
    CLEAR actual_details-time_stmp.

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
  ENDMETHOD.

  METHOD can_log_prott.

    DATA: msg_handle       TYPE balmsghndl,
          expected_details TYPE bal_s_msg,
          actual_details   TYPE bal_s_msg,
          actual_text      TYPE char200.

    "Solution manager doens't have PROTT. Therefore avoid using the concrete type
    DATA prott_data_ref TYPE REF TO data.
    FIELD-SYMBOLS <prott_structure> TYPE any.
    TRY.
        CREATE DATA prott_data_ref TYPE ('PROTT').
      CATCH cx_sy_create_data_error.
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

    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = msg_handle
      IMPORTING
        e_s_msg        = actual_details
        e_txt_msg      = actual_text.

    cl_aunit_assert=>assert_not_initial(
      act = actual_details-time_stmp
      msg = 'Did not log system message properly' ).

    expected_details-msg_count = 1.
    CLEAR actual_details-time_stmp.

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

  METHOD can_log_sprot.

    DATA: sprot_msg        TYPE pat_sprot,
          msg_handle       TYPE balmsghndl,
          expected_details TYPE bal_s_msg,
          actual_details   TYPE bal_s_msg,
          actual_text      TYPE char200.

    expected_details-msgty = sprot_msg-severity = 'W'.
    expected_details-msgid = sprot_msg-ag = 'BL'.
    expected_details-msgno = sprot_msg-msgnr = '001'.
    expected_details-msgv1 = sprot_msg-var1 = 'This'.
    expected_details-msgv2 = sprot_msg-var2 = 'is'.
    expected_details-msgv3 = sprot_msg-var3 = 'a'.
    expected_details-msgv4 = sprot_msg-var4 = 'test'.

    anon_log->add( sprot_msg ).

    msg_handle-log_handle = anon_log->handle.
    msg_handle-msgnumber  = '000001'.

    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = msg_handle
      IMPORTING
        e_s_msg        = actual_details
        e_txt_msg      = actual_text.

    cl_aunit_assert=>assert_not_initial(
      act = actual_details-time_stmp
      msg = 'Did not log system message properly' ).

    expected_details-msg_count = 1.
    CLEAR actual_details-time_stmp.

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

  METHOD can_log_bapirettab.
    DATA: bapi_messages TYPE bapirettab,
          bapi_msg      TYPE bapiret2,
          exp_texts     TYPE table_of_strings,
          exp_text      TYPE string,
          exp_details   TYPE bal_tt_msg,
          exp_detail    TYPE bal_s_msg,
          act_texts     TYPE table_of_strings,
          act_text      TYPE string,
          act_details   TYPE bal_tt_msg,
          act_detail    TYPE bal_s_msg.

    DEFINE bapiret_messages_are.
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
    END-OF-DEFINITION.

    bapiret_messages_are: 'S' 'BL' '001' 'This' 'is' 'happy' 'message',
                  'W' 'BL' '001' 'This' 'is' 'warning' 'message',
                  'E' 'BL' '001' 'This' 'is' 'angry' 'message'.

    anon_log->add( bapi_messages ).

    get_messages( EXPORTING log_handle  = anon_log->handle
                  IMPORTING texts       = act_texts
                            msg_details = act_details ).

    DO 3 TIMES.
      READ TABLE act_details INTO act_detail INDEX sy-index.
      READ TABLE exp_details INTO exp_detail INDEX sy-index.

      cl_aunit_assert=>assert_not_initial(
        act = act_detail-time_stmp
        msg = 'Did not log system message properly' ).

      exp_detail-msg_count = 1.
      CLEAR act_detail-time_stmp.

      cl_aunit_assert=>assert_equals(
        exp = exp_detail
        act = act_detail
        msg = 'Did not log bapirettab properly' ).

      READ TABLE act_texts INTO act_text INDEX sy-index.
      READ TABLE exp_texts INTO exp_text INDEX sy-index.
      cl_aunit_assert=>assert_equals(
        exp = exp_text
        act = condense( act_text )
        msg = 'Did not log bapirettab properly' ).
    ENDDO.

  ENDMETHOD.

  METHOD can_log_err.
    DATA: impossible_int TYPE i,
          err            TYPE REF TO cx_sy_zerodivide,
          act_txt        TYPE char255,
          exp_txt        TYPE char255,
          long_text      TYPE string,
          msg_handle     TYPE balmsghndl.

    TRY.
        impossible_int = 1 / 0.                            "Make an error!
      CATCH cx_sy_zerodivide INTO err.
        anon_log->add( err ).
        exp_txt   = err->if_message~get_text( ).
        long_text = err->if_message~get_longtext( ).
    ENDTRY.

    msg_handle-log_handle = anon_log->handle.
    msg_handle-msgnumber  = '000001'.
    CALL FUNCTION 'BAL_LOG_EXCEPTION_READ'
      EXPORTING
        i_s_msg_handle = msg_handle
      IMPORTING
        e_txt_msg      = act_txt.

    cl_aunit_assert=>assert_equals(
      exp = exp_txt "'Division by zero'
      act = act_txt
      msg = 'Did not log throwable correctly' ).

  ENDMETHOD.

  METHOD can_log_chained_exceptions.

    CONSTANTS c_chained_exception TYPE string VALUE 'Did not log chained exception correctly' ##NO_TEXT.

    DATA: main_exception     TYPE REF TO lcx_t100,
          previous_exception TYPE REF TO lcx_t100,
          caught_exception   TYPE REF TO lcx_t100,
          msg_handle         TYPE balmsghndl,
          msg_count          TYPE i,
          bal_msgs           TYPE bal_tt_msg,
          bal_msg            TYPE bal_s_msg.

    DEFINE exceptions_are.
      CREATE OBJECT main_exception
        EXPORTING
          previous = previous_exception
          id       = &1
          no       = &2
          msgv1    = &3
          msgv2    = &4
          msgv3    = &5
          msgv4    = &6.
      previous_exception = main_exception.
    END-OF-DEFINITION.

    "Given
    exceptions_are:
      'SABP_UNIT' '010' ''     ''   ''     '',
      'SABP_UNIT' '030' ''     ''   ''     '',
      'SABP_UNIT' '000' 'This' 'is' 'test' 'message'.


    "When
    TRY.
        RAISE EXCEPTION main_exception.
      CATCH lcx_t100 INTO caught_exception.
        anon_log->add( caught_exception ).
    ENDTRY.

    "Then
    get_messages( EXPORTING log_handle = anon_log->handle
                  IMPORTING msg_details = bal_msgs ).

    DESCRIBE TABLE bal_msgs LINES msg_count.
    cl_abap_unit_assert=>assert_equals(
      exp = 3
      act = msg_count
      msg = c_chained_exception ).

    READ TABLE bal_msgs INDEX 1 INTO bal_msg.
    cl_abap_unit_assert=>assert_equals(
      exp = 'SABP_UNIT010'                                 " 'Message 1'
      act = bal_msg-msgid && bal_msg-msgno
      msg = c_chained_exception ).

    READ TABLE bal_msgs INDEX 2 INTO bal_msg.
    cl_abap_unit_assert=>assert_equals(
      exp = 'SABP_UNIT030'                                 " 'Message 2'
      act = bal_msg-msgid && bal_msg-msgno
      msg = c_chained_exception ).

    READ TABLE bal_msgs INDEX 3 INTO bal_msg.
    cl_abap_unit_assert=>assert_equals(
      exp = 'SABP_UNIT000Thisistestmessage'                " 'Message: This is test message'
      act = bal_msg-msgid && bal_msg-msgno && bal_msg-msgv1 && bal_msg-msgv2 && bal_msg-msgv3 && bal_msg-msgv4
      msg = c_chained_exception ).

  ENDMETHOD.


  METHOD can_log_batch_msgs.

    CONSTANTS c_bdc_message TYPE string VALUE 'Did not log BDC return messages correctly' ##NO_TEXT.

    DATA: batch_msgs TYPE TABLE OF bdcmsgcoll,
          batch_msg  TYPE bdcmsgcoll,
          bal_msgs   TYPE bal_tt_msg,
          bal_msg    TYPE bal_s_msg,
          msg_count  TYPE i.

    DEFINE batch_messages_are.
      batch_msg-msgtyp = &1.
      batch_msg-msgid = &2.
      batch_msg-msgnr = &3.
      batch_msg-msgv1 = &4.
      batch_msg-msgv2 = &5.
      batch_msg-msgv3 = &6.
      batch_msg-msgv4 = &7.
      APPEND batch_msg TO batch_msgs.
    END-OF-DEFINITION.

    batch_messages_are:
      'S' 'SABP_UNIT' '010' ''     ''   ''     '',
      'S' 'SABP_UNIT' '030' ''     ''   ''     '',
      'S' 'SABP_UNIT' '000' 'This' 'is' 'test' 'message'.

    anon_log->add( batch_msgs ).

    get_messages( EXPORTING log_handle  = anon_log->handle
                  IMPORTING msg_details = bal_msgs ).

    DESCRIBE TABLE bal_msgs LINES msg_count.
    cl_aunit_assert=>assert_equals(
      exp = 3
      act = msg_count
      msg = c_bdc_message ).

    READ TABLE bal_msgs INDEX 1 INTO bal_msg.
    cl_abap_unit_assert=>assert_equals(
      exp = 'SABP_UNIT010'                                 " 'Message 1'
      act = bal_msg-msgid && bal_msg-msgno
      msg = c_bdc_message ).

    READ TABLE bal_msgs INDEX 2 INTO bal_msg.
    cl_abap_unit_assert=>assert_equals(
      exp = 'SABP_UNIT030'                                 " 'Message 2'
      act = bal_msg-msgid && bal_msg-msgno
      msg = c_bdc_message ).

    READ TABLE bal_msgs INDEX 3 INTO bal_msg.
    cl_abap_unit_assert=>assert_equals(
      exp = 'SABP_UNIT000Thisistestmessage'                " 'Message: This is test message'
      act = bal_msg-msgid && bal_msg-msgno && bal_msg-msgv1 && bal_msg-msgv2 && bal_msg-msgv3 && bal_msg-msgv4
      msg = c_bdc_message ).

  ENDMETHOD.


  METHOD can_log_any_simple_structure.
    TYPES: BEGIN OF ty_struct,
             comp1 TYPE string,
             comp2 TYPE i,
           END OF ty_struct.
    DATA: struct      TYPE ty_struct,
          act_table   TYPE table_of_strings,
          exp_table   TYPE table_of_strings,
          exp_line    LIKE LINE OF exp_table,
          msg_details TYPE bal_tt_msg.

    struct-comp1 = 'Demo'.
    struct-comp2 = 5.
    anon_log->e( struct ).

    get_messages( EXPORTING log_handle  = anon_log->handle
                  IMPORTING texts       = act_table
                            msg_details = msg_details ).

    exp_line = '--- Begin of structure ---'.
    APPEND exp_line TO exp_table.
    exp_line = 'comp1 = Demo'.
    APPEND exp_line TO exp_table.
    exp_line = 'comp2 = 5'.
    APPEND exp_line TO exp_table.
    exp_line = '--- End of structure ---'.
    APPEND exp_line TO exp_table.

    cl_aunit_assert=>assert_equals(
      exp = exp_table
      act = act_table
      msg = 'Simple structure was not logged correctly'
    ).
  ENDMETHOD.


  METHOD can_log_any_deep_structure.
    TYPES: BEGIN OF ty_struct,
             comp1 TYPE string,
             comp2 TYPE i,
           END   OF ty_struct,
             BEGIN OF ty_deep_struct,
             comp1 TYPE string,
             deep  TYPE ty_struct,
             END OF ty_deep_struct.
    DATA: struct      TYPE ty_deep_struct,
          act_table   TYPE table_of_strings,
          exp_table   TYPE table_of_strings,
          exp_line    LIKE LINE OF exp_table,
          msg_details TYPE bal_tt_msg.

    struct-comp1      = 'Demo'.
    struct-deep-comp1 = 'Inner component'.
    struct-deep-comp2 = 10.
    anon_log->e( struct ).

    get_messages( EXPORTING log_handle  = anon_log->handle
                  IMPORTING texts       = act_table
                            msg_details = msg_details ).

    exp_line = '--- Begin of structure ---'.
    APPEND exp_line TO exp_table.
    exp_line = 'comp1 = Demo'.
    APPEND exp_line TO exp_table.
    exp_line = '--- Begin of structure ---'.
    APPEND exp_line TO exp_table.
    exp_line = 'comp1 = Inner component'.
    APPEND exp_line TO exp_table.
    exp_line = 'comp2 = 10'.
    APPEND exp_line TO exp_table.
    exp_line = '--- End of structure ---'.
    APPEND exp_line TO exp_table.
    exp_line = '--- End of structure ---'.
    APPEND exp_line TO exp_table.

    cl_aunit_assert=>assert_equals(
      exp = exp_table
      act = act_table
      msg = 'Deep structure was not logged correctly'
    ).
  ENDMETHOD.


  METHOD can_add_msg_context.
    DATA: addl_context TYPE bezei20 VALUE 'Berlin',        "data element from dictionary!
          msg_handle   TYPE balmsghndl,
          act_details  TYPE bal_s_msg.

    anon_log->add( obj_to_log = 'Here is some text'
                   context = addl_context ).

    msg_handle-log_handle = anon_log->handle.
    msg_handle-msgnumber  = '000001'.
    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = msg_handle
      IMPORTING
        e_s_msg        = act_details.

    cl_aunit_assert=>assert_equals(
      exp = addl_context
      act = act_details-context-value
      msg = 'Did not add context correctly' ).
    cl_aunit_assert=>assert_equals(
      exp = 'BEZEI20'
      act = act_details-context-tabname
      msg = 'Did not add context correctly' ).

  ENDMETHOD.


  METHOD can_log_loggable_object.
    "given
    DATA loggable_message TYPE zif_loggable_object=>ty_message.
    DATA loggable         TYPE REF TO ltd_loggable_object.
    DATA dummy            TYPE string.
    CREATE OBJECT loggable TYPE ltd_loggable_object.

    MESSAGE s001(00) WITH 'I' 'test' 'the' 'logger.' INTO dummy.
    MOVE-CORRESPONDING sy TO loggable_message-symsg.
    loggable_message-type = sy-msgty.
    APPEND loggable_message TO loggable->messages.

    "when
    named_log->add( obj_to_log = loggable ).

    "then
    cl_aunit_assert=>assert_equals(
      exp = 'Itestthelogger.'
      act = get_first_message( named_log->handle )
      msg = 'Did not add loggable message correctly' ).
  ENDMETHOD.


  METHOD can_add_table_msg_context.
    DATA: addl_context TYPE bezei20 VALUE 'Berlin',        "data element from dictionary!
          msg_handle   TYPE balmsghndl,
          act_details  TYPE bal_s_msg.
    DATA  msg_table    TYPE table_of_strings.

    APPEND `Here is some text` TO msg_table.
    APPEND `Here is some other text` TO msg_table.


    anon_log->add( obj_to_log = msg_table
                   context    = addl_context ).

    msg_handle-log_handle = anon_log->handle.
    msg_handle-msgnumber  = '000001'.
    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = msg_handle
      IMPORTING
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
    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = msg_handle
      IMPORTING
        e_s_msg        = act_details.

    cl_aunit_assert=>assert_initial(
        act = act_details-context-value
        msg = 'Context should only be added to first line'  ).

    cl_aunit_assert=>assert_initial(
        act = act_details-context-tabname
        msg = 'Context should only be added to first line'  ).
  ENDMETHOD.

  METHOD can_add_callback_sub.
    DATA: msg_handle   TYPE balmsghndl,
          msg_detail   TYPE bal_s_msg,
          exp_callback TYPE bal_s_clbk.

    anon_log->add( obj_to_log = 'Message with Callback'
                   callback_form = 'FORM'
                   callback_prog = 'PROGRAM' ).

    msg_handle-log_handle = anon_log->handle.
    msg_handle-msgnumber  = '000001'.

    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = msg_handle
      IMPORTING
        e_s_msg        = msg_detail.

    exp_callback-userexitf = 'FORM'.
    exp_callback-userexitp = 'PROGRAM'.
    exp_callback-userexitt = ' '.

    cl_aunit_assert=>assert_equals(
      exp = exp_callback
      act = msg_detail-params-callback
      msg = 'Did not add callback correctly' ).

  ENDMETHOD.

  METHOD can_add_callback_fm.
    DATA: msg_handle   TYPE balmsghndl,
          msg_detail   TYPE bal_s_msg,
          exp_callback TYPE bal_s_clbk.

    anon_log->add( obj_to_log = 'Message with Callback'
                   callback_fm = 'FUNCTION' ).

    msg_handle-log_handle = anon_log->handle.
    msg_handle-msgnumber  = '000001'.

    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = msg_handle
      IMPORTING
        e_s_msg        = msg_detail.

    exp_callback-userexitf = 'FUNCTION'.
    exp_callback-userexitp = ' '.
    exp_callback-userexitt = 'F'.

    cl_aunit_assert=>assert_equals(
      exp = exp_callback
      act = msg_detail-params-callback
      msg = 'Did not add callback correctly' ).
  ENDMETHOD.

  METHOD must_use_factory.
    DATA: log TYPE REF TO object.
    TRY.
        CREATE OBJECT log TYPE ('ZCL_LOGGER').
        cl_aunit_assert=>fail( 'Did not force creation via factory' ).
      CATCH cx_sy_create_object_error.
        "PASSED
    ENDTRY.
  ENDMETHOD.

  METHOD can_use_and_chain_aliases.
    DATA: texts       TYPE table_of_strings,
          text        TYPE string,
          msg_details TYPE bal_tt_msg,
          msg_detail  TYPE bal_s_msg.

    anon_log->a( 'Severe Abort Error!' )->e( |Here's an error!| ).
    anon_log->w( 'This is a warning' )->i( `Helpful Information` ).
    anon_log->s( 'Great' && 'Success' ).

    get_messages( EXPORTING log_handle  = anon_log->handle
                  IMPORTING texts       = texts
                            msg_details = msg_details ).
    READ TABLE texts INDEX 1 INTO text.
    READ TABLE msg_details INDEX 1 INTO msg_detail.
    cl_aunit_assert=>assert_equals(
      exp = 'A'
      act = msg_detail-msgty
      msg = 'Didn''t log by alias' ).
    cl_aunit_assert=>assert_equals(
      exp = 'Severe Abort Error!'
      act = text
      msg = 'Didn''t log by alias' ).
    READ TABLE texts INDEX 2 INTO text.
    READ TABLE msg_details INDEX 2 INTO msg_detail.
    cl_aunit_assert=>assert_equals(
      exp = 'E'
      act = msg_detail-msgty
      msg = 'Didn''t log by alias' ).
    cl_aunit_assert=>assert_equals(
      exp = 'Here''s an error!'
      act = text
      msg = 'Didn''t log by alias' ).
    READ TABLE texts INDEX 3 INTO text.
    READ TABLE msg_details INDEX 3 INTO msg_detail.
    cl_aunit_assert=>assert_equals(
      exp = 'W'
      act = msg_detail-msgty
      msg = 'Didn''t log by alias' ).
    cl_aunit_assert=>assert_equals(
      exp = 'This is a warning'
      act = text
      msg = 'Didn''t log by alias' ).
    READ TABLE texts INDEX 4 INTO text.
    READ TABLE msg_details INDEX 4 INTO msg_detail.
    cl_aunit_assert=>assert_equals(
      exp = 'I'
      act = msg_detail-msgty
      msg = 'Didn''t log by alias' ).
    cl_aunit_assert=>assert_equals(
      exp = 'Helpful Information'
      act = text
      msg = 'Didn''t log by alias' ).
    READ TABLE texts INDEX 5 INTO text.
    READ TABLE msg_details INDEX 5 INTO msg_detail.
    cl_aunit_assert=>assert_equals(
      exp = 'S'
      act = msg_detail-msgty
      msg = 'Didn''t log by alias' ).
    cl_aunit_assert=>assert_equals(
      exp = 'GreatSuccess'
      act = text
      msg = 'Didn''t log by alias' ).

  ENDMETHOD.

  METHOD get_first_message.
    DATA: msg_handle TYPE balmsghndl.
    msg_handle-log_handle = log_handle.
    msg_handle-msgnumber  = '000001'.

    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = msg_handle
      IMPORTING
        e_txt_msg      = msg.
  ENDMETHOD.

  METHOD get_messages.

    DATA: handle_as_table TYPE bal_t_logh,
          message_handles TYPE bal_t_msgh,
          msg_handle      TYPE balmsghndl,
          msg_detail      TYPE bal_s_msg,
          msg_text        TYPE char255.

    APPEND log_handle TO handle_as_table.
    CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
      EXPORTING
        i_t_log_handle = handle_as_table
      IMPORTING
        e_t_msg_handle = message_handles.

    LOOP AT message_handles INTO msg_handle.
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = msg_handle
        IMPORTING
          e_s_msg        = msg_detail
          e_txt_msg      = msg_text.
      APPEND msg_detail TO msg_details.
      APPEND msg_text TO texts.
    ENDLOOP.

  ENDMETHOD.

  METHOD format_message.
    CALL FUNCTION 'FORMAT_MESSAGE'
      EXPORTING
        id        = id
        lang      = sy-langu
        no        = no
        v1        = v1
        v2        = v2
        v3        = v3
        v4        = v4
      IMPORTING
        msg       = msg
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
*      TODO: raise abap unit
  ENDMETHOD.

  METHOD teardown.
    CALL FUNCTION 'BAL_GLB_MEMORY_REFRESH'.
  ENDMETHOD.

  METHOD return_proper_status.

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

  ENDMETHOD.

  METHOD return_proper_length.

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

  ENDMETHOD.

ENDCLASS.       "lcl_Test
