*"* use this source file for your ABAP unit test classes
class lcl_logger_settings_should definition deferred.
class zcl_logger_settings definition local friends lcl_logger_settings_should.

class lcl_logger_settings_should definition for testing
  risk level harmless
  duration short.

  private section.
    data cut type ref to zcl_logger_settings.
    methods setup.
    methods have_correct_defaults for testing.
    methods set_autosave for testing.
    methods set_expiry_date for testing.
    methods set_expiry_in_days for testing.
    methods set_flag_to_keep_until_expiry for testing.
    methods set_usage_of_2nd_db_connection for testing.
    methods set_max_drilldown_level for testing.
endclass.

class lcl_logger_settings_should implementation.

  method setup.
    cut = new #( ).
  endmethod.

  method have_correct_defaults.
    cl_aunit_assert=>assert_equals(
      exporting
        exp     = abap_true
        act     = cut->zif_logger_settings~get_autosave( )
        msg     = |Auto save should be on by default|
    ).
    cl_aunit_assert=>assert_equals(
      exporting
        exp     = abap_true
        act     = cut->zif_logger_settings~get_usage_of_secondary_db_conn( )
        msg     = |2nd database connection should be used by default|
    ).
    cl_aunit_assert=>assert_equals(
      exporting
        exp     = abap_false
        act     = cut->zif_logger_settings~get_must_be_kept_until_expiry( )
        msg     = |Log should be deletable before expiry date is reached by default|
    ).
    cl_aunit_assert=>assert_initial(
      exporting
        act     = cut->zif_logger_settings~get_expiry_date( )
        msg     = |No expiry date set by default|
    ).
    cl_aunit_assert=>assert_equals(
      exporting
        exp     = 10
        act     = cut->zif_logger_settings~get_max_exception_drill_down( )
        msg     = |Max exception drill down should be 10 by default|
    ).
  endmethod.

  method set_autosave.
    cut->zif_logger_settings~set_autosave( abap_false ).
    cl_aunit_assert=>assert_equals(
      exporting
        exp     = abap_false
        act     = cut->zif_logger_settings~get_autosave( )
        msg     = |Auto save was not deactivated correctly|
    ).
  endmethod.

  method set_expiry_date.
    cut->zif_logger_settings~set_expiry_date( '20161030' ).
    cl_aunit_assert=>assert_equals(
      exporting
        exp     = '20161030'
        act     = cut->zif_logger_settings~get_expiry_date( )
        msg     = |Expiry date was not set correctly|
    ).
  endmethod.

  method set_expiry_in_days.
    cut->zif_logger_settings~set_expiry_in_days( -1 ).
    cl_aunit_assert=>assert_initial(
      exporting
        act     = cut->zif_logger_settings~get_expiry_date( )
        msg     = |Expiry in days should remain default when setting incorrect values.|
    ).

    cut->zif_logger_settings~set_expiry_in_days( 10 ).
    cl_aunit_assert=>assert_equals(
      exporting
        exp     = conv d( sy-datum + 10 )
        act     = cut->zif_logger_settings~get_expiry_date( )
        msg     = |Expiry in days was not set correctly.|
    ).
  endmethod.

  method set_flag_to_keep_until_expiry.
    cut->zif_logger_settings~set_must_be_kept_until_expiry( abap_true ).
    cl_aunit_assert=>assert_equals(
      exporting
        exp     = abap_true
        act     = cut->zif_logger_settings~get_must_be_kept_until_expiry( )
        msg     = |Setter for keeping log until expiry is not working correctly.|
    ).
  endmethod.

  method set_usage_of_2nd_db_connection.
    cut->zif_logger_settings~set_usage_of_secondary_db_conn( abap_false ).
    cl_aunit_assert=>assert_equals(
      exporting
        exp     = abap_false
        act     = cut->zif_logger_settings~get_usage_of_secondary_db_conn( )
        msg     = |Setter for using 2nd db connection is not working correctly.|
    ).
  endmethod.

  method set_max_drilldown_level.
    cut->zif_logger_settings~set_max_exception_drill_down( 20 ).
    cl_aunit_assert=>assert_equals(
      exporting
        exp     = 20
        act     = cut->zif_logger_settings~get_max_exception_drill_down( )
        msg     = |Setter for max drilldown level is not working correctly.|
    ).
    cut->zif_logger_settings~set_max_exception_drill_down( -1 ).
    cl_aunit_assert=>assert_equals(
      exporting
        exp     = 20
        act     = cut->zif_logger_settings~get_max_exception_drill_down( )
        msg     = |Max exception drill down level should not change if value is incorrect.|
    ).
    cut->zif_logger_settings~set_max_exception_drill_down( 0 ).
    cl_aunit_assert=>assert_equals(
      exporting
        exp     = 0
        act     = cut->zif_logger_settings~get_max_exception_drill_down( )
        msg     = |Max exception drill down should be deactivatable.|
    ).
  endmethod.

endclass.
