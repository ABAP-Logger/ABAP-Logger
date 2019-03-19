class zcl_logger_settings definition
  public
  final
  create private
  global friends zcl_logger_factory.

  public section.
    interfaces zif_logger_settings.
    methods constructor.
  protected section.
  private section.
    data auto_save type abap_bool.
    data expiry_date type aldate_del .
    data can_be_deleted_before_expiry type del_before.
    data max_exception_drill_down type i.
    data use_2nd_db_connection type flag.
endclass.



class zcl_logger_settings implementation.

  method constructor.
    can_be_deleted_before_expiry = abap_true.
    max_exception_drill_down = 10.
    use_2nd_db_connection = abap_true.
    auto_save = abap_true.
  endmethod.

  method zif_logger_settings~get_autosave.
    r_auto_save = auto_save.
  endmethod.

  method zif_logger_settings~set_autosave.
    auto_save = i_auto_save.
    r_self = me.
  endmethod.

  method zif_logger_settings~get_expiry_date.
    r_expiry_date = expiry_date.
  endmethod.

  method zif_logger_settings~set_expiry_date.
    expiry_date = i_expiry_date.
    r_self = me.
  endmethod.

  method zif_logger_settings~set_expiry_in_days.
    if i_num_days > 0.
      expiry_date = sy-datum + i_num_days.
    endif.
    r_self = me.
  endmethod.

  method zif_logger_settings~get_deletable_before_expiry.
    r_can_be_deleted = can_be_deleted_before_expiry.
  endmethod.

  method zif_logger_settings~set_deletable_before_expiry.
    can_be_deleted_before_expiry = i_can_be_deleted.
    r_self = me.
  endmethod.

  method zif_logger_settings~get_max_exception_drill_down.
    r_levels = max_exception_drill_down.
  endmethod.

  method zif_logger_settings~set_max_exception_drill_down.
    if i_levels >= 0.
      max_exception_drill_down = i_levels.
    endif.
    r_self = me.
  endmethod.

  method zif_logger_settings~get_usage_of_secondary_db_conn.
    r_2nd_db_connection_enabled = use_2nd_db_connection.
  endmethod.

  method zif_logger_settings~set_usage_of_secondary_db_conn.
    use_2nd_db_connection = i_use_2nd_db_connection.
    r_self = me.
  endmethod.

endclass.
