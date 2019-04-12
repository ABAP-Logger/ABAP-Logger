interface zif_logger_settings public .

  "! Is the log automatically saved when adding messages?
  "! See setter for more details.
  methods get_autosave
    returning
      value(r_auto_save) type abap_bool.

  "! Set to true if the log is automatically saved when adding messages.
  "!
  "! If auto save is disabled, the save() method has to be called manually
  "! to write the data to the database (it commits the LUW).
  "! If auto save is enabled, the save() method has no effect.
  "! By default, auto save is enabled.
  "!
  "! Be careful with enabled auto save when processing mass data because it
  "! can decrease system performance significantly.
  methods set_autosave
    importing
      i_auto_save   type abap_bool
    returning
      value(r_self) type ref to zif_logger_settings.

  "! Get the earliest date on which the log can be deleted.
  "! See setter for more details.
  methods get_expiry_date
    returning
      value(r_expiry_date) type aldate_del.

  "! Set the earliest date on which the log can be deleted.
  "! By default the log does not expire.
  "!
  "! Further information: https://launchpad.support.sap.com/#/notes/195157
  methods set_expiry_date
    importing
      i_expiry_date type aldate_del
    returning
      value(r_self) type ref to zif_logger_settings.

  "! Set the number of days after which the log can be deleted.
  "! By default the log does not expire.
  "!
  "! Further information: https://launchpad.support.sap.com/#/notes/195157
  methods set_expiry_in_days
    importing
      i_num_days    type i
    returning
      value(r_self) type ref to zif_logger_settings.

  "! Does the log have to be kept until the expiry date is reached?
  "! See setter for more details.
  methods get_must_be_kept_until_expiry
    returning
      value(r_must_be_kept_until_expiry) type del_before.

  "! Set to true if log must be kept until the expiry date is reached. It
  "! cannot be deleted before (in transaction SLG2).
  "! The default is false.
  "!
  "! Further information: https://launchpad.support.sap.com/#/notes/195157
  methods set_must_be_kept_until_expiry
    importing
      i_must_be_kept_until_expiry type del_before
    returning
      value(r_self)               type ref to zif_logger_settings.

  methods get_max_exception_drill_down
    returning
      value(r_levels) type i.

  methods set_max_exception_drill_down
    importing
      i_levels      type i
    returning
      value(r_self) type ref to zif_logger_settings.

  "! Is a secondary database connection used to write the log entries to the database?
  "! See setter for more details.
  methods get_usage_of_secondary_db_conn
    returning
      value(r_2nd_db_connection_enabled) type flag.

  "! Set to true if secondary database connection should be used to write the log entries to the database.
  "! This is important if main program does a rollback (on purpose or after a dump).
  "! The default is true.
  methods set_usage_of_secondary_db_conn
    importing
      i_use_2nd_db_connection type flag
    returning
      value(r_self)           type ref to zif_logger_settings.

endinterface.
