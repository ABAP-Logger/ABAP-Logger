INTERFACE zif_logger_settings PUBLIC .

  TYPES ty_profile_name TYPE string.

  "! Is the log automatically saved when adding messages?
  "! See setter for more details.
  METHODS get_autosave
    RETURNING
      VALUE(r_auto_save) TYPE abap_bool.

  "! Set to true if the log is automatically saved when adding messages.
  "!
  "! If auto save is disabled, the save() method has to be called manually
  "! to write the data to the database (it commits the LUW).
  "! If auto save is enabled, the save() method has no effect.
  "! By default, auto save is enabled.
  "!
  "! Be careful with enabled auto save when processing mass data because it
  "! can decrease system performance significantly.
  METHODS set_autosave
    IMPORTING
      i_auto_save   TYPE abap_bool
    RETURNING
      VALUE(r_self) TYPE REF TO zif_logger_settings.

  "! Get the earliest date on which the log can be deleted.
  "! See setter for more details.
  METHODS get_expiry_date
    RETURNING
      VALUE(r_expiry_date) TYPE aldate_del.

  "! Set the earliest date on which the log can be deleted.
  "! By default the log does not expire.
  "!
  "! Further information: https://launchpad.support.sap.com/#/notes/195157
  METHODS set_expiry_date
    IMPORTING
      i_expiry_date TYPE aldate_del
    RETURNING
      VALUE(r_self) TYPE REF TO zif_logger_settings.

  "! Set the number of days after which the log can be deleted.
  "! By default the log does not expire.
  "!
  "! Further information: https://launchpad.support.sap.com/#/notes/195157
  METHODS set_expiry_in_days
    IMPORTING
      i_num_days    TYPE i
    RETURNING
      VALUE(r_self) TYPE REF TO zif_logger_settings.

  "! Does the log have to be kept until the expiry date is reached?
  "! See setter for more details.
  METHODS get_must_be_kept_until_expiry
    RETURNING
      VALUE(r_must_be_kept_until_expiry) TYPE del_before.

  "! Set to true if log must be kept until the expiry date is reached. It
  "! cannot be deleted before (in transaction SLG2).
  "! The default is false.
  "!
  "! Further information: https://launchpad.support.sap.com/#/notes/195157
  METHODS set_must_be_kept_until_expiry
    IMPORTING
      i_must_be_kept_until_expiry TYPE del_before
    RETURNING
      VALUE(r_self)               TYPE REF TO zif_logger_settings.

  METHODS get_max_exception_drill_down
    RETURNING
      VALUE(r_levels) TYPE i.

  METHODS set_max_exception_drill_down
    IMPORTING
      i_levels      TYPE i
    RETURNING
      VALUE(r_self) TYPE REF TO zif_logger_settings.

  "! Is a secondary database connection used to write the log entries to the database?
  "! See setter for more details.
  METHODS get_usage_of_secondary_db_conn
    RETURNING
      VALUE(r_2nd_db_connection_enabled) TYPE flag.

  "! Set to true if secondary database connection should be used to write the log entries to the database.
  "! This is important if main program does a rollback (on purpose or after a dump).
  "! The default is true.
  METHODS set_usage_of_secondary_db_conn
    IMPORTING
      i_use_2nd_db_connection TYPE flag
    RETURNING
      VALUE(r_self)           TYPE REF TO zif_logger_settings.

  "! Set display profile it will be used to display custom fields from context
  "!display_profile_names default, self_defined, no_tree, single
  METHODS set_display_profile
    IMPORTING
      i_profile_name       TYPE ty_profile_name OPTIONAL
      i_display_profile TYPE bal_s_prof OPTIONAL
      i_context           TYPE simple OPTIONAL

    RETURNING
      VALUE(r_self)      TYPE REF TO zif_logger_settings.

  "! get display profile it will be used to display custom fields from context
  METHODS get_display_profile
    RETURNING
      VALUE(r_display_profile) TYPE bal_s_prof.

  "! set context
  METHODS set_context_tabname
    IMPORTING
      i_context_tabname    TYPE bal_s_cont-tabname
    RETURNING
      VALUE(r_self) TYPE REF TO zif_logger_settings.

  "! get context
  METHODS get_context_tabname
    RETURNING
      VALUE(r_context_tabname) TYPE bal_s_cont-tabname.

ENDINTERFACE.
