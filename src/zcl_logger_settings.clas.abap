CLASS zcl_logger_settings DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_logger_factory.

  PUBLIC SECTION.
    INTERFACES zif_logger_settings.
    METHODS constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA auto_save                 TYPE abap_bool.
    DATA expiry_date               TYPE aldate_del .
    DATA must_be_kept_until_expiry TYPE del_before.
    DATA max_exception_drill_down  TYPE i.
    DATA use_2nd_db_connection     TYPE flag.
ENDCLASS.



CLASS ZCL_LOGGER_SETTINGS IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER_SETTINGS->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    must_be_kept_until_expiry = abap_false.
    max_exception_drill_down  = 10.
    use_2nd_db_connection     = abap_true.
    auto_save                 = abap_true.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER_SETTINGS->ZIF_LOGGER_SETTINGS~GET_AUTOSAVE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R_AUTO_SAVE                    TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_logger_settings~get_autosave.
    r_auto_save = auto_save.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER_SETTINGS->ZIF_LOGGER_SETTINGS~GET_EXPIRY_DATE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R_EXPIRY_DATE                  TYPE        ALDATE_DEL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_logger_settings~get_expiry_date.
    r_expiry_date = expiry_date.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER_SETTINGS->ZIF_LOGGER_SETTINGS~GET_MAX_EXCEPTION_DRILL_DOWN
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R_LEVELS                       TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_logger_settings~get_max_exception_drill_down.
    r_levels = max_exception_drill_down.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER_SETTINGS->ZIF_LOGGER_SETTINGS~GET_MUST_BE_KEPT_UNTIL_EXPIRY
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R_MUST_BE_KEPT_UNTIL_EXPIRY    TYPE        DEL_BEFORE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_logger_settings~get_must_be_kept_until_expiry.
    r_must_be_kept_until_expiry = must_be_kept_until_expiry.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER_SETTINGS->ZIF_LOGGER_SETTINGS~GET_USAGE_OF_SECONDARY_DB_CONN
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R_2ND_DB_CONNECTION_ENABLED    TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_logger_settings~get_usage_of_secondary_db_conn.
    r_2nd_db_connection_enabled = use_2nd_db_connection.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER_SETTINGS->ZIF_LOGGER_SETTINGS~SET_AUTOSAVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_AUTO_SAVE                    TYPE        ABAP_BOOL
* | [<-()] R_SELF                         TYPE REF TO ZIF_LOGGER_SETTINGS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_logger_settings~set_autosave.
    auto_save = i_auto_save.
    r_self    = me.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER_SETTINGS->ZIF_LOGGER_SETTINGS~SET_EXPIRY_DATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_EXPIRY_DATE                  TYPE        ALDATE_DEL
* | [<-()] R_SELF                         TYPE REF TO ZIF_LOGGER_SETTINGS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_logger_settings~set_expiry_date.
    expiry_date = i_expiry_date.
    r_self      = me.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER_SETTINGS->ZIF_LOGGER_SETTINGS~SET_EXPIRY_IN_DAYS
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_NUM_DAYS                     TYPE        I
* | [<-()] R_SELF                         TYPE REF TO ZIF_LOGGER_SETTINGS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_logger_settings~set_expiry_in_days.
    IF i_num_days > 0.
      expiry_date = sy-datum + i_num_days.
    ENDIF.
    r_self = me.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER_SETTINGS->ZIF_LOGGER_SETTINGS~SET_MAX_EXCEPTION_DRILL_DOWN
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_LEVELS                       TYPE        I
* | [<-()] R_SELF                         TYPE REF TO ZIF_LOGGER_SETTINGS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_logger_settings~set_max_exception_drill_down.
    IF i_levels >= 0.
      max_exception_drill_down = i_levels.
    ENDIF.
    r_self = me.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER_SETTINGS->ZIF_LOGGER_SETTINGS~SET_MUST_BE_KEPT_UNTIL_EXPIRY
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_MUST_BE_KEPT_UNTIL_EXPIRY    TYPE        DEL_BEFORE
* | [<-()] R_SELF                         TYPE REF TO ZIF_LOGGER_SETTINGS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_logger_settings~set_must_be_kept_until_expiry.
    must_be_kept_until_expiry = i_must_be_kept_until_expiry.
    r_self                    = me.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER_SETTINGS->ZIF_LOGGER_SETTINGS~SET_USAGE_OF_SECONDARY_DB_CONN
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_USE_2ND_DB_CONNECTION        TYPE        FLAG
* | [<-()] R_SELF                         TYPE REF TO ZIF_LOGGER_SETTINGS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_logger_settings~set_usage_of_secondary_db_conn.
    use_2nd_db_connection = i_use_2nd_db_connection.
    r_self                = me.
  ENDMETHOD.
ENDCLASS.
