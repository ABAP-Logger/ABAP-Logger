CLASS zcl_logger_injector DEFINITION
  PUBLIC
  FINAL
  FOR TESTING
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS set_logger
      IMPORTING
        logger TYPE REF TO zif_logger.

    CLASS-METHODS set_settings
      IMPORTING
        settings TYPE REF TO zif_logger_settings.

    CLASS-METHODS set_collection
      IMPORTING
        collection TYPE REF TO zif_logger_collection.

    CLASS-METHODS set_display_profile
      IMPORTING
        display_profile TYPE REF TO zif_logger_display_profile.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_logger_injector IMPLEMENTATION.


  METHOD set_collection.
    zcl_logger_factory=>collection = collection.
  ENDMETHOD.


  METHOD set_display_profile.
    zcl_logger_factory=>display_profile = display_profile.
  ENDMETHOD.


  METHOD set_logger.
    zcl_logger_factory=>logger = logger.
  ENDMETHOD.


  METHOD set_settings.
    zcl_logger_factory=>settings = settings.
  ENDMETHOD.
ENDCLASS.
