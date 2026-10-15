INTERFACE zif_logger_ui PUBLIC.

  METHODS display_fullscreen
    IMPORTING
      logger  TYPE REF TO zif_logger
      profile TYPE REF TO zif_logger_display_profile OPTIONAL.

  METHODS display_as_popup
    IMPORTING
      logger  TYPE REF TO zif_logger
      profile TYPE REF TO zif_logger_display_profile OPTIONAL.

  METHODS display_in_container
    IMPORTING
      container TYPE REF TO cl_gui_container
      logger    TYPE REF TO zif_logger
      profile   TYPE REF TO zif_logger_display_profile OPTIONAL.

ENDINTERFACE.
