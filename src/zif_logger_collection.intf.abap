INTERFACE zif_logger_collection
  PUBLIC .

  METHODS add_logger
    IMPORTING
      logger TYPE REF TO zif_logger.
  METHODS display_logs
    IMPORTING
      display_profile_head_size TYPE i DEFAULT 125
      display_profile_tree_size TYPE i DEFAULT 25.
  METHODS display_logs_using_profile
    IMPORTING
      display_profile TYPE bal_s_prof.
ENDINTERFACE.
