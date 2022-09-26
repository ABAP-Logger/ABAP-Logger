INTERFACE zif_logger_display_profile
  PUBLIC .
  METHODS set
    IMPORTING
      i_detlevel    TYPE clike OPTIONAL
      i_no_tree     TYPE clike OPTIONAL
      i_popup       TYPE clike OPTIONAL
      i_single_log  TYPE clike OPTIONAL
      i_standard    TYPE clike DEFAULT abap_true
    RETURNING
      VALUE(r_self) TYPE REF TO zif_logger_display_profile.
  METHODS get
    RETURNING VALUE(r_display_profile) TYPE bal_s_prof.
  METHODS set_grid
    IMPORTING
      i_grid_mode   TYPE clike
    RETURNING
      VALUE(r_self) TYPE REF TO zif_logger_display_profile.
  METHODS set_value
    IMPORTING
      i_fld       TYPE clike
      i_val       TYPE any
    RETURNING
      VALUE(r_self) TYPE REF TO zif_logger_display_profile.
  METHODS set_context
    IMPORTING
      i_context_structure TYPE clike.

ENDINTERFACE.
