CLASS zcl_logger_display_profile DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
   GLOBAL FRIENDS zcl_logger_factory.

  PUBLIC SECTION.
    INTERFACES zif_logger_display_profile.
  PROTECTED SECTION.
    DATA display_profile TYPE bal_s_prof.
  PRIVATE SECTION.

    METHODS get_structure_components
      IMPORTING
        i_structure_name    TYPE clike
      RETURNING
        VALUE(r_components) TYPE cl_abap_structdescr=>component_table.
ENDCLASS.



CLASS zcl_logger_display_profile IMPLEMENTATION.

  METHOD get_structure_components.
    DATA strucdescr TYPE REF TO cl_abap_structdescr.
    strucdescr ?= cl_abap_structdescr=>describe_by_name( i_structure_name ).
    r_components = strucdescr->get_components( ).
  ENDMETHOD.

  METHOD zif_logger_display_profile~get.
    r_display_profile = display_profile.
  ENDMETHOD.

  METHOD zif_logger_display_profile~set.
    CASE abap_true.
      WHEN i_detlevel.
        CALL FUNCTION 'BAL_DSP_PROFILE_DETLEVEL_GET'
          IMPORTING
            e_s_display_profile = display_profile.
      WHEN i_no_tree.
        CALL FUNCTION 'BAL_DSP_PROFILE_NO_TREE_GET'
          IMPORTING
            e_s_display_profile = display_profile.
      WHEN i_popup.
        CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
          IMPORTING
            e_s_display_profile = display_profile.
      WHEN i_single_log.
        CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
          IMPORTING
            e_s_display_profile = display_profile.
      WHEN OTHERS.
        CALL FUNCTION 'BAL_DSP_PROFILE_STANDARD_GET'
          IMPORTING
            e_s_display_profile = display_profile.
    ENDCASE.

    r_self = me.
  ENDMETHOD.

  METHOD zif_logger_display_profile~set_context_message.
    CHECK display_profile IS NOT INITIAL.

    DATA colpos     TYPE i VALUE 100.
    DATA mess_fcat  LIKE LINE OF display_profile-mess_fcat.
    DATA component  TYPE cl_abap_structdescr=>component.
    DATA components TYPE cl_abap_structdescr=>component_table.

    components = get_structure_components( i_context_structure ).

    LOOP AT components INTO component.

      CLEAR mess_fcat.
      mess_fcat-ref_table = i_context_structure.
      mess_fcat-ref_field = component-name.
      mess_fcat-col_pos   = colpos.
      APPEND mess_fcat TO display_profile-mess_fcat.
      colpos = colpos + 1.

    ENDLOOP.

    r_self = me.
  ENDMETHOD.

  METHOD zif_logger_display_profile~set_context_tree.
    FIELD-SYMBOLS <lev1_fcat> TYPE bal_t_fcat.
    FIELD-SYMBOLS <lev2_fcat> TYPE bal_t_fcat.
    FIELD-SYMBOLS <lev1_sort> TYPE bal_t_sort.
    FIELD-SYMBOLS <lev2_sort> TYPE bal_t_sort.

    CHECK display_profile IS NOT INITIAL.

    IF i_under_log IS INITIAL.
      ASSIGN display_profile-lev1_fcat TO <lev1_fcat>.
      ASSIGN display_profile-lev2_fcat TO <lev2_fcat>.
      ASSIGN display_profile-lev1_sort TO <lev1_sort>.
      ASSIGN display_profile-lev2_sort TO <lev2_sort>.
    ELSE.
      ASSIGN display_profile-lev2_fcat TO <lev1_fcat>.
      ASSIGN display_profile-lev3_fcat TO <lev2_fcat>.
      ASSIGN display_profile-lev2_sort TO <lev1_sort>.
      ASSIGN display_profile-lev3_sort TO <lev2_sort>.
    ENDIF.
    CLEAR <lev1_fcat>.
    CLEAR <lev2_fcat>.
    CLEAR <lev1_sort>.
    CLEAR <lev2_sort>.

    DATA colpos     TYPE i VALUE 100.
    DATA sortpos    TYPE i VALUE 1.
    DATA lev_fcat   LIKE LINE OF display_profile-lev1_fcat.
    DATA lev_sort   LIKE LINE OF display_profile-lev2_sort.
    DATA component  TYPE cl_abap_structdescr=>component.
    DATA components TYPE cl_abap_structdescr=>component_table.

    components = get_structure_components( i_context_structure ).

    LOOP AT components INTO component.
      CLEAR lev_fcat.

      lev_fcat-ref_table = i_context_structure.
      lev_fcat-ref_field = component-name.
      lev_fcat-col_pos   = colpos.
      APPEND lev_fcat TO <lev1_fcat>.

      colpos = colpos + 1.

      CLEAR lev_sort.
      lev_sort-ref_table = i_context_structure.
      lev_sort-ref_field  = component-name.
      lev_sort-up         = 'X'.
      lev_sort-spos       = sortpos.
      APPEND lev_sort TO <lev1_sort>.

      sortpos = sortpos + 1.
    ENDLOOP.

    CLEAR lev_fcat.
    lev_fcat-ref_table = 'BAL_S_SHOW'.
    lev_fcat-ref_field = 'T_MSGTY'.
    lev_fcat-col_pos   = colpos.
    APPEND lev_fcat TO <lev2_fcat>.

    CLEAR lev_sort.
    lev_sort-ref_table = 'BAL_S_SHOW'.
    lev_sort-ref_field = 'T_MSGTY'.
    lev_sort-up        = 'X'.
    lev_sort-spos      = 1.
    APPEND lev_sort TO <lev2_sort>.

    r_self = me.
  ENDMETHOD.

  METHOD zif_logger_display_profile~set_grid.
    zif_logger_display_profile~set_value(
      i_fld = 'USE_GRID'
      i_val = i_grid_mode ).

    r_self = me.
  ENDMETHOD.

  METHOD zif_logger_display_profile~set_value.
    FIELD-SYMBOLS <value> TYPE any.
    ASSIGN COMPONENT i_fld OF STRUCTURE display_profile TO <value>.
    IF sy-subrc = 0.
      <value> = i_val.
      r_self = me.
    ELSE.
      RAISE EXCEPTION TYPE zcx_logger_display_profile
        EXPORTING
          info = |field { i_fld } does not exist| ##no_text.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
