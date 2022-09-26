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

ENDCLASS.



CLASS zcl_logger_display_profile IMPLEMENTATION.

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

  METHOD zif_logger_display_profile~set_grid.
    zif_logger_display_profile~set_value(
      i_fld = 'USE_GRID'
      i_val = i_grid_mode ).

    r_self = me.
  ENDMETHOD.

  METHOD zif_logger_display_profile~get.
    r_display_profile = display_profile.
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

  METHOD zif_logger_display_profile~set_context.
    CHECK display_profile IS NOT INITIAL.

    CLEAR display_profile-lev1_fcat.
    CLEAR display_profile-lev1_sort.
    CLEAR display_profile-lev2_fcat.
    CLEAR display_profile-lev2_sort.

    DATA strucdescr TYPE REF TO cl_abap_structdescr.
    DATA colpos TYPE i VALUE 100.
    DATA sortpos TYPE i VALUE 1.

    strucdescr ?= cl_abap_structdescr=>describe_by_name( i_context_structure ).

    DATA component TYPE abap_compdescr.
    DATA mess_fcat LIKE LINE OF display_profile-mess_fcat.
    DATA lev1_fcat LIKE LINE OF display_profile-lev1_fcat.
    DATA lev2_fcat LIKE LINE OF display_profile-lev1_fcat.
    DATA lev1_sort LIKE LINE OF display_profile-lev2_sort.
    DATA lev2_sort LIKE LINE OF display_profile-lev2_sort.


    LOOP AT strucdescr->components INTO component.

      CLEAR mess_fcat.
      CLEAR lev1_fcat.
      CLEAR lev1_sort.

      mess_fcat-ref_table = i_context_structure.
      mess_fcat-ref_field = component-name.
      mess_fcat-col_pos   = colpos.
      APPEND mess_fcat TO display_profile-mess_fcat.

      lev1_fcat-ref_table = i_context_structure.
      lev1_fcat-ref_field = component-name.
      lev1_fcat-col_pos   = colpos.
      APPEND lev1_fcat TO display_profile-lev1_fcat.

      colpos = colpos + 1.


      lev1_sort-ref_table = i_context_structure.
      lev1_sort-ref_field  = component-name.
      lev1_sort-up         = 'X'.
      lev1_sort-spos       = sortpos.
      APPEND lev1_sort TO display_profile-lev1_sort.

      sortpos = sortpos + 1.

    ENDLOOP.

    lev2_fcat-ref_table = 'BAL_S_SHOW'.
    lev2_fcat-ref_field = 'T_MSGTY'.
    lev2_fcat-col_pos   = colpos.
    APPEND lev2_fcat TO display_profile-lev2_fcat.

    lev2_sort-ref_table = 'BAL_S_SHOW'.
    lev2_sort-ref_field  = 'T_MSGTY'.
    lev2_sort-up         = 'X'.
    lev2_sort-spos       = 1.
    APPEND lev2_sort TO display_profile-lev2_sort.

  ENDMETHOD.

ENDCLASS.
