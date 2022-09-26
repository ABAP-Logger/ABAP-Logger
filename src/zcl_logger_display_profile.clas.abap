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

    ASSIGN COMPONENT i_fld OF STRUCTURE display_profile TO FIELD-SYMBOL(<value>).
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

    DATA(strucdescr) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( i_context_structure ) ).
    DATA(colpos) = 100.
    DATA(sortpos) = 1.
    LOOP AT strucdescr->components INTO DATA(component).

      APPEND VALUE #(
        ref_table = i_context_structure
        ref_field = component-name
        col_pos   = colpos ) TO display_profile-mess_fcat.

      APPEND VALUE #(
        ref_table = i_context_structure
        ref_field = component-name
        col_pos   = colpos ) TO display_profile-lev1_fcat.
      colpos = colpos + 1.

      APPEND VALUE #(
        ref_table = i_context_structure
        ref_field  = component-name
        up         = 'X'
        spos       = sortpos ) TO display_profile-lev1_sort.

      sortpos = sortpos + 1.

    ENDLOOP.

    APPEND VALUE #(
      ref_table = 'BAL_S_SHOW'
      ref_field = 'T_MSGTY'
      col_pos   = colpos ) TO display_profile-lev2_fcat.

    APPEND VALUE #(
      ref_table = 'BAL_S_SHOW'
      ref_field  = 'T_MSGTY'
      up         = 'X'
      spos       = 1 ) TO display_profile-lev2_sort.


  ENDMETHOD.

ENDCLASS.
