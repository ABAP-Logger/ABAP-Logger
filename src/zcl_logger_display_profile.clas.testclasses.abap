CLASS ltcl_display_profile_should DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zif_logger_display_profile.

    METHODS setup.
    METHODS adds_message_context_columns FOR TESTING.
    METHODS builds_context_tree_below_root FOR TESTING.
    METHODS builds_context_tree_below_log FOR TESTING.
    METHODS rejects_unknown_profile_field FOR TESTING.
ENDCLASS.

CLASS ltcl_display_profile_should IMPLEMENTATION.

  METHOD setup.
    cut = zcl_logger_ui=>create_display_profile( ).
    cut->set( i_standard = abap_true ).
  ENDMETHOD.

  METHOD adds_message_context_columns.
    DATA profile TYPE bal_s_prof.
    FIELD-SYMBOLS <field> LIKE LINE OF profile-mess_fcat.

    cut->set_context_message( i_context_structure = 'BAL_S_LOG' ).
    profile = cut->get( ).

    READ TABLE profile-mess_fcat ASSIGNING <field>
      WITH KEY ref_table = 'BAL_S_LOG' ref_field = 'OBJECT'.
    cl_abap_unit_assert=>assert_equals(
      act = sy-subrc
      exp = 0
      msg = |The context object must be added to the message catalogue| ).
    cl_abap_unit_assert=>assert_equals(
      act = <field>-col_pos
      exp = 101
      msg = |Context columns must start after the standard catalogue| ).
  ENDMETHOD.

  METHOD builds_context_tree_below_root.
    DATA profile TYPE bal_s_prof.

    cut->set_context_tree( i_context_structure = 'BAL_S_LOG' ).
    profile = cut->get( ).

    READ TABLE profile-lev1_fcat WITH KEY ref_table = 'BAL_S_LOG' ref_field = 'OBJECT'
      TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_equals(
      act = sy-subrc
      exp = 0
      msg = |Context fields must be displayed in the first tree level| ).
    READ TABLE profile-lev1_sort WITH KEY ref_table = 'BAL_S_LOG' ref_field = 'OBJECT'
      TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_equals(
      act = sy-subrc
      exp = 0
      msg = |Context fields must sort the first tree level| ).
    READ TABLE profile-lev2_fcat WITH KEY ref_table = 'BAL_S_SHOW' ref_field = 'T_MSGTY'
      TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_equals(
      act = sy-subrc
      exp = 0
      msg = |Message type must be shown below the context level| ).
  ENDMETHOD.

  METHOD builds_context_tree_below_log.
    DATA profile TYPE bal_s_prof.

    cut->set_context_tree(
      i_context_structure = 'BAL_S_LOG'
      i_under_log         = abap_true ).
    profile = cut->get( ).

    READ TABLE profile-lev2_fcat WITH KEY ref_table = 'BAL_S_LOG' ref_field = 'OBJECT'
      TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_equals(
      act = sy-subrc
      exp = 0
      msg = |Context fields must move below the log when requested| ).
    READ TABLE profile-lev3_fcat WITH KEY ref_table = 'BAL_S_SHOW' ref_field = 'T_MSGTY'
      TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_equals(
      act = sy-subrc
      exp = 0
      msg = |Message type must follow the context level below a log| ).
  ENDMETHOD.

  METHOD rejects_unknown_profile_field.
    DATA exception TYPE REF TO zcx_logger.

    TRY.
        cut->set_value(
          i_fld = 'DOES_NOT_EXIST'
          i_val = abap_true ).
        cl_abap_unit_assert=>fail( |An unknown profile field must be rejected| ).
      CATCH zcx_logger INTO exception.
        cl_abap_unit_assert=>assert_not_initial(
          act = exception->info
          msg = |The invalid field name should be included in the exception| ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
