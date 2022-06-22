CLASS zcl_logger_settings DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_logger_factory.

  PUBLIC SECTION.
    INTERFACES zif_logger_settings.
    METHODS constructor.

    ALIASES:
      set_context_tabname FOR zif_logger_settings~set_context_tabname,
      get_context_tabname FOR zif_logger_settings~get_context_tabname,
      set_display_profile FOR zif_logger_settings~set_display_profile,
      get_display_profile FOR zif_logger_settings~get_display_profile.

    CONSTANTS :
      BEGIN OF display_profile_names,
        default      TYPE zif_logger_settings~ty_profile_name VALUE 'STANDARD',
        self_defined TYPE zif_logger_settings~ty_profile_name VALUE 'SELF_DEFINED',
        no_tree      TYPE zif_logger_settings~ty_profile_name VALUE 'NO_TREE',
        single       TYPE zif_logger_settings~ty_profile_name VALUE 'SINGLE',
      END OF display_profile_names.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA auto_save                 TYPE abap_bool.
    DATA expiry_date               TYPE aldate_del .
    DATA must_be_kept_until_expiry TYPE del_before.
    DATA max_exception_drill_down  TYPE i.
    DATA use_2nd_db_connection     TYPE flag.
    DATA display_profile TYPE bal_s_prof.
    DATA context_tabname TYPE bal_s_cont-tabname.
    DATA profile_name TYPE zif_logger_settings~ty_profile_name.

    METHODS:
      get_standard_display_profile
        RETURNING
          VALUE(r_display_profile) TYPE bal_s_prof,

      get_no_tree_display_profile
        RETURNING
          VALUE(r_display_profile) TYPE bal_s_prof,

      get_single_display_profile
        RETURNING
          VALUE(r_display_profile) TYPE bal_s_prof,

      get_self_display_profile
        RETURNING
          VALUE(r_display_profile) TYPE bal_s_prof,

      get_fcat_fm_context_tab
        EXPORTING display_field_catalog TYPE bal_t_fcat.

ENDCLASS.

CLASS zcl_logger_settings IMPLEMENTATION.

  METHOD constructor.
    must_be_kept_until_expiry = abap_false.
    max_exception_drill_down  = 10.
    use_2nd_db_connection     = abap_true.
    auto_save                 = abap_true.
    profile_name              = display_profile_names-default.
  ENDMETHOD.

  METHOD zif_logger_settings~get_autosave.
    r_auto_save = auto_save.
  ENDMETHOD.

  METHOD zif_logger_settings~set_autosave.
    auto_save = i_auto_save.
    r_self    = me.
  ENDMETHOD.

  METHOD zif_logger_settings~get_expiry_date.
    r_expiry_date = expiry_date.
  ENDMETHOD.

  METHOD zif_logger_settings~set_expiry_date.
    expiry_date = i_expiry_date.
    r_self      = me.
  ENDMETHOD.

  METHOD zif_logger_settings~set_expiry_in_days.
    IF i_num_days > 0.
      expiry_date = sy-datum + i_num_days.
    ENDIF.
    r_self = me.
  ENDMETHOD.

  METHOD zif_logger_settings~get_must_be_kept_until_expiry.
    r_must_be_kept_until_expiry = must_be_kept_until_expiry.
  ENDMETHOD.

  METHOD zif_logger_settings~set_must_be_kept_until_expiry.
    must_be_kept_until_expiry = i_must_be_kept_until_expiry.
    r_self                    = me.
  ENDMETHOD.

  METHOD zif_logger_settings~get_max_exception_drill_down.
    r_levels = max_exception_drill_down.
  ENDMETHOD.

  METHOD zif_logger_settings~set_max_exception_drill_down.
    IF i_levels >= 0.
      max_exception_drill_down = i_levels.
    ENDIF.
    r_self = me.
  ENDMETHOD.

  METHOD zif_logger_settings~get_usage_of_secondary_db_conn.
    r_2nd_db_connection_enabled = use_2nd_db_connection.
  ENDMETHOD.

  METHOD zif_logger_settings~set_usage_of_secondary_db_conn.
    use_2nd_db_connection = i_use_2nd_db_connection.
    r_self                = me.
  ENDMETHOD.

  METHOD zif_logger_settings~set_display_profile.
    DATA:
      lr_typedesc TYPE REF TO cl_abap_typedescr,
      lr_struc    TYPE REF TO cl_abap_structdescr,
      lt_comp     TYPE abap_compdescr_tab,
      lv_context_tabname TYPE bal_s_cont-tabname.

    IF i_profile_name IS SUPPLIED.
      profile_name = i_profile_name.
    ENDIF.

    IF i_context IS SUPPLIED.
      lr_typedesc = cl_abap_typedescr=>describe_by_data( i_context ).
      lr_struc ?= lr_typedesc.
      lt_comp = lr_struc->components.
      lv_context_tabname = cl_abap_typedescr=>describe_by_data( i_context )->get_ddic_header( )-tabname.

      set_context_tabname( lv_context_tabname ).

    ENDIF.

    IF i_display_profile IS SUPPLIED.
      display_profile = i_display_profile.
    ENDIF.

    r_self = me.
  ENDMETHOD.

  METHOD zif_logger_settings~get_display_profile.

    CASE profile_name.
      WHEN display_profile_names-single.

        r_display_profile = get_single_display_profile( ).

      WHEN display_profile_names-no_tree.

        r_display_profile = get_no_tree_display_profile( ).

      WHEN display_profile_names-self_defined.

        r_display_profile =  get_self_display_profile(  ).

      WHEN OTHERS."Default profile

        r_display_profile = get_standard_display_profile( ).

    ENDCASE.

    get_fcat_fm_context_tab( IMPORTING display_field_catalog = r_display_profile-mess_fcat ).

  ENDMETHOD.

  METHOD get_standard_display_profile.

    CALL FUNCTION 'BAL_DSP_PROFILE_STANDARD_GET'
      IMPORTING
        e_s_display_profile = r_display_profile
      EXCEPTIONS
        OTHERS              = 1.

    r_display_profile-use_grid = 'X'.
    r_display_profile-disvariant-report = sy-cprog.
    r_display_profile-disvariant-handle = 'ALOG'.
    r_display_profile-show_all =       "all messages should be displayed immediately
    r_display_profile-cwidth_opt =     "application log: optimize message list column width
    r_display_profile-mess_mark = 'X'. "application log: messages in list selectable
    r_display_profile-exp_level = 1.

  ENDMETHOD.

  METHOD get_no_tree_display_profile.

    CALL FUNCTION 'BAL_DSP_PROFILE_NO_TREE_GET'
      IMPORTING
        e_s_display_profile = r_display_profile
      EXCEPTIONS
        OTHERS              = 1.

    r_display_profile-use_grid = 'X'.
    r_display_profile-disvariant-report = sy-cprog.
    r_display_profile-disvariant-handle = 'ALOG'.
    r_display_profile-show_all =       "all messages should be displayed immediately
    r_display_profile-cwidth_opt =     "application log: optimize message list column width
    r_display_profile-mess_mark = 'X'. "application log: messages in list selectable
    r_display_profile-exp_level = 1.

  ENDMETHOD.

  METHOD get_single_display_profile.

* get a prepared profile
    CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
      IMPORTING
        e_s_display_profile = r_display_profile
      EXCEPTIONS
        OTHERS              = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    r_display_profile-use_grid = 'X'.
    r_display_profile-disvariant-report = sy-cprog.
    r_display_profile-disvariant-handle = 'ALOG'.
    r_display_profile-show_all =       "all messages should be displayed immediately
    r_display_profile-cwidth_opt =     "application log: optimize message list column width
    r_display_profile-mess_mark = 'X'. "application log: messages in list selectable
    r_display_profile-exp_level = 1.

  ENDMETHOD.

  METHOD get_self_display_profile.

    r_display_profile = display_profile.


    IF display_profile-head_size IS INITIAL.
      r_display_profile-head_size = 47.
    ENDIF.
    r_display_profile-tree_size = 28.

    IF r_display_profile-disvariant-report IS INITIAL.
      r_display_profile-disvariant-report = sy-cprog.
    ENDIF.

    IF r_display_profile-disvariant-handle IS INITIAL.
      r_display_profile-disvariant-handle = 'ALOG'.
    ENDIF.

    r_display_profile-use_grid =
    r_display_profile-show_all =       "all messages should be displayed immediately
    r_display_profile-cwidth_opt =     "application log: optimize message list column width
    r_display_profile-mess_mark = 'X'. "application log: messages in list selectable
    r_display_profile-exp_level = 1.

    DATA:
      l_s_fcat           TYPE bal_s_fcat.

    l_s_fcat-ref_field = 'T_MSG'.
    l_s_fcat-ref_table ='BAL_S_SHOW'.
    l_s_fcat-outputlen = 85.
    l_s_fcat-col_pos   = 1.
    APPEND l_s_fcat TO r_display_profile-mess_fcat.

  ENDMETHOD.

  METHOD get_fcat_fm_context_tab.
    DATA:
      l_s_fcat    TYPE bal_s_fcat,
      lr_typedesc TYPE REF TO cl_abap_typedescr,
      lr_struc    TYPE REF TO cl_abap_structdescr,
      ls_comp     TYPE abap_compdescr.

    IF context_tabname IS NOT INITIAL.
      lr_typedesc = cl_abap_typedescr=>describe_by_name( context_tabname ).
      lr_struc ?= lr_typedesc.


      LOOP AT lr_struc->components INTO ls_comp.
        l_s_fcat-col_pos   = 100 + sy-tabix.

        READ TABLE display_profile-mess_fcat TRANSPORTING NO FIELDS
          WITH KEY ref_table = context_tabname  ref_field = ls_comp-name.
        IF sy-subrc NE 0.
          l_s_fcat-ref_field = ls_comp-name.
          l_s_fcat-ref_table = context_tabname.
          l_s_fcat-outputlen = ls_comp-length." + 1.
          l_s_fcat-col_pos   = l_s_fcat-col_pos.   "most right columns
          APPEND l_s_fcat TO display_field_catalog.
        ENDIF.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD zif_logger_settings~set_context_tabname.
    context_tabname = i_context_tabname.
  ENDMETHOD.

  METHOD zif_logger_settings~get_context_tabname.
    r_context_tabname = context_tabname.
  ENDMETHOD.

ENDCLASS.
