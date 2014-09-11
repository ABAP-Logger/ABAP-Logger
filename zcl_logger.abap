class ZCL_LOGGER definition
  public
  create private .

public section.
*"* public components of class ZCL_LOGGER
*"* do not include other source files here!!!

  data HEADER type BAL_S_LOG read-only .
  data HANDLE type BALLOGHNDL read-only .
  data DB_NUMBER type BALOGNR read-only .

  class-methods NEW
    importing
      !OBJECT type CSEQUENCE optional
      !SUBOBJECT type CSEQUENCE optional
      !DESC type CSEQUENCE optional
      !CONTEXT type SIMPLE optional
    returning
      value(R_LOG) type ref to ZCL_LOGGER .
  type-pools ABAP .
  class-methods OPEN
    importing
      !OBJECT type CSEQUENCE
      !SUBOBJECT type CSEQUENCE
      !DESC type CSEQUENCE optional
      !CREATE_IF_DOES_NOT_EXIST type ABAP_BOOL default ABAP_FALSE
    returning
      value(R_LOG) type ref to ZCL_LOGGER .
  methods ADD
    importing
      !OBJ_TO_LOG type ANY optional
      !CONTEXT type SIMPLE optional
      !CALLBACK_FORM type CSEQUENCE optional
      !CALLBACK_PROG type CSEQUENCE optional
      !CALLBACK_FM type CSEQUENCE optional
      !TYPE type SYMSGTY optional
      !IMPORTANCE type BALPROBCL optional
    preferred parameter OBJ_TO_LOG
    returning
      value(SELF) type ref to ZCL_LOGGER .
  methods A
    importing
      !OBJ_TO_LOG type ANY optional
      !CONTEXT type SIMPLE optional
      !CALLBACK_FORM type CSEQUENCE optional
      !CALLBACK_PROG type CSEQUENCE optional
      !CALLBACK_FM type CSEQUENCE optional
      !IMPORTANCE type BALPROBCL optional
    preferred parameter OBJ_TO_LOG
    returning
      value(SELF) type ref to ZCL_LOGGER .
  methods E
    importing
      !OBJ_TO_LOG type ANY optional
      !CONTEXT type SIMPLE optional
      !CALLBACK_FORM type CSEQUENCE optional
      !CALLBACK_PROG type CSEQUENCE optional
      !CALLBACK_FM type CSEQUENCE optional
      !IMPORTANCE type BALPROBCL optional
    preferred parameter OBJ_TO_LOG
    returning
      value(SELF) type ref to ZCL_LOGGER .
  methods W
    importing
      !OBJ_TO_LOG type ANY optional
      !CONTEXT type SIMPLE optional
      !CALLBACK_FORM type CSEQUENCE optional
      !CALLBACK_PROG type CSEQUENCE optional
      !CALLBACK_FM type CSEQUENCE optional
      !IMPORTANCE type BALPROBCL optional
    preferred parameter OBJ_TO_LOG
    returning
      value(SELF) type ref to ZCL_LOGGER .
  methods I
    importing
      !OBJ_TO_LOG type ANY optional
      !CONTEXT type SIMPLE optional
      !CALLBACK_FORM type CSEQUENCE optional
      !CALLBACK_PROG type CSEQUENCE optional
      !CALLBACK_FM type CSEQUENCE optional
      !IMPORTANCE type BALPROBCL optional
    preferred parameter OBJ_TO_LOG
    returning
      value(SELF) type ref to ZCL_LOGGER .
  methods S
    importing
      !OBJ_TO_LOG type ANY optional
      !CONTEXT type SIMPLE optional
      !CALLBACK_FORM type CSEQUENCE optional
      !CALLBACK_PROG type CSEQUENCE optional
      !CALLBACK_FM type CSEQUENCE optional
      !IMPORTANCE type BALPROBCL optional
    preferred parameter OBJ_TO_LOG
    returning
      value(SELF) type ref to ZCL_LOGGER .
  methods POPUP .
  methods FULLSCREEN .
protected section.
*"* protected components of class ZCL_LOGGER
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_LOGGER
*"* do not include other source files here!!!

  type-pools ABAP .
  data AUTO_SAVE type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_LOGGER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER->A
* +-------------------------------------------------------------------------------------------------+
* | [--->] OBJ_TO_LOG                     TYPE        ANY(optional)
* | [--->] CONTEXT                        TYPE        SIMPLE(optional)
* | [--->] CALLBACK_FORM                  TYPE        CSEQUENCE(optional)
* | [--->] CALLBACK_PROG                  TYPE        CSEQUENCE(optional)
* | [--->] CALLBACK_FM                    TYPE        CSEQUENCE(optional)
* | [--->] IMPORTANCE                     TYPE        BALPROBCL(optional)
* | [<-()] SELF                           TYPE REF TO ZCL_LOGGER
* +--------------------------------------------------------------------------------------</SIGNATURE>
method A.
  self = add(
    obj_to_log    = obj_to_log
    context       = context
    callback_form = callback_form
    callback_prog = callback_prog
    callback_fm   = callback_fm
    type          = 'A'
    importance    = importance ).
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER->ADD
* +-------------------------------------------------------------------------------------------------+
* | [--->] OBJ_TO_LOG                     TYPE        ANY(optional)
* | [--->] CONTEXT                        TYPE        SIMPLE(optional)
* | [--->] CALLBACK_FORM                  TYPE        CSEQUENCE(optional)
* | [--->] CALLBACK_PROG                  TYPE        CSEQUENCE(optional)
* | [--->] CALLBACK_FM                    TYPE        CSEQUENCE(optional)
* | [--->] TYPE                           TYPE        SYMSGTY(optional)
* | [--->] IMPORTANCE                     TYPE        BALPROBCL(optional)
* | [<-()] SELF                           TYPE REF TO ZCL_LOGGER
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD add.

  DATA: detailed_msg  TYPE bal_s_msg,
        free_text_msg TYPE char200,
        msg_type  TYPE REF TO cl_abap_typedescr,
        msg_table_type TYPE REF TO cl_abap_tabledescr,
        exception_data TYPE bal_s_exc,
        log_numbers TYPE bal_t_lgnm,
        log_handles TYPE bal_t_logh,
        log_number  TYPE bal_s_lgnm,
        formatted_context TYPE bal_s_cont,
        formatted_params TYPE bal_s_parm.

  FIELD-SYMBOLS: <table_of_messages> TYPE any table,
                 <message_line> TYPE any,
                 <bapi_msg> TYPE bapiret2,
                 <bdc_msg> TYPE bdcmsgcoll,
                 <context_val> TYPE c.

  IF context IS NOT INITIAL.
    ASSIGN context TO <context_val> CASTING.
    formatted_context-value = <context_val>.
    formatted_context-tabname =
      cl_abap_typedescr=>describe_by_data( context )->get_ddic_header( )-tabname.
  ENDIF.

  IF callback_fm IS NOT INITIAL.
    formatted_params-callback-userexitf = callback_fm.
    formatted_params-callback-userexitp = callback_prog.
    formatted_params-callback-userexitt = 'F'.
  ELSEIF callback_form IS NOT INITIAL.
    formatted_params-callback-userexitf = callback_form.
    formatted_params-callback-userexitp = callback_prog.
    formatted_params-callback-userexitt = ' '.
  ENDIF.

  msg_type = cl_abap_typedescr=>describe_by_data( obj_to_log ).

  IF obj_to_log IS INITIAL.
    detailed_msg-msgty = sy-msgty.
    detailed_msg-msgid = sy-msgid.
    detailed_msg-msgno = sy-msgno.
    detailed_msg-msgv1 = sy-msgv1.
    detailed_msg-msgv2 = sy-msgv2.
    detailed_msg-msgv3 = sy-msgv3.
    detailed_msg-msgv4 = sy-msgv4.
  ELSEIF msg_type->type_kind = cl_abap_typedescr=>typekind_oref.
    exception_data-exception = obj_to_log.
    exception_data-msgty = type.
    exception_data-probclass = importance.
  ELSEIF msg_type->type_kind = cl_abap_typedescr=>typekind_table.
    ASSIGN obj_to_log TO <table_of_messages>.
    LOOP AT <table_of_messages> ASSIGNING <message_line>.
      add( <message_line> ).
    ENDLOOP.
    RETURN.
  ELSEIF msg_type->absolute_name = '\TYPE=BAPIRET2'.
    ASSIGN obj_to_log TO <bapi_msg>.
    detailed_msg-msgty = <bapi_msg>-type.
    detailed_msg-msgid = <bapi_msg>-id.
    detailed_msg-msgno = <bapi_msg>-number.
    detailed_msg-msgv1 = <bapi_msg>-message_v1.
    detailed_msg-msgv2 = <bapi_msg>-message_v2.
    detailed_msg-msgv3 = <bapi_msg>-message_v3.
    detailed_msg-msgv4 = <bapi_msg>-message_v4.
  ELSEIF msg_type->absolute_name = '\TYPE=BDCMSGCOLL'.
    ASSIGN obj_to_log TO <bdc_msg>.
    detailed_msg-msgty = <bdc_msg>-msgtyp.
    detailed_msg-msgid = <bdc_msg>-msgid.
    detailed_msg-msgno = <bdc_msg>-msgnr.
    detailed_msg-msgv1 = <bdc_msg>-msgv1.
    detailed_msg-msgv2 = <bdc_msg>-msgv2.
    detailed_msg-msgv3 = <bdc_msg>-msgv3.
    detailed_msg-msgv4 = <bdc_msg>-msgv4.
  ELSE.
    free_text_msg = obj_to_log.
  ENDIF.

  IF free_text_msg IS NOT INITIAL.
    CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
      EXPORTING
        i_log_handle = me->handle
        i_msgty      = type
        i_probclass  = importance
        i_text       = free_text_msg
        i_s_context  = formatted_context
        i_s_params   = formatted_params.
  ELSEIF exception_data IS NOT INITIAL.
    CALL FUNCTION 'BAL_LOG_EXCEPTION_ADD'
      EXPORTING
        i_log_handle = me->handle
        i_s_exc      = exception_data.
  ELSEIF detailed_msg IS NOT INITIAL.
    detailed_msg-context = formatted_context.
    detailed_msg-params = formatted_params.
    detailed_msg-probclass = importance.
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = me->handle
        i_s_msg      = detailed_msg.
  ENDIF.

  IF auto_save = abap_true.
    APPEND me->handle TO log_handles.
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle   = log_handles
      IMPORTING
        e_new_lognumbers = log_numbers.
    IF me->db_number IS INITIAL.
      READ TABLE log_numbers INDEX 1 INTO log_number.
      me->db_number = log_number-lognumber.
    ENDIF.
  ENDIF.

  self = me.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER->E
* +-------------------------------------------------------------------------------------------------+
* | [--->] OBJ_TO_LOG                     TYPE        ANY(optional)
* | [--->] CONTEXT                        TYPE        SIMPLE(optional)
* | [--->] CALLBACK_FORM                  TYPE        CSEQUENCE(optional)
* | [--->] CALLBACK_PROG                  TYPE        CSEQUENCE(optional)
* | [--->] CALLBACK_FM                    TYPE        CSEQUENCE(optional)
* | [--->] IMPORTANCE                     TYPE        BALPROBCL(optional)
* | [<-()] SELF                           TYPE REF TO ZCL_LOGGER
* +--------------------------------------------------------------------------------------</SIGNATURE>
method E.
  self = add(
    obj_to_log    = obj_to_log
    context       = context
    callback_form = callback_form
    callback_prog = callback_prog
    callback_fm   = callback_fm
    type          = 'E'
    importance    = importance ).
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER->FULLSCREEN
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method FULLSCREEN.

  DATA: profile TYPE bal_s_prof.
  CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
    IMPORTING
      e_s_display_profile = profile.

  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXPORTING
      i_s_display_profile    = profile
      i_t_log_handle         = me->handle.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER->I
* +-------------------------------------------------------------------------------------------------+
* | [--->] OBJ_TO_LOG                     TYPE        ANY(optional)
* | [--->] CONTEXT                        TYPE        SIMPLE(optional)
* | [--->] CALLBACK_FORM                  TYPE        CSEQUENCE(optional)
* | [--->] CALLBACK_PROG                  TYPE        CSEQUENCE(optional)
* | [--->] CALLBACK_FM                    TYPE        CSEQUENCE(optional)
* | [--->] IMPORTANCE                     TYPE        BALPROBCL(optional)
* | [<-()] SELF                           TYPE REF TO ZCL_LOGGER
* +--------------------------------------------------------------------------------------</SIGNATURE>
method I.
  self = add(
    obj_to_log    = obj_to_log
    context       = context
    callback_form = callback_form
    callback_prog = callback_prog
    callback_fm   = callback_fm
    type          = 'I'
    importance    = importance ).
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_LOGGER=>NEW
* +-------------------------------------------------------------------------------------------------+
* | [--->] OBJECT                         TYPE        CSEQUENCE(optional)
* | [--->] SUBOBJECT                      TYPE        CSEQUENCE(optional)
* | [--->] DESC                           TYPE        CSEQUENCE(optional)
* | [--->] CONTEXT                        TYPE        SIMPLE(optional)
* | [<-()] R_LOG                          TYPE REF TO ZCL_LOGGER
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD new.

  FIELD-SYMBOLS <context_val> TYPE c.

  CREATE OBJECT r_log.
  r_log->header-object = object.
  r_log->header-subobject = subobject.
  r_log->header-extnumber = desc.
  IF object IS NOT INITIAL AND subobject IS NOT INITIAL.
    r_log->auto_save = abap_true.
  ENDIF.

  IF context IS SUPPLIED.
    r_log->header-context-tabname =
      cl_abap_typedescr=>describe_by_data( context )->get_ddic_header( )-tabname.
    ASSIGN context TO <context_val> CASTING.
    r_log->header-context-value = <context_val>.
  ENDIF.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log      = r_log->header
    IMPORTING
      e_log_handle = r_log->handle.

* BAL_LOG_CREATE will fill in some additional header data.
* This FM updates our instance attribute to reflect that.
  CALL FUNCTION 'BAL_LOG_HDR_READ'
    EXPORTING
      i_log_handle = r_log->handle
    IMPORTING
      e_s_log      = r_log->header.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_LOGGER=>OPEN
* +-------------------------------------------------------------------------------------------------+
* | [--->] OBJECT                         TYPE        CSEQUENCE
* | [--->] SUBOBJECT                      TYPE        CSEQUENCE
* | [--->] DESC                           TYPE        CSEQUENCE(optional)
* | [--->] CREATE_IF_DOES_NOT_EXIST       TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [<-()] R_LOG                          TYPE REF TO ZCL_LOGGER
* +--------------------------------------------------------------------------------------</SIGNATURE>
method OPEN.

  DATA: filter TYPE bal_s_lfil,
        desc_filter TYPE bal_s_extn,
        obj_filter TYPE bal_s_obj,
        subobj_filter TYPE bal_s_sub,

        found_headers TYPE balhdr_t,
        most_recent_header TYPE balhdr,
        handles_loaded TYPE bal_t_logh.

  desc_filter-option = subobj_filter-option = obj_filter-option = 'EQ'.
  desc_filter-sign = subobj_filter-sign = obj_filter-sign = 'I'.

  obj_filter-low = object.
  APPEND obj_filter TO filter-object.
  subobj_filter-low = subobject.
  APPEND subobj_filter TO filter-subobject.
  IF desc IS SUPPLIED.
    desc_filter-low = desc.
    APPEND desc_filter TO filter-extnumber.
  ENDIF.

  CALL FUNCTION 'BAL_DB_SEARCH'
    EXPORTING
      i_s_log_filter = filter
    IMPORTING
      e_t_log_header = found_headers
    EXCEPTIONS
      log_not_found  = 1.

  IF sy-subrc = 1.
    IF create_if_does_not_exist = abap_true.
      r_log = zcl_logger=>new( object    = object
                               subobject = subobject
                               desc      = desc ).
    ENDIF.
    RETURN.
  ENDIF.

* Delete all but the last row.  Keep the found_headers table this way
* so we can pass it to BAL_DB_LOAD.
  IF lines( found_headers ) > 1.
    DELETE found_headers TO ( lines( found_headers ) - 1 ).
  ENDIF.
  READ TABLE found_headers INDEX 1 INTO most_recent_header.

  CREATE OBJECT r_log.
  r_log->auto_save = abap_true.
  r_log->db_number = most_recent_header-lognumber.
  r_log->handle = most_recent_header-log_handle.

  CALL FUNCTION 'BAL_DB_LOAD'
    EXPORTING
      i_t_log_header = found_headers.

  CALL FUNCTION 'BAL_LOG_HDR_READ'
    EXPORTING
      i_log_handle = r_log->handle
    IMPORTING
      e_s_log      = r_log->header.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER->POPUP
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method POPUP.
* See SBAL_DEMO_04_POPUP for ideas

  DATA: profile TYPE bal_s_prof.
  CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
    IMPORTING
      e_s_display_profile = profile.

  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXPORTING
      i_s_display_profile    = profile
      i_t_log_handle         = me->handle.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER->S
* +-------------------------------------------------------------------------------------------------+
* | [--->] OBJ_TO_LOG                     TYPE        ANY(optional)
* | [--->] CONTEXT                        TYPE        SIMPLE(optional)
* | [--->] CALLBACK_FORM                  TYPE        CSEQUENCE(optional)
* | [--->] CALLBACK_PROG                  TYPE        CSEQUENCE(optional)
* | [--->] CALLBACK_FM                    TYPE        CSEQUENCE(optional)
* | [--->] IMPORTANCE                     TYPE        BALPROBCL(optional)
* | [<-()] SELF                           TYPE REF TO ZCL_LOGGER
* +--------------------------------------------------------------------------------------</SIGNATURE>
method S.
  self = add(
    obj_to_log    = obj_to_log
    context       = context
    callback_form = callback_form
    callback_prog = callback_prog
    callback_fm   = callback_fm
    type          = 'S'
    importance    = importance ).
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER->W
* +-------------------------------------------------------------------------------------------------+
* | [--->] OBJ_TO_LOG                     TYPE        ANY(optional)
* | [--->] CONTEXT                        TYPE        SIMPLE(optional)
* | [--->] CALLBACK_FORM                  TYPE        CSEQUENCE(optional)
* | [--->] CALLBACK_PROG                  TYPE        CSEQUENCE(optional)
* | [--->] CALLBACK_FM                    TYPE        CSEQUENCE(optional)
* | [--->] IMPORTANCE                     TYPE        BALPROBCL(optional)
* | [<-()] SELF                           TYPE REF TO ZCL_LOGGER
* +--------------------------------------------------------------------------------------</SIGNATURE>
method W.
  self = add(
    obj_to_log    = obj_to_log
    context       = context
    callback_form = callback_form
    callback_prog = callback_prog
    callback_fm   = callback_fm
    type          = 'W'
    importance    = importance ).
endmethod.
ENDCLASS.
