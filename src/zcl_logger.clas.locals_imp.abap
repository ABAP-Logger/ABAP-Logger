CLASS lcl_helper DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS get_bal_message
      IMPORTING
        obj_to_log    TYPE any
      RETURNING
        VALUE(result) TYPE bal_s_msg.

  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_struct_kind,
        syst               TYPE i VALUE 1,
        bapi               TYPE i VALUE 2,
        bdc                TYPE i VALUE 3,
        sprot              TYPE i VALUE 4,
        bapi_alm           TYPE i VALUE 5,
        bapi_meth          TYPE i VALUE 6,
        bapi_status_result TYPE i VALUE 7,
        kw                 TYPE i VALUE 8,
      END OF c_struct_kind.

    CLASS-METHODS get_struct_kind
      IMPORTING
        data_type     TYPE REF TO cl_abap_typedescr
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS map_syst_msg
      IMPORTING
        obj_to_log    TYPE any
      RETURNING
        VALUE(result) TYPE bal_s_msg.

    CLASS-METHODS map_bapi_msg
      IMPORTING
        obj_to_log    TYPE any
      RETURNING
        VALUE(result) TYPE bal_s_msg.

    CLASS-METHODS map_bdc_msg
      IMPORTING
        obj_to_log    TYPE any
      RETURNING
        VALUE(result) TYPE bal_s_msg.

    CLASS-METHODS map_sprot_msg
      IMPORTING
        obj_to_log    TYPE any
      RETURNING
        VALUE(result) TYPE bal_s_msg.

    CLASS-METHODS map_bapi_alm_msg
      IMPORTING
        obj_to_log    TYPE any
      RETURNING
        VALUE(result) TYPE bal_s_msg.

    CLASS-METHODS map_bapi_meth_msg
      IMPORTING
        obj_to_log    TYPE any
      RETURNING
        VALUE(result) TYPE bal_s_msg.

    CLASS-METHODS map_bapi_status_result
      IMPORTING
        obj_to_log    TYPE any
      RETURNING
        VALUE(result) TYPE bal_s_msg.

    CLASS-METHODS map_kw_msg
      IMPORTING
        obj_to_log    TYPE any
      RETURNING
        VALUE(result) TYPE bal_s_msg.

ENDCLASS.

CLASS lcl_helper IMPLEMENTATION.

  METHOD get_bal_message.

    DATA data_type TYPE REF TO cl_abap_typedescr.
    DATA struct_kind TYPE i.

    data_type = cl_abap_typedescr=>describe_by_data( obj_to_log ).

    ASSERT data_type->type_kind = cl_abap_typedescr=>typekind_struct1
        OR data_type->type_kind = cl_abap_typedescr=>typekind_struct2.

    struct_kind = get_struct_kind( data_type ).

    CASE struct_kind.
      WHEN c_struct_kind-syst.
        result = map_syst_msg( obj_to_log ).
      WHEN c_struct_kind-bapi.
        result = map_bapi_msg( obj_to_log ).
      WHEN c_struct_kind-bdc.
        result = map_bdc_msg( obj_to_log ).
      WHEN c_struct_kind-sprot.
        result = map_sprot_msg( obj_to_log ).
      WHEN c_struct_kind-bapi_alm.
        result = map_bapi_alm_msg( obj_to_log ).
      WHEN c_struct_kind-bapi_meth.
        result = map_bapi_meth_msg( obj_to_log ).
      WHEN c_struct_kind-bapi_status_result.
        result = map_bapi_status_result( obj_to_log ).
      WHEN c_struct_kind-kw.
        result = map_kw_msg( obj_to_log ).
      WHEN OTHERS.
        CLEAR result.
    ENDCASE.

  ENDMETHOD.


  METHOD get_struct_kind.

    DATA struct_type       TYPE REF TO cl_abap_structdescr.
    DATA components        TYPE abap_compdescr_tab.
    DATA component         LIKE LINE OF components.
    DATA syst_count        TYPE i.
    DATA bapi_count        TYPE i.
    DATA bdc_count         TYPE i.
    DATA sprot_count       TYPE i.
    DATA bapi_alm_count    TYPE i.
    DATA bapi_meth_count   TYPE i.
    DATA bapi_status_count TYPE i.
    DATA kw_count          TYPE i.

    struct_type ?= data_type.
    components   = struct_type->components.

    " Count number of fields expected for each supported type of message structure
    LOOP AT components INTO component.
      IF 'MSGTY,MSGID,MSGNO,MSGV1,MSGV2,MSGV3,MSGV4,' CS |{ component-name },|.
        syst_count = syst_count + 1.
      ENDIF.
      IF 'TYPE,NUMBER,ID,MESSAGE_V1,MESSAGE_V2,MESSAGE_V3,MESSAGE_V4,' CS |{ component-name },|.
        bapi_count = bapi_count + 1.
      ENDIF.
      IF 'MSGTYP,MSGID,MSGNR,MSGV1,MSGV2,MSGV3,MSGV4,' CS |{ component-name },|.
        bdc_count = bdc_count + 1.
      ENDIF.
      IF 'SEVERITY,AG,MSGNR,VAR1,VAR2,VAR3,VAR4,' CS |{ component-name },|.
        sprot_count = sprot_count + 1.
      ENDIF.
      IF 'TYPE,MESSAGE_ID,MESSAGE_NUMBER,MESSAGE_V1,MESSAGE_V2,MESSAGE_V3,MESSAGE_V4,' CS |{ component-name },|.
        bapi_alm_count = bapi_alm_count + 1.
      ENDIF.
      IF 'METHOD,OBJECT_TYPE,INTERNAL_OBJECT_ID,EXTERNAL_OBJECT_ID,MESSAGE_ID,MESSAGE_NUMBER,MESSAGE_TYPE,MESSAGE_TEXT,' CS |{ component-name },|.
        bapi_meth_count = bapi_meth_count + 1.
      ENDIF.
      IF 'OBJECTKEY,STATUS_ACTION,STATUS_TYPE,MESSAGE_ID,MESSAGE_NUMBER,MESSAGE_TYPE,MESSAGE_TEXT,' CS |{ component-name },|.
        bapi_status_count = bapi_status_count + 1.
      ENDIF.
      IF 'ID,TYPE,NO,V1,V2,V3,V4,' CS |{ component-name },|.
        kw_count = kw_count + 1.
      ENDIF.
    ENDLOOP.

    " Return message type if all expected fields are present
    IF syst_count = 7.
      result = c_struct_kind-syst.
    ELSEIF bapi_count = 7.
      result = c_struct_kind-bapi.
    ELSEIF bdc_count = 7.
      result = c_struct_kind-bdc.
    ELSEIF sprot_count = 7.
      result = c_struct_kind-sprot.
    ELSEIF bapi_alm_count = 7.
      result = c_struct_kind-bapi_alm.
    ELSEIF bapi_meth_count = 8.
      result = c_struct_kind-bapi_meth.
    ELSEIF bapi_status_count = 7.
      result = c_struct_kind-bapi_status_result.
    ELSEIF kw_count = 7.
      result = c_struct_kind-kw.
    ENDIF.

  ENDMETHOD.


  METHOD map_bapi_alm_msg.

    " Avoid using concrete type as certain systems may not have BAPI_ALM_RETURN
    DATA:
      BEGIN OF bapi_alm_message,
        type           TYPE bapi_mtype,
        message_id     TYPE symsgid,
        message_number TYPE symsgno,
        message_v1     TYPE symsgv,
        message_v2     TYPE symsgv,
        message_v3     TYPE symsgv,
        message_v4     TYPE symsgv,
      END OF bapi_alm_message.

    MOVE-CORRESPONDING obj_to_log TO bapi_alm_message.
    result-msgty = bapi_alm_message-type.
    result-msgid = bapi_alm_message-message_id.
    result-msgno = bapi_alm_message-message_number.
    result-msgv1 = bapi_alm_message-message_v1.
    result-msgv2 = bapi_alm_message-message_v2.
    result-msgv3 = bapi_alm_message-message_v3.
    result-msgv4 = bapi_alm_message-message_v4.

  ENDMETHOD.

  METHOD map_bapi_meth_msg.

    " Avoid using concrete type as certain systems may not have BAPI_METH_MESSAGE
    DATA:
      BEGIN OF bapi_meth_message,
        method             TYPE c LENGTH 32, " bapi_method,
        object_type        TYPE c LENGTH 32, " obj_typ,
        internal_object_id TYPE c LENGTH 90, " objidint,
        external_object_id TYPE c LENGTH 90, " objidext,
        message_id         TYPE c LENGTH 20, " bapi_msgid,
        message_number     TYPE msgno,
        message_type       TYPE msgty,
        message_text       TYPE c LENGTH 72, " bapi_text,
      END OF bapi_meth_message.

    MOVE-CORRESPONDING obj_to_log TO bapi_meth_message.
    result-msgty = bapi_meth_message-message_type.
    result-msgid = bapi_meth_message-message_id.
    result-msgno = bapi_meth_message-message_number.

  ENDMETHOD.

  METHOD map_bapi_msg.

    DATA bapi_message TYPE bapiret1.

    MOVE-CORRESPONDING obj_to_log TO bapi_message.
    result-msgty = bapi_message-type.
    result-msgid = bapi_message-id.
    result-msgno = bapi_message-number.
    result-msgv1 = bapi_message-message_v1.
    result-msgv2 = bapi_message-message_v2.
    result-msgv3 = bapi_message-message_v3.
    result-msgv4 = bapi_message-message_v4.

  ENDMETHOD.


  METHOD map_bapi_status_result.

    " Avoid using concrete type as certain systems may not have BAPI_STATUS_RESULT
    DATA:
      BEGIN OF bapi_status_result,
        objectkey      TYPE c LENGTH 90, "  OBJIDEXT,
        status_action  TYPE c LENGTH 1,  "  BAPI_STATUS_ACTION,
        status_type    TYPE c LENGTH 6,  "  BAPI_STATUS_TYPE,
        message_id     TYPE c LENGTH 20, "  BAPI_MSGID,
        message_number TYPE c LENGTH 3,  "  MSGNO,
        message_type   TYPE c LENGTH 1,  "  MSGTY,
        message_text   TYPE c LENGTH 72, "  BAPI_TEXT,
      END OF bapi_status_result.

    MOVE-CORRESPONDING obj_to_log TO bapi_status_result.
    result-msgty = bapi_status_result-message_type.
    result-msgid = bapi_status_result-message_id.
    result-msgno = bapi_status_result-message_number.

  ENDMETHOD.

  METHOD map_bdc_msg.

    DATA bdc_message TYPE bdcmsgcoll.

    MOVE-CORRESPONDING obj_to_log TO bdc_message.
    result-msgty = bdc_message-msgtyp.
    result-msgid = bdc_message-msgid.
    result-msgno = bdc_message-msgnr.
    result-msgv1 = bdc_message-msgv1.
    result-msgv2 = bdc_message-msgv2.
    result-msgv3 = bdc_message-msgv3.
    result-msgv4 = bdc_message-msgv4.

  ENDMETHOD.

  METHOD map_kw_msg.

    DATA kw_message TYPE skwf_error.

    MOVE-CORRESPONDING obj_to_log TO kw_message.
    result-msgty = kw_message-type.
    result-msgid = kw_message-id.
    result-msgno = kw_message-no.
    result-msgv1 = kw_message-v1.
    result-msgv2 = kw_message-v2.
    result-msgv3 = kw_message-v3.
    result-msgv4 = kw_message-v4.

  ENDMETHOD.

  METHOD map_sprot_msg.

    DATA sprot_message TYPE sprot_u.

    MOVE-CORRESPONDING obj_to_log TO sprot_message.
    result-msgty = sprot_message-severity.
    result-msgid = sprot_message-ag.
    result-msgno = sprot_message-msgnr.
    result-msgv1 = sprot_message-var1.
    result-msgv2 = sprot_message-var2.
    result-msgv3 = sprot_message-var3.
    result-msgv4 = sprot_message-var4.

  ENDMETHOD.

  METHOD map_syst_msg.

    DATA syst_message TYPE symsg.

    MOVE-CORRESPONDING obj_to_log TO syst_message.
    MOVE-CORRESPONDING syst_message TO result.

  ENDMETHOD.

ENDCLASS.
