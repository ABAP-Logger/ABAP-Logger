interface zif_logger
  public .
  methods add
    importing
      obj_to_log    type any optional
      context       type simple optional
      callback_form type csequence optional
      callback_prog type csequence optional
      callback_fm   type csequence optional
      type          type symsgty optional
      importance    type balprobcl optional
        preferred parameter obj_to_log
    returning
      value(self)   type ref to zcl_logger .
  methods a
    importing
      obj_to_log    type any optional
      context       type simple optional
      callback_form type csequence optional
      callback_prog type csequence optional
      callback_fm   type csequence optional
      importance    type balprobcl optional
        preferred parameter obj_to_log
    returning
      value(self)   type ref to zcl_logger .
  methods e
    importing
      obj_to_log    type any optional
      context       type simple optional
      callback_form type csequence optional
      callback_prog type csequence optional
      callback_fm   type csequence optional
      importance    type balprobcl optional
        preferred parameter obj_to_log
    returning
      value(self)   type ref to zcl_logger .
  methods w
    importing
      obj_to_log    type any optional
      context       type simple optional
      callback_form type csequence optional
      callback_prog type csequence optional
      callback_fm   type csequence optional
      importance    type balprobcl optional
        preferred parameter obj_to_log
    returning
      value(self)   type ref to zcl_logger .
  methods i
    importing
      obj_to_log    type any optional
      context       type simple optional
      callback_form type csequence optional
      callback_prog type csequence optional
      callback_fm   type csequence optional
      importance    type balprobcl optional
        preferred parameter obj_to_log
    returning
      value(self)   type ref to zcl_logger .
  methods s
    importing
      obj_to_log    type any optional
      context       type simple optional
      callback_form type csequence optional
      callback_prog type csequence optional
      callback_fm   type csequence optional
      importance    type balprobcl optional
        preferred parameter obj_to_log
    returning
      value(self)   type ref to zcl_logger .
  methods save .
  methods get_autosave
    returning
      value(auto_save) type abap_bool .
  methods set_autosave
    importing
      auto_save type abap_bool .
  methods export_to_table
    returning
      value(rt_bapiret) type bapirettab .
  methods fullscreen .
  methods popup .

endinterface.
