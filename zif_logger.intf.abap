interface zif_logger
  public .
  data handle type balloghndl read-only .
  data db_number type balognr read-only .
  data header type bal_s_log read-only .

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
      value(self)   type ref to zif_logger .

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
      value(self)   type ref to zif_logger .

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
      value(self)   type ref to zif_logger .

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
      value(self)   type ref to zif_logger .

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
      value(self)   type ref to zif_logger .

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
      value(self)   type ref to zif_logger .

  methods has_errors
    returning
      value(rv_yes) type abap_bool .

  methods has_warnings
    returning
      value(rv_yes) type abap_bool .

  methods is_empty
    returning
      value(rv_yes) type abap_bool .

  methods length
    returning
      value(rv_length) type i .

  "! Saves the log on demand. Intended to be called at the
  "! end of the log processing so that logs can be saved depending
  "! on other criteria, like the existence of error messages.
  "! If there are no error messages, it may not be desirable to save
  "! a log.
  "! If auto save is enabled, save will do nothing.
  methods save .

  methods export_to_table
    returning
      value(rt_bapiret) type bapirettab .

  methods fullscreen .

  methods popup
    importing
      profile type bal_s_prof optional.

endinterface.
