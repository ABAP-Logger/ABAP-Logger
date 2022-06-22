interface ZIF_LOGGER
  public .


  data HANDLE type BALLOGHNDL read-only .
  data DB_NUMBER type BALOGNR read-only .
  data HEADER type BAL_S_LOG read-only .

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
      value(SELF) type ref to ZIF_LOGGER .
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
      value(SELF) type ref to ZIF_LOGGER .
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
      value(SELF) type ref to ZIF_LOGGER .
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
      value(SELF) type ref to ZIF_LOGGER .
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
      value(SELF) type ref to ZIF_LOGGER .
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
      value(SELF) type ref to ZIF_LOGGER .
  methods HAS_ERRORS
    returning
      value(RV_YES) type ABAP_BOOL .
  methods HAS_WARNINGS
    returning
      value(RV_YES) type ABAP_BOOL .
  methods IS_EMPTY
    returning
      value(RV_YES) type ABAP_BOOL .
  methods LENGTH
    returning
      value(RV_LENGTH) type I .
  "! Saves the log on demand. Intended to be called at the
  "! end of the log processing so that logs can be saved depending
  "! on other criteria, like the existence of error messages.
  "! If there are no error messages, it may not be desirable to save
  "! a log.
  "! If auto save is enabled, save will do nothing.
  methods SAVE .
  methods EXPORT_TO_TABLE
    returning
      value(RT_BAPIRET) type BAPIRETTAB .
  methods FULLSCREEN .
  methods POPUP
    importing
      !PROFILE type BAL_S_PROF optional .
  methods SET_HEADER
    importing
      !DESCRIPTION type BAL_S_LOG-EXTNUMBER
    returning
      value(SELF) type ref to ZIF_LOGGER .
  methods EXPORT_TO_TABLE_WITH_CONTEXT
    exporting
      !MESS_TAB type STANDARD TABLE .
endinterface.
