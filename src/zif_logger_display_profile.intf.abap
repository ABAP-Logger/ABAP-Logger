interface ZIF_LOGGER_DISPLAY_PROFILE
  public .


  methods SET
    importing
      !I_DETLEVEL type CLIKE optional
      !I_NO_TREE type CLIKE optional
      !I_POPUP type CLIKE optional
      !I_SINGLE_LOG type CLIKE optional
      !I_STANDARD type CLIKE default ABAP_TRUE
    returning
      value(R_SELF) type ref to ZIF_LOGGER_DISPLAY_PROFILE .
  methods GET
    returning
      value(R_DISPLAY_PROFILE) type BAL_S_PROF .
  methods SET_GRID
    importing
      !I_GRID_MODE type CLIKE
    returning
      value(R_SELF) type ref to ZIF_LOGGER_DISPLAY_PROFILE .
  methods SET_VALUE
    importing
      !I_FLD type CLIKE
      !I_VAL type ANY
    returning
      value(R_SELF) type ref to ZIF_LOGGER_DISPLAY_PROFILE .
  methods SET_CONTEXT_TREE
    importing
      !I_CONTEXT_STRUCTURE type CLIKE
      !I_UNDER_LOG type CLIKE default SPACE .
  methods SET_CONTEXT_MESSAGE
    importing
      !I_CONTEXT_STRUCTURE type CLIKE .
endinterface.
