INTERFACE zif_logger_deprecated
  PUBLIC.
  METHODS a
    IMPORTING obj_to_log          TYPE any       DEFAULT sy
              !context            TYPE any       OPTIONAL
              callback_form       TYPE csequence OPTIONAL
              callback_prog       TYPE csequence OPTIONAL
              callback_fm         TYPE csequence OPTIONAL
              callback_parameters TYPE bal_t_par OPTIONAL
              importance          TYPE balprobcl OPTIONAL
              detlevel            TYPE ballevel  OPTIONAL
        PREFERRED PARAMETER obj_to_log
    RETURNING VALUE(self)         TYPE REF TO zif_logger.

  METHODS e
    IMPORTING obj_to_log          TYPE any       DEFAULT sy
              !context            TYPE any       OPTIONAL
              callback_form       TYPE csequence OPTIONAL
              callback_prog       TYPE csequence OPTIONAL
              callback_fm         TYPE csequence OPTIONAL
              callback_parameters TYPE bal_t_par OPTIONAL
              importance          TYPE balprobcl OPTIONAL
              detlevel            TYPE ballevel  OPTIONAL
        PREFERRED PARAMETER obj_to_log
    RETURNING VALUE(self)         TYPE REF TO zif_logger.

  METHODS w
    IMPORTING obj_to_log          TYPE any       DEFAULT sy
              !context            TYPE any       OPTIONAL
              callback_form       TYPE csequence OPTIONAL
              callback_prog       TYPE csequence OPTIONAL
              callback_fm         TYPE csequence OPTIONAL
              callback_parameters TYPE bal_t_par OPTIONAL
              importance          TYPE balprobcl OPTIONAL
              detlevel            TYPE ballevel  OPTIONAL
        PREFERRED PARAMETER obj_to_log
    RETURNING VALUE(self)         TYPE REF TO zif_logger.

  METHODS i
    IMPORTING obj_to_log          TYPE any       DEFAULT sy
              !context            TYPE any       OPTIONAL
              callback_form       TYPE csequence OPTIONAL
              callback_prog       TYPE csequence OPTIONAL
              callback_fm         TYPE csequence OPTIONAL
              callback_parameters TYPE bal_t_par OPTIONAL
              importance          TYPE balprobcl OPTIONAL
              detlevel            TYPE ballevel  OPTIONAL
        PREFERRED PARAMETER obj_to_log
    RETURNING VALUE(self)         TYPE REF TO zif_logger.

  METHODS s
    IMPORTING obj_to_log          TYPE any       DEFAULT sy
              !context            TYPE any       OPTIONAL
              callback_form       TYPE csequence OPTIONAL
              callback_prog       TYPE csequence OPTIONAL
              callback_fm         TYPE csequence OPTIONAL
              callback_parameters TYPE bal_t_par OPTIONAL
              importance          TYPE balprobcl OPTIONAL
              detlevel            TYPE ballevel  OPTIONAL
        PREFERRED PARAMETER obj_to_log
    RETURNING VALUE(self)         TYPE REF TO zif_logger.
ENDINTERFACE.
