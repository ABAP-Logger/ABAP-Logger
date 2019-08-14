"! <p class="shorttext synchronized" lang="en">Logger for XPRA reports - add messages to transport log</p>
class ZCL_LOGGER_XPRA definition
  public
  inheriting from ZCL_LOGGER
  create protected

  global friends ZCL_LOGGER_FACTORY .

public section.

  methods CONSTRUCTOR .
  protected section.
    methods save_log redefinition.

    "! <p class="shorttext synchronized" lang="en">Reformat message from application log for transport log</p>
    "!
    "! @parameter application_log_message | <p class="shorttext synchronized" lang="en">Application log message</p>
    "! @parameter result                  | <p class="shorttext synchronized" lang="en">Transport log message</p>
    methods map_from_application_log_messg importing application_log_message type bal_s_msg
                                           returning value(result)           type sprot_u.
    "! <p class="shorttext synchronized" lang="en">Determine message level from application log message</p>
    "!
    "! @parameter application_log_message | <p class="shorttext synchronized" lang="en">Application log message</p>
    "! @parameter result                  | <p class="shorttext synchronized" lang="en">Transport log message level</p>
    methods get_message_level
      importing
        application_log_message type bal_s_msg
      returning
        value(result)           type sprot_u-level.
    "! <p class="shorttext synchronized" lang="en">Determine message severity from application log message</p>
    "!
    "! @parameter application_log_message | <p class="shorttext synchronized" lang="en">Application log message</p>
    "! @parameter result                  | <p class="shorttext synchronized" lang="en">Transport log message severity</p>
    methods get_message_severity
      importing
        application_log_message type bal_s_msg
      returning
        value(result)           type sprot_u-severity.

  private section.
    "! <p class="shorttext synchronized" lang="en">Application Log problem class: Very important</p>
    constants bal_class_very_important type balprobcl value '1' ##no_text.
    "! <p class="shorttext synchronized" lang="en">Application Log problem class: Important</p>
    constants bal_class_important type balprobcl value '2' ##no_text.
    "! <p class="shorttext synchronized" lang="en">Application Log problem class: Medium</p>
    constants bal_class_medium type balprobcl value '3' ##no_text.
    "! <p class="shorttext synchronized" lang="en">Application Log problem class: Additional information</p>
    constants bal_class_additional_informatn type balprobcl value '4' ##no_text.
    "! <p class="shorttext synchronized" lang="en">Application Log problem class: Other</p>
    constants bal_class_other type balprobcl value ' ' ##no_text.

    "! <p class="shorttext synchronized" lang="en">Application Log message type: Abort</p>
    constants bal_type_abort type symsgty value 'A' ##no_text.
    "! <p class="shorttext synchronized" lang="en">Application Log message type: Error</p>
    constants bal_type_error type symsgty value 'E' ##no_text.
    "! <p class="shorttext synchronized" lang="en">Application Log message type: Warning</p>
    constants bal_type_warning type symsgty value 'W' ##no_text.
    "! <p class="shorttext synchronized" lang="en">Application Log message type: Information</p>
    constants bal_type_information type symsgty value 'I' ##no_text.
    "! <p class="shorttext synchronized" lang="en">Application Log message type: Success</p>
    constants bal_type_success type symsgty value 'S' ##no_text.
ENDCLASS.



CLASS ZCL_LOGGER_XPRA IMPLEMENTATION.


  method constructor.
*   Create an application log (memory only) to store messages until they are saved to the database
    super->constructor( ).
    call function 'BAL_LOG_CREATE'
      exporting
        i_s_log      = zif_logger~header
      importing
        e_log_handle = zif_logger~handle.
    call function 'BAL_LOG_HDR_READ'
      exporting
        i_log_handle = zif_logger~handle
      importing
        e_s_log      = zif_logger~header.
  endmethod.


  method get_message_level.
*   Determine message level from problem class or message type
    "! <p class="shorttext synchronized" lang="en">Statistics level for summary of program results</p>
    constants summary type sprot_u-level value '1' ##no_text.
    "! <p class="shorttext synchronized" lang="en">Error level for logging results with errors</p>
    constants error type sprot_u-level value '2' ##no_text.
    "! <p class="shorttext synchronized" lang="en">Overview level for logging all work steps</p>
    constants overview type sprot_u-level value '3' ##no_text.
    "! <p class="shorttext synchronized" lang="en">Detailed level, additional information for the Hotline</p>
    constants detail type sprot_u-level value '4' ##no_text.


    if application_log_message-probclass = bal_class_very_important
    or application_log_message-probclass = bal_class_important
    or application_log_message-msgty     = bal_type_abort
    or application_log_message-msgty     = bal_type_error.
      result = error.
    elseif application_log_message-probclass = bal_class_medium
        or application_log_message-probclass = bal_class_additional_informatn
        or application_log_message-msgty     = bal_type_warning.
      result = overview.
    elseif application_log_message-msgty = bal_type_information
        or application_log_message-msgty = bal_type_success.
      result = detail.
    else.
      result = summary.
    endif.
  endmethod.


  method get_message_severity.
*   Determine message severity from message type
    "! <p class="shorttext synchronized" lang="en">Cancelled (internal error)</p>
    constants cancelled type sprot_u-severity value 'A' ##no_text.
    "! <p class="shorttext synchronized" lang="en">Fatal error</p>
    constants fatal_error type sprot_u-severity value 'F' ##no_text.
    "! <p class="shorttext synchronized" lang="en">Error (could not execute function)</p>
    constants error type sprot_u-severity value 'E' ##no_text.
    "! <p class="shorttext synchronized" lang="en">Warning</p>
    constants warning type sprot_u-severity value 'W' ##no_text.
    "! <p class="shorttext synchronized" lang="en">Information</p>
    constants information type sprot_u-severity value 'I' ##no_text.
    "! <p class="shorttext synchronized" lang="en">tatus message</p>
    constants status type sprot_u-severity value 'S' ##no_text.
    "! <p class="shorttext synchronized" lang="en">Success (function executed)</p>
    constants success type sprot_u-severity value 'N' ##no_text.
    "! <p class="shorttext synchronized" lang="en">Open (function not executed yet)</p>
    constants open type sprot_u-severity value ' ' ##no_text.

    case application_log_message-msgty.
      when bal_type_abort.
        result = cancelled.
      when bal_type_error.
        result = error.
      when bal_type_warning.
        result = warning.
      when bal_type_information.
        result = information.
      when bal_type_success.
        result = success.
      when others.
        result = open.
    endcase.
  endmethod.


  method map_from_application_log_messg.
*   Convert application log message to transport log message
*   There is no equivalent to the new object indicator in application log

    result-level    = get_message_level( application_log_message ).
    result-severity = get_message_severity( application_log_message ).
    result-langu    = sy-langu.
    result-ag       = application_log_message-msgid.
    result-msgnr    = application_log_message-msgno.
    "result-newobj   = .
    result-var1     = application_log_message-msgv1.
    result-var2     = application_log_message-msgv2.
    result-var3     = application_log_message-msgv3.
    result-var4     = application_log_message-msgv4.
  endmethod.


  method save_log.
    data log_messages type standard table of sprot_u with default key.
    data message_handles type bal_t_msgh.
    data message type bal_s_msg.
    field-symbols <msg_handle> type balmsghndl.

    message_handles = get_message_handles( ).
    loop at message_handles assigning <msg_handle>.
      call function 'BAL_LOG_MSG_READ'
        exporting
          i_s_msg_handle = <msg_handle>
        importing
          e_s_msg        = message
        exceptions
          others         = 3.
      if sy-subrc is initial.
        append map_from_application_log_messg( message ) to log_messages.
      endif.
    endloop.
    call function 'TR_APPEND_LOG'
      tables
        xmsg           = log_messages
      exceptions
        file_not_found = 1
        wrong_call     = 2.
    if sy-subrc = 0.
      call function 'BAL_LOG_MSG_DELETE_ALL'
        exporting
          i_log_handle  = zif_logger~handle
        exceptions
          log_not_found = 0.
    endif.
  endmethod.
ENDCLASS.
