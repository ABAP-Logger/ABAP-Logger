"#autoformat (Pretty Print class automatically with abapCI plug-in)
"! <p class="shorttext synchronized" lang="en">Logger interface - transport log</p>
interface zif_logger_xpra
  public .
  interfaces zif_logger.

  constants:
    "! <p class="shorttext synchronized" lang="en">Level</p>
    begin of cs_level,
      "! <p class="shorttext synchronized" lang="en">Statistics level for summary of program results</p>
      summary  type sprot_u-level value '1' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Error level for logging results with errors</p>
      error    type sprot_u-level value '2' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Overview level for logging all work steps</p>
      overview type sprot_u-level value '3' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Detailed level, additional information for the Hotline</p>
      detail   type sprot_u-level value '4' ##no_text,
    end of cs_level.
  constants:
    "! <p class="shorttext synchronized" lang="en">Severity</p>
    begin of cs_severity,
      "! <p class="shorttext synchronized" lang="en">Cancelled (internal error)</p>
      cancelled   type sprot_u-severity value 'A' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Fatal error</p>
      fatal_error type sprot_u-severity value 'F' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Error (could not execute function)</p>
      error       type sprot_u-severity value 'E' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Warning</p>
      warning     type sprot_u-severity value 'W' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Information</p>
      information type sprot_u-severity value 'I' ##no_text,
      "! <p class="shorttext synchronized" lang="en">tatus message</p>
      status      type sprot_u-severity value 'S' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Success (function executed)</p>
      success     type sprot_u-severity value 'N' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Open (function not executed yet)</p>
      open        type sprot_u-severity value ' ' ##no_text,
    end of cs_severity.

  "! <p class="shorttext synchronized" lang="en">The next message will start a new section</p>
  "!
  "! @parameter ro_self | <p class="shorttext synchronized" lang="en"></p>
  methods start_new_section default ignore
    returning
      value(ro_self) type ref to zif_logger_xpra.

endinterface.
