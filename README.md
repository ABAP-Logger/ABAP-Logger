# ZCL_LOGGER

SAP Logging as painless as any other language

## One of these things is not like the others

One of the most effective ways of debugging and inspecting a program's operations is by writing to a log.  Logging is so powerful, just about every language has some way of doing it.  For example, if you want to write to a log in javascript, you just need to write the following line of code:

    console.log("Leapin' lizards, something went wrong!");

Or how about in Ruby? Ruby has the added benefit of different levels of messages.

    require 'logger'
    log = Logger.new('logfile.log')
    log.warn("You're on thin ice, bud.")
    log.info("Things are normal again.")

Writing an Android app? You're in luck - you can log and optionally tag messages, all in one line of Java.

    Log.e('MAPS_INTERFACE', 'The system is down!!!');

So, how does ABAP logging stack up to the other languages? What code is required to log a string to be viewed later?

    DATA: header TYPE bal_s_log,
          handle TYPE balloghndl,
          handles_to_save TYPE bal_t_logh.
    
    header-object = 'ZINTERFACES'.
    header-subobject = 'ACCOUNTING'.
    header-extnumber = 'Stuff imported from legacy systems'.
    
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = header
      IMPORTING
        e_log_handle = handle.
    
    CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
      EXPORTING
        i_log_handle = handle
        i_msgty = 'E'
        i_text = 'You see, what had happened was...'.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle = handles_to_save.

If you're not asleep after writing all that, then you've at least forgot what you were programming before you had to write to a log.  If anything, logging should be QUICK so you can get on with the real programming!

## Get out of my way

A better log would barely interrupt my code, so I can output messages in one line, and you don't lose the big picture as you are reading it. What do you wish the ABAP example above looked like?  How about this:

    DATA: log TYPE REF TO zcl_logger.
    log = zcl_logger=>new( object = 'ZINTERFACES'
                           subobject = 'ACCOUNTING'
                           desc = 'Stuff imported from legacy systems' ).
    log->e( 'You see, what had happened was...' ).

All the information, none of the boilerplate. This is the power of ZCL_LOGGER.

## Log anything

Making use of SAP's run-time type services, we can pass almost anything we might want to log to an instance of ZCL_LOGGER, and it will do the heavy lifting.

Log a string!

    log->s( 'Document 4800095710 created successfully' ).

Log a bapi return message!

    DATA: rtn TYPE bapiret2.
    log->add( rtn ).

Log a...gasp...TABLE of bapi return messages!

    DATA: msgs TYPE TABLE OF bapiret2.
    log->add( msgs ).

Log a HR error messages

    DATA: hr_msg TYPE HRPAD_MESSAGE .
    log->add( hr_msg ).
    
Log an exception? Yep!

    TRY.
        rubber_band_powered_spaceship=>fly_to( the_moon ).
      CATCH zcx_not_enough_power INTO err.
        log->e( err ).
    ENDTRY.

Log the current system message.

    MESSAGE e001(oo) WITH foo bar baz INTO dummy.
    log->add( ). "you don't even need to pass anything in, bro.

Log the return of a BDC call.

    CALL TRANSACTION 'CO07' USING bdc_tab MESSAGES INTO bdc_messages.
    log->add( bdc_messages ).

And that's every scenario I've been able to think of, so far.

## Don't Ignore SAP's Strengths

As frustrating as it can sometimes be, the SAP environment has a lot of power, and it would be good not to ignore it.  Transaction code SLG1 views and filters logs with ease, and it allows for added context variables and parameters. A new logger class should not reinvent the wheel, the wheelbarrow, or the mechanisms for saving and displaying logs.

If you have an instance of a log object, you can add context variables and a problem class, two of a few things that SLG1 handles well.

    log->w( obj_to_log = 'Document created with errors' "<-- Which document? Needs context.
            context = new_document ). "<-- Here's the context

Since this log is designed to be simple, it has compromised a lot of the more exotic function modules in the SBAL family. If your log needs to use one of these function modules, the log header, handle and database id are all read-only members of the class, so you can pass them right along to the function module.

    log->i( 'Results of system analysis' ).

    CALL FUNCTION 'BAL_LOG_MSG_CUMULATE'
      EXPORTING
        i_log_handle = log->handle
        i_s_msg = l_msg
        i_compare_attributes = abap_true.

## Chainable

It's 2014, so yes, you can chain method calls.

    zcl_logger=>new( object = 'foo' )->e( 'Bad things happened: See details' )->e( error ).

## Installation

1. Create the Subobject "LOGGER" for Object "ABAPUNIT" in transaction code SLG0.
2. Either copy-paste the source code and unit tests into your system, or use SAPLink to import the slinky file.

## Contributing

This tool has a lot of room for improvement, so if you have an idea, let me know.  Better yet, submit a pull request, and I'll merge it as long as it has good tests and makes sense.
