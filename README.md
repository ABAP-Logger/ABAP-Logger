ABAP-Logger
===========
ABAP Logging as painless as any other language

## Functionality
The class `ZCL_LOGGER` is designed to make logging in ABAP more like in other languages.  To create a log and add messages in Android Java, Ruby, and Javascript, at most two lines of code are required: one to create the log and one to add the message.  In addition, different types of objects can be logged by passing them to the same method.  `ZCL_LOGGER` also has these strengths.

An instance of this class can be passed a number of objects by different methods.  Method `ADD` accepts a string, bapiret2, bdcmsgcoll, exception object, or a table of any of those data types.  Methods A, E, W, I and S accept exactly the same but they add the type of message corresponding to their names.

## Examples

### Creating a Log in SLG1
    DATA: log TYPE REF TO zcl_logger.
    * If you create a log without any parameters, it will only be in memory
    log = zcl_logger=>new( ).
    
    * If you supply an object and subobject, the log will be created on the
    * database the first time a message is stored, so you can view it later
    * in SLG1.
    log = zcl_logger=>new(  object = 'WF'
                            subobject = 'NOTIFICATIONS'
                            desc = |Notifications on { sy-datum }| ). 
### Logging Strings
    DATA: log TYPE REF TO zcl_logger.
    log = zcl_logger=>new( ).
    log->s( 'This is a success message' ).
    log->w( 'This is a warning message' ). 

### Logging Errors
    DATA: log TYPE REF TO zcl_logger,
          l_err TYPE REF TO zcx_operation_failed.
    log = zcl_logger=>new( object = 'WF' subobject = 'NOTIFICATIONS' ).
    TRY.
        my_class=>do_some_operation( ).
      CATCH zcx_operation_failed INTO l_err.
        log->e( l_err ).
    ENDTRY. 

### Logging BAPI Messages
    DATA: rtn_msgs TYPE TABLE OF bapiret2.
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        parameter1 = foo
        parameter2 = bar
      TABLES
        return = rtn_msgs.
    IF rtn_msgs IS NOT INITIAL.
      log = zcl_logger=>new( object = 'ACCOUNTING' subobject = 'INTERFACES' ).
      log->add( rtn_msgs ).
    ENDIF. 

### Displaying a log
To display a log immediately to a user, just call `log->popup( ).` or `log->fullscreen( ).`

### Notes
Calls to the log can be chained, like:

    log->e( 'An error occurred. See following:' )->e( l_err ).

### Contributing
Any change is welcome as long as you add some corresponding unit tests.  You can run them and they will test the class in a number of different scenarios.  You can set breakpoints before running unit tests to see the behavior in action.
