"! <p class="shorttext synchronized" lang="en">Dummy logger which discards all input</p>
class zcl_logger_null_device definition 
  public
  final
  create private
  global friends zcl_logger_factory.

  public section.
    interfaces zif_logger.

    methods constructor.

  protected section.
  private section.
endclass.


class zcl_logger_null_device implementation.
  method constructor.
    zif_logger~handle = '/dev/null'.
    zif_logger~db_number = '/dev/null'.
    zif_logger~header-extnumber = '/dev/null'.
  endmethod.


  method zif_logger~a.
    self = me.
  endmethod.


  method zif_logger~add ##needed.
    self = me.
  endmethod.


  method zif_logger~e ##needed.
    self = me.
  endmethod.


  method zif_logger~export_to_table ##needed.
  endmethod.


  method zif_logger~fullscreen ##needed.
  endmethod.


  method zif_logger~has_errors  ##needed.
    rv_yes = abap_false.
  endmethod.


  method zif_logger~has_warnings ##needed.
    rv_yes = abap_false.
  endmethod.


  method zif_logger~i ##needed.
    self = me.
  endmethod.


  method zif_logger~is_empty ##needed.
    rv_yes = abap_true.
  endmethod.


  method zif_logger~length ##needed.
    rv_length = 0.
  endmethod.


  method zif_logger~popup ##needed.
  endmethod.


  method zif_logger~s ##needed.
    self = me.
  endmethod.


  method zif_logger~save ##needed.
  endmethod.


  method zif_logger~w ##needed.
    self = me.
  endmethod.
endclass.
