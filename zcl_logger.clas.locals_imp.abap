class lcx_t100 definition inheriting from cx_static_check.
  public section.
    interfaces if_t100_message.
    methods constructor importing previous like previous optional
                                  id       type symsgid
                                  no       type symsgno
                                  msgv1    type syst_msgv optional
                                  msgv2    type syst_msgv optional
                                  msgv3    type syst_msgv optional
                                  msgv4    type syst_msgv optional.
    data msgv1 type syst_msgv.
    data msgv2 type syst_msgv.
    data msgv3 type syst_msgv.
    data msgv4 type syst_msgv.
endclass.

class lcx_t100 implementation.
  method constructor.
    super->constructor( previous = previous ).
    me->msgv1                     = msgv1.
    me->msgv2                     = msgv2.
    me->msgv3                     = msgv3.
    me->msgv4                     = msgv4.
    if_t100_message~t100key-msgid = id.
    if_t100_message~t100key-msgno = no.
    if_t100_message~t100key-attr1 = 'MSGV1'.
    if_t100_message~t100key-attr2 = 'MSGV2'.
    if_t100_message~t100key-attr3 = 'MSGV3'.
    if_t100_message~t100key-attr4 = 'MSGV4'.
  endmethod.
endclass.
