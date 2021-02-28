class lcx_t100 definition inheriting from cx_static_check.
  public section.
    interfaces if_t100_message.
    methods constructor importing previous like previous optional
                                  id       type symsgid
                                  no       type symsgno
                                  msgv1    type symsgv optional
                                  msgv2    type symsgv optional
                                  msgv3    type symsgv optional
                                  msgv4    type symsgv optional.
    data msgv1 type symsgv.
    data msgv2 type symsgv.
    data msgv3 type symsgv.
    data msgv4 type symsgv.
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
