CLASS lcx_t100 DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    INTERFACES if_t100_message.
    METHODS constructor IMPORTING previous LIKE previous OPTIONAL
                                  id       TYPE symsgid
                                  no       TYPE symsgno
                                  msgv1    TYPE symsgv OPTIONAL
                                  msgv2    TYPE symsgv OPTIONAL
                                  msgv3    TYPE symsgv OPTIONAL
                                  msgv4    TYPE symsgv OPTIONAL.
    DATA msgv1 TYPE symsgv.
    DATA msgv2 TYPE symsgv.
    DATA msgv3 TYPE symsgv.
    DATA msgv4 TYPE symsgv.
ENDCLASS.

CLASS lcx_t100 IMPLEMENTATION.
  METHOD constructor.
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
  ENDMETHOD.
ENDCLASS.
