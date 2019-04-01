*"* use this source file for your ABAP unit test classes

class lcl_test definition for testing
  duration short
  risk level harmless.

  private section.
    constants c_bal_object type string value 'ABAPUNIT' ##no_text.
    constants c_bal_subobject type string value 'ZCL_LOGGER' ##no_text.

endclass.
