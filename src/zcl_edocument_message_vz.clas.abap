class zcl_edocument_message_vz definition public final create public.

  public section.
    interfaces if_badi_interface .
    interfaces if_edocument_message_ext .

endclass.


class zcl_edocument_message_vz implementation.

  method if_edocument_message_ext~determine_company_code.
  endmethod.


  method if_edocument_message_ext~determine_edoc_type.
    rv_edoc_type = 'ZVZINCOMIN'.
  endmethod.


  method if_edocument_message_ext~determine_file_type.
  endmethod.


  method if_edocument_message_ext~determine_operation.
    rv_operation = 'C'. "Create
  endmethod.


  method if_edocument_message_ext~determine_partner.
  endmethod.


  method if_edocument_message_ext~determine_process_attributes.
  endmethod.

endclass.
