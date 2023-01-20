class zcl_part_conn_change_email_vz definition public final create public.

  public section.
    interfaces if_badi_interface .
    interfaces if_edoc_part_conn_change_email .

endclass.

class zcl_part_conn_change_email_vz implementation.

  method if_edoc_part_conn_change_email~change_email_to_customer.

    emailrecipientsto = value #(
      ( 'rafael.vaz@sap.com' )
      ( 'j.bellini@sap.com' )
      ( 'yasmin.elkfury@sap.com' )
    ).

  endmethod.

endclass.
