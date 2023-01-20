class zcl_edoc_map_vz definition public final create public.

  public section.
    types:
      begin of ms_pull_req,
        edoc_incoming_message_tab type if_edoc_ext_datatypes=>mty_edoc_incom_msg_t,
        bukrs                     type bukrs,
        vat_id(9)                 type c,
      end of ms_pull_req.

    class-methods call_proxy_sd_service
      importing
        !is_source       type any
        !iv_logical_port type prx_logical_port_name
      exporting
        !ed_response     type ref to data
        !ev_xstring      type xstring.

    class-methods call_proxy
      importing
        !is_source       type any
        !iv_logical_port type prx_logical_port_name
      exporting
        !ed_response     type ref to data
        !ev_xstring      type xstring .

    class-methods call_status_proxy
      importing
        !is_source       type any
        !iv_logical_port type prx_logical_port_name
      exporting
        !ed_response     type ref to data
        !ev_xstring      type xstring .

    class-methods call_get_invocie_proxy
      importing
        !is_source   type any
      exporting
        !ed_response type ref to data
        !ev_xstring  type xstring .


    data ms_source type if_edoc_ext_datatypes=>mty_mapping_source_s .

    methods map_invoice_request_fi
      importing
        !is_source type any
      exporting
        !ed_target type ref to data .

    methods map_invoice_response_fi
      importing
        !is_source type any
      exporting
        !ed_target type ref to data .

    methods map_inno_request_fi
      importing
        !is_source type any
      exporting
        !ed_target type ref to data.

    methods map_inno_response_fi
      importing
        !is_source type any
      exporting
        !ed_target type ref to data.

    methods map_status_request
      importing
        !is_source type any
      exporting
        !ed_target type ref to data .

    methods map_status_response
      importing
        !is_source type any
      exporting
        !ed_target type ref to data .

    methods map_pull_request
      importing
        !is_source type any
      exporting
        !ed_target type ref to data .

    methods map_pull_response
      importing
        !is_source type any
      exporting
        !ed_target type ref to data .

endclass.

class zcl_edoc_map_vz implementation.

  method map_invoice_request_fi.
    data ld_target type ref to zedo_us_send_invoice_request.
    data ls_source type if_edoc_ext_datatypes=>mty_mapping_source_s.
    data ls_bseg like line of ls_source-bseg.
    data lv_amount like ls_bseg-wrbtr.

    field-symbols <ls_target> type zedo_us_send_invoice_request.
    create data ld_target.
    assign ld_target->* to <ls_target>.

    ls_source = is_source.

    <ls_target>-parameters-uuid = ls_source-reference-edoc_guid.
    <ls_target>-parameters-seller_id = ls_source-bkpf-bukrs.
    <ls_target>-parameters-invoice_id = ls_source-bkpf-belnr.
    <ls_target>-parameters-invoice_issue_date = ls_source-bkpf-budat.

    read table ls_source-bseg into ls_bseg with key koart = 'D'.
    <ls_target>-parameters-invoice_amount = ls_bseg-wrbtr.
    <ls_target>-parameters-buyer_id = ls_bseg-kunnr.
    ed_target = ld_target.
  endmethod.

  method call_proxy.
    data lo_proxy type ref to zedo_us_co_e_doc_usexensibilit.
    data ls_input type zedo_us_send_invoice_request.
    data ld_response type ref to zedo_us_send_invoice_response1.
    field-symbols <ls_output> type zedo_us_send_invoice_response1.
    data ls_response type zedo_us_send_invoice_response1.

    create data ld_response.
    assign ld_response->* to <ls_output>.

    ls_input = is_source.

    try.
        data(destination) = cl_soap_destination_provider=>create_by_comm_arrangement(
                              comm_scenario  = 'ZEDO_US_INVOICE'
                              service_id     = 'ZEDO_US_SEND_INVOICE_SPRX'
*                          comm_system_id = '<Communication System Identifier>'
                            ).
        data(proxy) = new zedo_us_co_e_doc_usexensibilit(
                        destination = destination
                      ).
        data(request) = value zedo_us_send_invoice_request( ).
        move-corresponding ls_input to request.
        proxy->send_invoice(
          exporting
            input = request
          importing
            output = <ls_output>
        ).
        "handle response
      catch cx_soap_destination_error.
        "handle error
      catch cx_ai_system_fault.
        "handle error
    endtry.
    call transformation zedo_us_send_invoice_request source root = ls_input
                            result xml ev_xstring.
    ed_response = ld_response.
  endmethod.

  method map_invoice_response_fi.
    data ld_target type ref to zif_edo_datatypes_us=>mty_mapping_target_us_s.
    data ls_source type zedo_us_send_invoice_response1.

    field-symbols <ls_target> type zif_edo_datatypes_us=>mty_mapping_target_us_s.

    create data ld_target.
    assign ld_target->* to <ls_target>.
    ls_source = is_source.

*    Prepare response XML
    call transformation zedo_us_send_invoice_response1 source root = ls_source
                          result xml data(lv_xml).

    <ls_target>-response_xml = lv_xml.


    <ls_target>-response_id = ls_source-parameters-tax_authority_id.
    if ls_source-parameters-tax_authority_id is not initial.
      <ls_target>-status = 'Success'.
      <ls_target>-track_id = ls_source-parameters-tax_authority_id.
    endif.

    ed_target = ld_target.
  endmethod.

  method map_inno_request_fi.
    data: ld_target    type ref to zvz_testzvz_fm_service,
          ls_source    type if_edoc_ext_datatypes=>mty_mapping_source_s,
          type_request type c.

    field-symbols <ls_target> type zvz_testzvz_fm_service.
    create data ld_target.
    assign ld_target->* to <ls_target>.

    ls_source = is_source.

    if ls_source-source_header-source_type = 'FI_INVOICE'. "FI_INVOICE
      type_request = 'P'.
    elseif ls_source-source_header-source_type = 'Billing'.
      type_request = 'B'.
    endif.

    <ls_target>-i_input_param1 = type_request.
    ed_target = ld_target.
  endmethod.

  method call_proxy_sd_service.
    field-symbols <ls_output> type zvz_testzvz_fm_serviceresponse.
    data: ld_response type ref to zvz_testzvz_fm_serviceresponse,
          ls_response type zvz_testzvz_fm_serviceresponse,
          lo_proxy type ref to zvz_testco_zvz_sd_test,
          ls_input type zvz_testzvz_fm_service.

    create data ld_response.
    assign ld_response->* to <ls_output>.

    ls_input = is_source.

    try.
      data(destination) = cl_soap_destination_provider=>create_by_comm_arrangement(
                              comm_scenario  = 'ZVZ_CS_TEST'
                              service_id     = 'ZVZ_SCM_TEST_SPRX'
*                              comm_system_id = '<Communication System Identifier>'
                            ).
        data(proxy) = new zvz_testco_zvz_sd_test(
                        destination = destination
                      ).
        data(request) = value zvz_testzvz_fm_service( ).
        move-corresponding ls_input to request.
        proxy->zvz_fm_service(
          exporting
            input = request
          importing
            output = <ls_output>
        ).
        "handle response
      catch cx_soap_destination_error.
        "handle error
      catch cx_ai_system_fault.
        "handle error
    endtry.

*    call transformation zvz_testzvz_fm_service source root = ls_input
*                          result xml ev_xstring.
    ed_response = ld_response.
  endmethod.

  method map_inno_response_fi.
    data ld_target type ref to zif_edo_datatypes_us=>mty_mapping_target_us_s.
    data ls_source type zvz_testzvz_fm_serviceresponse.

    field-symbols <ls_target> type zif_edo_datatypes_us=>mty_mapping_target_us_s.

    create data ld_target.
    assign ld_target->* to <ls_target>.
    ls_source = is_source.

*    Prepare response XML
*    call transformation zedo_us_send_invoice_response1 source root = ls_source
*                          result xml data(lv_xml).

*    <ls_target>-response_xml = lv_xml.


    <ls_target>-response_id = ls_source-e_output.
    if ls_source-e_output is not initial.
      <ls_target>-status = 'Success'.
      <ls_target>-track_id = ls_source-e_output.
    endif.

    ed_target = ld_target.
  endmethod.

  method map_status_request.
    data ld_target type ref to zedo_us_get_status_request.
    data ls_source type if_edoc_ext_datatypes=>mty_edocument_s.
*    DATA ls_zedous TYPE zedous.
    field-symbols <ls_target> type zedo_us_get_status_request.
    create data ld_target.
    assign ld_target->* to <ls_target>.

    ls_source = is_source.

    select single * from zvzedoc_cust2 where edoc_guid = @ls_source-edoc_guid into @data(customer_table).

    <ls_target>-parameters-track_id = customer_table-trackid.
    ed_target = ld_target.
  endmethod.

  method call_status_proxy.
    data lo_proxy type ref to zedo_us_co_e_doc_usget_status.
    data ls_input type zedo_us_get_status_request.
    data ld_response type ref to zedo_us_get_status_response.
    field-symbols <ls_output> type zedo_us_get_status_response.
    data ls_response type zedo_us_get_status_response.

    create data ld_response.
    assign ld_response->* to <ls_output>.

    ls_input = is_source.

    try.
        data(destination) = cl_soap_destination_provider=>create_by_comm_arrangement(
                              comm_scenario  = 'ZEDO_US_INVOICE'
                              service_id     = 'ZEDO_US_GET_STATUS_SPRX'
*                          comm_system_id = '<Communication System Identifier>'
                            ).
        data(proxy) = new zedo_us_co_e_doc_usget_status(
                        destination = destination
                      ).

        data(request) = value zedo_us_get_status_request( ).
        move-corresponding ls_input to request.
        proxy->get_status(
          exporting
            input = request
          importing
            output = <ls_output>
        ).
        "handle response
      catch cx_soap_destination_error.
        "handle error
      catch cx_ai_system_fault.
        "handle error
    endtry.
*    CALL TRANSFORMATION zedo_us_send_invoice_request SOURCE root = ls_input
*                            RESULT XML ev_xstring.
    ed_response = ld_response.
  endmethod.

  method map_status_response.
    data ld_target type ref to zif_edo_datatypes_us=>mty_mapping_target_us_s.
    data ls_source type zedo_us_get_status_response.

    field-symbols <ls_target> type zif_edo_datatypes_us=>mty_mapping_target_us_s.

    create data ld_target.
    assign ld_target->* to <ls_target>.
    ls_source = is_source.

*    Prepare response XML
*     CALL TRANSFORMATION ZEDO_US_SEND_INVOICE_RESPONSE1 SOURCE ROOT = ls_source
*                           RESULT XML DATA(lv_xml).

*      <ls_target>-response_xml = lv_xml.



    try.
        <ls_target>-response_xml = cl_edoc_util_ext=>convert_ddic_to_xml_xstring(
          exporting iv_abap_data =  ls_source
                    iv_ddic_type = 'ZEDO_US_GET_STATUS_RESPONSE' ).
      catch cx_edocument_ext.
        "handle exception
    endtry.
    <ls_target>-status = ls_source-parameters-status.
    ed_target = ld_target.
  endmethod.

  method map_pull_request.

    data:
      ls_source type if_edoc_ext_datatypes=>mty_edoc_s_bukrs_s,
      ld_target type ref to zedo_us_get_invoice_request.

    field-symbols: <fs_target> type zedo_us_get_invoice_request.

    create data ld_target.
    assign ld_target->* to <fs_target>.

    ls_source = is_source.

**** Mapping External target pull request
    <fs_target>-parameters-seller_id = ls_source-low.

    ed_target = ld_target.

  endmethod.

  method call_get_invocie_proxy.

    data ls_request type zedo_us_get_invoice_request.
    data ld_response type ref to zedo_us_get_invoice_response.
    field-symbols <ls_output> type zedo_us_get_invoice_response.
    data ls_response type zedo_us_get_status_response.

    create data ld_response.
    assign ld_response->* to <ls_output>.

    ls_request = is_source.

    try.
        data(destination) = cl_soap_destination_provider=>create_by_comm_arrangement(
                              comm_scenario  = 'ZEDO_US_INVOICE'
                              service_id     = 'ZEDO_US_GET_INVOICE_SPRX'
                            ).
        data(proxy) = new zedo_us_co_e_doc_usget_invoice(
                        destination = destination
                      ).
        data(request) = value zedo_us_get_invoice_request( ).
        proxy->get_invoice(
          exporting
            input = ls_request
          importing
            output = <ls_output>
        ).
        "handle response
      catch cx_soap_destination_error.
        "handle error
      catch cx_ai_system_fault.
        "handle error
    endtry.

    ed_response = ld_response.

  endmethod.

  method map_pull_response.

    data:
      ls_source  type zedo_us_get_invoice_response,
      ld_target  type ref to ms_pull_req,
      lv_xml     type xstring,
      lv_inv_xml type xstring,
      ls_msg     type if_edoc_ext_datatypes=>mty_edoc_incom_msg_s.

    field-symbols:
                   <fs_target> type  ms_pull_req.

*   Set source object
    ls_source = is_source.
    create data ld_target.
    assign ld_target->* to <fs_target>.

**** Mapping External target pull response

    "Convert XML into ABAP str.
    loop at ls_source-parameters-invoices into data(ls_inv).

      concatenate 'US_INCOMING' sy-datum sy-timlo
             into ls_msg-message_name separated by '_'.
      concatenate ls_msg-message_name '.xml' into ls_msg-message_name.
      ls_msg-xml_string = ls_inv-invoice.
      append ls_msg to <fs_target>-edoc_incoming_message_tab.

    endloop.

    ed_target = ld_target.

  endmethod.

endclass.
