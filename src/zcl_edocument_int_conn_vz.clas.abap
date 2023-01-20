class zcl_edocument_int_conn_vz definition public final create public.

  public section.
    interfaces if_badi_interface .
    interfaces if_edocument_int_conn_ext .

  private section.
    methods trigger_ubl_invoice
      importing
        iv_edoc_guid        type if_edoc_cloud_datatypes=>mty_edoc_guid
        io_edocument_ext_es type ref to cl_edocument_ext_es
        iv_interface_id     type if_edoc_ext_datatypes=>mty_interface_id optional
        iv_int_version      type if_edoc_ext_datatypes=>mty_int_version optional
        id_source_data      type ref to data
      raising
        cx_edocument_ext.

    methods trigger_inno_service
      importing
        iv_edoc_guid        type if_edoc_cloud_datatypes=>mty_edoc_guid
        io_edocument_ext_es type ref to cl_edocument_ext_es
        iv_interface_id     type if_edoc_ext_datatypes=>mty_interface_id optional
        iv_int_version      type if_edoc_ext_datatypes=>mty_int_version optional
        id_source_data      type ref to data
      raising
        cx_edocument_ext.

endclass.


class zcl_edocument_int_conn_vz implementation.

  method if_edocument_int_conn_ext~clean_up_messages.

  endmethod.

  method if_edocument_int_conn_ext~display_edocument.
    data:
      lo_edocument_ext_es type ref to cl_edocument_ext_es,
*      lo_edocument_ext    TYPE REF TO cl_edocument_ext,
      ls_preview          type abap_bool,
      lt_edocumentfile    type if_edoc_ext_datatypes=>mty_edocumentfile_t,
      ls_log_message      type bapiret2,
      ls_edocumentfile    type if_edoc_ext_datatypes=>mty_edocumentfile_s,
      lr_edocument        type ref to if_edoc_ext_datatypes=>mty_edocument_s,
      lr_error_text       type ref to string.

    data ls_req type zedo_us_send_invoice_request.
    field-symbols <ls_send_req> type zedo_us_send_invoice_request.
    data lr_send_req type ref to data.
    data ld_source_Data type ref to data.
*    DATA lr_edoc_map_us TYPE REF TO zcl_edoc_map_us.
    field-symbols <fs_source_Data> type any.

**   Get eDocument

    cl_edocument_ext_es=>retrieve_by_edoc_guid(
      exporting
        iv_edoc_guid          =  iv_edoc_guid                 " eDocument GUID
      receiving
        ro_edocument_ext_es   =  lo_edocument_ext_es
    ).

    lr_edocument = lo_edocument_ext_es->get_edocument_structure( ).

    case lr_edocument->proc_status.
      when 'CREATED' or 'SEND_REQ'.
        ls_preview = abap_true.
      when 'ACCEPT' or 'REJECT'.
        ls_preview = abap_false.
      when others.
        ls_preview = abap_true.
    endcase.


    if ls_preview is not initial and lr_edocument->process = 'ZVZINV'.
      " for Preview, Get the request payload

      data(edocument_mapping) = new zcl_edoc_map_vz( ).

      if lr_edocument->source_type = 'FI_INVOICE'.

        cl_edocument_ext_es=>retrieve_by_source_key(
          exporting iv_source_key = lr_edocument->source_key
                    iv_source_type = lr_edocument->source_type
          receiving ro_edocument_ext_es = lo_edocument_ext_es
        ).
        me->if_edocument_int_conn_ext~get_source_data(
          exporting io_edocument_ext_es = lo_edocument_ext_es
          changing ed_country_source_data = ld_source_Data
         ).

        assign ld_source_Data->* to <fs_source_Data>.
        edocument_mapping->map_invoice_request_fi(
          exporting is_source = <fs_source_data>
          importing ed_target = lr_send_req
        ).

        assign lr_send_req->* to <ls_send_req>.
        move-corresponding <ls_send_req> to ls_req.

      elseif lr_edocument->source_type = 'SD_INVOICE'.

      endif.

      call transformation zedo_us_send_invoice_request source root = ls_req
                           result xml data(lv_xml).

      ls_edocumentfile-create_date = sy-datum.
      ls_edocumentfile-create_time = sy-uzeit.
      ls_edocumentfile-file_raw    = lv_xml.
      ls_edocumentfile-edoc_guid   = iv_edoc_guid.
      concatenate lr_edocument->source_type
                  lr_edocument->source_key '.XML'
      into ls_edocumentfile-file_name
      separated by '_'.

      append ls_edocumentfile to lt_edocumentfile.


    elseif lr_edocument->process = 'ZVZINCOMIN'.
      lt_edocumentfile = lo_edocument_ext_es->get_edoc_files( ).
      read table lt_edocumentfile into ls_edocumentfile
                              with key file_type = 'INBOUND'.
      if sy-subrc <> 0 or ls_edocumentfile-file_raw is initial.

        lr_error_text = lo_edocument_ext_es->get_error_text( ).
*        MESSAGE e000(edocument) WITH 'No file for display' INTO lr_error_text->*.
        ls_log_message-message = lr_error_text->*.
        ls_log_message-type = 'W'.
        ls_log_message-id         = sy-msgid.
        ls_log_message-number     = sy-msgno.
        ls_log_message-message_v1 = 'No file for display'.
        append ls_log_message to ct_log_message.
        return.
      endif.

    else.

      lt_edocumentfile = lo_edocument_ext_es->get_edoc_files( ).
      read table lt_edocumentfile into ls_edocumentfile
                              with key file_type = 'REQUEST'.
      if sy-subrc <> 0 or ls_edocumentfile-file_raw is initial.

        lr_error_text = lo_edocument_ext_es->get_error_text( ).
*        MESSAGE e000(edocument) WITH 'No file for display' INTO lr_error_text->*.
        ls_log_message-message = lr_error_text->*.
        ls_log_message-type = 'W'.
        ls_log_message-id         = sy-msgid.
        ls_log_message-number     = sy-msgno.
        ls_log_message-message_v1 = 'No file for display'.
        append ls_log_message to ct_log_message.
        return.
      endif.
    endif.

    data: lo_util_ext type ref to cl_edoc_util_ext.
    data lv_folder  type  if_edoc_ext_datatypes=>mty_text_element.

    cl_edoc_util_ext=>display_xml(
      exporting
        iv_xml_string           = ls_edocumentfile-file_raw
        iv_file_name            = ls_edocumentfile-file_name                  " File Name
        iv_is_preview           = ls_preview
    ).

    cv_display_done = abap_true.

  endmethod.

  method if_edocument_int_conn_ext~get_source_data.

    data ld_source_data type ref to if_edoc_ext_datatypes=>mty_mapping_source_s.
    data ls_fi_invoice    type if_edoc_ext_datatypes=>mty_edoc_src_data_fi_inv_s.
    data lr_edocument     type ref to if_edoc_ext_datatypes=>mty_edocument_s.
    field-symbols: <ls_source_data> type if_edoc_ext_datatypes=>mty_mapping_source_s.

    create data ld_source_data.
    assign ld_source_data->* to <ls_source_data>.

    lr_edocument = io_edocument_ext_es->get_edocument_structure( ).

    case lr_edocument->source_type.
      when 'FI_INVOICE'.
        io_edocument_ext_es->get_source_data( importing es_source_data = ls_fi_invoice ).

        <ls_source_data>-source_header = ls_fi_invoice-source_header.
        <ls_source_data>-reference-edoc_guid = lr_edocument->edoc_guid.
        <ls_source_data>-bkpf = ls_fi_invoice-document_header.
        <ls_source_data>-bseg = ls_fi_invoice-document_item.
        <ls_source_data>-bset = ls_fi_invoice-tax_data.
        <ls_source_data>-bsec = ls_fi_invoice-onetime_customer.

      when others.
    endcase.

    ed_country_source_data = ld_source_data.
  endmethod.

  method if_edocument_int_conn_ext~prepare_messages.
  endmethod.

  method if_edocument_int_conn_ext~pull_messages.

    data: lv_method_name    type if_edoc_ext_datatypes=>mty_method_name,
          lv_map_class_name type if_edoc_ext_datatypes=>mty_class_name.

    data:
      lv_interface_id_out type if_edoc_ext_datatypes=>mty_interface_id,
      lv_int_version_out  type if_edoc_ext_datatypes=>mty_int_version,
      lv_int_version_in   type if_edoc_ext_datatypes=>mty_int_version,
      lv_xstring          type xstring,
      lo_edoc_util_ext    type ref to cl_edoc_util_ext,
      lo_map_class        type ref to object.

    field-symbols: <ls_req_source_structure> type any,
                   <ls_req_target_structure> type any.
    field-symbols: <ls_res_source_structure> type any,
                   <ls_res_target_structure> type any.

    data: ld_req_target type ref to data,
          ld_res_source type ref to data,
          ld_res_target type ref to data,
          ld_data       type ref to data.

    field-symbols: <fs_msg>     type any table,
                   <ls_message> type if_edoc_ext_datatypes=>mty_edoc_incom_msg_s,
                   <ls_msgtype> type if_edoc_ext_datatypes=>mty_edoc_msg_type_s.

    clear ct_message.

    lo_edoc_util_ext = cl_edoc_util_ext=>get_object( ).

*    DATA lv_burks TYPE burks.
    read table it_bukrs_rng into data(ls_burks) index 1.
*    IF sy-subrc = 0.
*      lv_burks = ls_burks-low.
*    ENDIF.

    loop at it_message_type assigning <ls_msgtype>.

      " Get the message type details
      lo_edoc_util_ext->determine_msg_type_interface(
                                    exporting iv_message_type = <ls_msgtype>-message_type
                                    importing es_edomsgtypeint = data(ls_edomsgtypeint) ).

* determine mapping class & Method
      lv_map_class_name = 'ZCL_EDOC_MAP_VZ'.
      lv_method_name = 'MAP_PULL_REQUEST'.

      create data ld_data type if_edoc_ext_datatypes=>MTY_EDOC_S_BUKRS_s.

      get reference of ls_burks into ld_data.
      assign  ld_data->* to <ls_req_source_structure>.

      create object lo_map_class type (lv_map_class_name).
      call method lo_map_class->(lv_method_name)
        exporting
          is_source = <ls_req_source_structure>
        importing
          ed_target = ld_req_target.

      assign ld_req_target->* to <ls_req_target_structure>.


* call get invoice proxy
      call method zcl_edoc_map_vz=>call_get_invocie_proxy
        exporting
          is_source   = <ls_req_target_structure>
        importing
          ed_response = ld_res_source
          ev_xstring  = lv_xstring.

* Response mapping

      assign ld_res_source->* to <ls_res_source_structure>.
      lv_method_name = 'MAP_PULL_RESPONSE'.

      call method lo_map_class->(lv_method_name)
        exporting
          is_source = <ls_res_source_structure>
        importing
          ed_target = ld_res_target.


* Mapping incoming message
      assign ld_res_target->* to <ls_res_target_structure>.

      assign component 'EDOC_INCOMING_MESSAGE_TAB' of structure <ls_res_target_structure> to <fs_msg>.

      loop at <fs_msg> assigning <ls_message>.
        move-corresponding <ls_msgtype> to <ls_message>.
        <ls_message>-interface_id_out  = ls_edomsgtypeint-interface_id_out.
*        <ls_message>-int_version_out = lv_int_version_out.
        <ls_message>-interface_id_in = ls_edomsgtypeint-interface_id_in.
*        <ls_message>-int_version_in  = lv_int_version_in.
        <ls_message>-bukrs = ls_burks-low.
        <ls_message>-land = <ls_msgtype>-land.
        <ls_message>-change_date = sy-datum.
        <ls_message>-change_time = sy-timlo.
        <ls_message>-message_type = <ls_msgtype>-message_type.
        get time stamp field data(ts).
        <ls_message>-received_at = ts.
      endloop.

      append lines of <fs_msg> to ct_message.

    endloop.

  endmethod.

  method if_edocument_int_conn_ext~trigger.
    data: lv_interface_id  type if_edoc_ext_datatypes=>mty_interface_id,
          lv_log_port_name type prx_logical_port_name,
          lv_error_txt     type string.

    data: ld_source_data type ref to data,
          lo_class_desc  type ref to cl_abap_typedescr,
          lv_class_name  type string.

    data: lv_process_step type if_edoc_ext_datatypes=>mty_edoc_process_step,
          lv_subrc        type sysubrc,
          lx_root         type ref to cx_root.

    data: lv_comm_action  type abap_bool.

    data: lv_method_name    type if_edoc_ext_datatypes=>mty_method_name,
          lv_map_class_name type if_edoc_ext_datatypes=>mty_class_name. "Check
    data lo_map_class type ref to object.
    data: ld_data      type ref to data,
          lr_edocument type ref to if_edoc_ext_datatypes=>mty_edocument_s,
          lv_variant   type if_edoc_ext_datatypes=>mty_edoc_proc_step_var,
          lv_xstring   type xstring.

    field-symbols: <ls_req_source_structure> type any,
                   <ls_req_target_structure> type any.
    field-symbols: <ls_res_source_structure> type any,
                   <ls_res_target_structure> type any.
    field-symbols: <fs_status_req_source> type any.

    data: ld_req_target type ref to data,
          ld_res_source type ref to data,
          ld_res_target type ref to data.

    if iv_interface_id = 'UBL_INVOICE_TRANSM'.
      trigger_ubl_invoice(
        iv_edoc_guid        = iv_edoc_guid
        io_edocument_ext_es = io_edocument_ext_es
        iv_interface_id     = iv_interface_id
        iv_int_version      = iv_int_version
        id_source_data      = id_source_data
      ).

    elseif iv_interface_id = 'ZDE_SELF-BILLING_SEND_REQ' or
           iv_interface_id = 'ZDE_SELF-BILLING_GET_REQ' or
           iv_interface_id = 'ZDE_PAYMENT_SEND_REQ' or
           iv_interface_id = 'ZDE_PAYMENT_GET_REQ' or
           iv_interface_id = 'ZDE_WTC_SEND_REQ' or
           iv_interface_id = 'ZDE_WTC_GET_REQ'.
      trigger_inno_service(
        iv_edoc_guid        = iv_edoc_guid
        io_edocument_ext_es = io_edocument_ext_es
        iv_interface_id     = iv_interface_id
        iv_int_version      = iv_int_version
        id_source_data      = id_source_data
      ).
      return.
    endif.

    assign id_source_data->* to <ls_req_source_structure>.
    lr_edocument = io_edocument_ext_es->get_edocument_structure( ).

*    *-- Logical port and interface determination
    lv_interface_id = iv_interface_id.

    lv_map_class_name = 'ZCL_EDOC_MAP_VZ'.


    if iv_interface_id = 'ZUS_INVOICE_SEND_REQ'.
      lv_process_step = 'SENDEDOC'.
*     Select the interface ID
      lv_interface_id = io_edocument_ext_es->determine_interface_id(
                          iv_process_step = lv_process_step ).


*determine mapping class

***determine mapping method using new badi
      lv_method_name = 'MAP_INVOICE_REQUEST_FI'.

*call mapping class -> in this partner will do mapping and will call proxy
*  ls_country_req_source = <ls_source_structure>.
      create object lo_map_class type (lv_map_class_name).
      call method lo_map_class->(lv_method_name)
        exporting
          is_source = <ls_req_source_structure>
        importing
          ed_target = ld_req_target.

      assign ld_req_target->* to <ls_req_target_structure>.
**call Proxy -> new badi will be provided to call proxy
      call method zcl_edoc_map_vz=>call_proxy
        exporting
          is_source       = <ls_req_target_structure>
          iv_logical_port = lv_log_port_name
        importing
          ed_response     = ld_res_source
          ev_xstring      = lv_xstring.

      get reference of lv_xstring into ld_data.
*communicate sendedoc process step
      io_edocument_ext_es->communicate_process_step(
                    exporting  iv_process_step   = 'SENDEDOC'
                               iv_interface_guid = 'STMP' "(or use any other non-empty ID that you prefer)
                               id_data           = ld_data ).

***determine mapping method using new badi
      lv_method_name = 'MAP_INVOICE_RESPONSE_FI'.
*call mapping response
      assign ld_res_source->* to <ls_res_source_structure>.
      call method lo_map_class->(lv_method_name)
        exporting
          is_source = <ls_res_source_structure>
        importing
          ed_target = ld_res_target.

*determine process step using new badi provided
      lv_process_step = 'PROC_RESP'.

*call communicate process step
      io_edocument_ext_es->communicate_process_step(
                 exporting  iv_process_step   = lv_process_step  "Process response
                            iv_interface_guid = 'STMP'
                            id_data           = ld_res_target
                            iv_variant        = lv_variant   "Result should be passed as variant
                            ).

    elseif iv_interface_id = 'ZUS_INVOICE_GET_REQ'.

*determine mapping class
*      lv_map_class_name = 'ZCL_EDOC_MAP_US'.
***determine mapping method using new badi
      lv_method_name = 'MAP_STATUS_REQUEST'.
      assign lr_edocument->* to <fs_status_req_source>.
      create object lo_map_class type (lv_map_class_name).
      call method lo_map_class->(lv_method_name)
        exporting
          is_source = <fs_status_req_source>
        importing
          ed_target = ld_req_target.

      assign ld_req_target->* to <ls_req_target_structure>.
**call Proxy -> new badi will be provided to call proxy
      call method zcl_edoc_map_vz=>call_status_proxy
        exporting
          is_source       = <ls_req_target_structure>
          iv_logical_port = lv_log_port_name
        importing
          ed_response     = ld_res_source
          ev_xstring      = lv_xstring.

      get reference of lv_xstring into ld_data.
***determine mapping method using new badi
      lv_method_name = 'MAP_STATUS_RESPONSE'.
*call mapping response
      assign ld_res_source->* to <ls_res_source_structure>.
      call method lo_map_class->(lv_method_name)
        exporting
          is_source = <ls_res_source_structure>
        importing
          ed_target = ld_res_target.

*determine process step using new badi provided
      lv_process_step = 'STATUS_RES'.

*call communicate process step
      io_edocument_ext_es->communicate_process_step(
                 exporting  iv_process_step   = lv_process_step  "Process response
                            iv_interface_guid = 'STMP'
                            id_data           = ld_res_target
                            iv_variant        = lv_variant   "Result should be passed as variant
                            ).

    elseif iv_interface_id = 'ZUS_INVOICE_REJECT_REQ'.
      data ld_target type ref to zif_edo_datatypes_us=>mty_mapping_target_us_s.
      field-symbols <ls_target> type zif_edo_datatypes_us=>mty_mapping_target_us_s.

      create data ld_target.
      assign ld_target->* to <ls_target>.

      <ls_target>-status = 'REJECT'.
*determine process step using new badi provided
      lv_process_step = 'REJECT_RES'.
*call communicate process step
      io_edocument_ext_es->communicate_process_step(
                 exporting  iv_process_step   = lv_process_step  "Process response
                            iv_interface_guid = 'STMP'
                            id_data           = ld_target
                            iv_variant        = lv_variant   "Result should be passed as variant
                            ).
    endif.
  endmethod.

  method trigger_ubl_invoice.

  endmethod.

  method trigger_inno_service.
    field-symbols: <ls_req_source_structure> type any,
                   <ls_req_target_structure> type any,
                   <ls_res_source_structure> type any,
                   <fs_status_req_source>    type any.

    data: lr_edocument    type ref to if_edoc_ext_datatypes=>mty_edocument_s,
          ld_req_target   type ref to data,
          ld_res_source   type ref to data,
          ld_res_target   type ref to data,
          lo_map_class    type ref to object,
          ld_data         type ref to data.

    data: lv_variant        type if_edoc_ext_datatypes=>mty_edoc_proc_step_var,
          lv_process_step   type if_edoc_ext_datatypes=>mty_edoc_process_step,
          lv_interface_id   type if_edoc_ext_datatypes=>mty_interface_id,
          lv_method_name    type if_edoc_ext_datatypes=>mty_method_name,
          lv_map_class_name type if_edoc_ext_datatypes=>mty_class_name,
          lv_log_port_name  type prx_logical_port_name,
          lv_xstring        type xstring.

    assign id_source_data->* to <ls_req_source_structure>.
    lr_edocument = io_edocument_ext_es->get_edocument_structure( ).

    lv_map_class_name = 'ZCL_EDOC_MAP_VZ'.

    if iv_interface_id = 'ZDE_PAYMENT_SEND_REQ' or iv_interface_id = 'ZDE_WTC_SEND_REQ' or
       iv_interface_id = 'ZDE_SELF-BILLING_SEND_REQ'.

      lv_process_step = 'SENDEDOC'.
      lv_interface_id = io_edocument_ext_es->determine_interface_id(
                        iv_process_step = lv_process_step ).

      lv_method_name = 'MAP_INNO_REQUEST_FI'.
      create object lo_map_class type (lv_map_class_name).
      call method lo_map_class->(lv_method_name)
        exporting
          is_source = <ls_req_source_structure>
        importing
          ed_target = ld_req_target.

      assign ld_req_target->* to <ls_req_target_structure>.
      call method zcl_edoc_map_vz=>call_proxy_sd_service
        exporting
          is_source       = <ls_req_target_structure>
          iv_logical_port = lv_log_port_name
        importing
          ed_response     = ld_res_source
          ev_xstring      = lv_xstring.

      get reference of lv_xstring into ld_data.

      io_edocument_ext_es->communicate_process_step(
                    exporting  iv_process_step   = 'SENDEDOC'
                               iv_interface_guid = 'STMP'
                               id_data           = ld_data ).

      lv_method_name = 'MAP_INNO_RESPONSE_FI'.

      assign ld_res_source->* to <ls_res_source_structure>.
      call method lo_map_class->(lv_method_name)
        exporting
          is_source = <ls_res_source_structure>
        importing
          ed_target = ld_res_target.

      lv_process_step = 'PROC_RESP'.
      io_edocument_ext_es->communicate_process_step(
                 exporting  iv_process_step   = lv_process_step
                            iv_interface_guid = 'STMP'
                            id_data           = ld_res_target
                            iv_variant        = lv_variant
                            ).
    elseif iv_interface_id = 'ZDE_PAYMENT_GET_REQ' or iv_interface_id = 'ZDE_WTC_GET_REQ' or
           iv_interface_id = 'ZDE_SELF-BILLING_GET_REQ'.

      data ld_target type ref to zif_edo_datatypes_us=>mty_mapping_target_us_s.
      field-symbols <ls_target> type zif_edo_datatypes_us=>mty_mapping_target_us_s.

      create data ld_target.
      assign ld_target->* to <ls_target>.

*      lv_process_step = 'SENDEDOC'.
*      lv_interface_id = io_edocument_ext_es->determine_interface_id(
*                        iv_process_step = lv_process_step ).

      <ls_target>-status = 'ACCEPT'.
      lv_process_step = 'STATUS_RES'.

      io_edocument_ext_es->communicate_process_step(
                 exporting  iv_process_step   = lv_process_step  "Process response
                            iv_interface_guid = 'STMP'
                            id_data           = ld_target
                            iv_variant        = lv_variant   "Result should be passed as variant
                            ).
    endif.
  endmethod.
endclass.
