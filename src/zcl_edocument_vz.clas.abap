class zcl_edocument_vz definition public final create public.

  public section.
    data: mv_interface_id type if_edoc_ext_datatypes=>mty_interface_id.

    methods process_create
      importing
        !iv_interface_guid   type if_edoc_ext_datatypes=>mty_interface_edoc_guid optional
        !iv_process_step     type if_edoc_ext_datatypes=>mty_edoc_process_step optional
        !iv_variant          type if_edoc_ext_datatypes=>mty_edoc_proc_step_var optional
        !io_edocument_ext_es type ref to cl_edocument_ext_es optional
      returning
        value(rv_result)     type if_edoc_ext_datatypes=>mty_edoc_proc_step_res .

    methods process_request_send
      importing
        !iv_interface_guid   type if_edoc_ext_datatypes=>mty_interface_edoc_guid optional
        !iv_process_step     type if_edoc_ext_datatypes=>mty_edoc_process_step optional
        !iv_variant          type if_edoc_ext_datatypes=>mty_edoc_proc_step_var optional
        !io_edocument_ext_es type ref to cl_edocument_ext_es optional
      returning
        value(rv_result)     type if_edoc_ext_datatypes=>mty_edoc_proc_step_res .
    methods process_sendedoc
      importing
        !iv_interface_guid   type if_edoc_ext_datatypes=>mty_interface_edoc_guid optional
        !iv_process_step     type if_edoc_ext_datatypes=>mty_edoc_process_step optional
        !iv_variant          type if_edoc_ext_datatypes=>mty_edoc_proc_step_var optional
        !io_edocument_ext_es type ref to cl_edocument_ext_es optional
        !is_data             type xstring optional
      returning
        value(rv_result)     type if_edoc_ext_datatypes=>mty_edoc_proc_step_res .
    methods process_response
      importing
        !iv_interface_guid   type if_edoc_ext_datatypes=>mty_interface_edoc_guid optional
        !iv_process_step     type if_edoc_ext_datatypes=>mty_edoc_process_step optional
        !iv_variant          type if_edoc_ext_datatypes=>mty_edoc_proc_step_var optional
        !io_edocument_ext_es type ref to cl_edocument_ext_es optional
        !is_data             type zif_edo_datatypes_us=>mty_mapping_target_us_s
      returning
        value(rv_result)     type if_edoc_ext_datatypes=>mty_edoc_proc_step_res
      raising
        cx_edocument_ext .
    methods process_trigger_send
      importing
        !iv_interface_guid   type if_edoc_ext_datatypes=>mty_interface_edoc_guid optional
        !iv_process_step     type if_edoc_ext_datatypes=>mty_edoc_process_step optional
        !iv_variant          type if_edoc_ext_datatypes=>mty_edoc_proc_step_var optional
        !io_edocument_ext_es type ref to cl_edocument_ext_es optional
      returning
        value(rv_result)     type if_edoc_ext_datatypes=>mty_edoc_proc_step_res .


    methods process_status_req
      importing
        !iv_interface_guid   type if_edoc_ext_datatypes=>mty_interface_edoc_guid optional
        !iv_process_step     type if_edoc_ext_datatypes=>mty_edoc_process_step optional
        !iv_variant          type if_edoc_ext_datatypes=>mty_edoc_proc_step_var optional
        !io_edocument_ext_es type ref to cl_edocument_ext_es optional
      returning
        value(rv_result)     type if_edoc_ext_datatypes=>mty_edoc_proc_step_res .
    methods process_status_res
      importing
        !iv_interface_guid   type if_edoc_ext_datatypes=>mty_interface_edoc_guid optional
        !iv_process_step     type if_edoc_ext_datatypes=>mty_edoc_process_step optional
        !iv_variant          type if_edoc_ext_datatypes=>mty_edoc_proc_step_var optional
        !io_edocument_ext_es type ref to cl_edocument_ext_es optional
        !is_data             type zif_edo_datatypes_us=>mty_mapping_target_us_s
      returning
        value(rv_result)     type if_edoc_ext_datatypes=>mty_edoc_proc_step_res
      raising
        cx_edocument_ext .


    "Inbound
    methods process_reject_req
      importing
        !iv_interface_guid   type if_edoc_ext_datatypes=>mty_interface_edoc_guid optional
        !iv_process_step     type if_edoc_ext_datatypes=>mty_edoc_process_step optional
        !iv_variant          type if_edoc_ext_datatypes=>mty_edoc_proc_step_var optional
        !io_edocument_ext_es type ref to cl_edocument_ext_es optional
      returning
        value(rv_result)     type if_edoc_ext_datatypes=>mty_edoc_proc_step_res .
    methods process_reject_res
      importing
        !iv_interface_guid   type if_edoc_ext_datatypes=>mty_interface_edoc_guid optional
        !iv_process_step     type if_edoc_ext_datatypes=>mty_edoc_process_step optional
        !iv_variant          type if_edoc_ext_datatypes=>mty_edoc_proc_step_var optional
        !io_edocument_ext_es type ref to cl_edocument_ext_es optional
        !is_data             type zif_edo_datatypes_us=>mty_mapping_target_us_s
      returning
        value(rv_result)     type if_edoc_ext_datatypes=>mty_edoc_proc_step_res
      raising
        cx_edocument_ext .


    methods process_cancel
      importing
        !iv_interface_guid   type if_edoc_ext_datatypes=>mty_interface_edoc_guid optional
        !iv_process_step     type if_edoc_ext_datatypes=>mty_edoc_process_step optional
        !iv_variant          type if_edoc_ext_datatypes=>mty_edoc_proc_step_var optional
        !io_edocument_ext_es type ref to cl_edocument_ext_es optional
      returning
        value(rv_result)     type if_edoc_ext_datatypes=>mty_edoc_proc_step_res .

    methods process_sendtocustomer
      importing
        !iv_interface_guid   type if_edoc_ext_datatypes=>mty_interface_edoc_guid optional
        !iv_process_step     type if_edoc_ext_datatypes=>mty_edoc_process_step optional
        !iv_variant          type if_edoc_ext_datatypes=>mty_edoc_proc_step_var optional
        !io_edocument_ext_es type ref to cl_edocument_ext_es optional
      returning
        value(rv_result)     type if_edoc_ext_datatypes=>mty_edoc_proc_step_res .


    methods process_display_pdf
      importing
        !iv_interface_guid   type if_edoc_ext_datatypes=>mty_interface_edoc_guid optional
        !iv_process_step     type if_edoc_ext_datatypes=>mty_edoc_process_step optional
        !iv_variant          type if_edoc_ext_datatypes=>mty_edoc_proc_step_var optional
        !io_edocument_ext_es type ref to cl_edocument_ext_es optional
      returning
        value(rv_result)     type if_edoc_ext_datatypes=>mty_edoc_proc_step_res .

  private section.

    methods update_file_table
      importing
        !iv_file                type xstring
        !iv_file_guid           type if_edoc_cloud_datatypes=>mty_edoc_guid
        !iv_file_type           type if_edoc_ext_datatypes=>mty_edoc_file_type
        !iv_file_name_extension type if_edoc_ext_datatypes=>mty_edoc_file_name_ext
        !io_edocument_ext_es    type ref to cl_edocument_ext_es optional .


endclass.



class zcl_edocument_vz implementation.

  method process_create.
    data  ls_edocument type ref to if_edoc_ext_datatypes=>mty_edocument_s.
    field-symbols <ls_edocument_fs> type if_edoc_ext_datatypes=>mty_edocument_s.

    if io_edocument_ext_es is bound.
      try.
          io_edocument_ext_es->process_create_global(
            exporting
              iv_interface_guid = iv_interface_guid                 " Interface Message ID
              iv_process_step   = iv_process_step                 " eDocument Process Step
              iv_variant        = iv_variant                 " eDocument Process Step Variant
            receiving
              rv_result         = rv_result                 " eDocument Process Step Result
          ).

          ls_edocument = io_edocument_ext_es->get_edocument_structure( ).

        catch cx_edocument_ext.
          "handle exception
      endtry.

      assign ls_edocument->* to <ls_edocument_fs>.

      data(customer_table) = value zvzedoc_cust2(
        edoc_guid = <ls_edocument_fs>-edoc_guid
        description = 'process_create'
      ).
      insert zvzedoc_cust2 from @customer_table.

    endif.

  endmethod.

  method process_request_send.
    if io_edocument_ext_es is bound.
      try.
          io_edocument_ext_es->process_req_send_global(
            exporting
              iv_interface_guid =     iv_interface_guid              " Interface Message ID
              iv_process_step   =     iv_process_step             " eDocument Process Step
              iv_variant        =     iv_variant             " eDocument Process Step Variant
            receiving
              rv_result         =     rv_result            " eDocument Process Step Result
          ).
        catch cx_edocument_ext.
          "handle exception
      endtry.
    endif.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDODUMENT_US->PROCESS_RESPONSE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INTERFACE_GUID              TYPE        IF_EDOC_EXT_DATATYPES=>MTY_INTERFACE_EDOC_GUID(optional)
* | [--->] IV_PROCESS_STEP                TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROCESS_STEP(optional)
* | [--->] IV_VARIANT                     TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_VAR(optional)
* | [--->] IO_EDOCUMENT_EXT_ES            TYPE REF TO CL_EDOCUMENT_EXT_ES(optional)
* | [--->] IS_DATA                        TYPE        ZIF_EDO_DATATYPES_US=>MTY_MAPPING_TARGET_US_S
* | [<-()] RV_RESULT                      TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_RES
* | [!CX!] CX_EDOCUMENT_EXT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method process_response.
* TRACKID
    data lv_file_guid type if_edoc_cloud_datatypes=>mty_edoc_guid.
    data ls_edoc_cust type zvzedoc_cust2.
    if is_data-status = 'Success'.
      rv_result = 'TRACKID'.
    else.
      rv_result = 'ERROR'.

      " Exception testing
      data lv_error_txt type string.
      lv_error_txt = 'Error while submitting the document'.
    endif.

    if is_data-response_xml is not initial.
*       Get the system UUID
      try.
          lv_file_guid  = cl_system_uuid=>create_uuid_c32_static( ).
        catch cx_uuid_error.
          "handle exception
      endtry.

      update_file_table( io_edocument_ext_es = io_edocument_ext_es
                         iv_file = is_data-response_xml
                         iv_file_guid = lv_file_guid
                         iv_file_type = 'RESPONSE'
                         iv_file_name_extension = 'XML' ).

    endif.


    ls_edoc_cust-edoc_guid = io_edocument_ext_es->get_edocument_structure( )->edoc_guid.
    ls_edoc_cust-trackid = is_data-track_id.
    ls_edoc_cust-description = rv_result.

    modify zvzedoc_cust2 from @ls_edoc_cust.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDODUMENT_US->PROCESS_SENDEDOC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INTERFACE_GUID              TYPE        IF_EDOC_EXT_DATATYPES=>MTY_INTERFACE_EDOC_GUID(optional)
* | [--->] IV_PROCESS_STEP                TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROCESS_STEP(optional)
* | [--->] IV_VARIANT                     TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_VAR(optional)
* | [--->] IO_EDOCUMENT_EXT_ES            TYPE REF TO CL_EDOCUMENT_EXT_ES(optional)
* | [--->] IS_DATA                        TYPE        XSTRING(optional)
* | [<-()] RV_RESULT                      TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_RES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method process_sendedoc.
    data: lv_file_guid  type if_edoc_cloud_datatypes=>mty_edoc_guid.
    clear rv_result.

    rv_result = iv_variant.
    if is_data is not initial.
*       Get the system UUID
      lv_file_guid  = cl_system_uuid=>create_uuid_c32_static( ).

      update_file_table( io_edocument_ext_es = io_edocument_ext_es
                         iv_file = is_data
                         iv_file_guid = lv_file_guid
                         iv_file_type = 'REQUEST'
                         iv_file_name_extension = 'XML' ).


    endif.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDODUMENT_US->PROCESS_TRIGGER_SEND
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INTERFACE_GUID              TYPE        IF_EDOC_EXT_DATATYPES=>MTY_INTERFACE_EDOC_GUID(optional)
* | [--->] IV_PROCESS_STEP                TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROCESS_STEP(optional)
* | [--->] IV_VARIANT                     TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_VAR(optional)
* | [--->] IO_EDOCUMENT_EXT_ES            TYPE REF TO CL_EDOCUMENT_EXT_ES(optional)
* | [<-()] RV_RESULT                      TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_RES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method process_trigger_send.

    if io_edocument_ext_es is bound.
      try.
          io_edocument_ext_es->process_trigger_send_global(
            exporting
              iv_interface_guid = iv_interface_guid                 " Interface Message ID
              iv_process_step   = iv_process_step                 " eDocument Process Step
              iv_variant        = iv_variant                 " eDocument Process Step Variant
            receiving
              rv_result         = rv_result                 " eDocument Process Step Result
          ).
        catch cx_edocument_ext.
          "handle exception
      endtry.
    endif.
  endmethod.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDODUMENT_US->UPDATE_FILE_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILE                        TYPE        XSTRING
* | [--->] IV_FILE_GUID                   TYPE        IF_EDOC_CLOUD_DATATYPES=>MTY_EDOC_GUID
* | [--->] IV_FILE_TYPE                   TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_FILE_TYPE
* | [--->] IV_FILE_NAME_EXTENSION         TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_FILE_NAME_EXT
* | [--->] IO_EDOCUMENT_EXT_ES            TYPE REF TO CL_EDOCUMENT_EXT_ES(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method update_file_table.
    data: ls_edocumentfile type if_edoc_ext_datatypes=>mty_edocumentfile_s,
          lr_edocument     type ref to if_edoc_ext_datatypes=>mty_edocument_s.

*   Move file parameters
    clear ls_edocumentfile.
    ls_edocumentfile-file_guid   = iv_file_guid.
    ls_edocumentfile-create_date = sy-datum.
    ls_edocumentfile-create_time = sy-uzeit.
    ls_edocumentfile-file_type   = iv_file_type.

    lr_edocument = io_edocument_ext_es->get_edocument_structure( ).

*   File name and file content
    concatenate
                lr_edocument->land
                '_'
                lr_edocument->source_key
                '_'
                ls_edocumentfile-create_date
                '_'
                ls_edocumentfile-create_time
                '.'
                iv_file_name_extension
           into ls_edocumentfile-file_name.

    ls_edocumentfile-file_raw = iv_file.

    if io_edocument_ext_es is bound.
      io_edocument_ext_es->add_file_to_edocumentfile( changing cv_edocumentfile = ls_edocumentfile ).
    endif.

  endmethod.

  method process_status_req.
    if io_edocument_ext_es is bound.
      data: lv_interface_id type if_edoc_ext_datatypes=>mty_interface_id.
      data lr_edocument     type ref to if_edoc_ext_datatypes=>mty_edocument_s.
      field-symbols <fs_edocument> type if_edoc_ext_datatypes=>mty_edocument_s.
      lr_edocument =  io_edocument_ext_es->get_edocument_structure(  ).
      assign lr_edocument->* to <fs_edocument>.

      mv_interface_id = io_edocument_ext_es->determine_interface_id( iv_process_step ).
      <fs_edocument>-interface_id = mv_interface_id.
      try.
          io_edocument_ext_es->process_trigger_send_global(
            exporting
              iv_interface_guid = iv_interface_guid                   " Interface Message ID
              iv_process_step   = iv_process_step                 " eDocument Process Step
              iv_variant        = iv_variant                 " eDocument Process Step Variant
            receiving
              rv_result         = rv_result                 " eDocument Process Step Result
          ).
        catch cx_edocument_ext.
          "handle exception
      endtry.
      clear rv_result.
    endif.
  endmethod.

  method process_status_res.
    data lv_file_guid type if_edoc_cloud_datatypes=>mty_edoc_guid.
    if is_data-status = 'ACCEPT'.
      rv_result = 'ACCEPT'.
    else.
      rv_result = 'REJECT'.

      " Exception testing
      data lv_error_txt type string.
      lv_error_txt = 'Error while submitting the document'.
    endif.

    if is_data-response_xml is not initial.
*       Get the system UUID
      lv_file_guid  = cl_system_uuid=>create_uuid_c32_static( ).

      update_file_table( io_edocument_ext_es = io_edocument_ext_es
                         iv_file = is_data-response_xml
                         iv_file_guid = lv_file_guid
                         iv_file_type = 'RESPONSE'
                         iv_file_name_extension = 'XML' ).

    endif.


  endmethod.

  method process_reject_req.
    data: lv_interface_id type if_edoc_ext_datatypes=>mty_interface_id.
    data lr_edocument     type ref to if_edoc_ext_datatypes=>mty_edocument_s.
    field-symbols <fs_edocument> type if_edoc_ext_datatypes=>mty_edocument_s.

    if io_edocument_ext_es is bound.
      lr_edocument =  io_edocument_ext_es->get_edocument_structure(  ).
      assign lr_edocument->* to <fs_edocument>.

      mv_interface_id = io_edocument_ext_es->determine_interface_id( iv_process_step ).
      <fs_edocument>-interface_id = mv_interface_id.
      try.
          io_edocument_ext_es->process_trigger_send_global(
            exporting
              iv_interface_guid = iv_interface_guid                   " Interface Message ID
              iv_process_step   = iv_process_step                 " eDocument Process Step
              iv_variant        = iv_variant                 " eDocument Process Step Variant
            receiving
              rv_result         = rv_result                 " eDocument Process Step Result
          ).
        catch cx_edocument_ext.
          "handle exception
      endtry.
      clear rv_result.
    endif.
  endmethod.

  method process_reject_res.
    data lv_file_guid type if_edoc_cloud_datatypes=>mty_edoc_guid.
    if is_data-status = 'REJECT'.
      rv_result = 'REJECTED'.
    else.
      rv_result = 'ERROR'.

      " Exception testing
      data lv_error_txt type string.
      lv_error_txt = 'Error while rejecting the document'.
    endif.

    if is_data-response_xml is not initial.

*       Get the system UUID
      lv_file_guid  = cl_system_uuid=>create_uuid_c32_static( ).

      update_file_table( io_edocument_ext_es = io_edocument_ext_es
                         iv_file = is_data-response_xml
                         iv_file_guid = lv_file_guid
                         iv_file_type = 'RESPONSE'
                         iv_file_name_extension = 'XML' ).

    endif.

  endmethod.

  method process_cancel.
    rv_result = ''.
  endmethod.

  method process_sendtocustomer.
    try.
        io_edocument_ext_es->send_email_to_cust( iv_generic_badi_filter = 'EXTDE' ).
      catch cx_edocument_ext.
        "handle exception
    endtry.
    rv_result = ''.
  endmethod.
  method process_display_pdf.
*    importing
*      !IO_ACTION_DIALOG type ref to CL_EDOC_DIALOG
*    raising
*      CX_EDOCUMENT .

    data lr_edocument     type ref to if_edoc_ext_datatypes=>mty_edocument_s.
    field-symbols <fs_edocument> type if_edoc_ext_datatypes=>mty_edocument_s.

    if io_edocument_ext_es is bound.
      lr_edocument =  io_edocument_ext_es->get_edocument_structure(  ).
      assign lr_edocument->* to <fs_edocument>.

    endif.

*    DATA: lt_edocumentfile TYPE TABLE OF edocumentfile,
*          ls_edocumentfile TYPE edocumentfile,
*          lv_content       TYPE xstring.
*
*    DATA: lo_ral          TYPE REF TO if_sral_rt_manager_edoc.
*
*    CONSTANTS: lc_pdf_preview TYPE edoc_file_type VALUE 'PDF_PREVW'.
*
*    IF ms_edocument IS NOT INITIAL.
*
*        lv_content = get_content( ).
*
*        CLEAR ls_edocumentfile.
*        ls_edocumentfile-create_date = sy-datum.
*        ls_edocumentfile-create_time = sy-uzeit.
*        ls_edocumentfile-edoc_guid   = ms_edocument-edoc_guid.
*        ls_edocumentfile-file_type   = lc_pdf_preview.
*        ls_edocumentfile-file_raw    = lv_content.
*
*
*          lo_ral = cl_sral_rt_manager_edoc=>get_instance(
*          iv_edoc_type = ms_edocument-edoc_type
*          iv_file_type = lc_pdf_preview
*          ).
*
*        IF lo_ral IS BOUND.
*          lo_ral->set_values(
*          is_edocument     = ms_edocument
*          is_edocumentfile = ls_edocumentfile
*          ).
*        ENDIF.
*
*        IF io_action_dialog->ms_dcc_attrib-ui_mode = '1'.
*
*          ls_edocumentfile-file_raw = lv_content.
*          io_action_dialog->ms_dcc_attrib-edocumentfile = ls_edocumentfile.
*
*        ELSE.
*
*          CALL FUNCTION 'EFG_DISPLAY_PDF'
*            EXPORTING
*              i_pdf = lv_content.
*
*        ENDIF.
*
*      ENDIF.

  endmethod.
endclass.
