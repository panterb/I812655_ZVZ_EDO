class zcl_edocument_base_ext_vz definition public final create public.

  public section.
    interfaces if_badi_interface .
    interfaces if_edocument_base_ext .

endclass.


class zcl_edocument_base_ext_vz implementation.

  method if_edocument_base_ext~determine_edocument_class.
    cv_edoc_class_name = 'ZCL_EDOCUMENT_VZ'.
  endmethod.

  method if_edocument_base_ext~determine_process.
    IF is_edocument-source_type = 'SRC_FILE'.
      cv_process_name = 'ZVZINCOMIN'.
      cv_process_version = '0001'.
    ELSE.
      cv_process_name = 'ZVZINV'.
      cv_process_version = '0001'.
    ENDIF.

    IF is_edocument-edoc_type = 'ZYEPAYMENT'.
      cv_process_name = 'ZYEPAYMENT'.
      cv_process_version = '0001'.
    ENDIF.

    IF is_edocument-edoc_type = 'ZARWTC'.
      cv_process_name = 'ZAR_WTC'.
      cv_process_version = '0001'.
    ENDIF.

    IF is_edocument-edoc_type = 'ZYE_SB'.
      cv_process_name = 'ZYESB'.
      cv_process_version = '0001'.
    ENDIF.
  endmethod.
endclass.
