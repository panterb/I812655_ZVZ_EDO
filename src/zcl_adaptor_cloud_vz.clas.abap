class zcl_adaptor_cloud_vz definition public final create public.

  public section.
    interfaces if_badi_interface .
    interfaces if_edoc_adaptor_cloud .

endclass.


class zcl_adaptor_cloud_vz implementation.

  method if_edoc_adaptor_cloud~change_edocument_type.

  endmethod.

  method if_edoc_adaptor_cloud~change_form.

    if is_edocument-edoc_type = 'ZVZINV'.
      cv_form_name = 'YY1_YY1_ZVZ_EDO'.
    endif.

  endmethod.

  method if_edoc_adaptor_cloud~get_variable_key.

  endmethod.

  method if_edoc_adaptor_cloud~is_relevant.

  endmethod.

  method if_edoc_adaptor_cloud~restrict_cancel.

  endmethod.

  method if_edoc_adaptor_cloud~set_output_data.

    if cs_output_data is initial.

    endif.

  endmethod.

endclass.
