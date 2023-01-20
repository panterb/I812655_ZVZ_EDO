class ZCL_EDOCUMENT_DE definition
  public
*  inheriting from cl_edocument
  create public.

*  global friends cl_edoc_process.

*  public section.
*    types:
*      tt_qekar type table of qekar.
*
*    data:
*      ms_edodepayment type zedodepayment,
*      ms_edodeinvoice type zedodeinvoice,
*      mo_badi_edoc_de type ref to zbadi_edocument_de,
*      ms_edodedeln    type zedodedeln.
*
*    constants:
*      begin of mc_edoc_type,
*        de_deln   type char7  value 'de_deln',
*        de_wtc    type char6  value 'de_wtc',
*        de_iwtc   type char10 value 'de_iwtc',
*      end of mc_edoc_type,
*
*      begin of mc_edoc_process,
*        dedeln    type char6 value 'dedeln',
*        dewtc     type char5 value 'dewatc',
*        deiwtc    type char9 value 'deiwtc',
*      end of mc_edoc_process.
*
*    data:
*      ms_edodewtcdoc              type zedodewtcdoc,
*      ms_edodewtc                 type zedodewtc,
*      ms_canc_code                type zedoc_de_canc_reason,
*      mo_badi_edocument_de_cancel type ref to zbadi_edocument_de_cancel.
*
*    class-methods class_constructor.
*
*    methods constructor
*      importing
*        !io_source_data                   type ref to cl_edoc_source optional
*        !iv_edoc_guid                     type edoc_guid optional
*        !iv_land                          type land optional
*        !iv_generic_badi_filter_adaptor   type edoc_ganeric_badi_filter optional
*        !iv_update_task                   type sap_bool default abap_false
*      raising
*        cx_edocument.
*
*    methods get_source_document_data
*      exporting
*        !es_src_doc_data      type edoc_src_data_fi_invoice
*        !et_related_doc_data  type zedoc_de_src_related_data_tab
*        !es_comp_data         type t001
*        !es_adrc_bukrs        type adrc
*        !es_cust_master       type kna1
*        !es_adrc_cust         type adrc
*        !et_t001z             type zedoc_de_t001z
*      raising
*        cx_edocument.
*
*    methods generate_number2
*      importing
*        !iv_edoc_type type zedode_document_type optional
*      exporting
*        !es_edodeserie2 type zedodeserie2
*      raising
*        cx_edocument.
*
*    methods generate_number
*      raising
*        cx_edocument.
*
*    methods create_multiple         redefinition.
*    methods is_cancelled            redefinition.
*    methods is_relevant             redefinition.
*    methods load_from_db            redefinition.
*    methods restrict_cancellation   redefinition.
*    methods save_to_db              redefinition.
*    methods send_email_to_cust      redefinition.

  protected section.
  private section.
endclass.



class zcl_edocument_de implementation.
endclass.
