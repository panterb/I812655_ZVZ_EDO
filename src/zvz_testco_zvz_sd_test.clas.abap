class ZVZ_TESTCO_ZVZ_SD_TEST definition
  public
  inheriting from CL_PROXY_CLIENT
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !DESTINATION type ref to IF_PROXY_DESTINATION optional
      !LOGICAL_PORT_NAME type PRX_LOGICAL_PORT_NAME optional
    preferred parameter LOGICAL_PORT_NAME
    raising
      CX_AI_SYSTEM_FAULT .
  methods ZVZ_FM_SERVICE
    importing
      !INPUT type ZVZ_TESTZVZ_FM_SERVICE
    exporting
      !OUTPUT type ZVZ_TESTZVZ_FM_SERVICERESPONSE
    raising
      CX_AI_SYSTEM_FAULT .
protected section.
private section.
ENDCLASS.



CLASS ZVZ_TESTCO_ZVZ_SD_TEST IMPLEMENTATION.


  method CONSTRUCTOR.

  super->constructor(
    class_name          = 'ZVZ_TESTCO_ZVZ_SD_TEST'
    logical_port_name   = logical_port_name
    destination         = destination
  ).

  endmethod.


  method ZVZ_FM_SERVICE.

  data(lt_parmbind) = value abap_parmbind_tab(
    ( name = 'INPUT' kind = '0' value = ref #( INPUT ) )
    ( name = 'OUTPUT' kind = '1' value = ref #( OUTPUT ) )
  ).
  if_proxy_client~execute(
    exporting
      method_name = 'ZVZ_FM_SERVICE'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.
ENDCLASS.
