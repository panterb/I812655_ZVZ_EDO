CLASS zar_witholding_tax_certificate DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    interfaces if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zar_witholding_tax_certificate IMPLEMENTATION.
    method if_oo_adt_classrun~main.
        out->write( 'Test' ).
    endmethod.

ENDCLASS.
