FUNCTION ZFM_EDOC_AUTO_GENERATE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_INVOICE) TYPE  VBELN
*"     VALUE(IM_DOCTYP) TYPE  ZTEINV_DOCTYP
*"----------------------------------------------------------------------


  WAIT UP TO 20 SECONDS.
  IF im_doctyp-einv = abap_true AND im_doctyp-instgen = abap_true.

    CALL FUNCTION 'ZFM_EINVOICE_AUTO_GENERATE'
    EXPORTING
      im_invoice = im_invoice.


  ENDIF.

  IF im_doctyp-eway = abap_true AND im_doctyp-instgen_eway = abap_true.

    CALL FUNCTION 'ZFM_EWAY_BILL_AUTO_GENERATE'
    EXPORTING
      im_invoice = im_invoice.

  ENDIF.



ENDFUNCTION.
