*----------------------------------------------------------------------*
***INCLUDE LZFG_EINVOICEF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CONVERT_CURRENCY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_EINV_API_ITM_UNIT_PRICE  text
*      -->P_LS_WB2_V_VBRK_VBRP2_KURSK_I  text
*      <--P_LS_EINV_API_ITM_UNIT_PRICE  text
*----------------------------------------------------------------------*
FORM convert_currency  USING    iv_value
      iv_kursk
CHANGING ev_value.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

  DATA:lv_date      TYPE sy-datum,
       lv_date1(10) TYPE c.

  SELECT SINGLE  * FROM j_1ig_invrefnum INTO wa_einvoice WHERE docno = g_vbeln AND
  irn_status = 'CNL'.
  IF sy-subrc IS NOT INITIAL.
    SELECT SINGLE  * FROM j_1ig_invrefnum INTO wa_einvoice WHERE docno = g_vbeln AND
    irn_status = 'ACT'.
  ENDIF.
  IF sy-subrc IS INITIAL.
    SORT gt_einvoice DESCENDING.
    IF wa_einvoice-ack_date IS NOT INITIAL.
      lv_date = wa_einvoice-ack_date.
      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
        EXPORTING
          date_internal            = lv_date
        IMPORTING
          date_external            = lv_date1
        EXCEPTIONS
          date_internal_is_invalid = 1
          OTHERS                   = 2.
      IF sy-subrc = 0.
        wa_einvoice-ack_date  = lv_date1.
      ENDIF.

    ENDIF.


    IF wa_einvoice-cancel_date IS NOT INITIAL.
      lv_date = wa_einvoice-cancel_date.
      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
        EXPORTING
          date_internal            = lv_date
        IMPORTING
          date_external            = lv_date1
        EXCEPTIONS
          date_internal_is_invalid = 1
          OTHERS                   = 2.
      IF sy-subrc = 0.
        wa_einvoice-cancel_date  = lv_date1.
      ENDIF.

    ENDIF.


  ENDIF.

* Select E-waybill No
  SELECT * FROM  j_1ig_ewaybill INTO TABLE gt_ewaybill WHERE docno = g_vbeln
  AND status = 'A'.
  IF sy-subrc IS INITIAL.
    SORT gt_ewaybill BY docno ebillno DESCENDING.
    READ TABLE  gt_ewaybill INTO wa_ewaybill INDEX 1.
  ENDIF.

ENDMODULE.
**&---------------------------------------------------------------------*
**& Form readtext_value
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> LV_TDNAME
**&---------------------------------------------------------------------*
FORM readtext_value  USING    p_lv_tdname TYPE tdobname
      p_id TYPE tdid
CHANGING c_value TYPE tdline.
*
  DATA: lw_lines TYPE tline,
        lt_lines TYPE TABLE OF tline.

  CLEAR: lt_lines,c_value.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = p_id
      language                = sy-langu
      name                    = p_lv_tdname
      object                  = gc_tobject_vbbk
    TABLES
      lines                   = lt_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ELSE.
    READ TABLE lt_lines INTO lw_lines INDEX 1.
    IF lw_lines-tdline IS NOT INITIAL.
      CONCATENATE '"'  lw_lines-tdline '"' INTO  c_value.
    ENDIF.

  ENDIF.
ENDFORM.
