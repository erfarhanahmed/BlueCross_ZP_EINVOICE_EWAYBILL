*----------------------------------------------------------------------*
***INCLUDE LZFG_EWAYBILLF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form convert_currency
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LW_EWAY_API_ITM_TAXABLE_AMOUNT
*&      --> LW_VBRK_KURRF
*&      <-- LW_EWAY_API_ITM_TAXABLE_AMOUNT
*&---------------------------------------------------------------------*
FORM convert_time  USING    p_lv_time2
CHANGING p_lv_time_gen.

  CONSTANTS:lc_time_am TYPE char2 VALUE 'AM',
  lc_time_pm TYPE char2 VALUE 'PM',
  lc_time    TYPE sy-uzeit VALUE '120000'.

  IF p_lv_time2 = lc_time_pm.
    p_lv_time_gen = p_lv_time_gen + lc_time.
ELSEIF p_lv_time2 = lc_time_am.
    IF p_lv_time_gen > lc_time.
      p_lv_time_gen = lc_time - p_lv_time_gen.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONVERT_CURRENCY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GW_FINAL_INVVAL  text
*      -->P_KURSK_I  text
*      <--P_GW_FINAL_INVVAL  text
*----------------------------------------------------------------------*
FORM convert_currency  USING    iv_value
      iv_kursk
CHANGING ev_value.

  DATA lv_value TYPE wertv13.

  lv_value  = iv_value *   iv_kursk.
  ev_value = lv_value.
ENDFORM.
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
    CLIENT                  = sy-mandt
    ID                      = p_id
    LANGUAGE                = sy-langu
    name                    = p_lv_tdname
    object                  = 'VBBK'
  TABLES
    LINES                   = lt_lines
  EXCEPTIONS
    ID                      = 1
    LANGUAGE                = 2
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
