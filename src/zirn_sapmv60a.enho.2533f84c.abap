"Name: \PR:SAPMV60A\FO:CUST_HEAD_ACTIVATE\SE:BEGIN\EI
ENHANCEMENT 0 ZIRN_SAPMV60A.
LOOP AT SCREEN.

  CALL FUNCTION 'ZFM_EINVOICE_SET_VBELN'
    EXPORTING
      lv_vbeln = vbrk-vbeln.


  IF screen-name = 'TABSTRIP_TAB06'.
*    gs_cust_tab-head_caption = 'E-Invoice'.
*    gs_cust_tab-head_program = 'SAPLZFG_EINVOICE'.
*    gs_cust_tab-head_dynpro = '9001'.
*
*    IF gs_cust_tab-head_dynpro IS NOT INITIAL.
*      screen-active = 1.
*      screen-invisible = 0.
*      MODIFY SCREEN.
*
*      tabstrip_tab06 = gs_cust_tab-head_caption.
*
*      "Optionally, add logic to modify the value
*    ENDIF.

*    STATICS: gr_badi TYPE REF TO zbadi_sd_cust_head.
*    TRY.
*        IF NOT gr_badi IS BOUND.
*          GET BADI gr_badi.
*        ENDIF.
*        IF gr_badi IS BOUND.
*          CALL BADI gr_badi->activate_tab_page
*            EXPORTING
*              fvbrk    = vbrk
*              fvbuk    = vbuk
*              fxvbpa   = xvbpa[]
*            IMPORTING
*              fcaption = gs_cust_tab-head_caption
*              fprogram = gs_cust_tab-head_program
*              fdynpro  = gs_cust_tab-head_dynpro
*              ftab     = gs_cust_tab-head_caption.
*        ENDIF.
*        IF gs_cust_tab-head_caption IS NOT INITIAL.
*          gs_cust_tab-head_caption = 'E-Invoice'.
*          gs_cust_tab-head_program = 'SAPLZFG_EINVOICE'.
*          gs_cust_tab-head_dynpro = '9001'.
*        ENDIF.
*
*
*        IF NOT gs_cust_tab-head_dynpro IS INITIAL.
*          screen-active = 1.
*          screen-invisible = 0.
*          MODIFY SCREEN.
*          tabstrip_tab06 = gs_cust_tab-head_caption.
*          EXIT.
*        ENDIF.
*      CATCH cx_badi_not_implemented.
*    ENDTRY.
  ENDIF.
ENDLOOP.

ENDENHANCEMENT.
