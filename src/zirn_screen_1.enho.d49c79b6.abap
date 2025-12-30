"Name: \PR:SAPMV60A\EX:MV60AF0C_CUST_HEAD_ACTIVATE_1\EI
ENHANCEMENT 0 ZIRN_SCREEN_1.
LOOP AT SCREEN.
  IF screen-name = 'TABSTRIP_TAB06'.
    gs_cust_tab-head_caption = 'E-Invoice'.
    gs_cust_tab-head_program = 'SAPLZFG_EINVOICE'.
    gs_cust_tab-head_dynpro = '9001'.

    IF gs_cust_tab-head_dynpro IS NOT INITIAL.
      screen-active = 1.
      screen-invisible = 0.
      MODIFY SCREEN.

      tabstrip_tab06 = gs_cust_tab-head_caption.

      "Optionally, add logic to modify the value
    ENDIF.
  ENDIF.
ENDLOOP.
ENDENHANCEMENT.
