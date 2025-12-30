class ZCL_IM_GTI_BADI_SD_CUST_HEAD definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces ZIF_EX_BADI_SD_CUST_HEAD .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_GTI_BADI_SD_CUST_HEAD IMPLEMENTATION.


  method ZIF_EX_BADI_SD_CUST_HEAD~ACTIVATE_TAB_PAGE.
*  INCLUDE wb2_param_control_global.
*  DATA: lr_badi TYPE REF TO if_ex_badi_sd_cust_head,
*        badi_activate type sap_bool.
*  CALL FUNCTION 'GET_HANDLE_SD_CUST_HEAD'
*  IMPORTING
*    handle = lr_badi
*    active = badi_activate.
*  IF badi_activate EQ abap_true.
*    IF cl_wb2_check_add_on_active=>a_gtm_active EQ abap_false.
*      badi_activate = abap_false.
*    ENDIF.
*    IF cl_wb2_check_add_on_active=>a_enhance EQ addon_all_active.
*      badi_activate = abap_false.
*    ENDIF.
*  ENDIF.
*    if badi_activate eq abap_false.
*       fcaption = 'E-Invoice'(c01).
*       fprogram = 'ZMV60AF_CUST_HEAD'.
*       fdynpro = '6001'.
*       ftab = fcaption.
*    endif.
*    loop at screen.
*     if screen-name = 'TABSTRIP_TAB13'.
*       screen-active = 1.
*       screen-invisible = 0.
*       MODIFY SCREEN.
*     endif.
*    endloop.
*
*  CALL FUNCTION 'WB2_IV_SET_HEADER_SCREEN'
*  EXPORTING
*    i_vbrk    = fvbrk
*  IMPORTING
*    e_caption = fcaption
*    e_program = fprogram
*    e_screen  = fdynpro.
  endmethod.


  METHOD zif_ex_badi_sd_cust_head~check_badi_activate.


  ENDMETHOD.


  method ZIF_EX_BADI_SD_CUST_HEAD~PASS_FCODE_TO_SUBSCREEN.
  endmethod.


  method ZIF_EX_BADI_SD_CUST_HEAD~TRANSFER_DATA_FROM_SUBSCREEN.
  endmethod.


  METHOD zif_ex_badi_sd_cust_head~transfer_data_to_subscreen.
** check add on active
*    check not a_gtm_active is initial.
** check enhancement active
*    CHECK a_enhance = 2 OR NOT a_cc_active IS INITIAL.
* set data
*    a_vbrk   = f_vbrk.
*    a_vbrkvb = a_vbrk.
*    a_actyp  = ft180-aktyp.
** set add on data
*    CALL FUNCTION 'WB2_IV_GET_HEADER_DATA'
*      EXPORTING
*        i_t180    = ft180
*      CHANGING
*        c_vbrk    = a_vbrk
*        ct_vbpa   = fxvbpa
*        ct_yvbpa  = fxyvbpa
*        ct_vbadr  = fxvbadr
*        ct_yvbadr = fxyvbadr.
** Daten export expense
*    CALL FUNCTION 'WB2_HANDLE_EXPENSE_IV'
*      EXPORTING
*        i_vbrk  = a_vbrkvb
*        i_actyp = a_actyp
*      TABLES
*        t_vbrp  = fxvbrp[].
*    IF a_cc_active IS NOT INITIAL.
** fill condition data
*      CALL FUNCTION 'WB2_PROCESS_SERVICE_IV1'
*        EXPORTING
*          it_vbrp = fxvbrp
*          i_vbrk  = a_vbrkvb.
*    ENDIF.
  ENDMETHOD.
ENDCLASS.
