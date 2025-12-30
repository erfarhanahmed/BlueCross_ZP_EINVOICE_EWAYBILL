*----------------------------------------------------------------------*
***INCLUDE ZREP_EINV_EWAY_REPORT_USER_I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  CASE sy-ucomm.

    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
      PERFORM exit_program.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  CHECK_SELECTED_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_selected_data CHANGING  p_val.

  DATA:
        lv_line  TYPE i.

  CLEAR:gt_index,gt_row.


  CALL METHOD gref_alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = gt_index
      et_row_no     = gt_row.



  DESCRIBE TABLE gt_row LINES lv_line.
  IF lv_line IS INITIAL.

    p_val = abap_true.
    MESSAGE 'Please select atleast one document'(013) TYPE 'I'.

    EXIT.

  ENDIF.




ENDFORM.
FORM set_selected_data.
  IF gt_index IS NOT INITIAL.
    CALL METHOD gref_alv_grid->set_selected_rows
      EXPORTING
        it_index_rows = gt_index
        it_row_no     = gt_row.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  EINVOICE_GENERATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM einvoice_generate .
  TYPES lr_fkart_type TYPE RANGE OF fkart.
  DATA : lr_fkart TYPE lr_fkart_type.

  DATA: lw_index            TYPE lvc_s_row,
        lv_val              TYPE xfeld,
        lw_token            TYPE string,
        lw_return           TYPE string,
        wa_return           TYPE string,
        lt_messages         TYPE bapiret2_t,
        lw_message          TYPE bapiret2,
        lt_show_message     TYPE esp1_message_tab_type,
        lw_show_message     LIKE LINE OF lt_show_message,
        lt_invref           TYPE TABLE OF j_1ig_invrefnum,
        lw_invref           TYPE  j_1ig_invrefnum,
        lt_einv_details     TYPE TABLE OF zteinv_details,
        lw_einv_details     TYPE zteinv_details,
        lw_einv             TYPE zteinv_details,
        lw_kna1             TYPE ty_kna1,
        lw_lfa1             TYPE ty_lfa1,
        lw_t005u            TYPE ty_t005u,
        lw_adrc             TYPE ty_adrc,
        lw_adrc1            TYPE ty_adrc1,
        lw_adr61            TYPE ty_adr6,
        lw_gstin            TYPE ty_j_1bbranch,
        lw_adr6             TYPE ty_adr6,
        lw_export           TYPE ty_export,
        lw_vbfa             TYPE ty_vbfa,
        lw_wb2_v_vbak_vbap2 TYPE ty_wb2_v_vbak_vbap2,
        lv_reverse_charge   TYPE i VALUE 1,
*Seller Details
        lw_t001w            TYPE ty_t001w1,
        lw_adrc_s           TYPE ty_adrc,
        lw_t005u_s          TYPE ty_t005u,
        lw_adr6_s           TYPE ty_adr6,
        lw_wb2_v_vbrk_vbrp2 TYPE ty_wb2_v_vbrk_vbrp2,
        lw_bseg             TYPE ty_bseg,
        lw_marc             TYPE ty_marc,
        lv_export           TYPE xfeld,
*Dispatch and Ship Details
        lw_likp             TYPE ty_likp,
        lw_kna1_d           TYPE ty_kna1,
        lw_t005u_d          TYPE ty_t005u,
        lw_adrc_d           TYPE ty_adrc,
        lt_lines            TYPE STANDARD TABLE OF tline,
        lw_lines            TYPE tline,
        lv_index            TYPE sy-tabix,
        lw_einv_api_hdr     TYPE zst_einv_api_struct,
        lw_einv_api_itm     TYPE zst_einv_api_struct_itm,
        ls_doctyp           TYPE zveinv_doctyp,
        ls_values           TYPE dd07v,
        lt_api_hdr          TYPE ztt_einv_api_struct,
        lt_api_itm          TYPE ztt_einv_api_struct_itm,
        lv_answer           TYPE c,
        lw_usrgstin         TYPE ty_api,
        lw_vbpa             TYPE ty_vbpa,
        lw_vbpa_ship        TYPE ty_vbpa,
        lw_konv             TYPE ty_konv,
        lw_gstrate          TYPE kbetr,
        ls_doc_uom          TYPE ty_doc_uom,
        lv_output_uom       TYPE meins,
        lw_doctyp           TYPE ty_doctyp,
        lw_tvarvc           TYPE ty_tvarvc,
        lv_tdname           TYPE thead-tdname,
        lv_tdline           TYPE tdline,
        lv_unit_price       TYPE dmbtr,
        lv_exp_curr         TYPE xfeld,
        lv_dmbtr            TYPE dmbtr,
        lv_rate             TYPE dmbtr,
        lv_item_amt         TYPE wertv13,
        lv_roundoff         TYPE dmbtr,
        lv_posnr            TYPE posnr,
        lv_hsn              TYPE steuc.

  DATA  : lv_i_amount TYPE j_1itaxvar-j_1itaxam1,
          lv_e_amount TYPE j_1itaxvar-j_1itaxam1.




  DATA: lv_total_assessable_value    TYPE wertv13,
        lv_total_cgst_value          TYPE wertv13,
        lv_total_sgst_value          TYPE wertv13,
        lv_total_igst_value          TYPE wertv13,
        lv_total_invoice_value       TYPE wertv13,
        lv_total_cess_value          TYPE wertv13,
        lv_total_cess_nonadvol_value TYPE wertv13,
        lv_total_roundoff            TYPE wertv13,
        lv_total_tcs_value           TYPE wertv13.

  DATA: lv_supply_type     TYPE char10 VALUE 'O',
        lv_document_type   TYPE char10 VALUE 1,
        lv_sub_supply_type TYPE char10 VALUE 1,
        lv_document_number TYPE i VALUE 1,
        lv_date            TYPE sy-datum.

  RANGES : rg_diff FOR prcd_elements-kschl.

  SELECT * FROM tvarvc INTO TABLE @DATA(it_diff) WHERE name = 'EDOC_ROUND_OFF'.

  LOOP AT it_diff INTO DATA(wa_diff) .
    rg_diff-low = wa_diff-low.
    rg_diff-option = 'EQ'.
    rg_diff-sign = 'I'.
    APPEND rg_diff.
  ENDLOOP.

  lr_fkart = VALUE lr_fkart_type(
  LET s = 'I'
  o = 'EQ'
  IN sign   = s
  option = o
  ( low = 'ZDOM' )
  ( low = 'ZEXS' )
  ( low = 'ZEXR' )
  ( low = 'ZEXL' )
  ( low = 'ZEXD' )
  ( low = 'ZSEZ' )
  ).
  SELECT * FROM ztt_state_code INTO TABLE @DATA(lt_state_code).
  DATA : ls_state_code TYPE ztt_state_code.
  FIELD-SYMBOLS <gw_final>  TYPE ty_final.


  CLEAR: lt_messages,lt_invref,lt_api_hdr,lt_api_itm,gt_einv_api_hdr,gt_einv_api_itm.

  PERFORM check_selected_data CHANGING lv_val.

  IF lv_val IS INITIAL.


    IF gv_download IS INITIAL AND gv_doc_download IS INITIAL.
      PERFORM call_popup USING   TEXT-t20 TEXT-t21
      CHANGING lv_answer.
    ELSE.
      lv_answer = '1'.
    ENDIF.


    IF lv_answer = '1'.
      LOOP AT  gt_index INTO lw_index.
        CLEAR:lw_wb2_v_vbrk_vbrp2.

        READ TABLE gt_final INTO gw_final INDEX lw_index-index.
        IF sy-subrc IS INITIAL AND gw_final-irn IS INITIAL AND gw_final-einv = abap_true.
          IF p_mod = gc_sd.
            READ TABLE gt_wb2_v_vbrk_vbrp2 INTO lw_wb2_v_vbrk_vbrp2 WITH KEY vbeln = gw_final-vbeln.
            IF sy-subrc = 0.
              IF lw_wb2_v_vbrk_vbrp2-fksto NE abap_true.

                lv_index = sy-tabix.


                lv_total_assessable_value    = 0.
                lv_total_cgst_value          = 0.
                lv_total_sgst_value          = 0.
                lv_total_igst_value          = 0.
                lv_total_invoice_value       = 0.
                lv_total_cess_value          = 0.
                lv_total_cess_nonadvol_value = 0.
                lv_total_roundoff            = 0.
                lv_total_tcs_value           = 0.

                CLEAR:lw_einv_api_hdr-total_assessable_value,lw_einv_api_hdr-total_cgst_value,lw_einv_api_hdr-total_sgst_value,lw_einv_api_hdr-total_igst_value,
                lw_einv_api_hdr-total_invoice_value,lw_einv_api_hdr-total_cess_value,lw_einv_api_hdr-total_cess_nonadvol_value.

                CLEAR: lv_total_assessable_value,lv_total_igst_value,  lv_total_cgst_value,lv_total_sgst_value, lv_total_cess_value,lv_total_cess_nonadvol_value, lv_total_invoice_value,
                lv_posnr.
*                data : l
                LOOP AT gt_wb2_v_vbrk_vbrp2 INTO lw_wb2_v_vbrk_vbrp2 FROM lv_index.
                  IF lw_wb2_v_vbrk_vbrp2-pstyv_i = 'ZANN'.
                    CONTINUE.
                  ENDIF.
                  IF lw_wb2_v_vbrk_vbrp2-vbeln NE gw_final-vbeln.
                    EXIT.
                  ENDIF.

                  lw_einv_api_hdr-vbeln  = lw_wb2_v_vbrk_vbrp2-vbeln.
                  lw_einv_api_hdr-bukrs  = lw_wb2_v_vbrk_vbrp2-bukrs.
                  lw_einv_api_hdr-doc_year = gw_final-gjahr.
                  lw_einv_api_hdr-doc_type = lw_wb2_v_vbrk_vbrp2-fkart.

*Data to send to the API IN JSON Scheme
                  READ TABLE gt_api  INTO lw_usrgstin WITH KEY apiid = gc_apiid_usrgstin.
                  IF sy-subrc IS INITIAL.
                    lw_einv_api_hdr-user_gstin   = lw_usrgstin-apiprov.
                  ELSE.
                    lw_einv_api_hdr-user_gstin   =  gw_final-sup_gstin.
                  ENDIF.
                  CONCATENATE  '"' lw_einv_api_hdr-user_gstin '"' INTO lw_einv_api_hdr-user_gstin.

                  lw_einv_api_hdr-supply_type   = gc_sup_type_b2b.
                  CASE lv_reverse_charge.
                    WHEN 1.
                      lw_einv_api_hdr-charge_type = gc_charge_n.
                    WHEN 2.
                      lw_einv_api_hdr-charge_type = gc_charge_y.
                  ENDCASE.
                  lw_einv_api_hdr-ecommerce_gstin = '""'.

* Change the charge type for reverse charge
                  READ TABLE gt_konv TRANSPORTING NO FIELDS WITH KEY  knumv = lw_wb2_v_vbrk_vbrp2-knumv
                  kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                  kschl = 'ZCGS'
                  BINARY SEARCH.
                  IF sy-subrc IS NOT INITIAL.
                    READ TABLE gt_konv TRANSPORTING NO FIELDS WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                    kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                    kschl = 'ZIGS'
                    BINARY SEARCH.
                  ENDIF.
                  IF sy-subrc IS INITIAL.
                    lw_einv_api_hdr-charge_type = gc_charge_y.
                  ENDIF.



                  IF lw_wb2_v_vbrk_vbrp2-fkart  = 'ZSEZ'.
                    lw_einv_api_hdr-supply_type   = gc_sup_type_sezwp.
                  ENDIF.

                  IF lw_wb2_v_vbrk_vbrp2-fkart  = 'ZEXD'.
                    lw_einv_api_hdr-supply_type   = gc_sup_type_dexp.
                  ENDIF.

                  CASE lv_document_number.
                    WHEN 1.   "Billing Number as same as ODN
                      lw_einv_api_hdr-document_number = lw_wb2_v_vbrk_vbrp2-vbeln.
                    WHEN 2.   "ODN
                      lw_einv_api_hdr-document_number = lw_wb2_v_vbrk_vbrp2-xblnr.
                  ENDCASE.
                  SHIFT lw_einv_api_hdr-document_number LEFT DELETING LEADING '0'.

                  IF lw_wb2_v_vbrk_vbrp2-fkdat IS NOT INITIAL.
                    CONCATENATE  lw_wb2_v_vbrk_vbrp2-fkdat+6(2) '/'
                    lw_wb2_v_vbrk_vbrp2-fkdat+4(2) '/'
                    lw_wb2_v_vbrk_vbrp2-fkdat+0(4)
                    INTO lw_einv_api_hdr-document_date.                                "Required         "Document Date "[2][0][1-2][0-9]-[0-1][0-9]-[0-3][0-9]"  Y
                  ENDIF.

                  READ TABLE gt_doctyp INTO lw_doctyp WITH KEY fkart = lw_wb2_v_vbrk_vbrp2-fkart
                  bukrs = lw_wb2_v_vbrk_vbrp2-bukrs
                  zmodule = gc_sd.
                  IF sy-subrc = 0.
                    lw_einv_api_hdr-document_type = lw_doctyp-edoc_type.
                  ELSE.
                    lw_einv_api_hdr-document_type = gc_docty_inv.
                  ENDIF.


                  CONCATENATE '"' lw_einv_api_hdr-document_type '"' INTO  lw_einv_api_hdr-document_type.
                  CONCATENATE '"' lw_einv_api_hdr-document_number '"' INTO  lw_einv_api_hdr-document_number.
                  CONCATENATE '"' lw_einv_api_hdr-document_date '"' INTO  lw_einv_api_hdr-document_date.

**Begin of seller_details
                  CLEAR: lw_t001w, lw_adrc_s, lw_adr6_s.
                  READ TABLE gt_t001w INTO lw_t001w WITH  KEY werks = lw_wb2_v_vbrk_vbrp2-werks_i BINARY SEARCH.
                  IF sy-subrc EQ 0.
                    READ TABLE gt_adrc_s INTO lw_adrc_s WITH  KEY addrnumber = lw_t001w-adrnr.
                    READ TABLE gt_adr6_s INTO lw_adr6_s WITH  KEY addrnumber = lw_adrc_s-addrnumber.
                    READ TABLE gt_api  INTO lw_usrgstin WITH KEY apiid = gc_apiid_usrgstin.
                    IF sy-subrc IS INITIAL.
                      lw_einv_api_hdr-gstin_s   = lw_usrgstin-apiprov.
                    ELSE.
                      lw_einv_api_hdr-gstin_s   =  gw_final-sup_gstin.
                    ENDIF.
                    CONCATENATE '"' lw_einv_api_hdr-gstin_s '"' INTO lw_einv_api_hdr-gstin_s.

                    IF lw_adrc_s-name1 IS INITIAL.
                      lw_adrc_s-name1 = lw_t001w-name1.
                    ENDIF.
                    CONCATENATE '"'   lw_adrc_s-name1  '"' INTO   lw_einv_api_hdr-legal_name_s   .     "Legalname   Y       "Required
                    CONCATENATE '"'   lw_adrc_s-name1  '"' INTO   lw_einv_api_hdr-trade_name_s    .    "Tradename   N
                    CONCATENATE '"'   lw_adrc_s-street  '"' INTO   lw_einv_api_hdr-address1_s      .   "Building no.    y     "Required
*                    CONCATENATE '"'   lw_t001w-name2  '"' INTO   lw_einv_api_hdr-address2_s
                    lw_einv_api_hdr-address2_s = '""'.               .  "Building name   N
                    CONCATENATE '"'   lw_t001w-ort01  '"' INTO   lw_einv_api_hdr-location_s       .  "Location    Y         "Required

                    lw_einv_api_hdr-pincode_s =  lw_t001w-pstlz    .  "Pincode   Y           "Required

                    CLEAR:lw_t005u_s.
                    READ TABLE lt_state_code INTO ls_state_code WITH KEY regio = lw_t001w-regio.
                    IF sy-subrc EQ 0.
                      CONCATENATE '"'   ls_state_code-gst_regio  '"' INTO  lw_einv_api_hdr-state_code_s .
                    ELSE.
                      READ TABLE gt_t005u_s INTO lw_t005u_s WITH  KEY bland = lw_t001w-regio.
                      IF sy-subrc EQ 0.
                        TRANSLATE lw_t005u_s-bezei TO UPPER CASE.
                        CONCATENATE '"'   lw_t005u_s-bezei  '"' INTO  lw_einv_api_hdr-state_code_s . "State code    Y        "Required
                      ENDIF.
                    ENDIF.


                    CONCATENATE '"'  lw_adrc_s-tel_number+0(10)  '"' INTO  lw_einv_api_hdr-phone_number_s.        "Phone or Mobile No.   N
                    CONCATENATE '"'    lw_adr6_s-smtp_addr '"' INTO    lw_einv_api_hdr-email_s.             "Email ID                                                  "Email-Id    N
                  ENDIF.
**End of seller_details

**Begin of buyer_details
                  CLEAR:lw_kna1, lw_adrc, lw_adr6.
                  READ TABLE gt_vbpa INTO lw_vbpa WITH KEY vbeln = lw_wb2_v_vbrk_vbrp2-vbeln
                  parvw = gc_parvw_re
                  BINARY SEARCH.
                  IF sy-subrc = 0.
                    READ TABLE gt_vbpa INTO lw_vbpa_ship WITH KEY vbeln = lw_wb2_v_vbrk_vbrp2-vbeln
                    parvw = gc_parvw_we.
                    IF sy-subrc IS INITIAL.
                      IF lw_vbpa-kunnr NE lw_vbpa_ship-kunnr.
                        lw_einv_api_hdr-ship_to = abap_true.
                      ENDIF.

                    ENDIF.
                    READ TABLE gt_kna1 INTO lw_kna1 WITH  KEY kunnr = lw_vbpa-kunnr BINARY SEARCH.
                    IF sy-subrc EQ 0.
                      IF lw_wb2_v_vbrk_vbrp2-fkart NE 'ZSCP'.
                        IF lw_kna1-land1 = 'IN' AND gw_final-igst_amt IS INITIAL AND
                          gw_final-cgst_amt IS INITIAL.
                          MESSAGE 'IRN is not Applicable for Zero tax' TYPE 'I'.
                          CLEAR : lw_einv_api_hdr.
                          CONTINUE.
                        ENDIF.
                      ENDIF..

                      READ TABLE gt_adrc INTO lw_adrc WITH  KEY addrnumber = lw_kna1-adrnr.
                      READ TABLE gt_adr6 INTO lw_adr6 WITH  KEY addrnumber = lw_adrc-addrnumber.
                      CONCATENATE '"'  lw_kna1-stcd3 '"' INTO          lw_einv_api_hdr-gstin_b .
                      CONCATENATE '"'  lw_kna1-name1 '"' INTO          lw_einv_api_hdr-legal_name_b.
                      CONCATENATE '"' lw_kna1-name1 '"' INTO lw_einv_api_hdr-trade_name_b  .
*                      CONCATENATE '"' lw_kna1-stras '"' INTO          lw_einv_api_hdr-address1_b.
*                      CONCATENATE '"' lw_kna1-name2 '"' INTO  lw_einv_api_hdr-address2_b.
*                      CONCATENATE '"'  lw_kna1-ort01 '"'  INTO           lw_einv_api_hdr-location_b.
*                      lw_einv_api_hdr-pincode_b  = lw_kna1-pstlz.
*
*                      lw_einv_api_hdr-place_of_supply_b = lw_kna1-regio.
*                      CONCATENATE '"' lw_einv_api_hdr-place_of_supply_b '"' INTO lw_einv_api_hdr-place_of_supply_b.
*                      CLEAR:lw_t005u.
*                      READ TABLE gt_t005u INTO lw_t005u WITH  KEY bland = lw_kna1-regio.
*                      IF sy-subrc EQ 0.
*                        TRANSLATE lw_t005u-bezei TO UPPER CASE.
*                        CONCATENATE '"' lw_t005u-bezei '"'  INTO  lw_einv_api_hdr-state_code_b.
*                        lw_einv_api_hdr-place_of_supply_b   =   lw_einv_api_hdr-state_code_b.                                           "State code    Y
*                      ENDIF.

                      READ TABLE gt_adrc_new INTO DATA(ls_adrc) WITH KEY addrnumber = lw_vbpa-adrnr.
                      IF sy-subrc IS INITIAL.
                        CONCATENATE ls_adrc-street ls_adrc-str_suppl1 INTO lw_einv_api_hdr-address1_b.
                        CONCATENATE '"' lw_einv_api_hdr-address1_b '"' INTO          lw_einv_api_hdr-address1_b.
                        CONCATENATE '"' ls_adrc-str_suppl2 ls_adrc-str_suppl3 '"' INTO  lw_einv_api_hdr-address2_b.
                        CONCATENATE '"'  ls_adrc-city1 '"'  INTO           lw_einv_api_hdr-location_b.
                        lw_einv_api_hdr-pincode_b  = ls_adrc-post_code1.

                        lw_einv_api_hdr-place_of_supply_b = ls_adrc-region.
                        CONCATENATE '"' lw_einv_api_hdr-place_of_supply_b '"' INTO lw_einv_api_hdr-place_of_supply_b.
                        CLEAR:lw_t005u.
                        READ TABLE lt_state_code INTO ls_state_code WITH KEY regio = ls_adrc-region.
                        IF sy-subrc EQ 0.
                          CONCATENATE '"' ls_state_code-gst_regio '"'  INTO  lw_einv_api_hdr-state_code_b.
                          lw_einv_api_hdr-place_of_supply_b   =   lw_einv_api_hdr-state_code_b.
                        ELSE.
                          READ TABLE gt_t005u INTO lw_t005u WITH  KEY bland = ls_adrc-region.
                          IF sy-subrc EQ 0.
                            TRANSLATE lw_t005u-bezei TO UPPER CASE.
                            CONCATENATE '"' lw_t005u-bezei '"'  INTO  lw_einv_api_hdr-state_code_b.
                            lw_einv_api_hdr-place_of_supply_b   =   lw_einv_api_hdr-state_code_b.                                           "State code    Y
                          ENDIF.
                        ENDIF.

                      ENDIF.


                      CONCATENATE '"' lw_adrc-tel_number+0(10) '"' INTO   lw_einv_api_hdr-phone_number_b.                                         "Phone or Mobile No.   N
                      CONCATENATE '"'   lw_adr6-smtp_addr '"' INTO  lw_einv_api_hdr-email_b.

                      IF lw_t001w-land1 NE lw_kna1-land1.
                        lw_einv_api_hdr-gstin_b = gc_gstin_urp.
                        READ TABLE gt_export INTO lw_export WITH KEY zport_code = gw_final-zport_code.
                        IF sy-subrc IS INITIAL.
                          CONCATENATE '"' lw_export-zport_address1 '"' INTO lw_einv_api_hdr-address1_b.
                          CONCATENATE '"' lw_export-zport_address2 '"' INTO lw_einv_api_hdr-address2_b.
                          CONCATENATE '"' lw_export-zport_place '"' INTO lw_einv_api_hdr-location_b.
                          lw_einv_api_hdr-pincode_b = lw_export-zport_pincode.

                          CLEAR:lw_t005u.
                          READ TABLE lt_state_code INTO ls_state_code WITH KEY regio = lw_export-zport_state.
                          IF sy-subrc EQ 0.
                            CONCATENATE '"' ls_state_code-gst_regio '"' INTO lw_einv_api_hdr-state_code_b.
                          ELSE.
                            READ TABLE gt_t005u INTO lw_t005u WITH  KEY bland = lw_export-zport_state.
                            IF sy-subrc EQ 0.
                              TRANSLATE lw_t005u-bezei TO UPPER CASE.
                              CONCATENATE '"' lw_t005u-bezei '"' INTO lw_einv_api_hdr-state_code_b.
                            ENDIF.
                          ENDIF.

                        ENDIF.
                        lw_einv_api_hdr-pincode_b = gc_export_pincode.
                        lw_einv_api_hdr-place_of_supply_b = gc_export_place.
                        lw_einv_api_hdr-state_code_b = gc_exp_state.
                      ENDIF.

                    ENDIF.
                  ENDIF.
**End of buyer_details


*begin of dispatch_details
                  CONCATENATE '"' lw_t001w-name1 '"' INTO lw_einv_api_hdr-comapny_name_d.
                  CONCATENATE '"' lw_t001w-name2 '"' INTO   lw_einv_api_hdr-address1_d .
                  CONCATENATE '"' lw_t001w-stras '"' INTO  lw_einv_api_hdr-address2_d .
                  CONCATENATE '"' lw_t001w-ort01 '"'  INTO     lw_einv_api_hdr-location_d .
                  IF lw_t001w-pstlz IS  NOT INITIAL.
                    lw_einv_api_hdr-pincode_d = lw_t001w-pstlz .
                  ENDIF.
                  CLEAR:lw_t005u_d.
                  READ TABLE gt_t005u_s INTO lw_t005u_s WITH  KEY bland = lw_t001w-regio.
                  IF sy-subrc EQ 0.
                    TRANSLATE lw_t005u_s-bezei TO UPPER CASE.
                    CONCATENATE '"' lw_t005u_s-bezei '"'  INTO lw_einv_api_hdr-state_code_d .
                  ENDIF.

* Dispatch address for ZTRD
                  IF lw_wb2_v_vbrk_vbrp2-fkart = 'ZTRD'.
                    lv_tdname = lw_wb2_v_vbrk_vbrp2-vbeln.
                    PERFORM readtext_value USING lv_tdname 'Z023' CHANGING lv_tdline.
                    IF lv_tdline IS NOT INITIAL.
                      lw_einv_api_hdr-comapny_name_d = lv_tdline.
                      lw_einv_api_hdr-dispatch = abap_true.
                      PERFORM readtext_value USING lv_tdname 'Z024' CHANGING lv_tdline.
                      IF lv_tdline IS NOT INITIAL.
                        lw_einv_api_hdr-address1_d = lv_tdline.
                      ENDIF.
                      lw_einv_api_hdr-address2_d = '""'.
                      PERFORM readtext_value USING lv_tdname 'Z025' CHANGING lv_tdline.
                      IF lv_tdline IS NOT INITIAL.
                        lw_einv_api_hdr-location_d = lv_tdline.
                      ENDIF.
                      PERFORM readtext_value USING lv_tdname 'Z026' CHANGING lv_tdline.
                      IF lv_tdline IS NOT INITIAL.
                        lw_einv_api_hdr-pincode_d = lv_tdline.
                      ENDIF.
                      PERFORM readtext_value USING lv_tdname 'Z027' CHANGING lv_tdline.
                      IF lv_tdline IS NOT INITIAL.
                        lw_einv_api_hdr-state_code_d = lv_tdline.
                      ENDIF.
                    ELSE.
                      SELECT SINGLE low FROM tvarvc INTO @DATA(lv_text) WHERE name = 'ZPORTCODE_TEXT'.
                      IF sy-subrc EQ 0.
                        DATA : lv_tdid TYPE tdid.
                        lv_tdid = lv_text.
                        PERFORM readtext_value USING lv_tdname lv_tdid CHANGING lv_tdline.
                        IF lv_tdline IS NOT INITIAL.
                          REPLACE ALL OCCURRENCES OF '"' IN lv_tdline WITH ''.
                          CONDENSE lv_tdline.
                          SELECT SINGLE * FROM ztedoc_export INTO @DATA(einv_export) WHERE zport_code = @lv_tdline.
                          IF sy-subrc EQ 0.
                            lw_einv_api_hdr-comapny_name_d = einv_export-zport.
                            lw_einv_api_hdr-dispatch = abap_true.
                            lw_einv_api_hdr-address1_d = einv_export-zport_address1.
                            lw_einv_api_hdr-location_d = einv_export-zport_place.
                            lw_einv_api_hdr-pincode_d = einv_export-zport_pincode.
                            lw_einv_api_hdr-state_code_d = einv_export-zport_state.
                          ELSE.
                            MESSAGE 'Please maintain port details in zeconfig.' TYPE 'E'.
                          ENDIF.
                        ENDIF.
                      ENDIF.
                    ENDIF.

                  ENDIF.
*begin of dispatch_details.
*
**Begin of ship_details

                  CLEAR:lw_vbpa.
                  READ TABLE gt_vbpa INTO lw_vbpa WITH  KEY vbeln = lw_wb2_v_vbrk_vbrp2-vbeln
                  parvw = gc_parvw_we
                  BINARY SEARCH.
                  IF sy-subrc EQ 0.
                    CLEAR:lw_kna1_d.
                    READ TABLE gt_kna1 INTO lw_kna1_d WITH  KEY kunnr = lw_vbpa-kunnr BINARY SEARCH.
                    IF sy-subrc EQ 0.
                      CONCATENATE '"'  lw_kna1_d-stcd3 '"' INTO lw_einv_api_hdr-gstin_sh .
                      CONCATENATE '"' lw_kna1_d-name1 '"' INTO lw_einv_api_hdr-legal_name_sh.
                      CONCATENATE '"' lw_kna1_d-name1 '"' INTO lw_einv_api_hdr-trade_name_sh.                                                           CONCATENATE '"' lw_kna1_d-name2 '"' INTO   lw_einv_api_hdr-address1_sh .
*                      CONCATENATE '"' lw_kna1_d-stras '"' INTO  lw_einv_api_hdr-address1_sh .
*                      CONCATENATE '"' lw_kna1_d-name2 '"' INTO  lw_einv_api_hdr-address2_sh .
*                      IF lw_kna1_d-name2 IS NOT INITIAL AND lw_kna1_d-stras IS INITIAL.
*                        lw_einv_api_hdr-address1_sh = lw_einv_api_hdr-address2_sh .
*                      ENDIF.
*                      CONCATENATE '"'  lw_kna1_d-ort01 '"'  INTO     lw_einv_api_hdr-location_sh .
*                      IF lw_kna1_d-pstlz IS  NOT INITIAL.
*                        lw_einv_api_hdr-pincode_sh = lw_kna1_d-pstlz.
*                      ELSE.
*                        lw_einv_api_hdr-pincode_sh = lw_einv_api_hdr-pincode_b .
*                      ENDIF.
*                      CLEAR:lw_t005u_d.
*                      READ TABLE gt_t005u INTO lw_t005u_d WITH  KEY bland = lw_kna1_d-regio.
*                      IF sy-subrc EQ 0.
*                        TRANSLATE lw_t005u_d-bezei TO UPPER CASE.
*                        CONCATENATE '"' lw_t005u_d-bezei '"' INTO  lw_einv_api_hdr-state_code_sh.
*                      ELSE.
*                        lw_einv_api_hdr-state_code_sh  = lw_einv_api_hdr-state_code_b.
*                      ENDIF.

                      READ TABLE gt_adrc_new INTO DATA(ls_adrc1) WITH KEY addrnumber = lw_vbpa-adrnr.
                      IF sy-subrc IS INITIAL.
                        CONCATENATE '"' ls_adrc1-street '"' INTO  lw_einv_api_hdr-address1_sh .
                        CONCATENATE '"' ls_adrc1-str_suppl3 '"' INTO  lw_einv_api_hdr-address2_sh .
                        CONCATENATE '"'  ls_adrc1-city1 '"'  INTO     lw_einv_api_hdr-location_sh .
                        IF ls_adrc1-post_code1 IS  NOT INITIAL.
                          lw_einv_api_hdr-pincode_sh = ls_adrc1-post_code1.
                        ELSE.
                          lw_einv_api_hdr-pincode_sh = lw_einv_api_hdr-pincode_b .
                        ENDIF.
                        CLEAR:lw_t005u_d.
                        READ TABLE lt_state_code INTO ls_state_code WITH KEY regio = ls_adrc1-region.
                        IF sy-subrc EQ 0.
                          CONCATENATE '"' ls_state_code-gst_regio '"' INTO  lw_einv_api_hdr-state_code_sh.
                        ELSE.
                          READ TABLE gt_t005u INTO lw_t005u_d WITH  KEY bland = ls_adrc1-region.
                          IF sy-subrc EQ 0.
                            TRANSLATE lw_t005u_d-bezei TO UPPER CASE.
                            CONCATENATE '"' lw_t005u_d-bezei '"' INTO  lw_einv_api_hdr-state_code_sh.
                          ELSE.
                            lw_einv_api_hdr-state_code_sh  = lw_einv_api_hdr-state_code_b.
                          ENDIF.
                        ENDIF.

                      ENDIF.




                      CLEAR lv_export.
                      IF lw_t001w-land1 NE lw_kna1_d-land1.
                        lv_export = abap_true.
                        lw_einv_api_hdr-gstin_sh = gc_gstin_urp.
                        READ TABLE gt_export INTO lw_export WITH KEY zport_code = gw_final-zport_code.
                        IF sy-subrc IS INITIAL.
                          CONCATENATE '"' lw_export-zport_address1 '"' INTO lw_einv_api_hdr-address1_sh.
                          CONCATENATE '"' lw_export-zport_address2 '"' INTO lw_einv_api_hdr-address2_sh.
                          CONCATENATE '"' lw_export-zport_place '"' INTO lw_einv_api_hdr-location_sh.
                          lw_einv_api_hdr-pincode_sh = lw_export-zport_pincode.

                          CLEAR:lw_t005u_d.
                          READ TABLE gt_t005u INTO lw_t005u_d WITH  KEY bland = lw_export-zport_state.
                          IF sy-subrc EQ 0.
                            TRANSLATE lw_t005u_d-bezei TO UPPER CASE.
                            CONCATENATE '"' lw_t005u_d-bezei '"' INTO lw_einv_api_hdr-state_code_sh.
                          ENDIF.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                  ENDIF.

*End of  ship_details

                  IF lw_kna1-kunnr NE lw_kna1_d-kunnr .
                    lw_einv_api_hdr-transaction_type = gc_ttype_shg.
                  ELSEIF lw_kna1-kunnr EQ lw_kna1_d-kunnr.
                    lw_einv_api_hdr-transaction_type = gc_ttype_reg.
                  ENDIF.

*Begin of export_details
                  IF lw_t001w-land1 NE lw_kna1-land1 AND 1 = 3.  "Seller and Buyer country is different, then its export
                    lw_einv_api_hdr-supply_type   = gc_sup_type_expwp.
                    lw_einv_api_hdr-ship_bill_number = lw_einv_api_hdr-document_number.
                    lw_einv_api_hdr-ship_bill_date = lw_einv_api_hdr-document_date.
                    CONCATENATE '"' lw_kna1-land1             '"' INTO lw_einv_api_hdr-country_code.
                    CONCATENATE '"' lw_wb2_v_vbrk_vbrp2-waerk '"' INTO lw_einv_api_hdr-foreign_currency.
                    CONCATENATE '"' ' '                       '"' INTO lw_einv_api_hdr-refund_claim.
                    CONCATENATE '"' ' '                       '"' INTO lw_einv_api_hdr-port_code  .
                  ELSE.
                    CONCATENATE  '"' ' ' '"' INTO lw_einv_api_hdr-ship_bill_number.
                    CONCATENATE  '"' ' ' '"' INTO lw_einv_api_hdr-ship_bill_date.
                    CONCATENATE  '"' ' ' '"' INTO lw_einv_api_hdr-country_code.
                    CONCATENATE  '"' ' ' '"' INTO lw_einv_api_hdr-foreign_currency.
                    CONCATENATE  '"' ' ' '"' INTO lw_einv_api_hdr-refund_claim.
                    CONCATENATE  '"' ' ' '"' INTO lw_einv_api_hdr-port_code  .
                  ENDIF.

                  IF lw_t001w-land1 NE lw_kna1-land1.
                    CONCATENATE '"' lw_kna1-land1 '"' INTO lw_einv_api_hdr-country_code.
                  ENDIF.
*End of export_details

**Begin of payment_details
                  CONCATENATE '"' ' ' '"' INTO lw_einv_api_hdr-bank_account_number.
                  CONCATENATE ' ' '0' ' ' INTO lw_einv_api_hdr-paid_balance_amount.
                  CONCATENATE ' ' '0' ' ' INTO lw_einv_api_hdr-credit_days.
                  CONCATENATE '"' ' ' '"' INTO lw_einv_api_hdr-credit_transfer.
                  CONCATENATE '"' ' ' '"' INTO lw_einv_api_hdr-direct_debit.
                  CONCATENATE '"' ' ' '"' INTO lw_einv_api_hdr-branch_or_ifsc.
                  CONCATENATE '"' ' ' '"' INTO lw_einv_api_hdr-payment_mode.
                  CONCATENATE '"' ' ' '"' INTO lw_einv_api_hdr-payee_name.
                  CONCATENATE '"' ' ' '"' INTO lw_einv_api_hdr-payment_due_date.
                  CONCATENATE '"' ' ' '"' INTO lw_einv_api_hdr-payment_instruction .
                  CONCATENATE '"' ' ' '"' INTO lw_einv_api_hdr-payment_term.

**End of payment_details


                  lv_tdname  = gw_final-vbeln.

                  CALL FUNCTION 'READ_TEXT'
                    EXPORTING
                      client                  = sy-mandt
                      id                      = gc_tdid_0002
                      language                = sy-langu
                      name                    = lv_tdname
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
                    CONCATENATE  '"' '' '"' INTO lw_einv_api_hdr-invoice_remarks. "Required
                  ELSE.
                    READ TABLE lt_lines INTO lw_lines INDEX 1.
                    CONCATENATE  '"' lw_lines-tdline '"' INTO lw_einv_api_hdr-invoice_remarks. "Required

                  ENDIF.

*Begin of reference_details
                  lw_einv_api_hdr-invoice_period_start_date = lw_einv_api_hdr-document_date.
                  lw_einv_api_hdr-invoice_period_end_date   = lw_einv_api_hdr-document_date.

**Begin of preceding_document_details
                  lw_einv_api_hdr-reference_of_original_invoice = lw_einv_api_hdr-document_number.
                  lw_einv_api_hdr-preceding_invoice_date        = lw_einv_api_hdr-document_date.
                  lw_einv_api_hdr-other_reference = '""'.
*End of preceding_document_details
*End of reference_details

*Begin of contract Details,
                  CLEAR: lw_vbfa, lw_wb2_v_vbak_vbap2.
                  READ TABLE gt_vbfa INTO lw_vbfa WITH KEY vbeln = gw_final-vbeln.
                  IF sy-subrc EQ 0.
                    READ TABLE gt_wb2_v_vbak_vbap2 INTO lw_wb2_v_vbak_vbap2 WITH KEY vbeln = lw_vbfa-vbelv
                    posnr_i = lw_vbfa-posnv
                    BINARY SEARCH.
                    IF sy-subrc EQ 0.
                      CONCATENATE '"' lw_wb2_v_vbak_vbap2-vbeln  '"' INTO    lw_einv_api_hdr-receipt_advice_number .
                      IF lw_wb2_v_vbak_vbap2-audat IS NOT INITIAL.
                        CONCATENATE '"' lw_wb2_v_vbak_vbap2-audat+6(2) '/'
                        lw_wb2_v_vbak_vbap2-audat+4(2) '/'
                        lw_wb2_v_vbak_vbap2-audat+0(4)
                        '"' INTO    lw_einv_api_hdr-receipt_advice_date .
                      ENDIF.
                      CONCATENATE '"' lw_wb2_v_vbrk_vbrp2-charg_i '"' INTO    lw_einv_api_hdr-batch_reference_number.
                      CONCATENATE '"' lw_wb2_v_vbak_vbap2-bname '"' INTO    lw_einv_api_hdr-contract_reference_number.
                      CONCATENATE '"' lw_wb2_v_vbak_vbap2-ihrez '"' INTO    lw_einv_api_hdr-other_reference_c.
                      lw_einv_api_hdr-project_reference_number = '""'.
                      CONCATENATE '"' lw_wb2_v_vbak_vbap2-bstnk '"' INTO    lw_einv_api_hdr-vendor_po_reference_number.

                      IF lw_wb2_v_vbak_vbap2-bstdk IS NOT INITIAL.
                        CONCATENATE '"' lw_wb2_v_vbak_vbap2-bstdk+6(2) '/'
                        lw_wb2_v_vbak_vbap2-bstdk+4(2) '/'
                        lw_wb2_v_vbak_vbap2-bstdk+0(4)
                        '"' INTO    lw_einv_api_hdr-vendor_po_reference_date.
                      ELSE.
                        lw_einv_api_hdr-vendor_po_reference_date = '""'.
                      ENDIF.
                      "End of contract_details
                    ENDIF.
                  ENDIF.
*Begin of'"additional_document_details"
                  lw_einv_api_hdr-supporting_document_url = '""'.
                  lw_einv_api_hdr-supporting_document     = '""'.
                  lw_einv_api_hdr-additional_information  = '""'.


*End of'"additional_document_details"

*Begin of value_details


********************* dafault all values to 0 ******************************
                  lw_einv_api_hdr-total_assessable_value          = '0'.
                  lw_einv_api_hdr-total_cgst_value                = '0'.
                  lw_einv_api_hdr-total_sgst_value                = '0'.
                  lw_einv_api_hdr-total_igst_value                = '0'.
                  lw_einv_api_hdr-total_cess_value                = '0'.
                  lw_einv_api_hdr-total_cess_nonadvol_value       = '0'.
                  lw_einv_api_hdr-total_invoice_value             = '0'.
                  lw_einv_api_hdr-total_cess_value_of_state       = '0'.
                  lw_einv_api_hdr-round_off_amount                = '0'.
                  lw_einv_api_hdr-total_invoice_value_additional  = '0'.
                  lw_einv_api_hdr-total_other_charge              = '0'.

********************* dafault all values to 0 ******************************

                  lw_einv_api_hdr-total_assessable_value = '1'.               "Required


****************************************************************************************************************************************************
*Begin of item_list


********************* dafault all values to 0 ******************************
                  lw_einv_api_itm-quantity                    = '0'.
                  lw_einv_api_itm-free_quantity               = '0'.
                  lw_einv_api_itm-unit_price                  = '0'.
                  lw_einv_api_itm-total_amount                = '0'.
                  lw_einv_api_itm-pre_tax_value               = '0'.
                  lw_einv_api_itm-discount                    = '0'.
                  lw_einv_api_itm-other_charge                = '0'.
                  lw_einv_api_itm-gst_rate                    = '0'.
                  lw_einv_api_itm-igst_amount                 = '0'.
                  lw_einv_api_itm-cgst_amount                 = '0'.
                  lw_einv_api_itm-sgst_amount                 = '0'.
                  lw_einv_api_itm-cess_rate                   = '0'.
                  lw_einv_api_itm-cess_amount                 = '0'.
                  lw_einv_api_itm-cess_nonadvol_value         = '0'.
                  lw_einv_api_itm-state_cess_rate             = '0'.
                  lw_einv_api_itm-state_cess_amount           = '0'.
                  lw_einv_api_itm-state_cess_nonadvol_amount  = '0'.
                  lw_einv_api_itm-total_item_value            = '0'.


********************* dafault all values to 0 ******************************
* check for export currency
                  CLEAR lv_exp_curr.
                  IF lw_wb2_v_vbrk_vbrp2-waerk NE  gc_curr_inr.
                    lv_exp_curr = abap_true.
                  ENDIF.


                  lw_einv_api_itm-bukrs  = lw_wb2_v_vbrk_vbrp2-bukrs.
                  lw_einv_api_itm-doc_year = gw_final-gjahr.
                  lw_einv_api_itm-doc_type = lw_wb2_v_vbrk_vbrp2-fkart.
                  lw_einv_api_itm-vbeln = lw_wb2_v_vbrk_vbrp2-vbeln.
                  lw_einv_api_itm-document_type = lw_wb2_v_vbrk_vbrp2-fkart  .
                  lw_einv_api_itm-document_number = lw_einv_api_hdr-document_number   .
                  SHIFT lw_einv_api_itm-document_number LEFT DELETING LEADING '0'.
                  IF lw_wb2_v_vbrk_vbrp2-fkdat IS NOT INITIAL.
                    CONCATENATE  lw_wb2_v_vbrk_vbrp2-fkdat+6(2) '/'
                    lw_wb2_v_vbrk_vbrp2-fkdat+4(2) '/'
                    lw_wb2_v_vbrk_vbrp2-fkdat+0(4)
                    INTO lw_einv_api_itm-document_date.
                  ENDIF.
                  CONCATENATE '"' lw_einv_api_itm-document_type '"' INTO  lw_einv_api_itm-document_type.
                  CONCATENATE '"' lw_einv_api_itm-document_date '"' INTO  lw_einv_api_itm-document_date.


                  lv_posnr = lv_posnr + 1.
                  CONCATENATE '"'  lv_posnr '"' INTO lw_einv_api_itm-item_serial_number .
                  REPLACE ALL OCCURRENCES OF '"' IN lw_wb2_v_vbrk_vbrp2-arktx_i WITH space.
                  CONCATENATE '"'  lw_wb2_v_vbrk_vbrp2-arktx_i '"' INTO lw_einv_api_itm-product_description .
***** ---logic for hsncode based on billing type----added by raghu on 27.03.2023****
                  CLEAR lv_hsn.
                  IF lw_wb2_v_vbrk_vbrp2-fkart IN lr_fkart.
                    lv_tdname  = gw_final-vbeln.
                    CALL FUNCTION 'READ_TEXT'
                      EXPORTING
                        client                  = sy-mandt
                        id                      = 'Z021'
                        language                = sy-langu
                        name                    = lv_tdname
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
                    IF sy-subrc = 0.
                      READ TABLE lt_lines INTO lw_lines INDEX 1.
                      IF lw_lines-tdline IS NOT INITIAL.
                        CONCATENATE  '"' lw_lines-tdline '"' INTO lw_einv_api_itm-hsn_code.
                        lv_hsn = lw_lines-tdline.
                      ENDIF.
                    ENDIF.
                  ENDIF.


                  IF lw_einv_api_itm-hsn_code IS INITIAL.
                    READ TABLE gt_marc INTO lw_marc WITH KEY matnr = lw_wb2_v_vbrk_vbrp2-matnr_i
                    werks = lw_wb2_v_vbrk_vbrp2-werks_i
                    BINARY SEARCH.
                    IF sy-subrc EQ 0.
                      CONCATENATE '"'  lw_marc-steuc  '"' INTO lw_einv_api_itm-hsn_code.
                      lv_hsn = lw_marc-steuc.
                    ENDIF.
                  ENDIF.
******------------end-----------------------------*****
                  IF lw_wb2_v_vbrk_vbrp2-pstyv_i EQ gc_pstyv_tad.
                    lw_einv_api_itm-is_service = gc_service_y.
                  ELSEIF  lv_hsn+0(2) = '99'.
                    lw_einv_api_itm-is_service = gc_service_y.
                  ELSE.
                    lw_einv_api_itm-is_service = gc_service_n.
                  ENDIF.

                  lw_einv_api_itm-bar_code =  '""'.
                  "Quantity
                  lw_einv_api_itm-quantity = lw_wb2_v_vbrk_vbrp2-fkimg_i . "'1'.
                  lw_einv_api_itm-free_quantity = '0'.

*Unit
                  CLEAR lv_output_uom.
                  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                    EXPORTING
                      input          = lw_wb2_v_vbrk_vbrp2-vrkme_i
                    IMPORTING
                      output         = lv_output_uom
                    EXCEPTIONS
                      unit_not_found = 1
                      OTHERS         = 2.
                  IF sy-subrc <> 0.
* Implement suitable error handling here
                  ENDIF.

                  READ TABLE gt_doc_uom INTO ls_doc_uom WITH KEY meins = lv_output_uom.
                  IF sy-subrc EQ 0.
                    IF strlen( ls_doc_uom-edoc_uom ) < 3.
                      ls_doc_uom-edoc_uom = 'OTH'.
                    ENDIF.
                    CONCATENATE '"' ls_doc_uom-edoc_uom '"' INTO lw_einv_api_itm-unit.
                  ELSE.
                    READ TABLE gt_doc_uom INTO ls_doc_uom WITH KEY meins =  lw_wb2_v_vbrk_vbrp2-vrkme_i.
                    IF sy-subrc IS INITIAL.
                      IF strlen( ls_doc_uom-edoc_uom ) < 3.
                        ls_doc_uom-edoc_uom = 'OTH'.
                      ENDIF.
                      CONCATENATE '"' ls_doc_uom-edoc_uom '"' INTO lw_einv_api_itm-unit.
                    ELSE.
                      IF strlen( lw_wb2_v_vbrk_vbrp2-vrkme_i ) < 3.
                        lw_wb2_v_vbrk_vbrp2-vrkme_i = 'OTH'.
                      ENDIF.
                      CONCATENATE '"' lw_wb2_v_vbrk_vbrp2-vrkme_i '"' INTO  lw_einv_api_itm-unit.
                    ENDIF.
                  ENDIF.

* Unit Price
                  IF lw_wb2_v_vbrk_vbrp2-fkimg_i IS NOT INITIAL  .
                    CLEAR:lv_rate,lv_dmbtr,lv_item_amt.
**                    LOOP AT gt_tvarvc INTO lw_tvarvc WHERE name = gc_var_unit.
**                      READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
**                                                                                 kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
**                                                                                 kschl = lw_tvarvc-low.
**                      IF sy-subrc IS INITIAL.
**                        EXIT.
**                      ENDIF.
**                    ENDLOOP.
**                    IF lw_konv IS NOT INITIAL.
**                      CLEAR:lv_rate,lv_dmbtr,lv_item_amt.
**                      IF lv_exp_curr IS NOT INITIAL.
***
**                        lv_rate = lw_konv-kbetr * lw_wb2_v_vbrk_vbrp2-kursk_i.
**                        lw_einv_api_itm-unit_price = lv_rate.
**                        lv_dmbtr = lw_wb2_v_vbrk_vbrp2-fkimg_i * lv_rate.
**                        lv_item_amt = ( ( lw_wb2_v_vbrk_vbrp2-netwr_i + lw_wb2_v_vbrk_vbrp2-mwsbp_i ) *
**                                       lw_wb2_v_vbrk_vbrp2-kursk_i )."  "lw_bseg-dmbtr.
**                      ELSE.
**
**                        lv_rate = lw_konv-kbetr.
**                        lw_einv_api_itm-unit_price = lv_rate.
**                        lv_dmbtr = lw_wb2_v_vbrk_vbrp2-fkimg_i * lv_rate.
**                        lw_einv_api_itm-total_amount = lv_dmbtr.
**                        lv_item_amt = lw_wb2_v_vbrk_vbrp2-netwr_i + lw_wb2_v_vbrk_vbrp2-mwsbp_i.
**                      ENDIF.
**                    ENDIF.
*

                    IF lw_wb2_v_vbrk_vbrp2-fkart = 'ZSAM'.
                      READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                      kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                      kschl = 'ZSAM'.
                      IF sy-subrc IS INITIAL.
                        lw_wb2_v_vbrk_vbrp2-netwr_i = lw_konv-kwert.
                      ENDIF.
                    ENDIF.


                    READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                    kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                    kschl = 'ZCES'.
                    IF sy-subrc IS INITIAL.
                      lw_wb2_v_vbrk_vbrp2-netwr_i  = lw_wb2_v_vbrk_vbrp2-netwr_i  -  lw_konv-kwert.
                    ENDIF.
                    DATA lv_diff TYPE kschl VALUE IS INITIAL.
                    CLEAR lv_diff.
*****                       ROund OFf LOgic Added BY Deep On 05.12.2023
                    lv_diff = 'DIFF'.
                    LOOP AT  gt_konv INTO lw_konv WHERE knumv = lw_wb2_v_vbrk_vbrp2-knumv
                    AND kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                    AND kschl EQ lv_diff.

*                    IF sy-subrc EQ 0.
                      lw_wb2_v_vbrk_vbrp2-netwr_i = lw_wb2_v_vbrk_vbrp2-netwr_i - lw_konv-kwert.
                      lv_total_roundoff = lv_total_roundoff + lw_konv-kwert.
*                        lw_einv_api_hdr-round_off_amount = lw_einv_api_hdr-round_off_amount + lw_konv-kwert.
*                    ENDIF.
                    ENDLOOP.

                    IF lv_exp_curr IS NOT INITIAL.

                      lv_rate = lw_wb2_v_vbrk_vbrp2-netwr_i / lw_wb2_v_vbrk_vbrp2-fkimg_i.
                      lw_einv_api_itm-unit_price = lv_rate.

                      PERFORM convert_currency USING lw_einv_api_itm-unit_price lw_wb2_v_vbrk_vbrp2-kurrf
                      CHANGING lw_einv_api_itm-unit_price.

                      lw_einv_api_itm-total_amount = lw_wb2_v_vbrk_vbrp2-netwr_i.
                      PERFORM convert_currency USING lw_einv_api_itm-total_amount lw_wb2_v_vbrk_vbrp2-kurrf
                      CHANGING lw_einv_api_itm-total_amount.

                      lv_item_amt = lw_wb2_v_vbrk_vbrp2-netwr_i + lw_wb2_v_vbrk_vbrp2-mwsbp_i.

                      PERFORM convert_currency USING lv_item_amt lw_wb2_v_vbrk_vbrp2-kurrf
                      CHANGING lv_item_amt.


                    ELSE.



                      lv_rate = lw_wb2_v_vbrk_vbrp2-netwr_i / lw_wb2_v_vbrk_vbrp2-fkimg_i.
                      lw_einv_api_itm-unit_price = lv_rate.
                      lw_einv_api_itm-total_amount = lw_wb2_v_vbrk_vbrp2-netwr_i.
                      lv_item_amt = lw_wb2_v_vbrk_vbrp2-netwr_i + lw_wb2_v_vbrk_vbrp2-mwsbp_i.
                    ENDIF.

                    CONDENSE:lw_einv_api_itm-unit_price,lw_einv_api_itm-total_amount.
                  ENDIF.
                  lw_einv_api_itm-pre_tax_value =  '0'.
                  CONDENSE:lw_einv_api_itm-pre_tax_value.

                  "Discount amount
                  LOOP AT gt_tvarvc INTO lw_tvarvc WHERE name = gc_var_disc.
                    READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                    kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                    kschl = lw_tvarvc-low
                    BINARY SEARCH.

                    IF sy-subrc EQ 0.
                      lw_konv-kwert = lw_konv-kwert * -1.
                      IF lv_exp_curr IS NOT INITIAL.
                        lw_konv-kwert = lw_konv-kwert * lw_wb2_v_vbrk_vbrp2-kurrf.
                      ENDIF.

                      lw_einv_api_itm-discount =  lw_einv_api_itm-discount + lw_konv-kwert.
                    ENDIF.
                  ENDLOOP.

                  IF lw_einv_api_itm-discount IS NOT INITIAL AND lv_exp_curr IS NOT INITIAL.
                    PERFORM convert_currency USING lw_einv_api_itm-discount lw_wb2_v_vbrk_vbrp2-kurrf
                    CHANGING lw_einv_api_itm-discount.
                  ENDIF.
                  CONDENSE:lw_einv_api_itm-discount.

                  lw_einv_api_itm-other_charge =  '0'.
                  lw_einv_api_itm-assessable_value = lw_einv_api_itm-total_amount -
                  lw_einv_api_itm-discount. "Required
                  CONDENSE:lw_einv_api_itm-assessable_value,lw_einv_api_itm-other_charge.
                  LOOP AT gt_konv INTO lw_konv WHERE knumv = lw_wb2_v_vbrk_vbrp2-knumv
                  AND kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                  AND kschl = 'ZTDS' .
                    lw_einv_api_itm-assessable_value = lw_einv_api_itm-assessable_value - lw_konv-kwert.
                    lw_einv_api_itm-total_amount = lw_einv_api_itm-total_amount - lw_konv-kwert.
                  ENDLOOP.
                  lv_total_assessable_value = lv_total_assessable_value  + lw_einv_api_itm-assessable_value.

                  CLEAR:lw_gstrate.

                  IF lw_wb2_v_vbrk_vbrp2-fkart NE 'ZSTI'.
                    IF lw_wb2_v_vbrk_vbrp2-fkart NE 'ZSAM'.

                      "IGST Tax amount
                      READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                      kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                      kschl = gc_kschl_joig
                      BINARY SEARCH.
                      IF sy-subrc IS NOT INITIAL.
                        READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                        kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                        kschl = 'ZIGS'
                        BINARY SEARCH.
                      ENDIF.
                      IF sy-subrc EQ 0.
                        IF lv_exp_curr IS NOT INITIAL.
                          PERFORM convert_currency USING lw_konv-kwert lw_wb2_v_vbrk_vbrp2-kurrf
                          CHANGING lw_konv-kwert.
                        ENDIF.

                        lw_gstrate =  lw_konv-kbetr. "Required
                        lw_einv_api_itm-igst_amount =  lw_konv-kwert.
                        CONDENSE:lw_einv_api_itm-gst_rate,lw_einv_api_itm-igst_amount.
                        lv_total_igst_value = lv_total_igst_value  + lw_einv_api_itm-igst_amount.
                      ENDIF.

                      "CGST Tax amount
                      READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                      kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                      kschl = gc_kschl_jocg
                      BINARY SEARCH.
                      IF sy-subrc IS NOT INITIAL.
                        READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                        kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                        kschl = 'ZCGS'
                        BINARY SEARCH.
                      ENDIF.
                      IF sy-subrc EQ 0.
                        IF lv_exp_curr IS NOT INITIAL.
                          PERFORM convert_currency USING lw_konv-kwert lw_wb2_v_vbrk_vbrp2-kurrf
                          CHANGING lw_konv-kwert.
                        ENDIF.
                        lw_einv_api_itm-gst_rate =  lw_konv-kbetr. "Required
                        CONDENSE lw_einv_api_itm-gst_rate.
                        ADD lw_einv_api_itm-gst_rate TO lw_gstrate.
                        lw_einv_api_itm-cgst_amount =  lw_konv-kwert.
                        CONDENSE:lw_einv_api_itm-gst_rate ,lw_einv_api_itm-cgst_amount.
                        lv_total_cgst_value = lv_total_cgst_value  + lw_einv_api_itm-cgst_amount.
                      ENDIF.

                      "SGST Tax amount
                      READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                      kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                      kschl = gc_kschl_josg
                      BINARY SEARCH.
                      IF sy-subrc IS NOT INITIAL.
                        "UGST Tax amount
                        READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                        kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                        kschl = gc_kschl_joug
                        BINARY SEARCH.
                        IF sy-subrc IS NOT INITIAL.
                          READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                          kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                          kschl = 'ZSGS'
                          BINARY SEARCH.
                        ENDIF.
                      ENDIF.
                      IF sy-subrc EQ 0.

                        IF lv_exp_curr IS NOT INITIAL.
                          PERFORM convert_currency USING lw_konv-kwert lw_wb2_v_vbrk_vbrp2-kurrf
                          CHANGING lw_konv-kwert.
                        ENDIF.

                        lw_einv_api_itm-gst_rate =  lw_konv-kbetr. "Required
                        CONDENSE lw_einv_api_itm-gst_rate.
                        ADD lw_einv_api_itm-gst_rate TO lw_gstrate.
                        lw_einv_api_itm-sgst_amount =  lw_konv-kwert.
                        CONDENSE:lw_einv_api_itm-gst_rate,lw_einv_api_itm-sgst_amount.
                        lv_total_sgst_value = lv_total_sgst_value  + lw_einv_api_itm-sgst_amount.
                      ENDIF.

                      "CESS
                      READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                      kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                      kschl = gc_kschl_cess
                      BINARY SEARCH.
                      IF sy-subrc EQ 0.
                        IF lv_exp_curr IS NOT INITIAL.
                          PERFORM convert_currency USING lw_konv-kwert lw_wb2_v_vbrk_vbrp2-kurrf
                          CHANGING lw_konv-kwert.
                        ENDIF.
                        lw_einv_api_itm-cess_rate =  lw_konv-kbetr / 10. "Required
                        lw_einv_api_itm-cess_amount =  lw_konv-kwert.
                        CONDENSE:lw_einv_api_itm-cess_rate,lw_einv_api_itm-cess_amount.
                        lv_total_cess_value = lv_total_cess_value  + lw_einv_api_itm-cess_amount.
                      ENDIF.

                      "cess_nonadvol_amount
                      READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                      kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                      kschl = 'ZCES'
                      BINARY SEARCH.
                      IF sy-subrc EQ 0.
                        IF lv_exp_curr IS NOT INITIAL.
                          PERFORM convert_currency USING lw_konv-kwert lw_wb2_v_vbrk_vbrp2-kurrf
                          CHANGING lw_konv-kwert.
                        ENDIF.
                        lw_einv_api_itm-cess_nonadvol_value =  lw_konv-kwert.
                        CONDENSE:lw_einv_api_itm-cess_nonadvol_value.
*                    lv_total_cess_nonadvol_value = lv_total_cess_nonadvol_value  + lw_einv_api_itm-cess_nonadvol_value.
                        lv_total_cess_value = lv_total_cess_value  + lw_einv_api_itm-cess_nonadvol_value.
                      ENDIF.

*                  TCS Charges JTC1'

                      LOOP AT gt_konv INTO lw_konv WHERE knumv = lw_wb2_v_vbrk_vbrp2-knumv
                      AND kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                      AND ( kschl = gc_kschl_jtc1 OR kschl = gc_kschl_ztcs OR kschl = 'JTC2' ).

                        lv_total_tcs_value = lv_total_tcs_value + lw_konv-kwert.

                      ENDLOOP.

                    ENDIF.
                  ENDIF.


                  lw_einv_api_itm-total_item_value =  ( lw_einv_api_itm-assessable_value +
                  lw_einv_api_itm-cgst_amount +
                  lw_einv_api_itm-sgst_amount +
                  lw_einv_api_itm-cess_amount +
                  lw_einv_api_itm-igst_amount +
                  lw_einv_api_itm-state_cess_amount +
                  lw_einv_api_itm-cess_nonadvol_value ).

                  CONDENSE:lw_einv_api_itm-total_item_value.

* round off value
                  CLEAR lv_roundoff.
*                  IF lv_item_amt NE lw_einv_api_itm-total_item_value.
*                    lv_roundoff = lv_item_amt - lw_einv_api_itm-total_item_value.
*                  ENDIF.


                  lv_total_invoice_value = lv_total_invoice_value +
                  lw_einv_api_itm-total_item_value +
                  lv_roundoff.


                  IF lv_total_roundoff < 0.
                    lv_total_roundoff  = lv_total_roundoff * -1.
                    lw_einv_api_hdr-round_off_amount  = lv_total_roundoff.
                    CONCATENATE '-' lw_einv_api_hdr-round_off_amount INTO lw_einv_api_hdr-round_off_amount.
                  ELSE.
                    lw_einv_api_hdr-round_off_amount  = lv_total_roundoff.
                  ENDIF.
                  CONDENSE lw_einv_api_hdr-round_off_amount NO-GAPS.



                  CONCATENATE '"' lw_wb2_v_vbrk_vbrp2-land1 '"' INTO lw_einv_api_itm-country_origin."'"52"'.
                  "get billing item text object VBBP
                  CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
                    EXPORTING
                      input  = lw_wb2_v_vbrk_vbrp2-matnr_i
                    IMPORTING
                      output = lw_wb2_v_vbrk_vbrp2-matnr_i.

                  CONCATENATE '"' lw_wb2_v_vbrk_vbrp2-matnr_i '"' INTO lw_einv_api_itm-order_line_reference.
                  CONCATENATE '"' lw_wb2_v_vbrk_vbrp2-matnr_i '"' INTO lw_einv_api_itm-product_serial_number.

*             '"batch details":' '{'
                  CONCATENATE '"' lw_wb2_v_vbrk_vbrp2-charg_i '"' INTO lw_einv_api_itm-name." =   '"aaa"'.  "Required
                  lw_einv_api_itm-expiry_date =  '" "'.
                  lw_einv_api_itm-warranty_date = '" "'.

*Begin of  attribute_details"
                  lw_einv_api_itm-item_attribute_details = gc_attr_einv.
                  lw_einv_api_itm-item_attribute_value = gc_attr_einv.
*End of of  attribute_details"
*End of item_list
                  IF lw_einv_api_itm-hsn_code IS INITIAL.
                    lw_einv_api_itm-hsn_code = '9917'.
                  ENDIF.
                  IF lw_einv_api_itm-other_charge < 0.
                    lw_einv_api_itm-other_charge = lw_einv_api_itm-other_charge * -1.
                  ENDIF.
                  lw_einv_api_itm-gst_rate = lw_gstrate.
                  CONDENSE:lw_einv_api_itm-gst_rate,lw_einv_api_itm-quantity.
                  IF lw_einv_api_itm-quantity IS INITIAL.
                    CONTINUE.
                  ENDIF.
                  IF lw_einv_api_itm-total_amount EQ 0.
                    CLEAR:lw_einv_api_itm.
                    CONTINUE.
                  ENDIF.

                  IF lw_wb2_v_vbrk_vbrp2-fkart = 'ZJEP'.
                    IF lw_einv_api_itm-quantity NE 0.

                      lv_total_assessable_value = lv_total_assessable_value  - lw_einv_api_itm-assessable_value.
                      lv_total_invoice_value = lv_total_invoice_value  - lw_einv_api_itm-total_item_value.

                      lv_unit_price = lw_einv_api_itm-total_amount / lw_einv_api_itm-quantity.
                      lw_einv_api_itm-unit_price = lv_unit_price.
                      lw_einv_api_itm-total_amount = lv_unit_price * lw_einv_api_itm-quantity.
                      lw_einv_api_itm-assessable_value = lw_einv_api_itm-total_amount.
                      lw_einv_api_itm-total_item_value = lw_einv_api_itm-total_amount.
                      CONDENSE : lw_einv_api_itm-assessable_value , lw_einv_api_itm-total_amount ,lw_einv_api_itm-unit_price ,
                        lw_einv_api_itm-total_item_value.
                      lv_total_assessable_value = lv_total_assessable_value  + lw_einv_api_itm-assessable_value.
                      lv_total_invoice_value = lv_total_invoice_value  + lw_einv_api_itm-total_item_value.
*                      lv_total_
                    ENDIF.
                  ENDIF.

                  APPEND lw_einv_api_itm TO gt_einv_api_itm.
                  CLEAR:lw_einv_api_itm .

                ENDLOOP.

* Set supply type
                IF lv_export IS NOT INITIAL AND lv_total_igst_value IS INITIAL.
                  lw_einv_api_hdr-supply_type = gc_sup_type_expwop.
                ENDIF.

                IF  lw_einv_api_hdr-supply_type   = gc_sup_type_sezwp.
                  IF lv_total_igst_value IS INITIAL AND lv_total_cgst_value IS INITIAL AND
                  lv_total_sgst_value IS INITIAL.
                    lw_einv_api_hdr-supply_type   = gc_sup_type_sezwop.
                  ENDIF.
                ENDIF.

                lw_einv_api_hdr-total_assessable_value = lv_total_assessable_value.
                lw_einv_api_hdr-total_cgst_value =       lv_total_cgst_value.
                lw_einv_api_hdr-total_sgst_value =  lv_total_sgst_value.
                lw_einv_api_hdr-total_igst_value =   lv_total_igst_value .

                lw_einv_api_hdr-total_cess_value =  lv_total_cess_value.
                lw_einv_api_hdr-total_cess_nonadvol_value =  lv_total_cess_nonadvol_value.
                lw_einv_api_hdr-total_other_charge  = lv_total_tcs_value.

                lw_einv_api_hdr-total_invoice_value =  lv_total_invoice_value + lw_einv_api_hdr-total_other_charge + lw_einv_api_hdr-round_off_amount.
                IF lw_einv_api_hdr-total_other_charge < 0.
                  lw_einv_api_hdr-total_other_charge = lw_einv_api_hdr-total_other_charge * -1.
                ENDIF.
                CONDENSE:lw_einv_api_hdr-total_assessable_value,lw_einv_api_hdr-total_cgst_value,lw_einv_api_hdr-total_sgst_value,lw_einv_api_hdr-total_igst_value,
                lw_einv_api_hdr-total_invoice_value,lw_einv_api_hdr-total_cess_value,lw_einv_api_hdr-total_cess_nonadvol_value,lw_einv_api_hdr-total_other_charge .
                APPEND lw_einv_api_hdr TO gt_einv_api_hdr.
                CLEAR:lw_einv_api_hdr.
              ENDIF.
            ENDIF.

          ELSEIF p_mod = gc_fi.


            READ TABLE gt_bseg INTO gw_bseg WITH KEY belnr = gw_final-vbeln
            koart = 'D'.
            IF sy-subrc IS NOT INITIAL.
              READ TABLE gt_bseg INTO gw_bseg WITH KEY belnr = gw_final-vbeln
              koart = 'K'.
            ENDIF.
            IF sy-subrc = 0.
              lv_index = sy-tabix.
              lv_total_assessable_value    = 0.
              lv_total_cgst_value          = 0.
              lv_total_sgst_value          = 0.
              lv_total_igst_value          = 0.
              lv_total_invoice_value       = 0.
              lv_total_cess_value          = 0.
              lv_total_cess_nonadvol_value = 0.
              CLEAR:lw_einv_api_hdr-total_assessable_value,lw_einv_api_hdr-total_cgst_value,lw_einv_api_hdr-total_sgst_value,lw_einv_api_hdr-total_igst_value,
              lw_einv_api_hdr-total_invoice_value,lw_einv_api_hdr-total_cess_value,lw_einv_api_hdr-total_cess_nonadvol_value,
              lw_gstrate.

              CLEAR: lv_total_assessable_value,lv_total_igst_value,  lv_total_cgst_value,lv_total_sgst_value, lv_total_cess_value,lv_total_cess_nonadvol_value, lv_total_invoice_value.

              READ TABLE gt_bkpf INTO gw_bkpf WITH KEY bukrs = gw_bseg-bukrs
              belnr = gw_bseg-belnr
              gjahr = gw_bseg-gjahr.
              IF sy-subrc IS INITIAL.
                lw_einv_api_hdr-vbeln  = gw_bseg-belnr.
                lw_einv_api_hdr-bukrs  = gw_bseg-bukrs.
                lw_einv_api_hdr-doc_year = gw_bseg-gjahr.
                lw_einv_api_hdr-doc_type = gw_bkpf-blart.

*Data to send to the API IN JSON Scheme
                READ TABLE gt_api  INTO lw_usrgstin WITH KEY apiid = gc_apiid_usrgstin.
                IF sy-subrc IS INITIAL.
                  lw_einv_api_hdr-user_gstin   = lw_usrgstin-apiprov.
                ELSE.
                  lw_einv_api_hdr-user_gstin   =  gw_final-sup_gstin.
                ENDIF.
                CONCATENATE  '"' lw_einv_api_hdr-user_gstin '"' INTO lw_einv_api_hdr-user_gstin.

                lw_einv_api_hdr-supply_type   = gc_sup_type_b2b.

                lw_einv_api_hdr-charge_type = gc_charge_n.
                lw_einv_api_hdr-ecommerce_gstin = '""'.

                lw_einv_api_hdr-document_number = gw_final-odnno.
*                lw_einv_api_hdr-document_number = gw_bkpf-belnr.
                SHIFT lw_einv_api_hdr-document_number LEFT DELETING LEADING '0'.
                IF gw_bkpf-budat IS NOT INITIAL.
                  CONCATENATE  gw_bkpf-budat+6(2) '/'
                  gw_bkpf-budat+4(2) '/'
                  gw_bkpf-budat+0(4)
                  INTO lw_einv_api_hdr-document_date.
                ENDIF.

                READ TABLE gt_doctyp INTO lw_doctyp WITH KEY  bukrs = gw_bkpf-bukrs
                fkart = gw_bkpf-blart
                zmodule = gc_fi.
                IF sy-subrc = 0.
                  lw_einv_api_hdr-document_type = lw_doctyp-edoc_type.
                ELSE.
                  lw_einv_api_hdr-document_type = gc_docty_inv.
                ENDIF.
                CONCATENATE '"' lw_einv_api_hdr-document_type '"' INTO  lw_einv_api_hdr-document_type.
                CONCATENATE '"' lw_einv_api_hdr-document_number '"' INTO  lw_einv_api_hdr-document_number.
                CONCATENATE '"' lw_einv_api_hdr-document_date '"' INTO  lw_einv_api_hdr-document_date.

**Begin of seller_details
                CLEAR: lw_gstin, lw_adrc.
                lw_einv_api_hdr-gstin_s   =  gw_final-sup_gstin.
                CONCATENATE '"' lw_einv_api_hdr-gstin_s '"' INTO lw_einv_api_hdr-gstin_s.

                READ TABLE gt_gstin INTO lw_gstin WITH KEY bukrs = gw_bseg-bukrs
                branch = gw_bseg-bupla.
                IF sy-subrc EQ 0.

                  READ TABLE gt_adrc1 INTO lw_adrc1 WITH  KEY addrnumber = lw_gstin-adrnr.
                  IF sy-subrc IS INITIAL.
                    CONCATENATE '"'   lw_adrc1-name1  '"' INTO   lw_einv_api_hdr-legal_name_s.
                    CONCATENATE '"'   lw_adrc1-name1  '"' INTO   lw_einv_api_hdr-trade_name_s.
                    IF lw_adrc1-name2 IS NOT INITIAL.
                      CONCATENATE '"'   lw_adrc1-name2  '"' INTO   lw_einv_api_hdr-address1_s .
                    ELSE.

                      CONCATENATE '"'   lw_adrc1-name1  '"' INTO   lw_einv_api_hdr-address1_s .

                    ENDIF.
                    CONCATENATE '"'   lw_adrc1-street  '"' INTO   lw_einv_api_hdr-address2_s.
                    CONCATENATE '"'   lw_adrc1-city1  '"' INTO   lw_einv_api_hdr-location_s.

                    lw_einv_api_hdr-pincode_s =  lw_adrc1-post_code1   .

                    READ TABLE gt_adr61 INTO lw_adr61 WITH KEY addrnumber = lw_adrc1-addrnumber.
                    CONCATENATE '"' lw_adr61-smtp_addr '"'INTO lw_einv_api_hdr-email_s.

                    READ TABLE gt_t005u INTO lw_t005u WITH  KEY bland = lw_adrc1-region.
                    IF sy-subrc EQ 0.
                      TRANSLATE lw_t005u-bezei TO UPPER CASE.
                      CONCATENATE '"'   lw_t005u-bezei  '"' INTO  lw_einv_api_hdr-state_code_s .
                    ENDIF.

                  ENDIF.
                ENDIF.
              ENDIF.
**End of seller_details

**Begin of buyer_details
              IF gw_bseg-kunnr IS NOT INITIAL.
                READ TABLE gt_kna1 INTO lw_kna1 WITH  KEY kunnr = gw_bseg-kunnr.
                IF sy-subrc IS INITIAL.
                  CONCATENATE '"'  lw_kna1-stcd3 '"' INTO          lw_einv_api_hdr-gstin_b .
                  CONCATENATE '"'  lw_kna1-name1 '"' INTO          lw_einv_api_hdr-legal_name_b.
                  CONCATENATE '"' lw_kna1-name1 '"' INTO lw_einv_api_hdr-trade_name_b  .
                  IF lw_kna1-name2 IS NOT INITIAL.
                    CONCATENATE '"' lw_kna1-name2 '"' INTO          lw_einv_api_hdr-address1_b.
                  ELSE.
                    CONCATENATE '"' lw_kna1-name1 '"' INTO          lw_einv_api_hdr-address1_b.
                  ENDIF.
                  CONCATENATE '"' lw_kna1-stras '"' INTO  lw_einv_api_hdr-address2_b.
                  CONCATENATE '"'  lw_kna1-ort01 '"'  INTO           lw_einv_api_hdr-location_b.
                  lw_einv_api_hdr-pincode_b  = lw_kna1-pstlz.

                  lw_einv_api_hdr-place_of_supply_b = lw_kna1-regio.

                  CONCATENATE '"' lw_einv_api_hdr-place_of_supply_b '"' INTO lw_einv_api_hdr-place_of_supply_b.
                  READ TABLE gt_t005u INTO lw_t005u WITH  KEY bland = lw_kna1-regio.
                  IF sy-subrc EQ 0.
                    TRANSLATE lw_t005u-bezei TO UPPER CASE.
                    CONCATENATE '"' lw_t005u-bezei '"'  INTO  lw_einv_api_hdr-state_code_b.
                    lw_einv_api_hdr-place_of_supply_b   =   lw_einv_api_hdr-state_code_b.                                           "State code    Y
                  ENDIF.
                ENDIF.


              ELSEIF  gw_bseg-lifnr IS NOT INITIAL.
                READ TABLE gt_lfa1 INTO lw_lfa1 WITH KEY lifnr = gw_bseg-lifnr.

                IF sy-subrc IS INITIAL.
                  CONCATENATE '"'   lw_lfa1-stcd3 '"' INTO          lw_einv_api_hdr-gstin_b .
                  CONCATENATE '"'  lw_lfa1-name1 '"' INTO          lw_einv_api_hdr-legal_name_b.
                  CONCATENATE '"' lw_lfa1-name1 '"' INTO lw_einv_api_hdr-trade_name_b  .
                  IF lw_lfa1-name2 IS NOT INITIAL.
                    CONCATENATE '"' lw_lfa1-name2 '"' INTO          lw_einv_api_hdr-address1_b.
                  ELSE.
                    CONCATENATE '"' lw_lfa1-name1 '"' INTO          lw_einv_api_hdr-address1_b.
                  ENDIF.
                  CONCATENATE '"' lw_lfa1-stras '"' INTO  lw_einv_api_hdr-address2_b.
                  CONCATENATE '"'  lw_lfa1-ort01 '"'  INTO           lw_einv_api_hdr-location_b.
                  lw_einv_api_hdr-pincode_b  = lw_lfa1-pstlz.

                  lw_einv_api_hdr-place_of_supply_b = lw_lfa1-regio.

                  CONCATENATE '"' lw_einv_api_hdr-place_of_supply_b '"' INTO lw_einv_api_hdr-place_of_supply_b.
                  READ TABLE gt_t005u INTO lw_t005u WITH  KEY bland = lw_lfa1-regio.
                  IF sy-subrc EQ 0.
                    TRANSLATE lw_t005u-bezei TO UPPER CASE.
                    CONCATENATE '"' lw_t005u-bezei '"'  INTO  lw_einv_api_hdr-state_code_b.
                    lw_einv_api_hdr-place_of_supply_b   =   lw_einv_api_hdr-state_code_b.                                           "State code    Y
                  ENDIF.
                ENDIF.
              ENDIF.
              CLEAR lw_adr61.
              READ TABLE gt_adr61 INTO lw_adr61 WITH KEY addrnumber = lw_adrc1-addrnumber.
              CONCATENATE '"' lw_adr61-smtp_addr '"'INTO lw_einv_api_hdr-email_b.

**End of buyer_details
*begin of dispatch_details
              lw_einv_api_hdr-comapny_name_d = lw_einv_api_hdr-trade_name_s.
              lw_einv_api_hdr-address1_d = lw_einv_api_hdr-address1_s.
              lw_einv_api_hdr-address2_d  = lw_einv_api_hdr-address2_s.
              lw_einv_api_hdr-location_d = lw_einv_api_hdr-location_s.
              lw_einv_api_hdr-pincode_d = lw_einv_api_hdr-pincode_s .
              lw_einv_api_hdr-state_code_d = lw_einv_api_hdr-state_code_s.
*begin of dispatch_details.


**Begin of ship_details

              lw_einv_api_hdr-gstin_sh  = lw_einv_api_hdr-gstin_b.
              lw_einv_api_hdr-legal_name_sh = lw_einv_api_hdr-legal_name_b.
              lw_einv_api_hdr-trade_name_sh = lw_einv_api_hdr-trade_name_b.
              lw_einv_api_hdr-address1_sh = lw_einv_api_hdr-address1_b.
              lw_einv_api_hdr-address2_sh = lw_einv_api_hdr-address2_b.
              lw_einv_api_hdr-location_sh  = lw_einv_api_hdr-location_b.
              lw_einv_api_hdr-state_code_sh  = lw_einv_api_hdr-state_code_b.
              lw_einv_api_hdr-pincode_sh  = lw_einv_api_hdr-pincode_b.
*End of  ship_details

*Begin of export_details

              CONCATENATE  '"' ' ' '"' INTO lw_einv_api_hdr-ship_bill_number.
              CONCATENATE  '"' ' ' '"' INTO lw_einv_api_hdr-ship_bill_date.
              CONCATENATE  '"' ' ' '"' INTO lw_einv_api_hdr-country_code.
              CONCATENATE  '"' ' ' '"' INTO lw_einv_api_hdr-foreign_currency.
              CONCATENATE  '"' ' ' '"' INTO lw_einv_api_hdr-refund_claim.
              CONCATENATE  '"' ' ' '"' INTO lw_einv_api_hdr-port_code  .
*End of export_details
**Begin of payment_details
              CONCATENATE '"' ' ' '"' INTO lw_einv_api_hdr-bank_account_number.
              CONCATENATE ' ' '0' ' ' INTO lw_einv_api_hdr-paid_balance_amount.
              CONCATENATE ' ' '0' ' ' INTO lw_einv_api_hdr-credit_days.
              CONCATENATE '"' ' ' '"' INTO lw_einv_api_hdr-credit_transfer.
              CONCATENATE '"' ' ' '"' INTO lw_einv_api_hdr-direct_debit.
              CONCATENATE '"' ' ' '"' INTO lw_einv_api_hdr-branch_or_ifsc.
              CONCATENATE '"' ' ' '"' INTO lw_einv_api_hdr-payment_mode.
              CONCATENATE '"' ' ' '"' INTO lw_einv_api_hdr-payee_name.
              CONCATENATE '"' ' ' '"' INTO lw_einv_api_hdr-payment_due_date.
              CONCATENATE '"' ' ' '"' INTO lw_einv_api_hdr-payment_instruction .
              CONCATENATE '"' ' ' '"' INTO lw_einv_api_hdr-payment_term.
**End of payment_details

              CONCATENATE  '"' '' '"' INTO lw_einv_api_hdr-invoice_remarks.

*Begin of reference_details
              lw_einv_api_hdr-invoice_period_start_date = lw_einv_api_hdr-document_date.
              lw_einv_api_hdr-invoice_period_end_date   = lw_einv_api_hdr-document_date.

**Begin of preceding_document_details
              lw_einv_api_hdr-reference_of_original_invoice = lw_einv_api_hdr-document_number.
              lw_einv_api_hdr-preceding_invoice_date        = lw_einv_api_hdr-document_date.
              lw_einv_api_hdr-other_reference = '""'.
*End of preceding_document_details
*End of reference_details

*Begin of contract Details,
              CONCATENATE '"' '' '"' INTO    lw_einv_api_hdr-receipt_advice_number .
              CONCATENATE '"' '' '"' INTO    lw_einv_api_hdr-batch_reference_number.
              CONCATENATE '"' '' '"' INTO    lw_einv_api_hdr-contract_reference_number.
              CONCATENATE '"' '' '"' INTO    lw_einv_api_hdr-other_reference_c.
              lw_einv_api_hdr-project_reference_number = '""'.
              CONCATENATE '"' '' '"' INTO    lw_einv_api_hdr-vendor_po_reference_number.
              CONCATENATE '"' '' '"' INTO    lw_einv_api_hdr-vendor_po_reference_date.
              "End of contract_details

*Begin of'"additional_document_details"
              lw_einv_api_hdr-supporting_document_url = '""'.
              lw_einv_api_hdr-supporting_document     = '""'.
              lw_einv_api_hdr-additional_information  = '""'.
*End of'"additional_document_details"

*Begin of value_details
********************* dafault all values to 0 ******************************
              lw_einv_api_hdr-total_assessable_value          = '0'.
              lw_einv_api_hdr-total_cgst_value                = '0'.
              lw_einv_api_hdr-total_sgst_value                = '0'.
              lw_einv_api_hdr-total_igst_value                = '0'.
              lw_einv_api_hdr-total_cess_value                = '0'.
              lw_einv_api_hdr-total_cess_nonadvol_value       = '0'.
              lw_einv_api_hdr-total_invoice_value             = '0'.
              lw_einv_api_hdr-total_cess_value_of_state       = '0'.
              lw_einv_api_hdr-round_off_amount                = '0'.
              lw_einv_api_hdr-total_invoice_value_additional  = '0'.
              lw_einv_api_hdr-total_other_charge              = '0'.
********************* dafault all values to 0 ******************************
              lw_einv_api_hdr-total_assessable_value = '1'.               "Required
****************************************************************************************************************************************************
*Begin of item_list
            ENDIF.

            IF gw_bkpf-blart = 'DN'.

*              lw_einv_api_hdr-document_number =
              LOOP AT gt_bseg INTO gw_bseg WHERE bukrs = gw_bkpf-bukrs
              AND  belnr = gw_bkpf-belnr
              AND  gjahr = gw_bkpf-gjahr
              AND  koart = 'S'
              AND hkont IN rt_hkont.



********************* dafault all values to 0 ******************************
                lw_einv_api_itm-quantity                    = '0'.
                lw_einv_api_itm-free_quantity               = '0'.
                lw_einv_api_itm-unit_price                  = '0'.
                lw_einv_api_itm-total_amount                = '0'.
                lw_einv_api_itm-pre_tax_value               = '0'.
                lw_einv_api_itm-discount                    = '0'.
                lw_einv_api_itm-other_charge                = '0'.
                lw_einv_api_itm-gst_rate                    = '0'.
                lw_einv_api_itm-igst_amount                 = '0'.
                lw_einv_api_itm-cgst_amount                 = '0'.
                lw_einv_api_itm-sgst_amount                 = '0'.
                lw_einv_api_itm-cess_rate                   = '0'.
                lw_einv_api_itm-cess_amount                 = '0'.
                lw_einv_api_itm-cess_nonadvol_value         = '0'.
                lw_einv_api_itm-state_cess_rate             = '0'.
                lw_einv_api_itm-state_cess_amount           = '0'.
                lw_einv_api_itm-state_cess_nonadvol_amount  = '0'.
                lw_einv_api_itm-total_item_value            = '0'.


********************* dafault all values to 0 ******************************
                lw_einv_api_itm-bukrs  = gw_bseg-bukrs.
                lw_einv_api_itm-doc_year = gw_bseg-gjahr.
                lw_einv_api_itm-doc_type = gw_bkpf-blart.
                lw_einv_api_itm-document_type = gw_bkpf-blart.
                lw_einv_api_itm-document_number = lw_einv_api_hdr-document_number   .
                SHIFT lw_einv_api_itm-document_number LEFT DELETING LEADING '0'.
                IF gw_bkpf-budat IS NOT INITIAL.
                  CONCATENATE  gw_bkpf-budat+6(2) '/'
                  gw_bkpf-budat+4(2) '/'
                  gw_bkpf-budat+0(4)
                  INTO lw_einv_api_itm-document_date.
                ENDIF.
                CONCATENATE '"' lw_einv_api_itm-document_type '"' INTO  lw_einv_api_itm-document_type.
                CONCATENATE '"' lw_einv_api_itm-document_date '"' INTO  lw_einv_api_itm-document_date.

                CONCATENATE '"'  gw_bseg-buzei '"' INTO lw_einv_api_itm-item_serial_number .
                CONCATENATE '"'  gw_bseg-sgtxt '"' INTO lw_einv_api_itm-product_description .


                lw_einv_api_itm-is_service = gc_service_y.
                lw_einv_api_itm-hsn_code = '9912'.

                lw_einv_api_itm-bar_code =  '""'.

                CONCATENATE '"'  '"' INTO lw_einv_api_itm-order_line_reference.
                CONCATENATE '"'  '"' INTO lw_einv_api_itm-product_serial_number.

*Begin of  attribute_details"
                lw_einv_api_itm-item_attribute_details = gc_attr_einv.
                lw_einv_api_itm-item_attribute_value = gc_attr_einv.
*End of of  attribute_details"

*Begin of  attribute_details"
                lw_einv_api_itm-item_attribute_details = gc_attr_einv.
                lw_einv_api_itm-item_attribute_value = gc_attr_einv.
*End of of  attribute_details"
*End of item_list

                "Quantity
                lw_einv_api_itm-quantity = gw_bseg-menge.
                IF lw_einv_api_itm-quantity IS INITIAL.
                  lw_einv_api_itm-quantity = '1'.
                ENDIF.
                lw_einv_api_itm-free_quantity = '0'.

*Unit
                CLEAR lv_output_uom.
                IF gw_bseg-meins IS NOT INITIAL.
                  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                    EXPORTING
                      input          = gw_bseg-meins
                    IMPORTING
                      output         = lv_output_uom
                    EXCEPTIONS
                      unit_not_found = 1
                      OTHERS         = 2.
                  IF sy-subrc <> 0.
* Implement suitable error handling here
                  ENDIF.

                  READ TABLE gt_doc_uom INTO ls_doc_uom WITH KEY meins = lv_output_uom.
                  IF sy-subrc EQ 0.
                    CONCATENATE '"' ls_doc_uom-edoc_uom '"' INTO lw_einv_api_itm-unit.
                  ELSE.
                    READ TABLE gt_doc_uom INTO ls_doc_uom WITH KEY meins =  gw_bseg-meins.
                    IF sy-subrc IS INITIAL.
                      CONCATENATE '"' ls_doc_uom-edoc_uom '"' INTO lw_einv_api_itm-unit.
                    ENDIF.
                  ENDIF.
                ELSE.
                  lw_einv_api_itm-unit = '"OTH"'.
                ENDIF.
                lw_einv_api_itm-other_charge =  '0'.

                lw_einv_api_itm-unit_price = gw_final-taxamt.
                lw_einv_api_itm-total_amount = gw_final-taxamt.
                lw_einv_api_itm-assessable_value =  gw_final-taxamt.

                CONDENSE:lw_einv_api_itm-unit_price,lw_einv_api_itm-total_amount,
                lw_einv_api_itm-assessable_value.
                lv_total_assessable_value = lv_total_assessable_value  + lw_einv_api_itm-assessable_value.

                lw_einv_api_itm-other_charge = 0.

                lw_einv_api_itm-sgst_amount = gw_final-sgst_amt.

                lw_einv_api_itm-cgst_amount = gw_final-cgst_amt.
                IF gw_final-cgst_amt IS NOT INITIAL AND gw_final-taxamt IS NOT INITIAL.
                  lw_einv_api_itm-gst_rate = ( gw_final-cgst_amt / gw_final-taxamt  ) * 200.
                ENDIF.

                lw_einv_api_itm-igst_amount = gw_final-igst_amt.
                IF gw_final-igst_amt IS NOT INITIAL AND gw_final-taxamt IS NOT INITIAL.
                  lw_einv_api_itm-gst_rate = ( gw_final-igst_amt / gw_final-taxamt  ) * 100.
                ENDIF.


                lv_i_amount = lw_einv_api_itm-gst_rate.

                IF lw_einv_api_itm-gst_rate IS NOT INITIAL.

                  CALL FUNCTION 'J_1I6_ROUND_TO_NEAREST_AMT'
                    EXPORTING
                      i_amount = lv_i_amount
                    IMPORTING
                      e_amount = lv_e_amount.
                  IF sy-subrc IS INITIAL.
                    lw_einv_api_itm-gst_rate = lv_e_amount.
                  ENDIF.

                ENDIF.



                CONDENSE:lw_einv_api_itm-gst_rate,lw_einv_api_itm-sgst_amount,lw_einv_api_itm-igst_amount,
                lw_einv_api_itm-cgst_amount.

                ADD lw_einv_api_itm-sgst_amount TO lv_total_sgst_value.
                ADD lw_einv_api_itm-cgst_amount TO lv_total_cgst_value.
                ADD lw_einv_api_itm-igst_amount TO lv_total_igst_value.


                lw_einv_api_itm-total_item_value =  ( lw_einv_api_itm-assessable_value +
                lw_einv_api_itm-cgst_amount +
                lw_einv_api_itm-sgst_amount +
                lw_einv_api_itm-cess_amount +
                lw_einv_api_itm-igst_amount +
                lw_einv_api_itm-state_cess_amount +
                lw_einv_api_itm-cess_nonadvol_value ).
                CONDENSE:lw_einv_api_itm-total_item_value.
                ADD lw_einv_api_itm-total_item_value TO  lv_total_invoice_value .
                IF lw_einv_api_itm-hsn_code IS INITIAL.
                  lw_einv_api_itm-hsn_code = '9917'.
                ENDIF.

                APPEND lw_einv_api_itm TO gt_einv_api_itm.
                CLEAR:lw_einv_api_itm .
                CONTINUE.
              ENDLOOP.

            ELSE.




              LOOP AT gt_bseg INTO gw_bseg WHERE bukrs = gw_bkpf-bukrs
              AND  belnr = gw_bkpf-belnr
              AND  gjahr = gw_bkpf-gjahr
              AND  koart = 'S'.
*              IF gw_bseg-belnr NE gw_final-vbeln.
*                EXIT.
*              ENDIF.
                IF gw_bseg-buzid IS INITIAL." AND gw_bseg-koart NE 'D'.
********************* dafault all values to 0 ******************************
                  lw_einv_api_itm-quantity                    = '0'.
                  lw_einv_api_itm-free_quantity               = '0'.
                  lw_einv_api_itm-unit_price                  = '0'.
                  lw_einv_api_itm-total_amount                = '0'.
                  lw_einv_api_itm-pre_tax_value               = '0'.
                  lw_einv_api_itm-discount                    = '0'.
                  lw_einv_api_itm-other_charge                = '0'.
                  lw_einv_api_itm-gst_rate                    = '0'.
                  lw_einv_api_itm-igst_amount                 = '0'.
                  lw_einv_api_itm-cgst_amount                 = '0'.
                  lw_einv_api_itm-sgst_amount                 = '0'.
                  lw_einv_api_itm-cess_rate                   = '0'.
                  lw_einv_api_itm-cess_amount                 = '0'.
                  lw_einv_api_itm-cess_nonadvol_value         = '0'.
                  lw_einv_api_itm-state_cess_rate             = '0'.
                  lw_einv_api_itm-state_cess_amount           = '0'.
                  lw_einv_api_itm-state_cess_nonadvol_amount  = '0'.
                  lw_einv_api_itm-total_item_value            = '0'.


********************* dafault all values to 0 ******************************

                  lw_einv_api_itm-bukrs  = gw_bseg-bukrs.
                  lw_einv_api_itm-doc_year = gw_bseg-gjahr.
                  lw_einv_api_itm-doc_type = gw_bkpf-blart.
                  lw_einv_api_itm-document_type = gw_bkpf-blart.
                  lw_einv_api_itm-document_number = lw_einv_api_hdr-document_number   .
                  SHIFT lw_einv_api_itm-document_number LEFT DELETING LEADING '0'.
                  IF gw_bkpf-budat IS NOT INITIAL.
                    CONCATENATE  gw_bkpf-budat+6(2) '/'
                    gw_bkpf-budat+4(2) '/'
                    gw_bkpf-budat+0(4)
                    INTO lw_einv_api_itm-document_date.
                  ENDIF.
                  CONCATENATE '"' lw_einv_api_itm-document_type '"' INTO  lw_einv_api_itm-document_type.
                  CONCATENATE '"' lw_einv_api_itm-document_date '"' INTO  lw_einv_api_itm-document_date.

                  CONCATENATE '"'  gw_bseg-buzei '"' INTO lw_einv_api_itm-item_serial_number .
                  CONCATENATE '"'  gw_bseg-sgtxt '"' INTO lw_einv_api_itm-product_description .


                  IF gw_bseg-matnr IS INITIAL.
                    lw_einv_api_itm-is_service = gc_service_y.
                  ELSE.
                    lw_einv_api_itm-is_service = gc_service_n.
                  ENDIF.

                  lw_einv_api_itm-hsn_code = gw_bseg-hsn_sac.
                  IF lw_einv_api_itm-hsn_code IS INITIAL.
                    lw_einv_api_itm-hsn_code  = '9917'.
                  ENDIF.

                  IF gw_bseg-hsn_sac IS NOT INITIAL .
                    IF gw_bseg-hsn_sac+0(2) = '99'.
                      lw_einv_api_itm-is_service = gc_service_y.
                    ELSE.
                      lw_einv_api_itm-is_service = gc_service_n.
                    ENDIF.
                  ENDIF.

                  lw_einv_api_itm-bar_code =  '""'.

                  CONCATENATE '"'  '"' INTO lw_einv_api_itm-order_line_reference.
                  CONCATENATE '"'  '"' INTO lw_einv_api_itm-product_serial_number.

*Begin of  attribute_details"
                  lw_einv_api_itm-item_attribute_details = gc_attr_einv.
                  lw_einv_api_itm-item_attribute_value = gc_attr_einv.
*End of of  attribute_details"

*Begin of  attribute_details"
                  lw_einv_api_itm-item_attribute_details = gc_attr_einv.
                  lw_einv_api_itm-item_attribute_value = gc_attr_einv.
*End of of  attribute_details"
*End of item_list

                  "Quantity
                  lw_einv_api_itm-quantity = gw_bseg-menge.
                  IF lw_einv_api_itm-quantity IS INITIAL.
                    lw_einv_api_itm-quantity = '1'.
                  ENDIF.
                  lw_einv_api_itm-free_quantity = '0'.

*Unit
                  CLEAR lv_output_uom.
                  IF gw_bseg-meins IS NOT INITIAL.
                    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                      EXPORTING
                        input          = gw_bseg-meins
                      IMPORTING
                        output         = lv_output_uom
                      EXCEPTIONS
                        unit_not_found = 1
                        OTHERS         = 2.
                    IF sy-subrc <> 0.
* Implement suitable error handling here
                    ENDIF.

                    READ TABLE gt_doc_uom INTO ls_doc_uom WITH KEY meins = lv_output_uom.
                    IF sy-subrc EQ 0.
                      CONCATENATE '"' ls_doc_uom-edoc_uom '"' INTO lw_einv_api_itm-unit.
                    ELSE.
                      READ TABLE gt_doc_uom INTO ls_doc_uom WITH KEY meins =  gw_bseg-meins.
                      IF sy-subrc IS INITIAL.
                        CONCATENATE '"' ls_doc_uom-edoc_uom '"' INTO lw_einv_api_itm-unit.
                      ENDIF.
                    ENDIF.
                  ELSE.
                    lw_einv_api_itm-unit = '"OTH"'.
                  ENDIF.
                  lw_einv_api_itm-other_charge =  '0'.
                  IF gw_bseg-buzid IS INITIAL.
                    IF gw_bseg-menge IS NOT INITIAL.
                      lv_rate = gw_bseg-dmbtr / gw_bseg-menge.
                      lw_einv_api_itm-unit_price = lv_rate.
                    ELSE.
                      lw_einv_api_itm-unit_price = '1.00'.
                    ENDIF.
                    lw_einv_api_itm-total_amount = gw_bseg-dmbtr.
                    lw_einv_api_itm-assessable_value = gw_bseg-dmbtr.
                  ENDIF.
                  CONDENSE:lw_einv_api_itm-unit_price,lw_einv_api_itm-total_amount,
                  lw_einv_api_itm-assessable_value.
                  lv_total_assessable_value = lv_total_assessable_value  + lw_einv_api_itm-assessable_value.
**READ TABLE gt_bseg INTO lw_bseg WITH KEY bukrs = gw_bseg-bukrs
**                                                                belnr = gw_bseg-belnr
**                                                                gjahr = gw_bseg-gjahr
**                                                                ktosl = gc_ktosl_tcs.
**                IF sy-subrc = 0 .
**                  lw_einv_api_itm-other_charge = lw_einv_api_itm-other_charge + lw_bseg-dmbtr.
**                  endif.
                  CLEAR:lw_bseg.
                  READ TABLE gt_bseg INTO lw_bseg WITH KEY bukrs = gw_bseg-bukrs
                  belnr = gw_bseg-belnr
                  gjahr = gw_bseg-gjahr
                  ktosl = gc_ktosl_jos.
                  IF sy-subrc IS NOT INITIAL.
                    READ TABLE gt_bseg INTO lw_bseg WITH KEY bukrs = gw_bseg-bukrs
                    belnr = gw_bseg-belnr
                    gjahr = gw_bseg-gjahr
                    ktosl = gc_ktosl_jou.
                  ENDIF.
                  IF sy-subrc = 0 .
                    lw_einv_api_itm-sgst_amount = lw_bseg-dmbtr.
                    IF lw_bseg-dmbtr IS NOT INITIAL.
                      lw_einv_api_itm-gst_rate = ( lw_bseg-dmbtr / gw_bseg-dmbtr ) * 100.
                    ENDIF.
                    CONDENSE:lw_einv_api_itm-gst_rate,lw_einv_api_itm-sgst_amount.
                    ADD lw_einv_api_itm-sgst_amount TO lv_total_sgst_value.
                    ADD lw_einv_api_itm-gst_rate TO lw_gstrate.
                  ENDIF.
                  READ TABLE gt_bseg INTO lw_bseg WITH KEY bukrs = gw_bseg-bukrs
                  belnr = gw_bseg-belnr
                  gjahr = gw_bseg-gjahr
                  ktosl = gc_ktosl_joc.
                  IF sy-subrc = 0.

                    lw_einv_api_itm-cgst_amount = lw_bseg-dmbtr.
                    IF lw_bseg-dmbtr IS NOT INITIAL.
                      lw_einv_api_itm-gst_rate = ( lw_bseg-dmbtr / gw_bseg-dmbtr ) * 100.
                    ENDIF.
                    CONDENSE:lw_einv_api_itm-gst_rate,lw_einv_api_itm-cgst_amount.
                    ADD lw_einv_api_itm-cgst_amount TO lv_total_cgst_value.
                    ADD lw_einv_api_itm-gst_rate TO lw_gstrate.
                  ENDIF.
                  READ TABLE gt_bseg INTO lw_bseg WITH KEY bukrs = gw_bseg-bukrs
                  belnr = gw_bseg-belnr
                  gjahr = gw_bseg-gjahr
                  ktosl = gc_ktosl_joi.
                  IF sy-subrc IS INITIAL.
                    lw_einv_api_itm-igst_amount = lw_bseg-dmbtr.
                    IF lw_bseg-dmbtr IS NOT INITIAL.
                      lw_einv_api_itm-gst_rate = ( lw_bseg-dmbtr / gw_bseg-dmbtr ) * 100.
                    ENDIF.
                    CONDENSE:lw_einv_api_itm-gst_rate,lw_einv_api_itm-igst_amount.
                    ADD lw_einv_api_itm-igst_amount TO lv_total_igst_value.
                    lw_gstrate = lw_einv_api_itm-gst_rate.
                  ENDIF.

                  lw_einv_api_itm-total_item_value =  ( lw_einv_api_itm-assessable_value +
                  lw_einv_api_itm-cgst_amount +
                  lw_einv_api_itm-sgst_amount +
                  lw_einv_api_itm-cess_amount +
                  lw_einv_api_itm-igst_amount +
                  lw_einv_api_itm-state_cess_amount +
                  lw_einv_api_itm-cess_nonadvol_value ).
                  CONDENSE:lw_einv_api_itm-total_item_value.
                  ADD lw_einv_api_itm-total_item_value TO  lv_total_invoice_value .
                  IF lw_einv_api_itm-hsn_code IS INITIAL.
                    lw_einv_api_itm-hsn_code = '9917'.
                  ENDIF.
                  lw_einv_api_itm-gst_rate = lw_gstrate.
                  CONDENSE:lw_einv_api_itm-gst_rate.
                  APPEND lw_einv_api_itm TO gt_einv_api_itm.
                  CLEAR:lw_einv_api_itm ,lw_gstrate.
                ENDIF.
              ENDLOOP.
            ENDIF.
*End of item_list
            lw_einv_api_hdr-total_assessable_value = lv_total_assessable_value.
            lw_einv_api_hdr-total_cgst_value       = lv_total_cgst_value.
            lw_einv_api_hdr-total_sgst_value       = lv_total_sgst_value.
            lw_einv_api_hdr-total_igst_value       = lv_total_igst_value .
            lw_einv_api_hdr-total_invoice_value    = lv_total_invoice_value.
            lw_einv_api_hdr-total_cess_value       = lv_total_cess_value.
            lw_einv_api_hdr-total_cess_nonadvol_value =  lv_total_cess_nonadvol_value.
            CONDENSE:lw_einv_api_hdr-total_assessable_value,lw_einv_api_hdr-total_cgst_value,lw_einv_api_hdr-total_sgst_value,lw_einv_api_hdr-total_igst_value,
            lw_einv_api_hdr-total_invoice_value,lw_einv_api_hdr-total_cess_value,lw_einv_api_hdr-total_cess_nonadvol_value.
            APPEND lw_einv_api_hdr TO gt_einv_api_hdr.
            CLEAR:lw_einv_api_hdr.


          ENDIF.
        ELSE.
          MESSAGE 'Please select appropriate documents for E-Doc generation'(039) TYPE 'I'.

        ENDIF.
      ENDLOOP.
      IF gt_einv_api_hdr IS NOT INITIAL.
        SORT gt_einv_api_hdr .
        SORT gt_einv_api_itm .



* when download exit the procress
        IF gv_doc_download IS NOT INITIAL.
          EXIT.
        ENDIF.

        DELETE ADJACENT DUPLICATES FROM   gt_einv_api_hdr  COMPARING ALL FIELDS.
        DELETE ADJACENT DUPLICATES FROM  gt_einv_api_itm  COMPARING ALL FIELDS.



        lt_api_hdr = gt_einv_api_hdr .
        lt_api_itm = gt_einv_api_itm .



        CLEAR:lw_token,lw_return.
        IF gw_token IS NOT INITIAL.
          lw_token = gw_token.
        ELSE.
          CALL FUNCTION 'ZFM_EINVOICE_OAUTH_API'
            IMPORTING
              ex_token    = lw_token
              ex_return   = lw_return
              et_messages = lt_messages.
        ENDIF.
        IF lw_token IS NOT INITIAL." AND lw_return EQ 'S'.

          CALL FUNCTION 'ZFM_EINVOICE_GENERATE_API'
            EXPORTING
              im_token        = lw_token
              im_api_hdr      = lt_api_hdr
              im_api_itm      = lt_api_itm
              im_api_dwn      = gv_download
            IMPORTING
              ex_return       = lw_return
              ex_invref       = lt_invref
              ex_messages     = lt_messages
              ex_einv_details = lt_einv_details.


          IF lt_invref IS NOT INITIAL."lw_return EQ 'S'.
            MODIFY j_1ig_invrefnum FROM TABLE lt_invref.
            IF sy-subrc EQ 0.
              MODIFY zteinv_details FROM TABLE lt_einv_details.

              LOOP AT lt_invref INTO lw_invref.
                READ TABLE gt_final ASSIGNING <gw_final> WITH KEY vbeln = lw_invref-docno.
                IF sy-subrc IS INITIAL.

                  <gw_final>-irn = lw_invref-irn.
                  <gw_final>-ack_no = lw_invref-ack_no.
                  IF lw_invref-ack_date IS NOT INITIAL.
                    lv_date = lw_invref-ack_date.
                    CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
                      EXPORTING
                        date_internal            = lv_date
                      IMPORTING
                        date_external            = <gw_final>-ack_dt
                      EXCEPTIONS
                        date_internal_is_invalid = 1
                        OTHERS                   = 2.
                    IF sy-subrc <> 0.
                      MESSAGE 'Date conversion error'(m02) TYPE 'I'.
                    ENDIF.
                  ENDIF.

                  <gw_final>-ernam = lw_invref-ernam.

                  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
                    EXPORTING
                      date_internal            = lw_invref-erdat
                    IMPORTING
                      date_external            = <gw_final>-erdat
                    EXCEPTIONS
                      date_internal_is_invalid = 1
                      OTHERS                   = 2.
                  IF sy-subrc <> 0.
                    MESSAGE 'Date conversion error'(m02) TYPE 'I'.
                  ENDIF.

                  <gw_final>-erzet = lw_invref-erzet.


                  IF lw_invref-irn_status = gc_irn_sts_act.
                    <gw_final>-status = 'Success'(060).
                    <gw_final>-icon    = gc_icon_08.
                    <gw_final>-einv_error = space.
                  ELSEIF lw_invref-irn_status = gc_irn_sts_err.
                    <gw_final>-status = 'Error'(061).
                    <gw_final>-icon    = gc_icon_0a.
                    READ TABLE lt_einv_details INTO lw_einv_details WITH KEY docno = lw_invref-docno.
                    IF  sy-subrc IS INITIAL.
                      <gw_final>-einv_error = lw_einv_details-einv_error.
                    ENDIF.
                  ELSEIF lw_invref-irn_status = gc_irn_sts_cnl.
                    <gw_final>-status = 'Cancelled'(062).
                    <gw_final>-icon    =  gc_icon_0w.
                  ENDIF.
                ENDIF.

                READ TABLE lt_einv_details INTO lw_einv WITH KEY docno =  lw_invref-docno.
                IF sy-subrc IS INITIAL.
                  <gw_final>-einv_print = lw_einv-einv_pdf.
                ENDIF.


              ENDLOOP.



* COmmit work
              COMMIT WORK.


              CALL METHOD gref_alv_grid->refresh_table_display
                EXCEPTIONS
                  finished = 1
                  OTHERS   = 2.
              IF sy-subrc <> 0.
                MESSAGE 'Table refresh error'(012) TYPE 'I'.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.


        CLEAR lt_show_message.
        IF  lt_messages IS NOT INITIAL.

          LOOP AT lt_messages INTO lw_message.
            IF lw_message-type IS NOT INITIAL.
              lw_show_message-msgid =  gc_msgid_01.
              lw_show_message-msgty =  lw_message-type.
              lw_show_message-msgno =  gc_msgno_319.
              lw_show_message-msgv1 =  lw_message-message_v1.
              lw_show_message-msgv2 =  lw_message-message_v2.
              lw_show_message-msgv3 =  lw_message-message_v3.
              lw_show_message-msgv4 =  lw_message-message_v4.
              APPEND lw_show_message TO lt_show_message.
            ENDIF.
          ENDLOOP.

          CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
            TABLES
              i_message_tab = lt_show_message.
        ENDIF.
      ELSE.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM EXIT_PROGRAM                                             *
*---------------------------------------------------------------------*
FORM exit_program.
  CALL METHOD gref_alv_container->free.
  LEAVE TO SCREEN 0.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EINVOICE_CANCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM einvoice_cancel USING p_lv_reason_code
      p_lv_reason.

  DATA:lw_token         TYPE string,
       lw_return        TYPE string,
       wa_return        TYPE string,
       lt_messages      TYPE bapiret2_t,
       lw_message       TYPE bapiret2,
       lt_show_message  TYPE esp1_message_tab_type,
       lw_show_message  LIKE LINE OF lt_show_message,
       lt_invref        TYPE TABLE OF j_1ig_invrefnum,
       lw_invref        TYPE  j_1ig_invrefnum,
       lw_index         LIKE LINE OF gt_index,
       lv_val           TYPE xfeld,
       lv_answer        TYPE c,
       lv_gstin         TYPE  kna1-stcd3,
       lv_reason        TYPE  char20,
       lv_remarks       TYPE  char255,
       ls_values        TYPE dd07v,
       lv_internal_date TYPE sy-datum,
       lw_usrgstin      TYPE ty_api,
       it_return        TYPE tab_bapiret1,
       lv_success       TYPE xfeld.

  FIELD-SYMBOLS <gw_final>  TYPE ty_final.




  PERFORM check_selected_data CHANGING lv_val.

  IF lv_val IS INITIAL.


    LOOP AT  gt_index INTO lw_index.

      READ TABLE gt_final INTO gw_final INDEX lw_index-index.
      IF sy-subrc IS INITIAL AND gw_final-irn IS NOT INITIAL.
        lv_gstin = gw_final-sup_gstin.
        SELECT * FROM j_1ig_invrefnum APPENDING TABLE lt_invref
        WHERE bukrs     = gw_final-bukrs AND
        docno     = gw_final-vbeln AND
        doc_type  = gw_final-fkart AND
        irn       = gw_final-irn.

      ENDIF.
    ENDLOOP.

    READ TABLE gt_api  INTO lw_usrgstin WITH KEY apiid = gc_apiid_usrgstin.
    IF sy-subrc IS INITIAL.
      lv_gstin   = lw_usrgstin-apiprov.
    ENDIF.



    IF lt_invref IS NOT INITIAL.
      CLEAR:lw_token,lw_return.
      IF gw_token IS NOT INITIAL.
        lw_token = gw_token.
      ELSE.

        CALL FUNCTION 'ZFM_EINVOICE_OAUTH_API'
          IMPORTING
            ex_token    = lw_token
            ex_return   = lw_return
            et_messages = lt_messages.
      ENDIF.
      IF lw_token IS NOT INITIAL.
        CONCATENATE '"' p_lv_reason_code '"' INTO lv_reason.
        CONCATENATE '"' p_lv_reason       '"' INTO lv_remarks.
        CONCATENATE '"' lv_gstin '"' INTO lv_gstin.


        CALL FUNCTION 'ZFM_EINVOICE_CANCEL_API'
          EXPORTING
            im_token    = lw_token
            im_invref   = lt_invref
            im_gstin    = lv_gstin
            im_reason   = lv_reason
            im_remarks  = lv_remarks
          IMPORTING
            ex_return   = lw_return
            ex_invref   = lt_invref
            ex_messages = lt_messages.

        IF lt_invref IS NOT INITIAL."lw_return EQ 'S'.
          MODIFY j_1ig_invrefnum FROM TABLE lt_invref.

          LOOP AT lt_invref INTO lw_invref.
            READ TABLE gt_final ASSIGNING <gw_final> WITH KEY vbeln = lw_invref-docno.
            IF sy-subrc IS INITIAL.

              IF lw_invref-cancel_date IS NOT INITIAL.
                lw_invref-irn_status     = gc_irn_sts_cnl.

                UPDATE zteinv_details SET e_reason_code = p_lv_reason_code
                e_reason      = p_lv_reason
                aenam         = sy-uname
                aedat         = sy-datum
                WHERE  bukrs  = lw_invref-bukrs AND
                docno  = lw_invref-docno AND
                doctyp = lw_invref-doc_type AND
                gjahr  = lw_invref-doc_year.

***********************added by raghu on 07.02.2023************
                PERFORM vf_einvoice_cancel USING <gw_final>-vbeln
                CHANGING it_return lv_success.
****************************************************************
                IF sy-subrc IS INITIAL.
                  lv_internal_date = lw_invref-cancel_date.
                  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
                    EXPORTING
                      date_internal            = lv_internal_date
                    IMPORTING
                      date_external            = <gw_final>-canc_dt
                    EXCEPTIONS
                      date_internal_is_invalid = 1
                      OTHERS                   = 2.
                  IF sy-subrc <> 0.
                    MESSAGE 'Date conversion error'(m02) TYPE 'I'.
                  ENDIF.
                ENDIF.
*                  ENDIF.
              ENDIF.

              IF lw_invref-irn_status = gc_irn_sts_act.
                <gw_final>-status = 'Success'(060).
                <gw_final>-icon    = gc_icon_08.
              ELSEIF lw_invref-irn_status = gc_irn_sts_err.
                <gw_final>-status = 'Error'(061).
                <gw_final>-icon    = gc_icon_0a.
              ELSEIF lw_invref-irn_status = gc_irn_sts_cnl.
                <gw_final>-status = 'Cancelled'(062).
                <gw_final>-icon    =  gc_icon_0w.
              ENDIF.
            ENDIF.
          ENDLOOP.

* Commit work
          COMMIT WORK.

          CALL METHOD gref_alv_grid->refresh_table_display
            EXCEPTIONS
              finished = 1
              OTHERS   = 2.
          IF sy-subrc <> 0.
            MESSAGE 'Table refresh error'(012) TYPE 'I'.
          ENDIF.


*            ENDIF.
        ENDIF.

      ENDIF.


      CLEAR lt_show_message.
      IF  lt_messages IS NOT INITIAL.
        LOOP AT lt_messages INTO lw_message.
          lw_show_message-msgid =  gc_msgid_01.
          lw_show_message-msgty =  lw_message-type.
          lw_show_message-msgno =  gc_msgno_319.
          lw_show_message-msgv1 =  lw_message-message_v1.
          lw_show_message-msgv2 =  lw_message-message_v2.
          lw_show_message-msgv3 =  lw_message-message_v3.
          lw_show_message-msgv4 =  lw_message-message_v4.
          APPEND lw_show_message TO lt_show_message.

        ENDLOOP.

        CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
          TABLES
            i_message_tab = lt_show_message.

      ENDIF.
    ELSE.
      MESSAGE 'Please select appropriate documents for E-Invoice cancellation'(014) TYPE 'I'.
    ENDIF.



  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALL_POPUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_T20  text
*      -->P_TEXT_T21  text
*----------------------------------------------------------------------*
FORM call_popup  USING    p_text_t20
      p_text_t21
CHANGING p_text_t22 TYPE c.


  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = p_text_t20
      text_question         = p_text_t21
      text_button_1         = 'Yes'(107)
      text_button_2         = 'No'(108)
      display_cancel_button = abap_true
    IMPORTING
      answer                = p_text_t22
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.




ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.

  DATA:lw_index  LIKE LINE OF gt_index.
  DATA:lv_answer TYPE c.
  FIELD-SYMBOLS <gw_final>  TYPE ty_final.

  DATA: lt_zteway_transport TYPE TABLE OF zteway_transport,
        ls_zteway_transport TYPE zteway_transport.

  CASE sy-ucomm.

    WHEN 'OK'.

      PERFORM call_popup USING   TEXT-t23 TEXT-t30
      CHANGING lv_answer.

      IF lv_answer = '1'.

        LOOP AT  gt_index INTO lw_index.
          READ TABLE gt_final ASSIGNING <gw_final> INDEX lw_index-index.
          IF sy-subrc IS INITIAL.

            <gw_final>-t_id      = zst_einv_api_struct-transporter_id.
            <gw_final>-t_name    = zst_einv_api_struct-transporter_name.
            <gw_final>-t_mode = zst_einv_api_struct-transportation_mode.
            <gw_final>-t_distance   = zst_einv_api_struct-transportation_distance.
            <gw_final>-t_doc_no  = zst_einv_api_struct-transporter_document_number.
            IF zst_einv_api_struct-transporter_document_date IS NOT INITIAL.
              CONCATENATE  zst_einv_api_struct-transporter_document_date+6(2) '/'
              zst_einv_api_struct-transporter_document_date+4(2) '/'
              zst_einv_api_struct-transporter_document_date+0(4)
              INTO <gw_final>-t_date.
            ENDIF.
            CONDENSE zst_einv_api_struct-vehicle_number NO-GAPS.
            <gw_final>-v_number  = zst_einv_api_struct-vehicle_number.
            <gw_final>-v_type = zst_einv_api_struct-vehicle_type.
            <gw_final>-t_r_distance           = zst_einv_api_struct-remaining_distance.
            <gw_final>-t_ext_valid_reason = zst_einv_api_struct-vehicle_type.
            <gw_final>-t_ext_valid_remarks    = zst_einv_api_struct-extend_remarks.
            <gw_final>-t_from_pin             = zst_einv_api_struct-from_pincode.
            <gw_final>-consignor_place             = zst_einv_api_struct-consignor_place.
            <gw_final>-consignor_state             = zst_einv_api_struct-consignor_state.
            <gw_final>-t_consignment_status   = zst_einv_api_struct-consignment_status.
            <gw_final>-t_transit_type         = zst_einv_api_struct-transit_type.
            IF zst_einv_api_struct-transportation_mode NE gc_tmode_5.
              <gw_final>-t_transit_type         = space.
            ENDIF.
            <gw_final>-t_address1             = zst_einv_api_struct-address_line1.
            <gw_final>-t_address2             = zst_einv_api_struct-address_line2.
            <gw_final>-t_address3             = zst_einv_api_struct-address_line3.
            <gw_final>-v_reason_code = zst_einv_api_struct-reason_code_for_vehicle_updati.
            <gw_final>-v_reason  = zst_einv_api_struct-reason_for_vehicle_updation.
            <gw_final>-zport_code  = zst_einv_api_struct-zport_code.
          ENDIF.

          ls_zteway_transport-bukrs        = <gw_final>-bukrs.
          ls_zteway_transport-doctyp       = <gw_final>-fkart.
          ls_zteway_transport-docno        = <gw_final>-vbeln.
          ls_zteway_transport-gjahr        = <gw_final>-gjahr.
          ls_zteway_transport-t_id         = <gw_final>-t_id.
          ls_zteway_transport-t_name       = <gw_final>-t_name.
          ls_zteway_transport-t_doc_no     = <gw_final>-t_doc_no.
          ls_zteway_transport-t_date       = zst_einv_api_struct-transporter_document_date.
          ls_zteway_transport-t_mode       = <gw_final>-t_mode.
          ls_zteway_transport-t_distance   = <gw_final>-t_distance.
          ls_zteway_transport-v_number     = <gw_final>-v_number.
          ls_zteway_transport-v_type       = <gw_final>-v_type.
          ls_zteway_transport-t_r_distance          = <gw_final>-t_r_distance.
          ls_zteway_transport-t_ext_valid_reason    = <gw_final>-t_ext_valid_reason.
          ls_zteway_transport-t_ext_valid_remarks   = <gw_final>-t_ext_valid_remarks.
          ls_zteway_transport-t_from_pin            = <gw_final>-t_from_pin.
          ls_zteway_transport-consignor_place           = <gw_final>-consignor_place.
          ls_zteway_transport-consignor_state            = <gw_final>-consignor_state.
          ls_zteway_transport-t_consignment_status  = <gw_final>-t_consignment_status.
          ls_zteway_transport-t_transit_type        = <gw_final>-t_transit_type.
          ls_zteway_transport-t_address1            = <gw_final>-t_address1.
          ls_zteway_transport-t_address2            = <gw_final>-t_address2.
          ls_zteway_transport-t_address3            = <gw_final>-t_address3.
          ls_zteway_transport-v_reason_code        = <gw_final>-v_reason_code.
          ls_zteway_transport-v_reason             = <gw_final>-v_reason.
          ls_zteway_transport-zport_code           = <gw_final>-zport_code.
          ls_zteway_transport-eway_print           = <gw_final>-eway_print.
          ls_zteway_transport-eway_error           = <gw_final>-eway_error.

          READ TABLE gt_zteway_transport TRANSPORTING NO FIELDS WITH KEY bukrs = <gw_final>-bukrs
          doctyp = <gw_final>-fkart
          docno = <gw_final>-vbeln.
          IF sy-subrc IS INITIAL.
            ls_zteway_transport-aenam        = sy-uname.
            ls_zteway_transport-aedat        = sy-datum.

          ELSE.
            ls_zteway_transport-ernam        = sy-uname.
            ls_zteway_transport-erdat        = sy-datum.
          ENDIF.


*          APPEND ls_zteway_transport TO lt_zteway_transport.
          MODIFY zteway_transport FROM ls_zteway_transport.
          IF sy-subrc IS NOT INITIAL.
            MESSAGE 'Data update error'(022) TYPE 'I'.
          ENDIF.
        ENDLOOP.

        CALL METHOD gref_alv_grid->refresh_table_display
          EXCEPTIONS
            finished = 1
            OTHERS   = 2.
        IF sy-subrc <> 0.
          MESSAGE 'Table refresh error'(012) TYPE 'I'.
        ENDIF.
        PERFORM set_selected_data.

        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  EWAYBILL_GENERATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ewaybill_generate.

  TYPES lr_fkart_type TYPE RANGE OF fkart.
  DATA : lr_fkart TYPE lr_fkart_type.
  DATA:lw_index            TYPE lvc_s_row,
       lv_val              TYPE xfeld,
       lv_answer           TYPE c,
       lv_index            TYPE sy-tabix,
       lw_token            TYPE string,
       lw_return           TYPE string,
       wa_return           TYPE string,
       lt_messages         TYPE bapiret2_t,
       lw_message          TYPE bapiret2,
       lt_show_message     TYPE esp1_message_tab_type,
       lw_show_message     LIKE LINE OF lt_show_message,
       lw_etransport       TYPE zteway_transport,
       lt_ewaybill         TYPE TABLE OF j_1ig_ewaybill,
       lw_ewaybill         TYPE j_1ig_ewaybill,
       lt_eway_transport   TYPE TABLE OF zteway_transport,
       ls_eway_transport   TYPE zteway_transport,
       lw_wb2_v_vbrk_vbrp2 TYPE ty_wb2_v_vbrk_vbrp2,
       lw_wb2_v_likp_lips2 TYPE ty_wb2_v_likp_lips2,
       ls_doc_uom          TYPE ty_doc_uom,
       ls_doctyp           TYPE ty_doctyp,
       lv_output_uom       TYPE meins,
       lt_api_hdr          TYPE ztt_eway_api_struct,
       lt_api_itm          TYPE ztt_eway_api_struct_itm,
       lw_eway_api_hdr     TYPE zst_eway_api_struct,
       lw_eway_api_itm     TYPE zst_eway_api_struct_itm,
       lw_t001w            TYPE ty_t001w1,
       lw_t005u_s          TYPE ty_t005u,
       lw_kna1             TYPE ty_kna1,
       lw_kna1_sh          TYPE ty_kna1,
       lw_t005u            TYPE ty_t005u,
       lw_adrc             TYPE ty_adrc,
       lw_adr6             TYPE ty_adr6,
       lw_marc             TYPE ty_marc,
       ls_export           TYPE ty_export,
       lw_vbpa             TYPE ty_vbpa,
       lw_usrgstin         TYPE ty_api,
       lw_konv             TYPE ty_konv,
       ls_values           TYPE dd07v,
       lv_error            TYPE xfeld,
       lv_string           TYPE string,
       lv_vbeln            TYPE vbrk-vbeln,
       lv_export           TYPE xfeld,
       lv_exp_curr         TYPE xfeld,
       gv_downld           TYPE string,
       lv_tdname           TYPE thead-tdname,
       lv_tdline           TYPE tdline,
       lv_tdline1          TYPE tdline,
       lt_lines            TYPE STANDARD TABLE OF tline,
       lw_lines            TYPE tline,
       lv_bill_ship        TYPE xfeld,
       lv_dispatch         TYPE xfeld.

  DATA: lv_supply_type     TYPE char10 VALUE 'O',
        lv_document_type   TYPE char10 VALUE 1,
        lv_sub_supply_type TYPE char10 VALUE 1,
        lv_document_number TYPE i VALUE 1.

  DATA: lv_total_invoice_value   TYPE netwr,
        lv_total_sgst_value      TYPE netwr,
        lv_total_cgst_value      TYPE netwr,
        lv_total_igst_value      TYPE netwr,
        lv_total_cess_value      TYPE netwr,
        lv_total_cessnoval_value TYPE netwr,
        lv_total_taxable_value   TYPE netwr,
        lv_total_other_value     TYPE netwr,
        lv_diff_value            TYPE netwr.

  RANGES : rg_diff FOR prcd_elements-kschl.
  SELECT * FROM tvarvc INTO TABLE @DATA(it_diff) WHERE name = 'EDOC_ROUND_OFF'.
  LOOP AT it_diff INTO DATA(wa_diff).
    rg_diff-low = wa_diff-low.
    rg_diff-sign = 'I'.
    rg_diff-option = 'EQ'.
    APPEND rg_diff.
  ENDLOOP.

  SELECT * FROM ztt_state_code INTO TABLE @DATA(lt_state_code).
  DATA : ls_state_code TYPE ztt_state_code.

  FIELD-SYMBOLS: <gw_final>    TYPE ty_final,
                 <lw_ewaybill> TYPE j_1ig_ewaybill.

  lr_fkart = VALUE lr_fkart_type(
  LET s = 'I'
  o = 'EQ'
  IN sign   = s
  option = o
  ( low = 'ZDOM' )
  ( low = 'ZEXS' )
  ( low = 'ZEXR' )
  ( low = 'ZEXL' )
  ( low = 'ZEXD' )
  ( low = 'ZSEZ' )
  ).

  CLEAR: lt_messages,lt_ewaybill,lt_api_hdr,lt_api_itm,gt_eway_api_hdr,gt_eway_api_itm,lt_show_message.

  PERFORM check_selected_data CHANGING lv_val.

  IF lv_val IS INITIAL.
    CLEAR lv_error.

    IF lv_error IS INITIAL.
      IF gv_download IS INITIAL.
        PERFORM call_popup USING   TEXT-t23 TEXT-t24
        CHANGING lv_answer.
      ELSE.
        lv_answer = '1'.

      ENDIF.
      SELECT * FROM ztt_state_code INTO TABLE @DATA(gt_state_code).
      IF lv_answer = '1'.
        LOOP AT  gt_index INTO lw_index.

          READ TABLE gt_final INTO gw_final INDEX lw_index-index.
          IF sy-subrc IS INITIAL AND ( gw_final-eway_num IS INITIAL OR gw_final-eway_status = 'Cancelled'(062) )
          AND gw_final-eway = abap_true.

            IF gv_doc_download IS INITIAL.
              IF gw_final-einv = abap_true AND gw_final-irn IS INITIAL.
                MESSAGE 'IRN is not yet generated' TYPE 'I'.
                CONTINUE.
              ENDIF.
            ENDIF.
            IF gw_final-t_name = 'SELF TRANSPORT'.
              MESSAGE 'Eway bill is not applicable for Self Transport ' TYPE 'I'.
              CONTINUE.
            ENDIF.


            IF p_mod = gc_sd.
              READ TABLE gt_wb2_v_vbrk_vbrp2 INTO lw_wb2_v_vbrk_vbrp2 WITH KEY vbeln = gw_final-vbeln.
              IF sy-subrc = 0.
                IF lw_wb2_v_vbrk_vbrp2-fksto NE abap_true.
                  lv_index = sy-tabix.

*** Reset header value for every document
                  lv_total_invoice_value   = 0.
                  lv_total_sgst_value      = 0.
                  lv_total_cgst_value      = 0.
                  lv_total_igst_value      = 0.
                  lv_total_cess_value      = 0.
                  lv_total_cessnoval_value = 0.
                  lv_total_taxable_value    = 0.
                  lv_total_other_value    = 0.
                  lv_diff_value    = 0.


                  LOOP AT gt_wb2_v_vbrk_vbrp2 INTO lw_wb2_v_vbrk_vbrp2 FROM lv_index.


                    IF lw_wb2_v_vbrk_vbrp2-vbeln NE gw_final-vbeln.
                      EXIT.
                    ENDIF.

********************************* Begin of Fill header derails   ***********************************************
                    lw_eway_api_hdr-bukrs       = lw_wb2_v_vbrk_vbrp2-bukrs.

                    READ TABLE gt_api  INTO lw_usrgstin WITH KEY apiid = gc_apiid_eusrgstin.
                    IF sy-subrc IS INITIAL.
                      CONCATENATE '"' lw_usrgstin-apiprov '"' INTO lw_eway_api_hdr-user_gstin.
                    ELSE.
                      CONCATENATE '"' gw_final-sup_gstin '"' INTO lw_eway_api_hdr-user_gstin.
                    ENDIF.

                    READ TABLE gt_doctyp INTO ls_doctyp WITH KEY fkart = lw_wb2_v_vbrk_vbrp2-fkart.
                    IF sy-subrc EQ 0.
                      lv_supply_type = ls_doctyp-sup_type.
                      lv_sub_supply_type = ls_doctyp-sub_type.
                      lv_document_type = ls_doctyp-edoc_type.
                    ENDIF.

*& Supply Type
                    READ TABLE gt_supply_values INTO ls_values WITH KEY domvalue_l = lv_supply_type.
                    IF sy-subrc EQ 0.
                      TRANSLATE ls_values-ddtext TO UPPER CASE.
                      CONCATENATE '"' ls_values-ddtext '"' INTO lw_eway_api_hdr-supply_type.
                    ENDIF.

*& Sub Supply Type
                    READ TABLE gt_sub_supply_values INTO ls_values WITH KEY domvalue_l = lv_sub_supply_type.
                    IF sy-subrc EQ 0.
                      TRANSLATE ls_values-ddtext TO UPPER CASE.
                      CONCATENATE '"' ls_values-ddtext '"' INTO lw_eway_api_hdr-sub_supply_type.
                      CONCATENATE '"' ls_values-ddtext '"' INTO lw_eway_api_hdr-sub_supply_description.
                    ENDIF.

*& Document Type
                    READ TABLE gt_doctyp_values INTO ls_values WITH KEY domvalue_l = lv_document_type.
                    IF sy-subrc EQ 0.
                      TRANSLATE ls_values-ddtext TO UPPER CASE.
                      CONCATENATE '"' ls_values-ddtext '"' INTO lw_eway_api_hdr-document_type.
                    ENDIF.

                    CONCATENATE '"' gw_final-gjahr '"' INTO lw_eway_api_hdr-doc_year.

                    CASE lv_document_number.
                      WHEN 1.   "Billing Number as same as ODN
                        CONCATENATE '"' lw_wb2_v_vbrk_vbrp2-vbeln '"' INTO lw_eway_api_hdr-document_number.
                      WHEN 2.   "ODN
                        CONCATENATE '"' lw_wb2_v_vbrk_vbrp2-xblnr '"' INTO lw_eway_api_hdr-document_number.
                    ENDCASE.
                    lw_eway_api_hdr-vbeln = lw_wb2_v_vbrk_vbrp2-vbeln.

                    CONCATENATE '"'
                    lw_wb2_v_vbrk_vbrp2-fkdat+6(2) '/'
                    lw_wb2_v_vbrk_vbrp2-fkdat+4(2) '/'
                    lw_wb2_v_vbrk_vbrp2-fkdat+0(4)
                    '"' INTO lw_eway_api_hdr-document_date.

*** Supplier details
                    CLEAR: lw_t001w. ", lw_adrc_s, lw_adr6_s.
                    READ TABLE gt_t001w INTO lw_t001w WITH  KEY werks = lw_wb2_v_vbrk_vbrp2-werks_i BINARY SEARCH.
                    IF sy-subrc EQ 0.

                      READ TABLE gt_api  INTO lw_usrgstin WITH KEY apiid = gc_apiid_esupgstin.
                      IF sy-subrc IS INITIAL.
                        CONCATENATE '"' lw_usrgstin-apiprov '"' INTO lw_eway_api_hdr-gstin_of_consignor.
                      ELSE.
                        CONCATENATE '"' gw_final-sup_gstin '"' INTO lw_eway_api_hdr-gstin_of_consignor.
                      ENDIF.
                      READ TABLE gt_adrc_s INTO DATA(gs_adrc_s) WITH KEY addrnumber = lw_t001w-adrnr.
                      IF sy-subrc EQ 0.
*                        lw_t001w-name1 = gs_adrc_s-name1.
                      ENDIF.
                      IF gs_adrc_s-name1 IS INITIAL.
                        gs_adrc_s-name1 = lw_t001w-name1.
                      ENDIF.

                      CONCATENATE '"' gs_adrc_s-name1 '"' INTO lw_eway_api_hdr-legal_name_of_consignor.
                      CONCATENATE '"' gs_adrc_s-street '"' INTO lw_eway_api_hdr-address1_of_consignor.
*                      CONCATENATE '"' lw_t001w-name2 '"' INTO lw_eway_api_hdr-address2_of_consignor.
                      lw_eway_api_hdr-address2_of_consignor = '""'.
                      CONCATENATE '"' lw_t001w-ort01 '"' INTO lw_eway_api_hdr-place_of_consignor.
                      CONCATENATE '"' lw_t001w-pstlz '"' INTO lw_eway_api_hdr-pincode_of_consignor.
                      CLEAR:lw_t005u_s.
                      READ TABLE lt_state_code INTO ls_state_code WITH KEY regio = lw_t001w-regio.
                      IF sy-subrc EQ 0.
                        CONCATENATE '"' ls_state_code-gst_regio  '"' INTO  lw_eway_api_hdr-state_of_consignor.
                        lw_eway_api_hdr-actual_from_state_name = lw_eway_api_hdr-state_of_consignor.
                      ELSE.
                        READ TABLE gt_t005u_s INTO lw_t005u_s WITH  KEY bland = lw_t001w-regio.
                        IF sy-subrc EQ 0.
                          TRANSLATE lw_t005u_s-bezei TO UPPER CASE.
                          CONCATENATE '"' lw_t005u_s-bezei  '"' INTO  lw_eway_api_hdr-state_of_consignor.
                          CONCATENATE '"' lw_t005u_s-bezei '"' INTO lw_eway_api_hdr-actual_from_state_name.
                        ENDIF.
                      ENDIF.


                      CLEAR lv_dispatch.
                      IF lw_wb2_v_vbrk_vbrp2-fkart = 'ZTRD'.
                        lv_tdname  = gw_final-vbeln.
                        PERFORM readtext_value USING lv_tdname 'Z023' CHANGING lv_tdline1.
                        PERFORM readtext_value USING lv_tdname 'Z024' CHANGING lv_tdline.
                        IF lv_tdline IS NOT INITIAL.
                          CONCATENATE lv_tdline1 lv_tdline INTO lw_eway_api_hdr-address1_of_consignor SEPARATED BY space.
                          lw_eway_api_hdr-address2_of_consignor = '""'.
                          lv_dispatch = abap_true.
                          PERFORM readtext_value USING lv_tdname 'Z025' CHANGING lv_tdline.
                          IF lv_tdline IS NOT INITIAL.
                            lw_eway_api_hdr-place_of_consignor = lv_tdline.
                          ENDIF.
                          PERFORM readtext_value USING lv_tdname 'Z026' CHANGING lv_tdline.
                          IF lv_tdline IS NOT INITIAL.
                            lw_eway_api_hdr-pincode_of_consignor = lv_tdline.
                          ENDIF.
                          PERFORM readtext_value USING lv_tdname 'Z027' CHANGING lv_tdline.
                          IF lv_tdline IS NOT INITIAL.
                            lw_eway_api_hdr-actual_from_state_name = lv_tdline.
                          ENDIF.
                        ELSE.      " Checking for Port details..
                          SELECT SINGLE low FROM tvarvc INTO @DATA(lv_text1) WHERE name = 'ZPORTCODE_TEXT'.
                          IF sy-subrc EQ 0.
                            DATA : lv_tdid1 TYPE tdid.
                            lv_tdid1 = lv_text1.
                            PERFORM readtext_value USING lv_tdname lv_tdid1 CHANGING lv_tdline.
                            IF lv_tdline IS NOT INITIAL .
                              REPLACE ALL OCCURRENCES OF '"' IN lv_tdline WITH ''.
                              CONDENSE lv_tdline.
                              SELECT SINGLE * FROM ztedoc_export INTO @DATA(eway_export) WHERE zport_code = @lv_tdline.
                              IF sy-subrc EQ 0.
                                CONCATENATE eway_export-zport eway_export-zport_address1 INTO
                                lw_eway_api_hdr-address1_of_consignor SEPARATED BY space.
                                lw_eway_api_hdr-place_of_consignor = eway_export-zport_place.
                                lw_eway_api_hdr-pincode_of_consignor = eway_export-zport_pincode.
                                lw_eway_api_hdr-actual_from_state_name = eway_export-zport_state.
                              ELSE.
                                MESSAGE 'Please maintain port details in zeconfig' TYPE 'E'.
                              ENDIF.
                            ENDIF.
                          ENDIF.

                        ENDIF.

                      ENDIF.
                    ENDIF.

*** Buyer details
                    CLEAR:lw_kna1, lw_adrc, lw_adr6,lv_bill_ship.

                    READ TABLE gt_vbpa INTO lw_vbpa WITH  KEY vbeln = lw_wb2_v_vbrk_vbrp2-vbeln
                    parvw = gc_parvw_re
                    BINARY SEARCH.
                    IF sy-subrc IS INITIAL.
                      READ TABLE gt_vbpa INTO DATA(lw_vbpa_ship) WITH  KEY vbeln = lw_wb2_v_vbrk_vbrp2-vbeln
                            parvw = gc_parvw_we.
                      IF sy-subrc IS INITIAL.
                        IF lw_vbpa-kunnr NE lw_vbpa_ship-kunnr.
                          lv_bill_ship = abap_true.
                        ENDIF.

                        READ TABLE gt_adrc_new INTO DATA(ls_adrc_re) WITH KEY addrnumber  = lw_vbpa-adrnr.
                        IF sy-subrc = 0.
                          READ TABLE gt_adrc_new INTO DATA(ls_adrc_we) WITH KEY addrnumber  = lw_vbpa_ship-adrnr.
                          IF sy-subrc IS INITIAL.
                            IF ls_adrc_re-post_code1 NE ls_adrc_we-post_code1.
                              lv_bill_ship = abap_true.
                            ENDIF.
                          ENDIF.
                        ENDIF.
                      ENDIF.

                      READ TABLE gt_kna1 INTO lw_kna1 WITH  KEY kunnr = lw_vbpa-kunnr BINARY SEARCH.
                      IF sy-subrc EQ 0.

                        READ TABLE gt_api  INTO lw_usrgstin WITH KEY apiid = gc_apiid_ebuygstin.
                        IF sy-subrc IS INITIAL.
                          CONCATENATE '"' lw_usrgstin-apiprov '"' INTO lw_eway_api_hdr-gstin_of_consignee.
                        ELSE.
                          IF lw_kna1-stcd3 IS NOT INITIAL.
                            CONCATENATE '"' lw_kna1-stcd3 '"' INTO lw_eway_api_hdr-gstin_of_consignee.
                          ELSE.
                            lw_eway_api_hdr-gstin_of_consignee = gc_gstin_urp.
                          ENDIF.
                        ENDIF.
                        CONCATENATE '"' lw_kna1-name1 '"' INTO lw_eway_api_hdr-legal_name_of_consignee.

                        CLEAR:lw_t005u.
                        READ TABLE lt_state_code INTO ls_state_code WITH KEY regio = lw_kna1-regio.
                        IF sy-subrc EQ 0.
                          CONCATENATE '"' ls_state_code-gst_regio '"' INTO lw_eway_api_hdr-actual_to_state_name.
                          lw_eway_api_hdr-state_of_supply = lw_eway_api_hdr-actual_to_state_name.
                        ELSE.
                          READ TABLE gt_t005u INTO lw_t005u WITH  KEY bland = lw_kna1-regio.
                          IF sy-subrc EQ 0.
                            TRANSLATE lw_t005u-bezei TO UPPER CASE.
                            CONCATENATE '"' lw_t005u-bezei '"' INTO lw_eway_api_hdr-actual_to_state_name.
                            CONCATENATE '"' lw_t005u-bezei '"' INTO lw_eway_api_hdr-state_of_supply.
                          ENDIF.
                        ENDIF.




                        READ TABLE gt_vbpa INTO lw_vbpa WITH  KEY vbeln = lw_wb2_v_vbrk_vbrp2-vbeln
                        parvw = gc_parvw_we.
*                                                       BINARY SEARCH.
                        IF sy-subrc IS INITIAL.
                          READ TABLE gt_kna1 INTO lw_kna1 WITH KEY kunnr = lw_vbpa-kunnr BINARY SEARCH.
                          IF sy-subrc IS INITIAL.
                            READ TABLE gt_adrc INTO lw_adrc WITH  KEY addrnumber = lw_kna1-adrnr.
                            READ TABLE gt_adr6 INTO lw_adr6 WITH  KEY addrnumber = lw_adrc-addrnumber.

*                            CONCATENATE '"' lw_kna1-name1 '"' INTO lw_eway_api_hdr-address1_of_consignee.
*                            CONCATENATE '"' lw_kna1-stras '"' INTO lw_eway_api_hdr-address2_of_consignee.
*                            CONCATENATE '"' lw_kna1-ort01 '"' INTO lw_eway_api_hdr-place_of_consignee.
*                            CONCATENATE '"' lw_kna1-pstlz '"' INTO lw_eway_api_hdr-pincode_of_consignee.
*                            CLEAR:lw_t005u.
*                            READ TABLE gt_t005u INTO lw_t005u WITH  KEY bland = lw_kna1-regio.
*                            IF sy-subrc EQ 0.
*                              TRANSLATE lw_t005u-bezei TO UPPER CASE.
*
*                              CONCATENATE '"' lw_t005u-bezei '"' INTO lw_eway_api_hdr-actual_to_state_name.
*                            ENDIF.

                            READ TABLE gt_adrc_new INTO DATA(ls_adrc) WITH KEY addrnumber = lw_vbpa-adrnr.
                            IF sy-subrc IS INITIAL.
                              CONCATENATE ls_adrc-name1 ',' ls_adrc-street INTO lw_eway_api_hdr-address1_of_consignee SEPARATED BY space..
                              CONCATENATE '"' lw_eway_api_hdr-address1_of_consignee '"' INTO lw_eway_api_hdr-address1_of_consignee.
                              CONCATENATE '"' ls_adrc-str_suppl1 ls_adrc-str_suppl2 '"' INTO lw_eway_api_hdr-address2_of_consignee.
                              CONCATENATE '"' ls_adrc-city1 '"' INTO lw_eway_api_hdr-place_of_consignee.
                              CONCATENATE '"' ls_adrc-post_code1 '"' INTO lw_eway_api_hdr-pincode_of_consignee.
                              CLEAR:lw_t005u.
                              READ TABLE lt_state_code INTO ls_state_code WITH KEY regio = ls_adrc-region.
                              IF sy-subrc EQ 0.
                                CONCATENATE '"' ls_state_code-gst_regio '"' INTO lw_eway_api_hdr-actual_to_state_name.
                              ELSE.
                                READ TABLE gt_t005u INTO lw_t005u WITH  KEY bland = ls_adrc-region.
                                IF sy-subrc EQ 0.
                                  TRANSLATE lw_t005u-bezei TO UPPER CASE.
                                  CONCATENATE '"' lw_t005u-bezei '"' INTO lw_eway_api_hdr-actual_to_state_name.
                                ENDIF.
                              ENDIF.

                            ENDIF.


* Set the export flag
                            CLEAR lv_export.
                            IF lw_t001w-land1 NE lw_kna1-land1.
                              lv_export = abap_true.
                            ENDIF.


                            IF lw_eway_api_hdr-sub_supply_type      = gc_sub_suptyp_exp OR
                            lv_export = abap_true.
                              lw_eway_api_hdr-state_of_supply       = gc_exp_state.
                              lw_eway_api_hdr-gstin_of_consignee    = gc_gstin_urp.
                              lw_eway_api_hdr-sub_supply_type = '"EXPORT"'.
                              lw_eway_api_hdr-sub_supply_description = '"EXPORT"'.
                              lw_eway_api_hdr-actual_to_state_name = '"97"'.
                              lw_eway_api_hdr-state_of_supply = '"96"'.
                              lw_eway_api_hdr-pincode_of_consignee = '"999999"'.
                              READ TABLE gt_export INTO ls_export WITH KEY zport_code = gw_final-zport_code.
                              IF sy-subrc IS INITIAL.
                                CONCATENATE '"' ls_export-zport_address1 '"' INTO lw_eway_api_hdr-address1_of_consignee.
                                CONCATENATE '"' ls_export-zport_address2 '"' INTO lw_eway_api_hdr-address2_of_consignee.
                                CONCATENATE '"' ls_export-zport_place '"' INTO lw_eway_api_hdr-place_of_consignee.
                                CONCATENATE '"' ls_export-zport_pincode '"' INTO lw_eway_api_hdr-pincode_of_consignee.
                                CLEAR:lw_t005u.
                                LOOP AT gt_state_code INTO DATA(gw_scode) WHERE gst_regio = ls_export-zport_state.
                                  READ TABLE lt_state_code INTO ls_state_code WITH KEY regio = gw_scode-regio.
                                  IF sy-subrc EQ 0.
                                    CONCATENATE '"' ls_state_code-gst_regio '"' INTO lw_eway_api_hdr-actual_to_state_name.
                                  ELSE.
                                    READ TABLE gt_t005u INTO lw_t005u WITH  KEY bland = gw_scode-regio.
                                    IF sy-subrc EQ 0.
                                      TRANSLATE lw_t005u-bezei TO UPPER CASE.
                                      CONCATENATE '"' lw_t005u-bezei '"' INTO lw_eway_api_hdr-actual_to_state_name.
                                    ELSE.
                                      CLEAR lw_t005u.
                                      SELECT SINGLE spras, land1, bland, bezei FROM t005u INTO @lw_t005u WHERE bland = @gw_scode-regio AND land1 = 'IN'.
                                      IF sy-subrc IS INITIAL.
                                        TRANSLATE lw_t005u-bezei TO UPPER CASE.
                                        CONCATENATE '"' lw_t005u-bezei '"' INTO lw_eway_api_hdr-actual_to_state_name.
                                      ENDIF.
                                    ENDIF.
                                  ENDIF.

                                  CLEAR:lw_t005u.
                                ENDLOOP.

*                                READ TABLE gt_t005u INTO lw_t005u WITH  KEY bland = ls_export-zport_state.
*                                IF sy-subrc EQ 0.
*                                  TRANSLATE lw_t005u-bezei TO UPPER CASE.
*                                  CONCATENATE '"' lw_t005u-bezei '"' INTO lw_eway_api_hdr-actual_to_state_name.
*                                ENDIF.
                              ENDIF.
*                              if lw_eway_api_hdr-pincode_of_consignee = '""'.
*                                lw_eway_api_hdr-pincode_of_consignee = '"999999"'.
*                              endif.
*                              if lw_eway_api_hdr-state_of_supply = '"OTHER COUNTRY"'.
*                                  lw_eway_api_hdr-state_of_supply = '"96"'.
*                              endif.
                              IF sy-sysid = 'DS4'.
                                lw_eway_api_hdr-user_gstin = '"05AAABB0639G1Z8"'.
                                lw_eway_api_hdr-gstin_of_consignor = '"05AAABB0639G1Z8"'.
                              ENDIF.
                            ELSE.
                              IF sy-sysid = 'DS4'.
                                lw_eway_api_hdr-user_gstin = '"05AAABB0639G1Z8"'.
                                lw_eway_api_hdr-gstin_of_consignor = '"05AAABB0639G1Z8"'.
                                lw_eway_api_hdr-gstin_of_consignee = '"05AAABC0181E1ZE"'.
                              ENDIF.
                            ENDIF.
                          ENDIF.
                        ENDIF.
                      ENDIF.
                    ENDIF.

*** Value details

                    "default value
                    lw_eway_api_hdr-other_value            = '0'.
                    lw_eway_api_hdr-total_invoice_value    = '0'.
                    lw_eway_api_hdr-taxable_amount         = '0'.
                    lw_eway_api_hdr-cgst_amount            = '0'.
                    lw_eway_api_hdr-sgst_amount            = '0'.
                    lw_eway_api_hdr-igst_amount            = '0'.
                    lw_eway_api_hdr-cess_amount            = '0'.
                    lw_eway_api_hdr-cess_nonadvol_value    = '0'.


*                    """"""""added by manisha
*                    IF lw_wb2_v_vbrk_vbrp2-fkart = 'ZSN'.
*                      CLEAR lv_tdname.
*                      lv_tdname = lw_wb2_v_vbrk_vbrp2-vbeln.
*                      PERFORM readtext_value USING lv_tdname 'Z032' CHANGING lv_tdline.
*                      lv_bill_ship = abap_true.
*                      IF lv_tdline IS NOT INITIAL.
*                        lw_eway_api_hdr-address1_of_consignee = lv_tdline.
*                      ENDIF.
*                      PERFORM readtext_value USING lv_tdname 'Z033' CHANGING lv_tdline.
*                      IF lv_tdline IS NOT INITIAL.
*                        lw_eway_api_hdr-address2_of_consignee  = lv_tdline.
*                      ENDIF.
*                      PERFORM readtext_value USING lv_tdname 'Z034' CHANGING lv_tdline.
*                      IF lv_tdline IS NOT INITIAL.
*                        lw_eway_api_hdr-place_of_consignee = lv_tdline.
*                      ENDIF.
*                      PERFORM readtext_value USING lv_tdname 'Z035' CHANGING lv_tdline.
*                      IF lv_tdline IS NOT INITIAL.
*                        lw_eway_api_hdr-pincode_of_consignee  = lv_tdline.
*                      ENDIF.
*                      PERFORM readtext_value USING lv_tdname 'Z036' CHANGING lv_tdline.
*                      IF lv_tdline IS NOT INITIAL.
*                        lw_eway_api_hdr-state_of_supply  = lv_tdline.
*                        lw_eway_api_hdr-actual_to_state_name = lv_tdline.
*                      ENDIF.
*
*
*                    ENDIF.
*                    """""""ended by manisha

* Transaction Type

                    lw_eway_api_hdr-transaction_type = 1.
                    IF lv_bill_ship = abap_true.
                      lw_eway_api_hdr-transaction_type = 2.
                    ENDIF.
                    IF lv_dispatch = abap_true AND lv_bill_ship = abap_false.
                      lw_eway_api_hdr-transaction_type = 3.
                    ENDIF.
                    IF lv_dispatch = abap_true AND lv_bill_ship = abap_true.
                      lw_eway_api_hdr-transaction_type = 4.
                    ENDIF.
                    CONDENSE lw_eway_api_hdr-transaction_type.

                    lw_eway_api_hdr-generate_status = gc_gen_sts_1.
                    lw_eway_api_hdr-data_source = gc_dsource_erp.
                    CONCATENATE '"' sy-uname '"' INTO lw_eway_api_hdr-user_ref.
                    CONCATENATE '"' lw_kna1-ort01 '"' INTO lw_eway_api_hdr-location_code.

*& Generate E-Way Bill Part A & Part B.
                    IF gv_e_comm EQ gc_ucomm_eeayg.
                      lw_eway_api_hdr-eway_bill_status = gc_ewaysts_abc.
                    ELSE.
*& Generate E-Way Bill Part A only.
                      lw_eway_api_hdr-eway_bill_status = gc_ewaysts_ac.
                    ENDIF.

*& Auto print E-way Bill
                    lw_eway_api_hdr-auto_print = '" "'.
                    CONCATENATE '"' '"' INTO lw_eway_api_hdr-email.

*** Transport details

                    CONCATENATE '"' gw_final-t_id '"' INTO lw_eway_api_hdr-transporter_id.

                    CONCATENATE '"' gw_final-t_name '"' INTO lw_eway_api_hdr-transporter_name.

                    READ TABLE gt_t_mode_values INTO ls_values WITH KEY domvalue_l = gw_final-t_mode.
                    IF sy-subrc EQ 0.
                      TRANSLATE ls_values-ddtext TO UPPER CASE.
                      CONCATENATE '"' ls_values-ddtext '"' INTO lw_eway_api_hdr-transportation_mode.
                    ELSE.
                      CONCATENATE '"'  '"' INTO lw_eway_api_hdr-transportation_mode.
                    ENDIF.

                    CONCATENATE '"' gw_final-t_distance '"' INTO lw_eway_api_hdr-transportation_distance.

                    CONCATENATE '"' gw_final-t_doc_no '"' INTO lw_eway_api_hdr-transporter_document_number.
                    CONCATENATE '"' gw_final-t_date '"' INTO lw_eway_api_hdr-transporter_document_date.

                    CONCATENATE '"' gw_final-v_number '"' INTO lw_eway_api_hdr-vehicle_number.
                    READ TABLE gt_v_type_values INTO ls_values WITH KEY domvalue_l = gw_final-v_type.
                    IF sy-subrc EQ 0.
                      TRANSLATE ls_values-ddtext TO UPPER CASE.
                      CONCATENATE '"' ls_values-ddtext '"' INTO lw_eway_api_hdr-vehicle_type.
                    ENDIF.


* Fill eway billby IRN related fields:
                    CONCATENATE '"' gw_final-sup_gstin '"' INTO lw_eway_api_hdr-user_gstin_irn.
                    CONCATENATE '"' gw_final-t_mode '"' INTO lw_eway_api_hdr-t_mode.
                    IF gw_final-v_type = '1'.
                      lw_eway_api_hdr-v_type = '"R"'.
                    ELSEIF gw_final-v_type = '2'.
                      lw_eway_api_hdr-v_type = '"O"'.
                    ENDIF.


*********************************   End of Fill header derails   ***********************************************


********************************** Begin of Fill item derails   ************************************************

* check for export currency
                    CLEAR lv_exp_curr.
                    IF lw_wb2_v_vbrk_vbrp2-waerk NE  gc_curr_inr .
                      lv_exp_curr = abap_true.
                    ENDIF.


*** Rest item tax rate
                    lw_eway_api_itm-cgst_rate = '0'.
                    lw_eway_api_itm-sgst_rate = '0'.
                    lw_eway_api_itm-igst_rate = '0'.
                    lw_eway_api_itm-cess_rate = '0'.
                    lw_eway_api_itm-cessnonadvol = '0'.


                    lw_eway_api_itm-bukrs            = lw_eway_api_hdr-bukrs.
                    lw_eway_api_itm-document_number  = lw_eway_api_hdr-document_number.
                    lw_eway_api_itm-doc_year         = lw_eway_api_hdr-doc_year.
                    lw_eway_api_itm-document_type    = lw_eway_api_hdr-document_type.
                    lw_eway_api_itm-document_date    = lw_eway_api_hdr-document_date.
                    REPLACE ALL  OCCURRENCES OF '"' IN lw_wb2_v_vbrk_vbrp2-arktx_i WITH space.
*                    CONCATENATE '"' lw_wb2_v_vbrk_vbrp2-matnr_i '"' INTO lw_eway_api_itm-product_name.
                    CONCATENATE '"' lw_wb2_v_vbrk_vbrp2-arktx_i '"' INTO lw_eway_api_itm-product_name.
*                    CONCATENATE '"' lw_wb2_v_vbrk_vbrp2-arktx_i '"' INTO lw_eway_api_itm-product_description.
                    CONCATENATE '"'  '"' INTO lw_eway_api_itm-product_description.
***** ---logic for hsncode based on billing type----added by raghu on 27.03.2023****

                    IF lw_wb2_v_vbrk_vbrp2-fkart IN lr_fkart.
                      lv_tdname  = gw_final-vbeln.
                      CALL FUNCTION 'READ_TEXT'
                        EXPORTING
                          client                  = sy-mandt
                          id                      = 'Z021'
                          language                = sy-langu
                          name                    = lv_tdname
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
                      IF sy-subrc = 0.
                        READ TABLE lt_lines INTO lw_lines INDEX 1.
                        IF lw_lines-tdline IS NOT INITIAL.
                          CONCATENATE  '"' lw_lines-tdline '"' INTO lw_eway_api_itm-hsn_code.
                        ENDIF.
                      ENDIF.
                    ENDIF.

                    IF lw_eway_api_itm-hsn_code IS INITIAL.
                      READ TABLE gt_marc INTO lw_marc WITH KEY matnr = lw_wb2_v_vbrk_vbrp2-matnr_i
                      werks = lw_wb2_v_vbrk_vbrp2-werks_i
                      BINARY SEARCH.
                      IF sy-subrc EQ 0.
                        CONCATENATE '"' lw_marc-steuc '"' INTO lw_eway_api_itm-hsn_code.
                      ENDIF.
                    ENDIF.
******------------end-----------------------------*****
                    lw_eway_api_itm-quantity         = lw_wb2_v_vbrk_vbrp2-fkimg_i.

                    CLEAR lv_output_uom.
                    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                      EXPORTING
                        input          = lw_wb2_v_vbrk_vbrp2-vrkme_i
                      IMPORTING
                        output         = lv_output_uom
                      EXCEPTIONS
                        unit_not_found = 1
                        OTHERS         = 2.
                    IF sy-subrc <> 0.
* Implement suitable error handling here
                    ENDIF.

                    READ TABLE gt_doc_uom INTO ls_doc_uom WITH KEY meins = lv_output_uom.
                    IF sy-subrc EQ 0.
                      CONCATENATE '"' ls_doc_uom-edoc_uom '"' INTO lw_eway_api_itm-unit_of_product.
                    ELSE.

                      READ TABLE gt_doc_uom INTO ls_doc_uom WITH KEY meins =  lw_wb2_v_vbrk_vbrp2-vrkme_i.
                      IF sy-subrc IS INITIAL.
                        CONCATENATE '"' ls_doc_uom-edoc_uom '"' INTO lw_eway_api_itm-unit_of_product.
                      ELSE.
                        CONCATENATE '"' lw_wb2_v_vbrk_vbrp2-vrkme_i '"' INTO  lw_eway_api_itm-unit_of_product.

                      ENDIF.
                    ENDIF.
                    "Item Amount

                    IF lw_wb2_v_vbrk_vbrp2-fkart = 'ZSAM'.
                      READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                      kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                      kschl = 'ZSAM'.
                      IF sy-subrc IS INITIAL.
                        lw_wb2_v_vbrk_vbrp2-netwr_i = lw_konv-kwert.
                      ENDIF.
                    ENDIF.


                    READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                    kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                    kschl = 'ZCES'.
                    IF sy-subrc IS INITIAL.
                      lw_wb2_v_vbrk_vbrp2-netwr_i = lw_wb2_v_vbrk_vbrp2-netwr_i - lw_konv-kwert.
                    ENDIF.
                    DATA lv_diff TYPE kschl VALUE IS INITIAL.
                    DATA:      lv_total_roundoff            TYPE wertv13.
                    CLEAR lv_diff.

                    lv_diff = 'DIFF'.
                    READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                                                             kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                                                             kschl = lv_diff BINARY SEARCH.

                    IF sy-subrc EQ 0.
                      lw_wb2_v_vbrk_vbrp2-netwr_i = lw_wb2_v_vbrk_vbrp2-netwr_i - lw_konv-kwert.
                      lv_total_roundoff = lv_total_roundoff + lw_konv-kwert.
*                        lw_einv_api_hdr-round_off_amount = lw_einv_api_hdr-round_off_amount + lw_konv-kwert.
                    ENDIF.


                    lw_eway_api_itm-taxable_amount = lw_wb2_v_vbrk_vbrp2-netwr_i.
                    IF lv_exp_curr IS NOT INITIAL.
                      PERFORM convert_currency USING lw_eway_api_itm-taxable_amount lw_wb2_v_vbrk_vbrp2-kurrf
                      CHANGING lw_eway_api_itm-taxable_amount.
                    ENDIF.

                    IF lw_wb2_v_vbrk_vbrp2-fkart NE 'ZSTI'.
                      IF lw_wb2_v_vbrk_vbrp2-fkart NE 'ZSAM'.

                        "CGST Tax amount
                        READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                        kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                        kschl = gc_kschl_jocg.
                        IF sy-subrc IS NOT INITIAL.
                          READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                          kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                          kschl = 'ZCGS'.
                        ENDIF.
                        IF sy-subrc EQ 0.
                          IF lv_exp_curr IS NOT INITIAL.
                            PERFORM convert_currency USING lw_konv-kwert lw_wb2_v_vbrk_vbrp2-kurrf
                            CHANGING lw_konv-kwert.
                          ENDIF.
                          lw_eway_api_itm-cgst_rate = lw_konv-kbetr.
                          lv_total_cgst_value = lv_total_cgst_value + lw_konv-kwert.
                        ENDIF.
                        "SGST Tax amount
                        READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                        kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                        kschl = gc_kschl_josg.
                        IF sy-subrc IS NOT INITIAL.
                          READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                          kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                          kschl = gc_kschl_joug.
                          IF sy-subrc IS NOT INITIAL.
                            READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                            kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                            kschl = 'ZSGS'.
                          ENDIF.
                        ENDIF.
                        IF sy-subrc EQ 0.
                          IF lv_exp_curr IS NOT INITIAL.
                            PERFORM convert_currency USING lw_konv-kwert lw_wb2_v_vbrk_vbrp2-kurrf
                            CHANGING lw_konv-kwert.
                          ENDIF.
                          lw_eway_api_itm-sgst_rate = lw_konv-kbetr.
                          lv_total_sgst_value = lv_total_sgst_value + lw_konv-kwert.
                        ENDIF.
                        "IGST Tax amount
                        READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                        kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                        kschl = gc_kschl_joig.
                        IF sy-subrc IS NOT INITIAL.
                          READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                          kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                          kschl = 'ZIGS'.
                        ENDIF.
                        IF sy-subrc EQ 0.
                          IF lv_exp_curr IS NOT INITIAL.
                            PERFORM convert_currency USING lw_konv-kwert lw_wb2_v_vbrk_vbrp2-kurrf
                            CHANGING lw_konv-kwert.
                          ENDIF.
                          lw_eway_api_itm-igst_rate = lw_konv-kbetr.
                          lv_total_igst_value = lv_total_igst_value + lw_konv-kwert.
                        ENDIF.
                        "CESS Tax amount
                        READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                        kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                        kschl = gc_kschl_cess.
                        IF sy-subrc EQ 0.
                          IF lv_exp_curr IS NOT INITIAL.
                            PERFORM convert_currency USING lw_konv-kwert lw_wb2_v_vbrk_vbrp2-kurrf
                            CHANGING lw_konv-kwert.
                          ENDIF.
                          lw_eway_api_itm-cess_rate = lw_konv-kbetr.
                          lv_total_cess_value = lv_total_cess_value + lw_konv-kwert.
                        ENDIF.

                        READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
                        kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                        kschl = 'ZCES'.
                        IF sy-subrc EQ 0.
                          IF lv_exp_curr IS NOT INITIAL.
                            PERFORM convert_currency USING lw_konv-kwert lw_wb2_v_vbrk_vbrp2-kurrf
                            CHANGING lw_konv-kwert.
                          ENDIF.
                          lw_eway_api_itm-cessnonadvol = lw_konv-kbetr.
                          lv_total_cessnoval_value = lv_total_cessnoval_value + lw_konv-kwert.
                        ENDIF.

                        "TCS Charges JTC1

                        LOOP AT gt_konv INTO lw_konv WHERE knumv = lw_wb2_v_vbrk_vbrp2-knumv
                        AND kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                        AND ( kschl EQ gc_kschl_jtc1 OR kschl EQ gc_kschl_ztcs OR kschl EQ 'JTC2' ).
                          IF lv_exp_curr IS NOT INITIAL.
                            PERFORM convert_currency USING lw_konv-kwert lw_wb2_v_vbrk_vbrp2-kurrf
                            CHANGING lw_konv-kwert.
                          ENDIF.
                          lv_total_other_value = lv_total_other_value + lw_konv-kwert.
                        ENDLOOP.

                        IF rg_diff IS NOT INITIAL.
                          LOOP AT gt_konv INTO lw_konv WHERE knumv = lw_wb2_v_vbrk_vbrp2-knumv
                          AND kposn = lw_wb2_v_vbrk_vbrp2-posnr_i
                          AND kschl IN rg_diff.

                            IF lv_exp_curr IS NOT INITIAL.
                              PERFORM convert_currency USING lw_konv-kwert lw_wb2_v_vbrk_vbrp2-kurrf
                              CHANGING lw_konv-kwert.
                            ENDIF.

                            lv_diff_value = lv_diff_value + lw_konv-kwert.
*                      lw_eway_api_itm-cess_rate = lw_konv-kbetr.
*                      lv_total_cess_value = lv_total_cess_value + lw_konv-kwert.
                          ENDLOOP.
                        ENDIF.

                      ENDIF.
                    ENDIF.

                    "lw_wb2_v_vbrk_vbrp2-netwr_i.
                    IF lw_wb2_v_vbrk_vbrp2-fkart = 'ZJEP' AND lw_eway_api_itm-quantity IS NOT INITIAL .
                      DATA : lv_unit_price TYPE dmbtr.
                      lv_unit_price = lw_eway_api_itm-taxable_amount / lw_eway_api_itm-quantity.
                      lw_eway_api_itm-taxable_amount = lv_unit_price * lw_eway_api_itm-quantity.
*                      lw_eway_api_itm-
                    ENDIF.


                    IF lw_wb2_v_vbrk_vbrp2-fkart EQ 'ZJEP' AND lw_wb2_v_vbrk_vbrp2-pstyv_i = 'ZANN'..
                      lw_eway_api_itm-taxable_amount = 0.
                    ELSE.
                      lv_total_taxable_value  = lv_total_taxable_value  + lw_eway_api_itm-taxable_amount.
                    ENDIF.
                    APPEND lw_eway_api_itm TO gt_eway_api_itm.
                    CLEAR:lw_eway_api_itm.

***********************************  End of Fill item derails   ************************************************
                  ENDLOOP.


**** Header total values
                  lv_total_taxable_value = lv_total_taxable_value - lv_diff_value.
                  lw_eway_api_hdr-total_invoice_value  = lv_total_taxable_value +
                  lv_total_cgst_value +
                  lv_total_sgst_value +
                  lv_total_igst_value +
                  lv_total_cess_value +
                  lv_total_cessnoval_value +
                  lv_total_other_value
                  + lv_total_roundoff.
*                  IF lv_diff_value LT 0.
*                    lv_diff_value = lv_diff_value * -1.
*                  ENDIF.
                  lw_eway_api_hdr-other_value          = lv_total_other_value + lv_diff_value + lv_total_roundoff.
                  IF lw_eway_api_hdr-other_value CS '-'.
                    REPLACE ALL OCCURRENCES OF '-' IN lw_eway_api_hdr-other_value WITH ''.
                    CONCATENATE '-' lw_eway_api_hdr-other_value INTO lw_eway_api_hdr-other_value.
                    CONDENSE lw_eway_api_hdr-other_value NO-GAPS.
                  ENDIF.
                  lw_eway_api_hdr-taxable_amount       = lv_total_taxable_value.
                  lw_eway_api_hdr-cgst_amount          = lv_total_cgst_value.
                  lw_eway_api_hdr-sgst_amount          = lv_total_sgst_value.
                  lw_eway_api_hdr-igst_amount          = lv_total_igst_value.
                  lw_eway_api_hdr-cess_amount          = lv_total_cess_value.
                  lw_eway_api_hdr-cess_nonadvol_value  = lv_total_cessnoval_value.

*                  IF ( lv_total_other_value + lv_total_roundoff ) < 0.
*                    lw_eway_api_hdr-other_value = ( lv_total_other_value + lv_total_roundoff ) * -1.
*                    CONDENSE lw_eway_api_hdr-other_value.
*                    CONCATENATE '-' lw_eway_api_hdr-other_value INTO lw_eway_api_hdr-other_value.
*                  ELSE.
*                    lw_eway_api_hdr-other_value          = lv_total_other_value + lv_total_roundoff.
*                  ENDIF.
*                  lw_eway_api_hdr-other_value          = lv_total_other_value + lv_total_roundoff.

*                  IF lw_eway_api_hdr-other_value < 0.
*                    lw_eway_api_hdr-other_value  = lw_eway_api_hdr-other_value * -1.
*                    CONCATENATE '-' lw_eway_api_hdr-other_value INTO lw_eway_api_hdr-other_value.
*                  ENDIF.
*                  CONDENSE lw_eway_api_hdr-other_value NO-GAPS.

                  APPEND lw_eway_api_hdr TO gt_eway_api_hdr.
                  CLEAR:lw_eway_api_hdr , lv_total_roundoff , lv_total_other_value , lv_diff_value.

                ELSE.

                  MESSAGE 'Please select appropriate documents for E-Doc generation'(039) TYPE 'I'.
                ENDIF.


**** Header total values
* Delivery Challan
              ELSE.

                READ TABLE gt_wb2_v_likp_lips2 INTO lw_wb2_v_likp_lips2 WITH KEY vbeln = gw_final-vbeln.
                IF sy-subrc = 0.


                  lv_index = sy-tabix.

*** Reset header value for every document
                  lv_total_invoice_value   = 0.
                  lv_total_sgst_value      = 0.
                  lv_total_cgst_value      = 0.
                  lv_total_igst_value      = 0.
                  lv_total_cess_value      = 0.
                  lv_total_cessnoval_value = 0.
                  lv_total_taxable_value   = 0.

                  LOOP AT gt_wb2_v_likp_lips2 INTO lw_wb2_v_likp_lips2 FROM lv_index.
                    IF lw_wb2_v_likp_lips2-vbeln NE gw_final-vbeln.
                      EXIT.
                    ENDIF.

********************************* Begin of Fill header derails   ***********************************************
                    lw_eway_api_hdr-bukrs       = p_ccode.

                    READ TABLE gt_api  INTO lw_usrgstin WITH KEY apiid = gc_apiid_eusrgstin.
                    IF sy-subrc IS INITIAL.
                      CONCATENATE '"' lw_usrgstin-apiprov '"' INTO lw_eway_api_hdr-user_gstin.
                    ELSE.
                      CONCATENATE '"' gw_final-sup_gstin '"' INTO lw_eway_api_hdr-user_gstin.
                    ENDIF.

                    READ TABLE gt_doctyp INTO ls_doctyp WITH KEY fkart = lw_wb2_v_likp_lips2-lfart.
                    IF sy-subrc EQ 0.
                      lv_supply_type = ls_doctyp-sup_type.
                      lv_sub_supply_type = ls_doctyp-sub_type.
                      lv_document_type = ls_doctyp-edoc_type.
                    ENDIF.

*& Supply Type
                    READ TABLE gt_supply_values INTO ls_values WITH KEY domvalue_l = lv_supply_type.
                    IF sy-subrc EQ 0.
                      TRANSLATE ls_values-ddtext TO UPPER CASE.
                      CONCATENATE '"' ls_values-ddtext '"' INTO lw_eway_api_hdr-supply_type.
                    ENDIF.

*& Sub Supply Type
                    READ TABLE gt_sub_supply_values INTO ls_values WITH KEY domvalue_l = lv_sub_supply_type.
                    IF sy-subrc EQ 0.
                      TRANSLATE ls_values-ddtext TO UPPER CASE.
                      CONCATENATE '"' ls_values-ddtext '"' INTO lw_eway_api_hdr-sub_supply_type.
                      CONCATENATE '"' ls_values-ddtext '"' INTO lw_eway_api_hdr-sub_supply_description.
                    ENDIF.

*& Document Type
                    READ TABLE gt_doctyp_values INTO ls_values WITH KEY domvalue_l = lv_document_type.
                    IF sy-subrc EQ 0.
                      TRANSLATE ls_values-ddtext TO UPPER CASE.

                      CONCATENATE '"' ls_values-ddtext '"' INTO lw_eway_api_hdr-document_type.
                    ENDIF.

                    CONCATENATE '"' lw_wb2_v_likp_lips2-lfgja_i '"' INTO lw_eway_api_hdr-doc_year.

                    CASE lv_document_number.
                      WHEN 1.   "Billing Number as same as ODN
                        CONCATENATE '"' lw_wb2_v_likp_lips2-vbeln '"' INTO lw_eway_api_hdr-document_number.
                      WHEN 2.   "ODN
                        CONCATENATE '"' lw_wb2_v_likp_lips2-xblnr '"' INTO lw_eway_api_hdr-document_number.
                    ENDCASE.
                    lw_eway_api_hdr-vbeln = lw_wb2_v_likp_lips2-vbeln.

                    CONCATENATE '"'
                    lw_wb2_v_likp_lips2-fkdat+6(2) '/'
                    lw_wb2_v_likp_lips2-fkdat+4(2) '/'
                    lw_wb2_v_likp_lips2-fkdat+0(4)
                    '"' INTO lw_eway_api_hdr-document_date.

*** Supplier details
                    CLEAR: lw_t001w.
                    READ TABLE gt_t001w INTO lw_t001w WITH  KEY werks = lw_wb2_v_likp_lips2-werks_i BINARY SEARCH.
                    IF sy-subrc EQ 0.

                      READ TABLE gt_api  INTO lw_usrgstin WITH KEY apiid = gc_apiid_esupgstin.
                      IF sy-subrc IS INITIAL.
                        CONCATENATE '"' lw_usrgstin-apiprov '"' INTO lw_eway_api_hdr-gstin_of_consignor.
                      ELSE.
                        CONCATENATE '"' gw_final-sup_gstin '"' INTO lw_eway_api_hdr-gstin_of_consignor.
                      ENDIF.


                      CONCATENATE '"' lw_t001w-name1 '"' INTO lw_eway_api_hdr-legal_name_of_consignor.
                      CONCATENATE '"' lw_t001w-name2 '"' INTO lw_eway_api_hdr-address1_of_consignor.
                      CONCATENATE '"' lw_t001w-stras '"' INTO lw_eway_api_hdr-address2_of_consignor.
                      CONCATENATE '"' lw_t001w-ort01 '"' INTO lw_eway_api_hdr-place_of_consignor.
                      CONCATENATE '"' lw_t001w-pstlz '"' INTO lw_eway_api_hdr-pincode_of_consignor.
                      CLEAR:lw_t005u_s.
                      READ TABLE gt_t005u_s INTO lw_t005u_s WITH  KEY bland = lw_t001w-regio.
                      IF sy-subrc EQ 0.
                        TRANSLATE lw_t005u_s-bezei TO UPPER CASE.
                        CONCATENATE '"' lw_t005u_s-bezei  '"' INTO  lw_eway_api_hdr-state_of_consignor.
                      ENDIF.

                      CONCATENATE '"' lw_t005u_s-bezei '"' INTO lw_eway_api_hdr-actual_from_state_name.

                    ELSE.
                    ENDIF.

*** Buyer details
                    CLEAR:lw_kna1, lw_adrc, lw_adr6.
                    READ TABLE gt_kna1 INTO lw_kna1 WITH  KEY kunnr = lw_wb2_v_likp_lips2-kunag BINARY SEARCH.
                    IF sy-subrc EQ 0.

                      READ TABLE gt_api  INTO lw_usrgstin WITH KEY apiid = gc_apiid_ebuygstin.
                      IF sy-subrc IS INITIAL.
                        CONCATENATE '"' lw_usrgstin-apiprov '"' INTO lw_eway_api_hdr-gstin_of_consignee.
                      ELSE.
                        CONCATENATE '"' lw_kna1-stcd3 '"' INTO lw_eway_api_hdr-gstin_of_consignee.
                      ENDIF.

                      READ TABLE gt_adrc INTO lw_adrc WITH  KEY addrnumber = lw_kna1-adrnr.
                      READ TABLE gt_adr6 INTO lw_adr6 WITH  KEY addrnumber = lw_adrc-addrnumber.

                      CONCATENATE '"' lw_kna1-name1 '"' INTO lw_eway_api_hdr-legal_name_of_consignee.
                      CONCATENATE '"' lw_kna1-stras '"' INTO lw_eway_api_hdr-address1_of_consignee.
                      CONCATENATE '"' lw_kna1-name2 '"' INTO lw_eway_api_hdr-address2_of_consignee.
                      CONCATENATE '"' lw_kna1-ort01 '"' INTO lw_eway_api_hdr-place_of_consignee.
                      CONCATENATE '"' lw_t001w-pstlz '"' INTO lw_eway_api_hdr-pincode_of_consignee.
                      CLEAR:lw_t005u.
                      READ TABLE gt_t005u INTO lw_t005u WITH  KEY bland = lw_kna1-regio.
                      IF sy-subrc EQ 0.
                        TRANSLATE lw_t005u-bezei TO UPPER CASE.
                        CONCATENATE '"' lw_t005u-bezei '"' INTO lw_eway_api_hdr-state_of_supply.
                      ENDIF.
                      CONCATENATE '"' lw_t005u-bezei '"' INTO lw_eway_api_hdr-actual_to_state_name.

                    ENDIF.

*** Value details

                    "default value
                    lw_eway_api_hdr-other_value            = '0'.
                    lw_eway_api_hdr-total_invoice_value    = '0'.
                    lw_eway_api_hdr-taxable_amount         = '0'.
                    lw_eway_api_hdr-cgst_amount            = '0'.
                    lw_eway_api_hdr-sgst_amount            = '0'.
                    lw_eway_api_hdr-igst_amount            = '0'.
                    lw_eway_api_hdr-cess_amount            = '0'.
                    lw_eway_api_hdr-cess_nonadvol_value    = '0'.


                    CONCATENATE '"' 'REGULAR' '"' INTO lw_eway_api_hdr-transaction_type.
                    CONCATENATE '"' '1' '"' INTO lw_eway_api_hdr-generate_status.
                    CONCATENATE '"' 'ERP' '"' INTO lw_eway_api_hdr-data_source.
                    CONCATENATE '"' sy-uname '"' INTO lw_eway_api_hdr-user_ref.
                    CONCATENATE '"' lw_kna1-ort01 '"' INTO lw_eway_api_hdr-location_code.

*& Generate E-Way Bill Part A & Part B.
                    IF gv_e_comm EQ 'EEAYG'.
                      CONCATENATE '"' 'ABC' '"' INTO lw_eway_api_hdr-eway_bill_status.
                    ELSE.
*& Generate E-Way Bill Part A only.
                      CONCATENATE '"' 'AC' '"' INTO lw_eway_api_hdr-eway_bill_status.
                    ENDIF.

*& Auto print E-way Bill
                    CONCATENATE '"' 'Y' '"' INTO lw_eway_api_hdr-auto_print.
                    CONCATENATE '"' lw_adr6-smtp_addr '"' INTO lw_eway_api_hdr-email.

*** Transport details


                    CONCATENATE '"' gw_final-t_id '"' INTO lw_eway_api_hdr-transporter_id.
                    CONCATENATE '"' gw_final-t_name '"' INTO lw_eway_api_hdr-transporter_name.

                    READ TABLE gt_t_mode_values INTO ls_values WITH KEY domvalue_l = gw_final-t_mode.
                    IF sy-subrc EQ 0.
                      TRANSLATE ls_values-ddtext TO UPPER CASE.
                      CONCATENATE '"' ls_values-ddtext '"' INTO lw_eway_api_hdr-transportation_mode.
                    ENDIF.
                    CONCATENATE '"' gw_final-t_distance '"' INTO lw_eway_api_hdr-transportation_distance.

                    CONCATENATE '"' gw_final-t_doc_no '"' INTO lw_eway_api_hdr-transporter_document_number.
                    CONCATENATE '"' gw_final-t_date '"' INTO lw_eway_api_hdr-transporter_document_date.

                    CONCATENATE '"' gw_final-v_number '"' INTO lw_eway_api_hdr-vehicle_number.
                    READ TABLE gt_v_type_values INTO ls_values WITH KEY domvalue_l = gw_final-v_type.
                    IF sy-subrc EQ 0.
                      TRANSLATE ls_values-ddtext TO UPPER CASE.
                      CONCATENATE '"' ls_values-ddtext '"' INTO lw_eway_api_hdr-vehicle_type.
                    ENDIF.

*********************************   End of Fill header derails   ***********************************************


********************************** Begin of Fill item derails   ************************************************
*** Rest item tax rate
                    lw_eway_api_itm-cgst_rate = '0'.
                    lw_eway_api_itm-sgst_rate = '0'.
                    lw_eway_api_itm-igst_rate = '0'.
                    lw_eway_api_itm-cess_rate = '0'.
                    lw_eway_api_itm-cessnonadvol = '0'.


                    lw_eway_api_itm-bukrs            = lw_eway_api_hdr-bukrs.
                    lw_eway_api_itm-document_number  = lw_eway_api_hdr-document_number.
                    lw_eway_api_itm-doc_year         = lw_eway_api_hdr-doc_year.
                    lw_eway_api_itm-document_type    = lw_eway_api_hdr-document_type.
                    lw_eway_api_itm-document_date    = lw_eway_api_hdr-document_date.

                    CONCATENATE '"' lw_wb2_v_likp_lips2-matnr_i '"' INTO lw_eway_api_itm-product_name.
                    CONCATENATE '"' lw_wb2_v_likp_lips2-arktx_i '"' INTO lw_eway_api_itm-product_description.

                    READ TABLE gt_marc INTO lw_marc WITH KEY matnr = lw_wb2_v_likp_lips2-matnr_i
                    werks = lw_wb2_v_likp_lips2-werks_i
                    BINARY SEARCH.
                    IF sy-subrc EQ 0.
                      CONCATENATE '"' lw_marc-steuc '"' INTO lw_eway_api_itm-hsn_code.
                    ENDIF.

                    lw_eway_api_itm-quantity         = lw_wb2_v_likp_lips2-lfimg_i.

                    CLEAR lv_output_uom.
                    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                      EXPORTING
                        input          = lw_wb2_v_likp_lips2-meins_i
                      IMPORTING
                        output         = lv_output_uom
                      EXCEPTIONS
                        unit_not_found = 1
                        OTHERS         = 2.
                    IF sy-subrc <> 0.
* Implement suitable error handling here
                    ENDIF.

                    READ TABLE gt_doc_uom INTO ls_doc_uom WITH KEY meins = lv_output_uom.
                    IF sy-subrc EQ 0.
                      CONCATENATE '"' ls_doc_uom-edoc_uom '"' INTO lw_eway_api_itm-unit_of_product.
                    ENDIF.
                    "Item Amount
                    lw_eway_api_itm-taxable_amount = lw_wb2_v_likp_lips2-netwr_i.

                    "CGST Tax amount
                    READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_likp_lips2-knumv
                    kposn = lw_wb2_v_likp_lips2-posnr_i
                    kschl = gc_kschl_jocg
                    BINARY SEARCH.
                    IF sy-subrc EQ 0.
                      lw_eway_api_itm-cgst_rate = lw_konv-kbetr / 10.
                      lv_total_cgst_value = lv_total_cgst_value + lw_konv-kwert.
                    ENDIF.
                    "SGST Tax amount
                    READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_likp_lips2-knumv
                    kposn = lw_wb2_v_likp_lips2-posnr_i
                    kschl = gc_kschl_josg
                    BINARY SEARCH.
                    IF sy-subrc EQ 0.
                      lw_eway_api_itm-sgst_rate = lw_konv-kbetr / 10.
                      lv_total_sgst_value = lv_total_sgst_value + lw_konv-kwert.
                    ENDIF.
                    "IGST Tax amount
                    READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_likp_lips2-knumv
                    kposn = lw_wb2_v_likp_lips2-posnr_i
                    kschl = gc_kschl_joig
                    BINARY SEARCH.
                    IF sy-subrc EQ 0.
                      lw_eway_api_itm-igst_rate = lw_konv-kbetr / 10.
                      lv_total_igst_value = lv_total_igst_value + lw_konv-kwert.
                    ENDIF.
                    "CESS Tax amount
                    READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_likp_lips2-knumv
                    kposn = lw_wb2_v_likp_lips2-posnr_i
                    kschl = gc_kschl_cess
                    BINARY SEARCH.
                    IF sy-subrc EQ 0.
                      lw_eway_api_itm-cess_rate = lw_konv-kbetr / 10.
                      lv_total_cess_value = lv_total_cess_value + lw_konv-kwert.
                    ENDIF.
                    lw_eway_api_itm-cessnonadvol = 0.
                    lv_total_cessnoval_value = lv_total_cessnoval_value + 0.

                    lv_total_taxable_value = lv_total_taxable_value + lw_wb2_v_likp_lips2-netwr.

                    APPEND lw_eway_api_itm TO gt_eway_api_itm.
                    CLEAR:lw_eway_api_itm.

***********************************  End of Fill item derails   ************************************************


                  ENDLOOP.

                  lw_eway_api_hdr-total_invoice_value  = lv_total_taxable_value +
                  lv_total_cgst_value +
                  lv_total_sgst_value +
                  lv_total_igst_value +
                  lv_total_cess_value +
                  lv_total_cessnoval_value.

                  lw_eway_api_hdr-other_value          = 0.
                  lw_eway_api_hdr-taxable_amount       = lv_total_taxable_value.
                  lw_eway_api_hdr-cgst_amount          = lv_total_cgst_value.
                  lw_eway_api_hdr-sgst_amount          = lv_total_sgst_value.
                  lw_eway_api_hdr-igst_amount          = lv_total_igst_value.
                  lw_eway_api_hdr-cess_amount          = lv_total_cess_value.
                  lw_eway_api_hdr-cess_nonadvol_value  = lv_total_cessnoval_value.

                  APPEND lw_eway_api_hdr TO gt_eway_api_hdr.
                  CLEAR:lw_eway_api_hdr.

                ENDIF.
              ENDIF.

* Eway bill for purchase returns.

            ELSEIF p_mod = gc_mm.

* eway bill - Purchase return
              READ TABLE gt_bkpf INTO gw_bkpf WITH KEY belnr = gw_final-vbeln.
              IF sy-subrc IS INITIAL.
                lv_index = sy-tabix.

*header value for every document
                lv_total_invoice_value   = 0.
                lv_total_sgst_value      = 0.
                lv_total_cgst_value      = 0.
                lv_total_igst_value      = 0.
                lv_total_cess_value      = 0.
                lv_total_cessnoval_value = 0.
                lv_total_taxable_value   = 0.


                lw_eway_api_hdr-bukrs       = gw_final-bukrs.

                READ TABLE gt_api  INTO lw_usrgstin WITH KEY apiid = gc_apiid_eusrgstin.
                IF sy-subrc IS INITIAL.
                  CONCATENATE '"' lw_usrgstin-apiprov '"' INTO lw_eway_api_hdr-user_gstin.
                ELSE.
                  CONCATENATE '"' gw_final-sup_gstin '"' INTO lw_eway_api_hdr-user_gstin.
                ENDIF.

                READ TABLE gt_doctyp INTO ls_doctyp WITH KEY fkart = gw_bkpf-blart.
                IF sy-subrc EQ 0.
                  lv_supply_type = ls_doctyp-sup_type.
                  lv_sub_supply_type = ls_doctyp-sub_type.
                  lv_document_type = ls_doctyp-edoc_type.
                ENDIF.

*& Supply Type
                READ TABLE gt_supply_values INTO ls_values WITH KEY domvalue_l = lv_supply_type.
                IF sy-subrc EQ 0.
                  TRANSLATE ls_values-ddtext TO UPPER CASE.
                  CONCATENATE '"' ls_values-ddtext '"' INTO lw_eway_api_hdr-supply_type.
                ENDIF.

*& Sub Supply Type
                READ TABLE gt_sub_supply_values INTO ls_values WITH KEY domvalue_l = lv_sub_supply_type.
                IF sy-subrc EQ 0.
                  TRANSLATE ls_values-ddtext TO UPPER CASE.
                  CONCATENATE '"' ls_values-ddtext '"' INTO lw_eway_api_hdr-sub_supply_type.
                  CONCATENATE '"' ls_values-ddtext '"' INTO lw_eway_api_hdr-sub_supply_description.
                ENDIF.

*& Document Type
                READ TABLE gt_doctyp_values INTO ls_values WITH KEY domvalue_l = lv_document_type.
                IF sy-subrc EQ 0.
                  TRANSLATE ls_values-ddtext TO UPPER CASE.
                  CONCATENATE '"' ls_values-ddtext '"' INTO lw_eway_api_hdr-document_type.
                ENDIF.

                lw_eway_api_hdr-doc_year = gw_final-gjahr.

                CONCATENATE '"' gw_final-odnno '"' INTO lw_eway_api_hdr-document_number.
                lw_eway_api_hdr-vbeln = gw_final-vbeln.

                CONCATENATE '"'
                gw_final-t_date
                '"' INTO lw_eway_api_hdr-document_date.


                READ TABLE gt_bseg INTO gw_bseg WITH KEY bukrs = gw_bkpf-bukrs
                belnr = gw_bkpf-belnr
                gjahr = gw_bkpf-gjahr
                koart = 'D'.
                IF sy-subrc IS NOT INITIAL.
                  READ TABLE gt_bseg INTO gw_bseg WITH KEY bukrs = gw_bkpf-bukrs
                  belnr = gw_bkpf-belnr
                  gjahr = gw_bkpf-gjahr
                  koart = 'K'.
                ENDIF.

                IF sy-subrc IS INITIAL.


                  READ TABLE gt_api  INTO lw_usrgstin WITH KEY apiid = gc_apiid_esupgstin.
                  IF sy-subrc IS INITIAL.
                    CONCATENATE '"' lw_usrgstin-apiprov '"' INTO lw_eway_api_hdr-gstin_of_consignor.
                  ELSE.
                    CONCATENATE '"' gw_final-sup_gstin '"' INTO lw_eway_api_hdr-gstin_of_consignor.
                  ENDIF.


*                  READ TABLE gt_gstin INTO DATA(lw_gstin) WITH KEY bukrs = gw_bseg-bukrs
*                                                             branch = gw_bseg-bupla.
                  READ TABLE gt_t001w_mm INTO DATA(lw_plant) WITH KEY werks = gw_final-werks.
                  IF sy-subrc IS INITIAL.

                    READ TABLE gt_adrc1 INTO DATA(ls_adrc1) WITH KEY addrnumber = lw_plant-adrnr.
                    IF sy-subrc IS INITIAL.

                      CONCATENATE '"' lw_plant-name1 '"' INTO lw_eway_api_hdr-legal_name_of_consignor.
                      CONCATENATE '"' ls_adrc1-street '"' INTO lw_eway_api_hdr-address1_of_consignor.

                      lw_eway_api_hdr-address2_of_consignor = '""'.
                      CONCATENATE '"' ls_adrc1-city1 '"' INTO lw_eway_api_hdr-place_of_consignor.
                      CONCATENATE '"' ls_adrc1-post_code1 '"' INTO lw_eway_api_hdr-pincode_of_consignor.
                      READ TABLE gt_t005u INTO DATA(ls_t005u) WITH  KEY bland = ls_adrc1-region.
                      IF sy-subrc EQ 0.
                        TRANSLATE ls_t005u-bezei TO UPPER CASE.
                        CONCATENATE '"' ls_t005u-bezei  '"' INTO  lw_eway_api_hdr-state_of_consignor.
                      ENDIF.

                      CONCATENATE '"' ls_t005u-bezei '"' INTO lw_eway_api_hdr-actual_from_state_name.
                    ENDIF.
                  ENDIF.
                ENDIF.


*** Buyer details
                DATA:ls_adrc1_mm TYPE  ty_adrc1.
                CLEAR ls_adrc1_mm.
                READ TABLE gt_lfa1 INTO DATA(ls_lfa1) WITH KEY lifnr = gw_bseg-lifnr.
                IF sy-subrc IS INITIAL.
                  READ TABLE gt_adrc1 INTO ls_adrc1_mm WITH KEY addrnumber = ls_lfa1-adrnr.
*                ELSE.
*                  READ TABLE gt_kna1 INTO DATA(ls_kna1) WITH KEY kunnr = gw_bseg-kunnr.
*                  IF sy-subrc IS INITIAL.
*                    READ TABLE gt_Adrc1 INTO ls_Adrc1_mm WITH KEY addrnumber = ls_kna1-adrnr.
*                  ENDIF.
                ENDIF.
                IF ls_adrc1_mm IS NOT INITIAL.
                  READ TABLE gt_api  INTO lw_usrgstin WITH KEY apiid = gc_apiid_ebuygstin.
                  IF sy-subrc IS INITIAL.
                    CONCATENATE '"' lw_usrgstin-apiprov '"' INTO lw_eway_api_hdr-gstin_of_consignee.
                  ELSE.
                    IF ls_lfa1-stcd3 IS NOT INITIAL.
                      CONCATENATE '"' ls_lfa1-stcd3 '"' INTO lw_eway_api_hdr-gstin_of_consignee.
                    ELSE.
                      lw_eway_api_hdr-gstin_of_consignee = gc_gstin_urp.
                    ENDIF.
                  ENDIF.
                  CONCATENATE '"' ls_lfa1-name1 '"' INTO lw_eway_api_hdr-legal_name_of_consignee.


                  CONCATENATE '"' ls_adrc1_mm-street '"' INTO lw_eway_api_hdr-address1_of_consignee.

                  lw_eway_api_hdr-address2_of_consignee = '""'.
                  CONCATENATE '"' ls_adrc1_mm-city1 '"' INTO lw_eway_api_hdr-place_of_consignee.
                  CONCATENATE '"' ls_adrc1_mm-post_code1 '"' INTO lw_eway_api_hdr-pincode_of_consignee.
                  CLEAR:lw_t005u.
                  READ TABLE gt_t005u INTO lw_t005u WITH  KEY bland = ls_adrc1_mm-region.
                  IF sy-subrc EQ 0.
                    TRANSLATE lw_t005u-bezei TO UPPER CASE.
                    CONCATENATE '"' lw_t005u-bezei '"' INTO lw_eway_api_hdr-actual_to_state_name.
                    CONCATENATE '"' lw_t005u-bezei '"' INTO lw_eway_api_hdr-state_of_supply.
                  ENDIF.

* Set the export flag
                  CLEAR lv_export.
                  IF ls_lfa1-land1 NE 'IN'.
                    lv_export = abap_true.
                  ENDIF.


                  IF lw_eway_api_hdr-sub_supply_type      = gc_sub_suptyp_exp OR
                  lv_export = abap_true.
                    lw_eway_api_hdr-state_of_supply       = gc_export_place.
                    lw_eway_api_hdr-gstin_of_consignee    = gc_gstin_urp.
                    lw_eway_api_hdr-sub_supply_type = '"EXPORT"'.
                    lw_eway_api_hdr-sub_supply_description = '"EXPORT"'.
                    READ TABLE gt_export INTO ls_export WITH KEY zport_code = gw_final-zport_code.
                    IF sy-subrc IS INITIAL..
                      CONCATENATE '"' ls_export-zport_address1 '"' INTO lw_eway_api_hdr-address1_of_consignee.
                      CONCATENATE '"' ls_export-zport_address2 '"' INTO lw_eway_api_hdr-address2_of_consignee.
                      CONCATENATE '"' ls_export-zport_place '"' INTO lw_eway_api_hdr-place_of_consignee.
                      CONCATENATE '"' ls_export-zport_pincode '"' INTO lw_eway_api_hdr-pincode_of_consignee.
                      CLEAR:lw_t005u.
                      READ TABLE gt_t005u INTO lw_t005u WITH  KEY bland = ls_export-zport_state.
                      IF sy-subrc EQ 0.
                        TRANSLATE lw_t005u-bezei TO UPPER CASE.
                        CONCATENATE '"' lw_t005u-bezei '"' INTO lw_eway_api_hdr-actual_to_state_name.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.

*** Value details

                "default value
                lw_eway_api_hdr-other_value            = '0'.
                lw_eway_api_hdr-total_invoice_value    = '0'.
                lw_eway_api_hdr-taxable_amount         = '0'.
                lw_eway_api_hdr-cgst_amount            = '0'.
                lw_eway_api_hdr-sgst_amount            = '0'.
                lw_eway_api_hdr-igst_amount            = '0'.
                lw_eway_api_hdr-cess_amount            = '0'.
                lw_eway_api_hdr-cess_nonadvol_value    = '0'.


                lw_eway_api_hdr-transaction_type = 1.

                CONDENSE lw_eway_api_hdr-transaction_type.

                lw_eway_api_hdr-generate_status = gc_gen_sts_1.
                lw_eway_api_hdr-data_source = gc_dsource_erp.
                CONCATENATE '"' sy-uname '"' INTO lw_eway_api_hdr-user_ref.
                CONCATENATE '"' ls_lfa1-ort01 '"' INTO lw_eway_api_hdr-location_code.

*& Generate E-Way Bill Part A & Part B.
                IF gv_e_comm EQ gc_ucomm_eeayg.
                  lw_eway_api_hdr-eway_bill_status = gc_ewaysts_abc.
                ELSE.
*& Generate E-Way Bill Part A only.
                  lw_eway_api_hdr-eway_bill_status = gc_ewaysts_ac.
                ENDIF.

*& Auto print E-way Bill
                lw_eway_api_hdr-auto_print = '" "'.
                CONCATENATE '"' '"' INTO lw_eway_api_hdr-email.



*** Transport details

                CONCATENATE '"' gw_final-t_id '"' INTO lw_eway_api_hdr-transporter_id.

                CONCATENATE '"' gw_final-t_name '"' INTO lw_eway_api_hdr-transporter_name.

                READ TABLE gt_t_mode_values INTO ls_values WITH KEY domvalue_l = gw_final-t_mode.
                IF sy-subrc EQ 0.
                  TRANSLATE ls_values-ddtext TO UPPER CASE.
                  CONCATENATE '"' ls_values-ddtext '"' INTO lw_eway_api_hdr-transportation_mode.
                ELSE.
                  CONCATENATE '"'  '"' INTO lw_eway_api_hdr-transportation_mode.
                ENDIF.

                CONCATENATE '"' gw_final-t_distance '"' INTO lw_eway_api_hdr-transportation_distance.

                CONCATENATE '"' gw_final-t_doc_no '"' INTO lw_eway_api_hdr-transporter_document_number.
                CONCATENATE '"' gw_final-t_date '"' INTO lw_eway_api_hdr-transporter_document_date.

                CONCATENATE '"' gw_final-v_number '"' INTO lw_eway_api_hdr-vehicle_number.
                READ TABLE gt_v_type_values INTO ls_values WITH KEY domvalue_l = gw_final-v_type.
                IF sy-subrc EQ 0.
                  TRANSLATE ls_values-ddtext TO UPPER CASE.
                  CONCATENATE '"' ls_values-ddtext '"' INTO lw_eway_api_hdr-vehicle_type.
                ENDIF.


* Fill eway billby IRN related fields:
                CONCATENATE '"' gw_final-sup_gstin '"' INTO lw_eway_api_hdr-user_gstin_irn.
                CONCATENATE '"' gw_final-t_mode '"' INTO lw_eway_api_hdr-t_mode.
                IF gw_final-v_type = '1'.
                  lw_eway_api_hdr-v_type = '"R"'.
                ELSEIF gw_final-v_type = '2'.
                  lw_eway_api_hdr-v_type = '"O"'.
                ENDIF.


*********************************   End of Fill header derails   ***********************************************



********************************** Begin of Fill item derails   ************************************************

* check for export currency
                CLEAR lv_exp_curr.
                IF lw_wb2_v_vbrk_vbrp2-waerk NE  gc_curr_inr .
                  lv_exp_curr = abap_true.
                ENDIF.
*** Rest item tax rate
                lw_eway_api_itm-cgst_rate = '0'.
                lw_eway_api_itm-sgst_rate = '0'.
                lw_eway_api_itm-igst_rate = '0'.
                lw_eway_api_itm-cess_rate = '0'.
                lw_eway_api_itm-cessnonadvol = '0'.





                LOOP AT gt_final_temp INTO DATA(gw_final_temp) WHERE bukrs = gw_bkpf-bukrs
                      AND  vbeln = gw_bkpf-belnr
                      AND  gjahr = gw_bkpf-gjahr.

                  lw_eway_api_itm-bukrs            = lw_eway_api_hdr-bukrs.
                  lw_eway_api_itm-document_number  = lw_eway_api_hdr-document_number.
                  lw_eway_api_itm-doc_year         = lw_eway_api_hdr-doc_year.
                  lw_eway_api_itm-document_type    = lw_eway_api_hdr-document_type.
                  lw_eway_api_itm-document_date    = lw_eway_api_hdr-document_date.


                  READ TABLE gt_makt INTO DATA(ls_makt) WITH KEY matnr = gw_final_temp-matnr.
                  IF sy-subrc IS INITIAL.

                    REPLACE ALL  OCCURRENCES OF '"' IN ls_makt-maktx WITH space.

                    CONCATENATE '"' ls_makt-maktx '"' INTO lw_eway_api_itm-product_name.

                    CONCATENATE '"' ls_makt-maktx '"' INTO lw_eway_api_itm-product_description.
                  ENDIF.

                  READ TABLE gt_marc INTO DATA(ls_marc) WITH KEY matnr  = gw_final_temp-matnr
                        werks  = gw_final_temp-werks.
                  IF sy-subrc IS INITIAL.
                    CONCATENATE '"' ls_marc-steuc '"' INTO lw_eway_api_itm-hsn_code.
                  ENDIF.

                  lw_eway_api_itm-quantity         = gw_final_temp-menge.

                  CLEAR lv_output_uom.
                  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                    EXPORTING
                      input          = gw_final_temp-meins
                    IMPORTING
                      output         = lv_output_uom
                    EXCEPTIONS
                      unit_not_found = 1
                      OTHERS         = 2.
                  IF sy-subrc <> 0.
* Implement suitable error handling here
                  ENDIF.

                  READ TABLE gt_doc_uom INTO ls_doc_uom WITH KEY meins = lv_output_uom.
                  IF sy-subrc EQ 0.
                    CONCATENATE '"' ls_doc_uom-edoc_uom '"' INTO lw_eway_api_itm-unit_of_product.
                  ELSE.

                    READ TABLE gt_doc_uom INTO ls_doc_uom WITH KEY meins =  gw_final_temp-meins.
                    IF sy-subrc IS INITIAL.
                      CONCATENATE '"' ls_doc_uom-edoc_uom '"' INTO lw_eway_api_itm-unit_of_product.
                    ELSE.
                      CONCATENATE '"' gw_final_temp-meins '"' INTO  lw_eway_api_itm-unit_of_product.

                    ENDIF.
                  ENDIF.


                  lw_eway_api_itm-taxable_amount = gw_final_temp-taxamt.
                  lw_eway_api_itm-cgst_rate = gw_final_temp-cgst_rate.
                  lv_total_cgst_value = lv_total_cgst_value + gw_final_temp-cgst_amt.
                  lw_eway_api_itm-sgst_rate = gw_final_temp-sgst_rate.
                  lv_total_sgst_value = lv_total_sgst_value + gw_final_temp-sgst_amt.
                  lw_eway_api_itm-igst_rate = gw_final_temp-igst_rate.
                  lv_total_igst_value = lv_total_igst_value + gw_final_temp-igst_amt.

                  lv_total_taxable_value  = lv_total_taxable_value  + lw_eway_api_itm-taxable_amount.
                  APPEND lw_eway_api_itm TO gt_eway_api_itm.
                  CLEAR:lw_eway_api_itm.
                ENDLOOP.


* **** Header total values


                lw_eway_api_hdr-total_invoice_value  = lv_total_taxable_value +
                lv_total_cgst_value +
                lv_total_sgst_value +
                lv_total_igst_value +
                lv_total_cess_value +
                lv_total_cessnoval_value +
                lv_total_other_value.

                lw_eway_api_hdr-other_value          = lv_total_other_value.
                IF lw_eway_api_hdr-other_value CS '-'.
                  REPLACE ALL OCCURRENCES OF '-' IN lw_eway_api_hdr-other_value WITH ''.
                  CONCATENATE '-' lw_eway_api_hdr-other_value INTO lw_eway_api_hdr-other_value.
                  CONDENSE lw_eway_api_hdr-other_value NO-GAPS.
                ENDIF.
                lw_eway_api_hdr-taxable_amount       = lv_total_taxable_value.
                lw_eway_api_hdr-cgst_amount          = lv_total_cgst_value.
                lw_eway_api_hdr-sgst_amount          = lv_total_sgst_value.
                lw_eway_api_hdr-igst_amount          = lv_total_igst_value.
                lw_eway_api_hdr-cess_amount          = lv_total_cess_value.
                lw_eway_api_hdr-cess_nonadvol_value  = lv_total_cessnoval_value.

                APPEND lw_eway_api_hdr TO gt_eway_api_hdr.
                CLEAR:lw_eway_api_hdr , lv_total_other_value.


              ENDIF.

            ENDIF.
          ELSE.
            MESSAGE 'Please select appropriate documents for E-Doc generation'(039) TYPE 'I'.
          ENDIF.
        ENDLOOP.


        IF gt_eway_api_hdr IS NOT INITIAL.
          SORT gt_eway_api_hdr .
          SORT gt_eway_api_itm .


* when download exit the procress
          IF gv_doc_download IS NOT INITIAL.
            EXIT.
          ENDIF.

          DELETE ADJACENT DUPLICATES FROM   gt_eway_api_hdr  COMPARING ALL FIELDS.
          DELETE ADJACENT DUPLICATES FROM  gt_einv_api_itm  COMPARING ALL FIELDS.

          lt_api_hdr = gt_eway_api_hdr .
          lt_api_itm = gt_eway_api_itm .
          CLEAR lv_error.
          CALL FUNCTION 'ZFM_EWAY_BILL_VALIDATION'
            EXPORTING
              im_ucomm      = gv_e_comm
              im_api_header = gt_eway_api_hdr
            IMPORTING
              ex_error      = lv_error.
          IF lv_error = ' '.
            CLEAR:lw_token,lw_return.
            IF gw_token IS NOT INITIAL.
              lw_token = gw_token.
            ELSE.
              CALL FUNCTION 'ZFM_EINVOICE_OAUTH_API'
                IMPORTING
                  ex_token    = lw_token
                  ex_return   = lw_return
                  et_messages = lt_messages.
            ENDIF.
            IF lw_token IS NOT INITIAL.

              CALL FUNCTION 'ZFM_EWAY_BILL_GENERATE_API'
                EXPORTING
                  im_token          = lw_token
                  im_api_header     = lt_api_hdr
                  im_api_item       = lt_api_itm
                  im_api_dwn        = gv_download
                IMPORTING
                  ex_return         = lw_return
                  ex_messages       = lt_messages
                  ex_ewaybill       = lt_ewaybill
                  ex_eway_transport = lt_eway_transport.

              IF lt_ewaybill IS NOT INITIAL.
                LOOP AT lt_ewaybill ASSIGNING <lw_ewaybill>.
                  READ TABLE gt_final INTO gw_final WITH KEY vbeln = <lw_ewaybill>-docno.
                  IF sy-subrc = 0.
                    <lw_ewaybill>-gjahr = gw_final-gjahr.
                    <lw_ewaybill>-doctyp = gw_final-fkart.
                  ENDIF.
                ENDLOOP.
                MODIFY j_1ig_ewaybill FROM TABLE lt_ewaybill.
                IF sy-subrc EQ 0.
                  COMMIT WORK.

                  LOOP AT lt_ewaybill INTO lw_ewaybill.
                    READ TABLE gt_final ASSIGNING <gw_final> WITH KEY vbeln = lw_ewaybill-docno.
                    IF sy-subrc IS INITIAL.
                      <gw_final>-eway_num = lw_ewaybill-ebillno.

                      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
                        EXPORTING
                          date_internal            = lw_ewaybill-egen_dat
                        IMPORTING
                          date_external            = <gw_final>-eway_date
                        EXCEPTIONS
                          date_internal_is_invalid = 1
                          OTHERS                   = 2.
                      IF sy-subrc <> 0.
                        MESSAGE 'Date conversion error'(m02) TYPE 'I'.
                      ENDIF.

                      <gw_final>-eway_ernam = lw_ewaybill-ernam.

                      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
                        EXPORTING
                          date_internal            = lw_ewaybill-erdat
                        IMPORTING
                          date_external            = <gw_final>-eway_erdat
                        EXCEPTIONS
                          date_internal_is_invalid = 1
                          OTHERS                   = 2.
                      IF sy-subrc <> 0.
                        MESSAGE 'Date conversion error'(m02) TYPE 'I'.
                      ENDIF.

                      <gw_final>-eway_erzet = lw_ewaybill-egen_time.

*Valid upto


                      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
                        EXPORTING
                          date_internal            = lw_ewaybill-vdtodate
                        IMPORTING
                          date_external            = <gw_final>-eway_v_to
                        EXCEPTIONS
                          date_internal_is_invalid = 1
                          OTHERS                   = 2.
                      IF sy-subrc <> 0.
                        MESSAGE 'Date conversion error'(m02) TYPE 'I'.
                      ENDIF.

                      IF lw_ewaybill-status = gc_eway_sts_a.
                        <gw_final>-eway_status = 'Success'(060).
                        <gw_final>-eway_icon    = gc_icon_08.
                        <gw_final>-eway_error   = space.
                      ELSEIF lw_ewaybill-status = gc_eway_sts_e.
                        <gw_final>-eway_status = 'Error'(061).
                        <gw_final>-eway_icon    = gc_icon_0a.
                      ELSEIF lw_ewaybill-status = gc_eway_sts_c.
                        <gw_final>-eway_status = 'Cancelled'(062).
                        <gw_final>-eway_icon    = gc_icon_0w.
                      ENDIF.
                    ENDIF.
                  ENDLOOP.

                  CLEAR lt_show_message.
                  IF  lt_messages IS NOT INITIAL.
                    LOOP AT gt_index INTO lw_index.

                      READ TABLE gt_final ASSIGNING <gw_final> INDEX lw_index-index.
                      IF sy-subrc EQ 0.

                        CLEAR: lv_string,lw_etransport.
                        LOOP AT lt_messages INTO lw_message WHERE message_v3 = <gw_final>-vbeln.

                          lw_show_message-msgid =  gc_msgid_01.
                          lw_show_message-msgty =  lw_message-type.
                          lw_show_message-msgno =  gc_msgno_319.
                          lw_show_message-msgv1 =  lw_message-message_v1.
                          lw_show_message-msgv2 =  lw_message-message_v2.
                          lw_show_message-msgv3 =  lw_message-message_v3.
                          lw_show_message-msgv4 =  lw_message-message_v4.
                          APPEND lw_show_message TO lt_show_message.

                          IF lw_message-type = gc_mtype_s.
                            READ TABLE lt_eway_transport INTO ls_eway_transport WITH KEY docno = <gw_final>-vbeln.
                            IF sy-subrc IS INITIAL.
                              <gw_final>-eway_print = ls_eway_transport-eway_print.
                            ELSE.
                              <gw_final>-eway_print = lw_message-message_v4.
                            ENDIF.
                            UPDATE zteway_transport SET eway_print = <gw_final>-eway_print
                            WHERE  bukrs = <gw_final>-bukrs AND
                            gjahr = <gw_final>-gjahr AND
                            docno = lw_message-message_v3 AND
                            doctyp = <gw_final>-fkart.
                            IF sy-subrc IS NOT INITIAL.
                              MOVE-CORRESPONDING <gw_final> TO lw_etransport.
                              lw_etransport-t_date = <gw_final>-fkdat_db.
                              lw_etransport-doctyp = <gw_final>-fkart.
                              lw_etransport-docno = <gw_final>-vbeln.
                              lw_etransport-gjahr =  <gw_final>-gjahr.

                              READ TABLE lt_eway_transport INTO ls_eway_transport WITH KEY docno = <gw_final>-vbeln.
                              IF sy-subrc IS INITIAL.
                                <gw_final>-eway_print = ls_eway_transport-eway_print.

                              ELSE.
                                <gw_final>-eway_print = lw_message-message_v4.
                              ENDIF.
                              lw_etransport-gjahr =  <gw_final>-gjahr.
                              lw_etransport-ernam = sy-uname.
                              lw_etransport-erdat = sy-datum.

                              MODIFY zteway_transport FROM lw_etransport.
                              IF sy-subrc IS NOT INITIAL.
                                MESSAGE 'Data update error'(022) TYPE 'I'.
                              ENDIF.
                            ENDIF.
                            CONTINUE.
                          ELSEIF lw_message-type = gc_mtype_e.

                            CONCATENATE lv_string lw_message-message INTO lv_string SEPARATED BY '||'.

                          ENDIF.

                        ENDLOOP.
                        IF lv_string IS NOT INITIAL.
                          <gw_final>-eway_error = lv_string.
                          UPDATE zteway_transport SET eway_error = lv_string
                          WHERE  bukrs = <gw_final>-bukrs AND
                          gjahr = <gw_final>-gjahr AND
                          docno = lw_message-message_v3 AND
                          doctyp = <gw_final>-fkart.
                          IF sy-subrc IS NOT INITIAL.
                            MOVE-CORRESPONDING <gw_final> TO lw_etransport.
                            lw_etransport-t_date = <gw_final>-fkdat_db.
                            lw_etransport-doctyp = <gw_final>-fkart.
                            lw_etransport-docno = <gw_final>-vbeln.
                            lw_etransport-ernam = sy-uname.
                            lw_etransport-erdat = sy-datum.
                            MODIFY zteway_transport FROM lw_etransport.
                            IF sy-subrc IS NOT INITIAL.
                              MESSAGE 'Data update error'(022) TYPE 'I'.
                            ENDIF.
                          ENDIF.
                        ENDIF.
                      ENDIF.
                    ENDLOOP.


                    CALL METHOD gref_alv_grid->refresh_table_display
                      EXCEPTIONS
                        finished = 1
                        OTHERS   = 2.
                    IF sy-subrc <> 0.
                      MESSAGE 'Table refresh error'(012) TYPE 'I'.
                    ENDIF.


                    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
                      TABLES
                        i_message_tab = lt_show_message.

                  ENDIF.


                ENDIF.

              ENDIF.


            ELSE.
              MESSAGE 'Access token generation error'(015) TYPE 'I'.
              LEAVE LIST-PROCESSING.

            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EWAYBILL_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ewaybill_update_transporter.



*
*  DATA:lv_index        TYPE sy-tabix.
*
  DATA:lw_token                    TYPE string,
       lw_return                   TYPE string,
       wa_return                   TYPE string,
       lt_messages                 TYPE bapiret2_t,
       lw_message                  TYPE bapiret2,
       lt_show_message             TYPE esp1_message_tab_type,
       lw_show_message             LIKE LINE OF lt_show_message,
       lt_ewaybill                 TYPE TABLE OF j_1ig_ewaybill,
       lw_ewaybill                 TYPE j_1ig_ewaybill,
       lw_index                    TYPE lvc_s_row,
       lv_val                      TYPE xfeld,
       lv_answer                   TYPE c,
       lw_usrgstin                 TYPE ty_api,
       lt_eway_upd_transporter_api TYPE ztt_eway_bill_updat_trn_id_str,
       ls_eway_upd_transporter_api TYPE zst_eway_bill_updat_trn_id_str,
       lt_eway_ext_validity        TYPE ztt_eway_bill_extend_validity,
       ls_eway_ext_validity        TYPE zst_eway_bill_extend_validity,
       lv_return                   TYPE xfeld.


  FIELD-SYMBOLS <gw_final>  TYPE ty_final.



  CLEAR: lt_messages,lt_ewaybill,lt_eway_upd_transporter_api.

  PERFORM check_selected_data CHANGING lv_val.

  IF lv_val IS INITIAL.

    PERFORM call_popup USING   TEXT-t23 TEXT-t25
    CHANGING lv_answer.

    IF lv_answer = '1'.
      LOOP AT  gt_index INTO lw_index.

        READ TABLE gt_final INTO gw_final INDEX lw_index-index.
        IF sy-subrc IS INITIAL AND gw_final-eway_num IS NOT INITIAL AND gw_final-eway_icon = gc_icon_08.


********************************* Begin of Fill header derails   ***********************************************

          READ TABLE gt_api  INTO lw_usrgstin WITH KEY apiid = gc_apiid_eusrgstin.
          IF sy-subrc IS INITIAL.
            CONCATENATE '"' lw_usrgstin-apiprov '"' INTO ls_eway_upd_transporter_api-user_gstin.
          ELSE.
            CONCATENATE '"' gw_final-sup_gstin '"' INTO ls_eway_upd_transporter_api-user_gstin.
          ENDIF.

          ls_eway_upd_transporter_api-ebillno = gw_final-eway_num.

          CONCATENATE '"' gw_final-t_id   '"' INTO ls_eway_upd_transporter_api-transporter_id.
          CONCATENATE '"' gw_final-t_name '"' INTO ls_eway_upd_transporter_api-transporter_name.

*********************************   End of Fill header derails   ***********************************************

          APPEND ls_eway_upd_transporter_api TO lt_eway_upd_transporter_api.
          CLEAR:ls_eway_upd_transporter_api.

        ENDIF.
      ENDLOOP.


      IF lt_eway_upd_transporter_api IS NOT INITIAL.
        SORT lt_eway_upd_transporter_api.

        DELETE ADJACENT DUPLICATES FROM  lt_eway_upd_transporter_api  COMPARING ALL FIELDS.
        CLEAR lv_return.
        CALL FUNCTION 'ZFM_EWAY_BILL_VALIDATION'
          EXPORTING
            im_ucomm         = gc_ucomm_ewayu
            im_api_transport = lt_eway_upd_transporter_api
          IMPORTING
            ex_error         = lv_return.
        IF lv_return = ' '.

          CLEAR:lw_token,lw_return.
          IF gw_token IS NOT INITIAL.
            lw_token = gw_token.
          ELSE.
            CALL FUNCTION 'ZFM_EINVOICE_OAUTH_API'
              IMPORTING
                ex_token    = lw_token
                ex_return   = lw_return
                et_messages = lt_messages.
          ENDIF.
          IF lw_token IS NOT INITIAL.
            CALL FUNCTION 'ZFM_EWAY_BILL_UPD_TRANSID_API'
              EXPORTING
                im_token    = lw_token
                im_api_data = lt_eway_upd_transporter_api
              IMPORTING
                ex_return   = lw_return
                ex_messages = lt_messages
                ex_api_data = lt_eway_ext_validity.

            IF lt_eway_ext_validity IS NOT INITIAL."lw_return EQ 'S'.
              LOOP AT lt_eway_ext_validity INTO ls_eway_ext_validity.
                READ TABLE gt_final INTO gw_final WITH KEY eway_num = ls_eway_ext_validity-ewaybillno.
                IF sy-subrc = 0.

                  lw_ewaybill-bukrs = gw_final-bukrs.
                  lw_ewaybill-docno = gw_final-vbeln.
                  lw_ewaybill-gjahr = gw_final-gjahr.
                  lw_ewaybill-doctyp = gw_final-fkart.
                  lw_ewaybill-ebillno = ls_eway_ext_validity-ewaybillno.
                  lw_ewaybill-aenam     = sy-uname.
                  lw_ewaybill-aedat     = sy-datum.

                  UPDATE j_1ig_ewaybill SET aenam    = lw_ewaybill-aenam
                  aedat    = lw_ewaybill-aedat
                  WHERE bukrs  = lw_ewaybill-bukrs AND
                  docno  = lw_ewaybill-docno AND
                  doctyp = lw_ewaybill-doctyp AND
                  gjahr  = lw_ewaybill-gjahr AND
                  ebillno = lw_ewaybill-ebillno.
                  IF sy-subrc = 0.
                    APPEND  lw_ewaybill TO lt_ewaybill.
                  ENDIF.
                ENDIF.
              ENDLOOP.

              IF sy-subrc EQ 0.
                COMMIT WORK.


                CALL METHOD gref_alv_grid->refresh_table_display
                  EXCEPTIONS
                    finished = 1
                    OTHERS   = 2.
                IF sy-subrc <> 0.
                  MESSAGE 'Table refresh error'(012) TYPE 'I'.
                ENDIF.

              ENDIF.


            ENDIF.

            CLEAR lt_show_message.
            IF  lt_messages IS NOT INITIAL.
              LOOP AT lt_messages INTO lw_message.
                lw_show_message-msgid =  gc_msgid_01.
                lw_show_message-msgty =  lw_message-type.
                lw_show_message-msgno =  gc_msgno_319.
                lw_show_message-msgv1 =  lw_message-message_v1.
                lw_show_message-msgv2 =  lw_message-message_v2.
                lw_show_message-msgv3 =  lw_message-message_v3.
                lw_show_message-msgv4 =  lw_message-message_v4.
                APPEND lw_show_message TO lt_show_message.

              ENDLOOP.

              CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
                TABLES
                  i_message_tab = lt_show_message.

            ENDIF.

          ENDIF.
        ENDIF.

      ELSE.
        MESSAGE 'Please select appropriate documents for E-Waybill update'(016) TYPE 'I'.
      ENDIF.
    ENDIF.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EWAYBILL_EXTEND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ewaybill_extend .


  DATA:lw_index              TYPE lvc_s_row,
       lv_val                TYPE xfeld,
       lv_answer             TYPE c,
       lw_token              TYPE string,
       lw_return             TYPE string,
       wa_return             TYPE string,
       lt_messages           TYPE bapiret2_t,
       lw_message            TYPE bapiret2,
       lt_show_message       TYPE esp1_message_tab_type,
       lw_show_message       LIKE LINE OF lt_show_message,
       lt_ewaybill           TYPE TABLE OF j_1ig_ewaybill,
       lw_ewaybill           TYPE j_1ig_ewaybill,
       lw_wb2_v_vbrk_vbrp2   TYPE ty_wb2_v_vbrk_vbrp2,
       lt_eway_ext_valid_api TYPE ztt_eway_bill_ext_valid_api,
       ls_eway_ext_valid_api TYPE zst_eway_bill_ext_valid_api,
       lt_eway_ext_validity  TYPE ztt_eway_bill_extend_validity,
       ls_eway_ext_validity  TYPE zst_eway_bill_extend_validity,
       lw_t001w              TYPE ty_t001w1,
       lw_t005u_s            TYPE ty_t005u,
       lw_usrgstin           TYPE ty_api,
       ls_values             TYPE dd07v,
       lv_return             TYPE xfeld.

  FIELD-SYMBOLS <gw_final>  TYPE ty_final.


  CLEAR: lt_messages,lt_ewaybill,lt_eway_ext_valid_api.

  PERFORM check_selected_data CHANGING lv_val.

  IF lv_val IS INITIAL.

    PERFORM call_popup USING   TEXT-t23 TEXT-t26
    CHANGING lv_answer.

    IF lv_answer = '1'.
      LOOP AT  gt_index INTO lw_index.

        READ TABLE gt_final INTO gw_final INDEX lw_index-index.
        IF sy-subrc IS INITIAL AND gw_final-eway_num IS NOT INITIAL.

********************************* Begin of Fill header derails   ***********************************************


          ls_eway_ext_valid_api-ebillno = gw_final-eway_num.
          READ TABLE gt_api  INTO lw_usrgstin WITH KEY apiid = gc_apiid_eusrgstin.
          IF sy-subrc IS INITIAL.
            CONCATENATE '"' lw_usrgstin-apiprov '"' INTO ls_eway_ext_valid_api-user_gstin.
          ELSE.
            CONCATENATE '"' gw_final-sup_gstin '"' INTO ls_eway_ext_valid_api-user_gstin.
          ENDIF.

          CONCATENATE '"' gw_final-v_number '"' INTO ls_eway_ext_valid_api-vehicle_number.


*** Supplier details
          CLEAR: lw_t001w.
*          READ TABLE gt_t001w INTO lw_t001w WITH  KEY werks = gw_final-werks BINARY SEARCH.
*          IF sy-subrc EQ 0.
*            CONCATENATE '"' lw_t001w-ort01 '"' INTO ls_eway_ext_valid_api-place_of_consignor.
*            CLEAR:lw_t005u_s.
*            READ TABLE gt_t005u_s INTO lw_t005u_s WITH  KEY bland = lw_t001w-regio.
*            IF sy-subrc EQ 0.
*              TRANSLATE lw_t005u_s-bezei TO UPPER CASE.
*              CONCATENATE '"' lw_t005u_s-bezei  '"' INTO  ls_eway_ext_valid_api-state_of_consignor.
*            ENDIF.
*          ELSE.
*          ENDIF.

          CONCATENATE '"' gw_final-consignor_place '"' INTO ls_eway_ext_valid_api-place_of_consignor.
          CLEAR:lw_t005u_s.
          READ TABLE gt_t005u INTO lw_t005u_s WITH  KEY bland = gw_final-consignor_state.
          IF sy-subrc EQ 0.
            TRANSLATE lw_t005u_s-bezei TO UPPER CASE.
            CONCATENATE '"' lw_t005u_s-bezei  '"' INTO  ls_eway_ext_valid_api-state_of_consignor.
          ENDIF.


          READ TABLE gt_t_mode_values INTO ls_values WITH KEY domvalue_l = gw_final-t_mode.
          IF sy-subrc EQ 0.
            TRANSLATE ls_values-ddtext TO UPPER CASE.
            CONCATENATE '"' ls_values-ddtext '"' INTO ls_eway_ext_valid_api-transportation_mode.
          ENDIF.
          CONCATENATE '"' gw_final-t_r_distance '"' INTO ls_eway_ext_valid_api-remaining_distance.
          CONCATENATE '"' gw_final-t_doc_no '"' INTO ls_eway_ext_valid_api-transporter_document_number.
          CONCATENATE '"' gw_final-t_date '"' INTO ls_eway_ext_valid_api-transporter_document_date.

          READ TABLE gt_t_ext_reason_values INTO ls_values WITH KEY domvalue_l = gw_final-t_ext_valid_reason.
          IF sy-subrc EQ 0.
            TRANSLATE ls_values-ddtext TO UPPER CASE.
            CONCATENATE '"' ls_values-ddtext '"' INTO ls_eway_ext_valid_api-extend_validity_reason.
          ENDIF.

          CONCATENATE '"' gw_final-t_ext_valid_remarks '"' INTO ls_eway_ext_valid_api-extend_remarks.
          ls_eway_ext_valid_api-from_pincode  = gw_final-t_from_pin.


          CONCATENATE '"' gw_final-t_consignment_status '"' INTO ls_eway_ext_valid_api-consignment_status.


*& Transit is moving or not
          IF gw_final-eway_v_to GT sy-datum.

            READ TABLE gt_t_transit_type_values INTO ls_values WITH KEY domvalue_l = gw_final-t_transit_type.
            IF sy-subrc EQ 0.
              TRANSLATE ls_values-ddtext TO UPPER CASE.
              CONCATENATE '"' ls_values-ddtext '"' INTO ls_eway_ext_valid_api-transit_type.
            ENDIF.
            IF gw_final-t_transit_type IS INITIAL.
              CONCATENATE '"' ' ' '"' INTO ls_eway_ext_valid_api-transit_type.
            ENDIF.
            CONCATENATE '"' gw_final-t_address1 '"' INTO ls_eway_ext_valid_api-address_line1.
            CONCATENATE '"' gw_final-t_address2 '"' INTO ls_eway_ext_valid_api-address_line2.
            CONCATENATE '"' gw_final-t_address3 '"' INTO ls_eway_ext_valid_api-address_line3.
          ELSE.
            CONCATENATE '"' ' ' '"' INTO ls_eway_ext_valid_api-transit_type.
            CONCATENATE '"' ' ' '"' INTO ls_eway_ext_valid_api-address_line1.
            CONCATENATE '"' ' ' '"' INTO ls_eway_ext_valid_api-address_line2.
            CONCATENATE '"' ' ' '"' INTO ls_eway_ext_valid_api-address_line3.
          ENDIF.

*********************************   End of Fill header derails   ***********************************************

          APPEND ls_eway_ext_valid_api TO lt_eway_ext_valid_api.
          CLEAR:ls_eway_ext_valid_api.

        ENDIF.
      ENDLOOP.


      IF lt_eway_ext_valid_api IS NOT INITIAL.
        SORT lt_eway_ext_valid_api.

        DELETE ADJACENT DUPLICATES FROM  lt_eway_ext_valid_api  COMPARING ALL FIELDS.
        CALL FUNCTION 'ZFM_EWAY_BILL_VALIDATION'
          EXPORTING
            im_ucomm      = gc_ucomm_ewaye
            im_api_extend = lt_eway_ext_valid_api
          IMPORTING
            ex_error      = lv_return.

        CLEAR:lw_token,lw_return.
        IF gw_token IS NOT INITIAL.
          lw_token = gw_token.
        ELSE.
          CALL FUNCTION 'ZFM_EINVOICE_OAUTH_API'
            IMPORTING
              ex_token    = lw_token
              ex_return   = lw_return
              et_messages = lt_messages.
        ENDIF.
        IF lw_token IS NOT INITIAL." AND lw_return EQ 'S'.

          CALL FUNCTION 'ZFM_EWAY_BILL_EXTEND_VALID_API'
            EXPORTING
              im_token       = lw_token
              im_api_data    = lt_eway_ext_valid_api
            IMPORTING
              ex_return      = lw_return
              ex_messages    = lt_messages
              ex_exd_details = lt_eway_ext_validity.


          IF lt_eway_ext_validity IS NOT INITIAL."lw_return EQ 'S'.
            LOOP AT lt_eway_ext_validity INTO ls_eway_ext_validity.
              READ TABLE gt_final ASSIGNING <gw_final> WITH KEY eway_num = ls_eway_ext_validity-ewaybillno.
              IF sy-subrc = 0.
                IF ls_eway_ext_validity-validupto IS NOT INITIAL.
                  lw_ewaybill-bukrs = <gw_final>-bukrs.
                  lw_ewaybill-docno = <gw_final>-vbeln.
                  lw_ewaybill-gjahr = <gw_final>-gjahr.
                  lw_ewaybill-doctyp = <gw_final>-fkart.
                  lw_ewaybill-ebillno = ls_eway_ext_validity-ewaybillno.
                  lw_ewaybill-vdtodate =   ls_eway_ext_validity-validupto.
                  lw_ewaybill-aenam     = sy-uname.
                  lw_ewaybill-aedat     = sy-datum.

                  UPDATE j_1ig_ewaybill SET vdtodate = lw_ewaybill-vdtodate
                  aenam    = lw_ewaybill-aenam
                  aedat    = lw_ewaybill-aedat
                  WHERE bukrs  = lw_ewaybill-bukrs AND
                  docno  = lw_ewaybill-docno AND
                  doctyp = lw_ewaybill-doctyp AND
                  gjahr  = lw_ewaybill-gjahr AND
                  ebillno = lw_ewaybill-ebillno.
                  IF sy-subrc = 0.
                    CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
                      EXPORTING
                        date_internal            = lw_ewaybill-vdtodate
                      IMPORTING
                        date_external            = <gw_final>-eway_v_to
                      EXCEPTIONS
                        date_internal_is_invalid = 1
                        OTHERS                   = 2.
                    IF sy-subrc <> 0.
                      MESSAGE 'Data Conversion is incomplete' TYPE 'I'.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDLOOP.

            IF sy-subrc EQ 0.
              COMMIT WORK.


              CALL METHOD gref_alv_grid->refresh_table_display
                EXCEPTIONS
                  finished = 1
                  OTHERS   = 2.
              IF sy-subrc <> 0.
                MESSAGE 'Table refresh error'(012) TYPE 'I'.
              ENDIF.

              CLEAR lt_show_message.
              IF  lt_messages IS NOT INITIAL.
                LOOP AT lt_messages INTO lw_message.
                  lw_show_message-msgid =  gc_msgid_01.
                  lw_show_message-msgty =  lw_message-type.
                  lw_show_message-msgno =  gc_msgno_319.
                  lw_show_message-msgv1 =  lw_message-message_v1.
                  lw_show_message-msgv2 =  lw_message-message_v2.
                  lw_show_message-msgv3 =  lw_message-message_v3.
                  lw_show_message-msgv4 =  lw_message-message_v4.
                  APPEND lw_show_message TO lt_show_message.

                ENDLOOP.

                CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
                  TABLES
                    i_message_tab = lt_show_message.

              ENDIF.


            ENDIF.

          ENDIF.

        ENDIF.


      ELSE.
        MESSAGE 'Please select appropriate documents for E-Waybill extension'(017) TYPE 'I'.
      ENDIF.
    ENDIF.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EWAYBILL_CANCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ewaybill_cancel USING p_lv_reason_code
      p_lv_reason.

  DATA:lw_token           TYPE string,
       lw_return          TYPE string,
       wa_return          TYPE string,
       lt_messages        TYPE bapiret2_t,
       lw_message         TYPE bapiret2,
       lt_show_message    TYPE esp1_message_tab_type,
       lw_show_message    LIKE LINE OF lt_show_message,
       lt_ewaybill        TYPE TABLE OF j_1ig_ewaybill,
       lw_ewaybill        TYPE j_1ig_ewaybill,
       lt_eway_cancel_api TYPE ztt_eway_bill_cancel_str,
       ls_eway_cancel_api TYPE zst_eway_bill_cancel_str,
       lt_eway_cancel     TYPE ztt_eway_bill_cancel_out_str,
       ls_eway_cancel     TYPE zst_eway_bill_cancel_out_str,
       lw_usrgstin        TYPE ty_api,
       ls_values          TYPE dd07v.

  FIELD-SYMBOLS <gw_final>  TYPE ty_final.


  CLEAR: lt_messages,lt_ewaybill,lt_eway_cancel_api.


  LOOP AT  gt_index INTO lw_index.

    READ TABLE gt_final INTO gw_final INDEX lw_index-index.
    IF sy-subrc IS INITIAL AND gw_final-eway_num IS NOT INITIAL.

********************************* Begin of Fill header derails   ***********************************************


      READ TABLE gt_api  INTO lw_usrgstin WITH KEY apiid = gc_apiid_eusrgstin.
      IF sy-subrc IS INITIAL.
        CONCATENATE '"' lw_usrgstin-apiprov '"' INTO ls_eway_cancel_api-user_gstin.
      ELSE.
        CONCATENATE '"' gw_final-sup_gstin '"' INTO ls_eway_cancel_api-user_gstin.
      ENDIF.

      IF sy-sysid = 'DS4'.
        ls_eway_cancel_api-user_gstin = '"05AAABB0639G1Z8"'.
      ENDIF.
      ls_eway_cancel_api-ebillno = gw_final-eway_num.

      ls_eway_cancel_api-data_source = gc_dsource_erp.

      READ TABLE gt_cancel_values INTO ls_values WITH KEY domvalue_l = p_lv_reason_code.
      IF sy-subrc EQ 0.
        TRANSLATE ls_values-ddtext TO UPPER CASE.
        CONCATENATE '"' ls_values-ddtext   '"' INTO ls_eway_cancel_api-reason_of_cancel.
      ENDIF.
      CONCATENATE '"' p_lv_reason       '"' INTO ls_eway_cancel_api-cancel_remark.

*********************************   End of Fill header derails   ***********************************************


      APPEND ls_eway_cancel_api TO lt_eway_cancel_api.
      CLEAR:ls_eway_cancel_api.

    ENDIF.
  ENDLOOP.


  IF lt_eway_cancel_api IS NOT INITIAL.
    SORT lt_eway_cancel_api.

    DELETE ADJACENT DUPLICATES FROM  lt_eway_cancel_api  COMPARING ALL FIELDS.

    CLEAR:lw_token,lw_return.
    IF gw_token IS NOT INITIAL.
      lw_token = gw_token.
    ELSE.
      CALL FUNCTION 'ZFM_EINVOICE_OAUTH_API'
        IMPORTING
          ex_token    = lw_token
          ex_return   = lw_return
          et_messages = lt_messages.
    ENDIF.
    IF lw_token IS NOT INITIAL.

      CALL FUNCTION 'ZFM_EWAY_BILL_CANCEL_API'
        EXPORTING
          im_token           = lw_token
          im_api_data        = lt_eway_cancel_api
        IMPORTING
          ex_return          = lw_return
          ex_messages        = lt_messages
          ex_ewaybill_cancel = lt_eway_cancel.


      IF lt_eway_cancel IS NOT INITIAL.
        LOOP AT lt_eway_cancel INTO ls_eway_cancel.
          READ TABLE gt_final ASSIGNING  <gw_final> WITH KEY eway_num = ls_eway_cancel-ewaybillno.
          IF sy-subrc = 0.

            lw_ewaybill-bukrs = gw_final-bukrs.
            lw_ewaybill-docno = gw_final-vbeln.
            lw_ewaybill-gjahr = gw_final-gjahr.
            lw_ewaybill-doctyp = gw_final-fkart.
            lw_ewaybill-ebillno = ls_eway_cancel-ewaybillno.
            lw_ewaybill-aenam     = sy-uname.
            lw_ewaybill-aedat     = sy-datum.
            lw_ewaybill-status    = gw_final-eway_status.

            IF ls_eway_cancel-canceldate IS NOT INITIAL.
              lw_ewaybill-status     = gc_eway_sts_c.
              UPDATE j_1ig_ewaybill SET status   = lw_ewaybill-status
              aenam    = lw_ewaybill-aenam
              aedat    = lw_ewaybill-aedat
              WHERE bukrs  = lw_ewaybill-bukrs AND
              docno  = lw_ewaybill-docno AND
              doctyp = lw_ewaybill-doctyp AND
              gjahr  = lw_ewaybill-gjahr AND
              ebillno = lw_ewaybill-ebillno.
              IF sy-subrc = 0.
                UPDATE zteway_transport SET c_reason_code = p_lv_reason_code
                c_reason      = p_lv_reason
                aenam         = lw_ewaybill-aenam
                aedat         = lw_ewaybill-aedat
                WHERE  bukrs  = lw_ewaybill-bukrs AND
                docno  = lw_ewaybill-docno AND
                doctyp = lw_ewaybill-doctyp AND
                gjahr  = lw_ewaybill-gjahr.


                IF sy-subrc IS INITIAL.
                  <gw_final>-eway_status = 'Cancelled'(062).
                  <gw_final>-eway_icon    = gc_icon_0w.

                  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
                    EXPORTING
                      date_internal            = sy-datum
                    IMPORTING
                      date_external            = <gw_final>-eway_canc_dt
                    EXCEPTIONS
                      date_internal_is_invalid = 1
                      OTHERS                   = 2.
                  IF sy-subrc <> 0.
                    MESSAGE 'Data Conversion is incomplete' TYPE 'I'.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.

        COMMIT WORK.


        CALL METHOD gref_alv_grid->refresh_table_display
          EXCEPTIONS
            finished = 1
            OTHERS   = 2.
        IF sy-subrc <> 0.
          MESSAGE 'Table refresh error'(012) TYPE 'I'.
        ENDIF.

        CLEAR lt_show_message.
        IF  lt_messages IS NOT INITIAL.
          LOOP AT lt_messages INTO lw_message.
            lw_show_message-msgid =  gc_msgid_01.
            lw_show_message-msgty =  lw_message-type.
            lw_show_message-msgno =  gc_msgno_319.
            lw_show_message-msgv1 =  lw_message-message_v1.
            lw_show_message-msgv2 =  lw_message-message_v2.
            lw_show_message-msgv3 =  lw_message-message_v3.
            lw_show_message-msgv4 =  lw_message-message_v4.
            APPEND lw_show_message TO lt_show_message.

          ENDLOOP.

          CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
            TABLES
              i_message_tab = lt_show_message.

        ENDIF.

      ENDIF.

    ENDIF.


  ELSE.
    MESSAGE 'Please select appropriate documents to cancel E-Waybill'(018) TYPE 'I'.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EWAYBILL_UPDATE_VEHICLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ewaybill_update_vehicle .


  DATA:lw_index  TYPE lvc_s_row,
       lv_val    TYPE xfeld,
       lv_answer TYPE c.

  DATA:lv_index        TYPE sy-tabix.

  DATA:lw_token        TYPE string,
       lw_return       TYPE string,
       wa_return       TYPE string,
       lt_messages     TYPE bapiret2_t,
       lw_message      TYPE bapiret2,
       lt_show_message TYPE esp1_message_tab_type,
       lw_show_message LIKE LINE OF lt_show_message.

  DATA:lt_ewaybill TYPE TABLE OF j_1ig_ewaybill,
       lw_ewaybill TYPE j_1ig_ewaybill.

  DATA :lw_wb2_v_vbrk_vbrp2 TYPE ty_wb2_v_vbrk_vbrp2.

  DATA: lt_eway_upd_vehile_api TYPE ztt_eway_bill_updat_vehno_str,
        ls_eway_upd_vehile_api TYPE zst_eway_bill_updat_vehno_str.

  DATA: lt_eway_ext_validity TYPE ztt_eway_bill_extend_validity,
        ls_eway_ext_validity TYPE zst_eway_bill_extend_validity.


  DATA: lw_t001w   TYPE ty_t001w1,
        lw_t005u_s TYPE ty_t005u.

  DATA:lw_usrgstin TYPE ty_api,
       lv_datum    TYPE sydatum.

  FIELD-SYMBOLS <gw_final>  TYPE ty_final.
  DATA: ls_values  TYPE dd07v.
  DATA:lv_return TYPE xfeld.
  CLEAR: lt_messages,lt_ewaybill,lt_eway_upd_vehile_api.

  PERFORM check_selected_data CHANGING lv_val.

  IF lv_val IS INITIAL.

    PERFORM call_popup USING   TEXT-t23 TEXT-t25
    CHANGING lv_answer.

    IF lv_answer = '1'.
      LOOP AT  gt_index INTO lw_index.

        READ TABLE gt_final INTO gw_final INDEX lw_index-index.
        IF sy-subrc IS INITIAL AND gw_final-eway_num IS NOT INITIAL AND gw_final-eway_icon = gc_icon_08..

********************************* Begin of Fill header derails   ***********************************************


          READ TABLE gt_api  INTO lw_usrgstin WITH KEY apiid = gc_apiid_eusrgstin.
          IF sy-subrc IS INITIAL.
            CONCATENATE '"' lw_usrgstin-apiprov '"' INTO ls_eway_upd_vehile_api-user_gstin.
          ELSE.
            CONCATENATE '"' gw_final-sup_gstin '"' INTO ls_eway_upd_vehile_api-user_gstin.
          ENDIF.

          ls_eway_upd_vehile_api-ebillno = gw_final-eway_num.

          ls_eway_upd_vehile_api-data_source = gc_dsource_erp.
          CONCATENATE '"' gw_final-v_number '"' INTO ls_eway_upd_vehile_api-vehicle_number.

          READ TABLE gt_v_type_values INTO ls_values WITH KEY domvalue_l = gw_final-v_type.
          IF sy-subrc EQ 0.
            TRANSLATE ls_values-ddtext TO UPPER CASE.
            CONCATENATE '"' ls_values-ddtext   '"' INTO ls_eway_upd_vehile_api-vehicle_type.
          ENDIF.
*** Supplier details
          IF p_mod = gc_mm.
            gt_t001w = gt_t001w_mm.
            gt_t005u_s = gt_t005u.
          ENDIF.

          CLEAR: lw_t001w. ", lw_adrc_s, lw_adr6_s.
          READ TABLE gt_t001w INTO lw_t001w WITH  KEY werks = gw_final-werks BINARY SEARCH.
          IF sy-subrc EQ 0.

            CONCATENATE '"' lw_t001w-ort01 '"' INTO ls_eway_upd_vehile_api-place_of_consignor.

            CLEAR:lw_t005u_s.
            READ TABLE gt_t005u_s INTO lw_t005u_s WITH  KEY bland = lw_t001w-regio.
            IF sy-subrc EQ 0.
              TRANSLATE lw_t005u_s-bezei TO UPPER CASE.
              CONCATENATE '"' lw_t005u_s-bezei  '"' INTO  ls_eway_upd_vehile_api-state_of_consignor.
            ENDIF.
          ENDIF.

*** Transport details
          READ TABLE gt_t_mode_values INTO ls_values WITH KEY domvalue_l = gw_final-t_mode.
          IF sy-subrc EQ 0.
            TRANSLATE ls_values-ddtext TO UPPER CASE.
            CONCATENATE '"' ls_values-ddtext '"' INTO ls_eway_upd_vehile_api-transportation_mode.
          ENDIF.
          CONCATENATE '"' gw_final-t_doc_no '"' INTO ls_eway_upd_vehile_api-transporter_document_number.
          CONCATENATE '"' gw_final-t_date '"' INTO ls_eway_upd_vehile_api-transporter_document_date.

          READ TABLE gt_v_reason_values INTO ls_values WITH KEY domvalue_l = gw_final-v_reason_code.
          IF sy-subrc EQ 0.
            TRANSLATE ls_values-ddtext TO UPPER CASE.
            CONCATENATE '"' ls_values-ddtext '"' INTO ls_eway_upd_vehile_api-reason_code_for_vehicle_updati.
          ENDIF.

          CONCATENATE '"' gw_final-v_reason '"' INTO ls_eway_upd_vehile_api-reason_for_vehicle_updation.



*********************************   End of Fill header derails   ***********************************************

          APPEND ls_eway_upd_vehile_api TO lt_eway_upd_vehile_api.
          CLEAR:ls_eway_upd_vehile_api.

        ENDIF.
      ENDLOOP.


      IF lt_eway_upd_vehile_api IS NOT INITIAL.
        SORT lt_eway_upd_vehile_api.

        DELETE ADJACENT DUPLICATES FROM  lt_eway_upd_vehile_api  COMPARING ALL FIELDS.
        CLEAR lv_return.
        CALL FUNCTION 'ZFM_EWAY_BILL_VALIDATION'
          EXPORTING
            im_ucomm       = gc_ucomm_ewayv
            im_api_vehicle = lt_eway_upd_vehile_api
          IMPORTING
            ex_error       = lv_return.
        IF lv_return = ' '.
          CLEAR:lw_token,lw_return.
          IF gw_token IS NOT INITIAL.
            lw_token = gw_token.
          ELSE.
            CALL FUNCTION 'ZFM_EINVOICE_OAUTH_API'
              IMPORTING
                ex_token    = lw_token
                ex_return   = lw_return
                et_messages = lt_messages.
          ENDIF.
          IF lw_token IS NOT INITIAL." AND lw_return EQ 'S'.

            CALL FUNCTION 'ZFM_EWAY_BILL_UPD_VEHL_NO_API'
              EXPORTING
                im_token    = lw_token
                im_api_data = lt_eway_upd_vehile_api
              IMPORTING
                ex_return   = lw_return
                ex_messages = lt_messages
                ex_api_data = lt_eway_ext_validity.


            IF lt_eway_ext_validity IS NOT INITIAL."lw_return EQ 'S'.
              LOOP AT lt_eway_ext_validity INTO ls_eway_ext_validity.
                READ TABLE gt_final ASSIGNING <gw_final> WITH KEY eway_num = ls_eway_ext_validity-ewaybillno.
                IF sy-subrc = 0.

                  IF ls_eway_ext_validity-validupto IS NOT INITIAL.


                    lw_ewaybill-bukrs = gw_final-bukrs.
                    lw_ewaybill-docno = gw_final-vbeln.
                    lw_ewaybill-gjahr = gw_final-gjahr.
                    lw_ewaybill-doctyp = gw_final-fkart.
                    lw_ewaybill-ebillno = ls_eway_ext_validity-ewaybillno.
                    lw_ewaybill-vdtodate = ls_eway_ext_validity-validupto.
                    lw_ewaybill-vdtotime = ls_eway_ext_validity-vdtotime.


                    UPDATE j_1ig_ewaybill SET vdtodate  = lw_ewaybill-vdtodate
                    vdtotime  = lw_ewaybill-vdtotime
                    aenam    = sy-uname
                    aedat    = sy-datum
                    WHERE bukrs  = lw_ewaybill-bukrs AND
                    docno  = lw_ewaybill-docno AND
                    doctyp = lw_ewaybill-doctyp AND
                    gjahr  = lw_ewaybill-gjahr AND
                    ebillno = lw_ewaybill-ebillno.
                    IF sy-subrc = 0.

                      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
                        EXPORTING
                          date_internal            = lw_ewaybill-vdtodate
                        IMPORTING
                          date_external            = <gw_final>-eway_v_to
                        EXCEPTIONS
                          date_internal_is_invalid = 1
                          OTHERS                   = 2.
                      IF sy-subrc <> 0.
                        MESSAGE 'Date convertion error'(010) TYPE 'I'.
                      ENDIF.


                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDLOOP.

              IF sy-subrc EQ 0.
                COMMIT WORK.

                CALL METHOD gref_alv_grid->refresh_table_display
                  EXCEPTIONS
                    finished = 1
                    OTHERS   = 2.
                IF sy-subrc <> 0.
                  MESSAGE 'Table refresh error'(012) TYPE 'I'.
                ENDIF.

                CLEAR lt_show_message.
                IF  lt_messages IS NOT INITIAL.
                  LOOP AT lt_messages INTO lw_message.
                    lw_show_message-msgid =  gc_msgid_01.
                    lw_show_message-msgty =  lw_message-type.
                    lw_show_message-msgno =  gc_msgno_319.
                    lw_show_message-msgv1 =  lw_message-message_v1.
                    lw_show_message-msgv2 =  lw_message-message_v2.
                    lw_show_message-msgv3 =  lw_message-message_v3.
                    lw_show_message-msgv4 =  lw_message-message_v4.
                    APPEND lw_show_message TO lt_show_message.

                  ENDLOOP.

                  CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
                    TABLES
                      i_message_tab = lt_show_message.

                ENDIF.


              ENDIF.

            ENDIF.

          ENDIF.

        ENDIF.
      ELSE.
        MESSAGE 'Please select appropriate documents for E-Waybill update'(016) TYPE 'I'.
      ENDIF.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EWAYBILL_PRINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ewaybill_print .

  DATA:lw_index  TYPE lvc_s_row,
       lv_val    TYPE xfeld,
       lv_answer TYPE c.

  DATA:lv_index        TYPE sy-tabix.

  DATA:lw_token        TYPE string,
       lw_return       TYPE string,
       wa_return       TYPE string,
       lt_messages     TYPE bapiret2_t,
       lw_message      TYPE bapiret2,
       lt_show_message TYPE esp1_message_tab_type,
       lw_show_message LIKE LINE OF lt_show_message.

  DATA: lv_url TYPE string.

  DATA: lo_http_client TYPE REF TO if_http_client,
        lv_service     TYPE string,
        lv_result      TYPE string,
        lo_ixml        TYPE REF TO if_ixml,
        lv_url_print   TYPE string.


*  DATA: ls_config TYPE zteinv_api.

  DATA:lt_ewaybill TYPE TABLE OF j_1ig_ewaybill,
       lw_ewaybill TYPE j_1ig_ewaybill.

  DATA:lw_eway_transport TYPE zteway_transport.

  FIELD-SYMBOLS <gw_final>  TYPE ty_final.


  CLEAR: lt_messages,lt_ewaybill.


  CLEAR: lv_url,lv_url_print.

  PERFORM check_selected_data CHANGING lv_val.

  IF lv_val IS INITIAL.

    PERFORM call_popup USING   TEXT-t23 TEXT-t29
    CHANGING lv_answer.

    IF lv_answer = '1'.

      SELECT SINGLE apiuri FROM zteinv_api INTO lv_url_print WHERE apiid = 'EWAY_PRI'.


      LOOP AT  gt_index INTO lw_index.

        READ TABLE gt_final INTO gw_final INDEX lw_index-index.
        IF sy-subrc IS INITIAL AND gw_final-eway_num IS NOT INITIAL.

          IF gw_final-eway_print IS NOT INITIAL.
            IF gw_final-eway_print+0(1) = '/'.
              IF lv_url_print IS NOT INITIAL.
                CONCATENATE lv_url_print gw_final-eway_print INTO gw_final-eway_print.
              ENDIF.
            ELSEIF gw_final-eway_print+0(4) = 'http'.

            ELSE.
              CLEAR lv_url_print.
            ENDIF.
          ENDIF.
          IF lv_url_print IS NOT INITIAL.
*            CONCATENATE gw_final-eway_print gw_final-eway_num '.pdf'(110) INTO lv_url.
            CONCATENATE gw_final-eway_print gw_final-eway_num INTO lv_url.
          ELSE.
*            CONCATENATE 'https://'(109) gw_final-eway_print gw_final-eway_num '.pdf'(110) INTO lv_url.
            CONCATENATE 'https://'(109) gw_final-eway_print INTO lv_url.
          ENDIF.
          CONDENSE lv_url NO-GAPS.

          CALL METHOD cl_gui_frontend_services=>execute
            EXPORTING
              document = lv_url
            EXCEPTIONS
              OTHERS   = 1.
          IF sy-subrc <> 0.
            MESSAGE 'Pdf couldn not find'(023) TYPE 'I'.
          ENDIF.
********************************* Begin of Fill header derails   ***********************************************


*********************************   End of Fill header derails   ***********************************************

        ENDIF.
      ENDLOOP.


      CLEAR lt_show_message.
      IF  lt_messages IS NOT INITIAL.
        LOOP AT lt_messages INTO lw_message.
          lw_show_message-msgid =  '01'.
          lw_show_message-msgty =  lw_message-type.
          lw_show_message-msgno =  '319'.
          lw_show_message-msgv1 =  lw_message-message_v1.
          lw_show_message-msgv2 =  lw_message-message_v2.
          lw_show_message-msgv3 =  lw_message-message_v3.
          lw_show_message-msgv4 =  lw_message-message_v4.
          APPEND lw_show_message TO lt_show_message.

        ENDLOOP.

        CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
          TABLES
            i_message_tab = lt_show_message.

      ENDIF.

    ENDIF.


  ELSE.
    MESSAGE 'Please select appropriate documents for E-Waybill print'(020) TYPE 'I'.
  ENDIF.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_EWAY_CANCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_eway_cancel TABLES ifields TYPE ty_sval
USING  ok_code_save
      error
      h_show_popup.
  DATA: ls_ifields TYPE sval.
  DATA:lv_reason_code TYPE zde_cancel_reason,
       lv_reason      TYPE char120.

  CASE ok_code_save.
    WHEN gc_okcode_furt.
      LOOP AT ifields INTO  ls_ifields.
        IF ls_ifields-fieldname = gc_fname_crec_code."'C_REASON_CODE'.
          lv_reason_code = ls_ifields-value.
        ELSEIF ls_ifields-fieldname = gc_fname_crec."'C_REASON'.
          lv_reason = ls_ifields-value.
        ENDIF.
      ENDLOOP.
      PERFORM ewaybill_cancel USING lv_reason_code lv_reason.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EWAYBILL_CANCEL_OLD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ewaybill_cancel_old .


  DATA:        lv_val    TYPE xfeld.
  DATA: vl_program      TYPE sy-repid,
        vl_formname     TYPE char30,
        vl_popup_title  TYPE char30,
        vl_okpushbutton TYPE svalbutton-buttontext,
        vl_buttonicon   TYPE icon-name,
        lt_sval         TYPE ty_sval,
        lv_msg          TYPE xfeld,
        lv_answer       TYPE c,
        lw_final        TYPE ty_final.

  FIELD-SYMBOLS:<ls_sval> TYPE sval.


  PERFORM check_selected_data CHANGING lv_val.

  IF lv_val IS INITIAL.


    PERFORM call_popup USING   TEXT-t23 TEXT-t22
    CHANGING lv_answer.

    IF lv_answer = '1'.
      CLEAR lv_msg.
      LOOP AT gt_index INTO lw_index.
        READ TABLE gt_final INTO lw_final INDEX lw_index-index.
        IF sy-subrc IS INITIAL AND lw_final-eway_icon = gc_icon_0w.
          MESSAGE 'Document already cancelled'(037) TYPE 'I'.
          lv_msg = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF lv_msg IS INITIAL.
*& Add title & FORMNAME for Cancel pop-up
        vl_program        = sy-repid.
        vl_formname       = 'USER_COMMAND_EWAY_CANCEL'.
        vl_popup_title    = 'Confirm to cancel E-way Bill'(114).
        vl_okpushbutton   = 'Cancel E-Way Bill'(116).
        vl_buttonicon     = gc_icon_b2.
*& Add two fields to input cancel reason
        APPEND INITIAL LINE TO lt_sval ASSIGNING <ls_sval>.
        <ls_sval>-tabname = 'ZTEWAY_TRANSPORT'.
        <ls_sval>-fieldname = 'C_REASON_CODE'.
        <ls_sval>-field_obl = abap_true.
        <ls_sval>-fieldtext = 'Reason for cancel'(113).
        APPEND INITIAL LINE TO lt_sval ASSIGNING <ls_sval>.
        <ls_sval>-tabname = 'ZTEWAY_TRANSPORT'.
        <ls_sval>-fieldname = 'C_REASON'.
        <ls_sval>-field_obl = abap_true.
        <ls_sval>-fieldtext = 'Remarks'(115).
        PERFORM popup_to_confirm_with_input USING vl_program
              vl_formname
              vl_popup_title
              vl_okpushbutton
              vl_buttonicon
              lt_sval.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EWAYBILL_CANCEL_OLD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM einvoice_cancel_old .


  DATA:lv_val          TYPE xfeld,
       vl_program      TYPE sy-repid,
       vl_formname     TYPE char30,
       vl_popup_title  TYPE char30,
       vl_okpushbutton TYPE svalbutton-buttontext,
       vl_buttonicon   TYPE icon-name,
       lt_sval         TYPE ty_sval,
       lw_index        TYPE lvc_s_row,
       lv_msg          TYPE xfeld,
       lv_answer       TYPE c,
       lw_final        TYPE ty_final.


  FIELD-SYMBOLS:<ls_sval> TYPE sval.


  PERFORM check_selected_data CHANGING lv_val.

  IF lv_val IS INITIAL.

    PERFORM call_popup USING   TEXT-t20 TEXT-t22
    CHANGING lv_answer.

    IF lv_answer = 1.
      CLEAR lv_msg.
      LOOP AT gt_index INTO lw_index.
        READ TABLE gt_final INTO lw_final INDEX lw_index-index.
        IF sy-subrc IS INITIAL AND lw_final-icon = gc_icon_0w.
          MESSAGE 'Document already cancelled'(037) TYPE 'I'.
          lv_msg = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_msg IS INITIAL.

*& Add title & FORMNAME for Cancel pop-up
        vl_program        = sy-repid.
        vl_formname       = 'USER_COMMAND_EINV_CANCEL'.
        vl_popup_title    = 'Confirm to cancel E-Invoice'(111).
        vl_okpushbutton   = 'Cancel E-Invoice'(112).
        vl_buttonicon     = '@B2@'."gc_icon_b2.
*& Add two fields to input cancel reason
        APPEND INITIAL LINE TO lt_sval ASSIGNING <ls_sval>.
        <ls_sval>-tabname = 'ZTEINV_DETAILS'."gc_tname_einv.
        <ls_sval>-fieldname = 'E_REASON_CODE'.
        <ls_sval>-field_obl = abap_true.
        <ls_sval>-fieldtext = 'Reason for cancel'(113).
        APPEND INITIAL LINE TO lt_sval ASSIGNING <ls_sval>.
        <ls_sval>-tabname = 'ZTEINV_DETAILS'.
        <ls_sval>-fieldname = 'E_REASON'.
        <ls_sval>-field_obl = abap_true.
        <ls_sval>-fieldtext = 'Remarks'(115).
        PERFORM popup_to_confirm_with_input USING vl_program
              vl_formname
              vl_popup_title
              vl_okpushbutton
              vl_buttonicon
              lt_sval.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&      Form  USER_COMMAND_EINVOICE_CANCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_einv_cancel TABLES ifields TYPE ty_sval
USING  ok_code_save
      error
      h_show_popup.
  DATA: ls_ifields TYPE sval.
  DATA:lv_reason_code TYPE zde_cancel_reason,
       lv_reason      TYPE char120.

  CASE ok_code_save.
    WHEN gc_okcode_furt.
      LOOP AT ifields INTO  ls_ifields.
        IF ls_ifields-fieldname = 'E_REASON_CODE'.
          lv_reason_code = ls_ifields-value.
        ELSEIF ls_ifields-fieldname =  'E_REASON'.
          lv_reason = ls_ifields-value.
        ENDIF.
      ENDLOOP.
      PERFORM einvoice_cancel USING lv_reason_code lv_reason.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EINVOICE_PRINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM einvoice_print .

  DATA:lw_index  TYPE lvc_s_row,
       lv_val    TYPE xfeld,
       lv_answer TYPE c.

*  DATA:lv_index        TYPE sy-tabix.
  DATA: lv_url TYPE string.

  CLEAR lv_url.

  PERFORM check_selected_data CHANGING lv_val.

  IF lv_val IS INITIAL.

    PERFORM call_popup USING   TEXT-t20 TEXT-t31
    CHANGING lv_answer.

    IF lv_answer = '1'.

      LOOP AT  gt_index INTO lw_index.

        READ TABLE gt_final INTO gw_final INDEX lw_index-index.
        IF sy-subrc IS INITIAL AND gw_final-irn IS NOT INITIAL.

          lv_url = gw_final-einv_print.
          CONDENSE lv_url NO-GAPS.

          CALL METHOD cl_gui_frontend_services=>execute
            EXPORTING
              document = lv_url
            EXCEPTIONS
              OTHERS   = 1.
          IF sy-subrc <> 0.
            MESSAGE 'Pdf couldn not find'(023) TYPE 'I'.
          ENDIF.
********************************* Begin of Fill header derails   ***********************************************


*********************************   End of Fill header derails   ***********************************************
        ELSE.
          MESSAGE 'Please select appropriate documents for E-Invoice print' TYPE 'I'.

        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form vf_einvoice_cancel
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <GW_FINAL>_VBELN
*&      <-- IT_RETURN
*&      <-- LV_SUCCESS
*&---------------------------------------------------------------------*
FORM vf_einvoice_cancel  USING iv_docno TYPE vbeln
CHANGING ct_return TYPE tab_bapiret1
  cv_sucess TYPE xfeld.
  DATA: lt_bapireturn1     TYPE TABLE OF bapireturn1,
        ls_bapireturn1     LIKE LINE OF lt_bapireturn1,
        lt_bapivbrksuccess TYPE TABLE OF bapivbrksuccess,
        ls_bapivbrksuccess LIKE LINE OF lt_bapivbrksuccess,
        lv_bill_doc        LIKE bapivbrksuccess-bill_doc.

  CLEAR lv_bill_doc .

  lv_bill_doc  = iv_docno.
****cancel billing doc (transaction VF11)
  CALL FUNCTION 'BAPI_BILLINGDOC_CANCEL1'
    EXPORTING
      billingdocument = lv_bill_doc
    TABLES
      return          = lt_bapireturn1
      success         = lt_bapivbrksuccess.

  IF lt_bapivbrksuccess IS NOT INITIAL.
    cv_sucess = abap_true.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*      EXPORTING
*        wait = 'X'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_EDOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_edoc.

  DATA:lv_val       TYPE xfeld,
       itab1        TYPE truxs_t_text_data,
       lt_text      TYPE truxs_t_text_data,
       lw_text      LIKE LINE OF lt_text,
       lv_file_name TYPE string,
       lv_file      TYPE string,
       lt_file      TYPE rlgrap-filename,
       lv_filename  TYPE string,
       lv_path      TYPE string,
       lv_result    TYPE i,
       lv_fullpath  TYPE string,
       lv_count     TYPE i.

  DATA:lw_download     TYPE zst_edoc_download_format,
       lw_einv_api_hdr TYPE zst_einv_api_struct,
       lw_einv_api_itm TYPE zst_einv_api_struct_itm,
       lw_eway_api_hdr TYPE zst_eway_api_struct,
       lw_eway_api_itm TYPE zst_eway_api_struct_itm.


  CLEAR: gt_download,lw_download..
* Set ownload flag:

  gv_doc_download = abap_true.

  PERFORM check_selected_data CHANGING lv_val.

  IF lv_val IS INITIAL.


    IF p_mod = gc_sd OR p_mod = gc_fi.
      PERFORM einvoice_generate.
    ENDIF.
    PERFORM ewaybill_generate.

    LOOP AT gt_einv_api_hdr INTO lw_einv_api_hdr.
      CLEAR lv_count.
      LOOP AT gt_einv_api_itm INTO lw_einv_api_itm WHERE vbeln = lw_einv_api_hdr-vbeln.

        MOVE-CORRESPONDING lw_einv_api_itm  TO lw_download.
        MOVE-CORRESPONDING lw_einv_api_hdr  TO lw_download.
        lw_download-generate_type = '1'.
        lv_count = lv_count + 1.
        CLEAR:lw_download-supporting_document_url,
        lw_download-supporting_document,
        lw_download-supporting_document,
        lw_download-additional_information,
        lw_download-bar_code,
        lw_download-vendor_po_reference_number,
        lw_download-vendor_po_reference_date.

        IF lw_einv_api_hdr-dispatch IS INITIAL.
          CLEAR: lw_download-comapny_name_d,
          lw_download-address1_d,
          lw_download-address2_d,
          lw_download-location_d,
          lw_download-pincode_d,
          lw_download-state_code_d.

        ENDIF.


        IF lw_einv_api_hdr-ship_to IS INITIAL.
          CLEAR: lw_download-gstin_sh,
          lw_download-legal_name_sh,
          lw_download-trade_name_sh,
          lw_download-address1_sh,
          lw_download-address2_sh,
          lw_download-location_sh,
          lw_download-pincode_sh,
          lw_download-state_code_sh.

        ENDIF.

        lw_download-item_serial_number = lv_count.
        READ TABLE gt_eway_api_hdr INTO lw_eway_api_hdr WITH KEY vbeln = lw_einv_api_hdr-vbeln.
        IF sy-subrc IS INITIAL.
          lw_download-generate_type = '3'.
          lw_download-transaction_type = lw_eway_api_hdr-transaction_type.
          lw_download-sub_supply_description  = lw_eway_api_hdr-sub_supply_description.
          lw_download-sub_supply_type = lw_eway_api_hdr-sub_supply_type.
          lw_download-eway_supply_type = lw_eway_api_hdr-supply_type.
          lw_download-transporter_id = lw_eway_api_hdr-transporter_id.
          lw_download-transporter_name = lw_eway_api_hdr-transporter_name.
          lw_download-transportation_mode = lw_eway_api_hdr-transportation_mode.
          lw_download-transportation_distance = lw_eway_api_hdr-transportation_distance.
          lw_download-transporter_document_number = lw_eway_api_hdr-transporter_document_number.
          lw_download-transporter_document_date = lw_eway_api_hdr-transporter_document_date.
          lw_download-vehicle_number = lw_eway_api_hdr-vehicle_number.
          lw_download-vehicle_type = lw_eway_api_hdr-vehicle_type.
          READ TABLE gt_final INTO DATA(lw_final) WITH KEY vbeln = lw_eway_api_hdr-vbeln
                gjahr = lw_eway_api_hdr-doc_year.
          IF sy-subrc IS INITIAL.
            lw_download-transportation_mode = lw_final-t_mode.
            IF lw_final-v_type = '1'.
              lw_download-vehicle_type = 'R'.
            ELSEIF lw_final-v_type = '2'.
              lw_download-vehicle_type = 'O'.
            ENDIF.
          ELSE.
            lw_download-transportation_mode = lw_eway_api_hdr-t_mode.
            lw_download-vehicle_type =  lw_eway_api_hdr-v_type.
          ENDIF.
        ENDIF.


        APPEND lw_download TO gt_download.
        CLEAR lw_download.
      ENDLOOP.
      DELETE gt_eway_api_hdr WHERE vbeln = lw_einv_api_hdr-vbeln.
    ENDLOOP.


    LOOP AT gt_eway_api_hdr INTO lw_eway_api_hdr.
      LOOP AT gt_eway_api_itm INTO lw_eway_api_itm WHERE document_number = lw_eway_api_hdr-document_number..
        lw_download-user_gstin = lw_eway_api_hdr-user_gstin.
        lw_download-document_type = lw_eway_api_hdr-document_type.
        lw_download-document_number = lw_eway_api_hdr-document_number .
        lw_download-document_date = lw_eway_api_hdr-document_date.
        lw_download-gstin_s = lw_eway_api_hdr-gstin_of_consignor.
        lw_download-legal_name_s = lw_eway_api_hdr-legal_name_of_consignor.
        lw_download-state_code_s = lw_eway_api_hdr-state_of_consignor.
        lw_download-gstin_b = lw_eway_api_hdr-gstin_of_consignee.
        lw_download-legal_name_b  = lw_eway_api_hdr-legal_name_of_consignee.
        lw_download-state_code_b = lw_eway_api_hdr-state_of_supply.
        lw_download-address1_d = lw_eway_api_hdr-address1_of_consignor.
        lw_download-address2_d = lw_eway_api_hdr-address2_of_consignor.
        lw_download-location_d = lw_eway_api_hdr-place_of_consignor.
        lw_download-pincode_d = lw_eway_api_hdr-pincode_of_consignor.
        lw_download-state_code_d = lw_eway_api_hdr-actual_from_state_name.
        lw_download-address1_sh = lw_eway_api_hdr-address1_of_consignee.
        lw_download-address2_sh = lw_eway_api_hdr-address2_of_consignee.
        lw_download-location_sh = lw_eway_api_hdr-place_of_consignee.
        lw_download-pincode_sh = lw_eway_api_hdr-pincode_of_consignee.
        lw_download-state_code_sh = lw_eway_api_hdr-actual_to_state_name.
        lw_download-transporter_id = lw_eway_api_hdr-transporter_id.
        lw_download-transporter_name = lw_eway_api_hdr-transporter_name.
        lw_download-transportation_mode = lw_eway_api_hdr-transportation_mode.
        lw_download-transportation_distance = lw_eway_api_hdr-transportation_distance.
        lw_download-transporter_document_number = lw_eway_api_hdr-transporter_document_number.
        lw_download-transporter_document_date = lw_eway_api_hdr-transporter_document_date.
        lw_download-vehicle_number = lw_eway_api_hdr-vehicle_number.
        lw_download-vehicle_type = lw_eway_api_hdr-vehicle_type.
        lw_download-product_description = lw_eway_api_itm-product_description.
        lw_download-hsn_code = lw_eway_api_itm-hsn_code.
        lw_download-quantity = lw_eway_api_itm-quantity.
        lw_download-unit = lw_eway_api_itm-unit_of_product.
        lw_download-assessable_value = lw_eway_api_itm-taxable_amount.
        lw_download-cess_nonadvol_value = lw_eway_api_itm-cessnonadvol.
        lw_download-product_serial_number = lw_eway_api_itm-product_name.
        lw_download-generate_type = '2'.
        lw_download-transaction_type = lw_eway_api_hdr-transaction_type.
        lw_download-sub_supply_description = lw_eway_api_hdr-sub_supply_description.
        lw_download-sub_supply_type = lw_eway_api_hdr-sub_supply_type.
        lw_download-eway_supply_type = lw_eway_api_hdr-supply_type.
        READ TABLE gt_final INTO lw_final WITH KEY vbeln = lw_eway_api_hdr-vbeln
        gjahr = lw_eway_api_hdr-doc_year.
        IF sy-subrc IS INITIAL.
          lw_download-transportation_mode = lw_final-t_mode.
          IF lw_final-v_type = '1'.
            lw_download-vehicle_type = 'R'.
          ELSEIF lw_final-v_type = '2'.
            lw_download-vehicle_type = 'O'.
          ENDIF.
        ENDIF.
        APPEND lw_download TO gt_download.
        CLEAR lw_download.
      ENDLOOP.
    ENDLOOP.


    IF gt_download IS NOT INITIAL.

      CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
        EXPORTING
          i_field_seperator    = ','  " Comma seperator
        TABLES
          i_tab_sap_data       = gt_download
        CHANGING
          i_tab_converted_data = lt_text
        EXCEPTIONS
          conversion_failed    = 1
          OTHERS               = 2.
      IF sy-subrc = 0.

        IF lt_text[] IS NOT INITIAL.

          CONCATENATE
          'user_gstin'(a01)
          'trans.supply_type'(a02)
          'trans.charge_type'(a03)
          'trans.igst_on_intra'(a04)
          'trans.ecommerce_gstin'(a05)
          'doc.doc_type'(a06)
          'doc.doc_num'(a07)
          'doc.doc_date'(a08)
          'seller.gstin'(a09)
          'seller.legal_name'(a10)
          'seller.trade_name'(a11)
          'seller.address1'(a12)
          'seller.address2'(a13)
          'seller.location'(a14)
          'seller.pincode'(a15)
          'seller.state_code'(a16)
          'seller.phone_num'(a17)
          'seller.email'(a18)
          'buyer.gstin'(a19)
          'buyer.legal_name'(a20)
          'buyer.trade_name'(a21)
          'buyer.address1'(a22)
          'buyer.address2'(a23)
          'buyer.location'(a24)
          'buyer.pincode'(a25)
          'buyer.place_of_supply'(a26)
          'buyer.state_code'(a27)
          'buyer.phone_num'(a28)
          'buyer.email'(a29)
          'dispatch.company_name'(a30)
          'dispatch.address1'(a31)
          'dispatch.address2'(a32)
          'dispatch.location'(a33)
          'dispatch.pincode'(a34)
          'dispatch.state_code'(a35)
          'ship.gstin'(a36)
          'ship.legal_name'(a37)
          'ship.trade_name'(a38)
          'ship.address1'(a39)
          'ship.address2'(a40)
          'ship.location'(a41)
          'ship.pincode'(a42)
          'ship.state_code'(a43)
          'export.ship_bill_num'(a44)
          'export.ship_bill_date'(a45)
          'export.country_code'(a46)
          'export.foreign_currency'(a47)
          'export.refund_claim'(a48)
          'export.port_code'(a49)
          'export.export_duty'(a50)
          'payment.bank_account_num'(a51)
          'payment.paid_balance_amt'(a52)
          'payment.credit_days'(a53)
          'payment.credit_transfer'(a54)
          'payment.direct_debit'(a55)
          'payment.branch_or_ifsc'(a56)
          'payment.payment_mode'(a57)
          'payment.payee_name'(a58)
          'payment.payment_due_date'(a60)
          'payment.payment_instruction'(a61)
          'payment.payment_term'(a62)
          'reference.invoice_remarks'(a63)
          'reference.doc_period.invoice_period_start_date'(a64)
          'reference.doc_period.invoice_period_end_date'(a65)
          'reference.preceding_doc.reference_of_original_invoice'(a66)
          'reference.preceding_doc.preceding_invoice_date'(a67)
          'reference.preceding_doc.other_reference'(a68)
          'reference.contract.receipt_advice_num'(a69)
          'reference.contract.receipt_advice_date'(a70)
          'reference.contract.batch_reference_num'(a71)
          'reference.contract.contract_reference_num'(a72)
          'reference.contract.other_reference'(a73)
          'reference.contract.project_reference_num'(a74)
          'reference.contract.vendor_po_reference_num'(a75)
          'reference.contract.vendor_po_reference_date'(a76)
          'additional_doc.supporting_doc_url'(a77)
          'additional_doc.supporting_doc'(a78)
          'additional_doc.additional_info'(a79)
          'value.discount'(a86)
          'value.total_other_charge'(a87)
          'value.round_off_amt'(a90)
          'value.total_invoice_value_additional_currency'(a91)
          'ewaybill.transporter_id'(a92)
          'ewaybill.transporter_name'(a93)
          'ewaybill.transportation_mode'(a94)
          'ewaybill.transportation_distance'(a95)
          'ewaybill.transporter_doc_num'(a96)
          'ewaybill.transporter_doc_date'(a97)
          'ewaybill.vehicle_num'(a98)
          'ewaybill.vehicle_type'(a99)
          'item_list.item_serial_num'(b01)
          'item_list.product_description'(b02)
          'item_list.is_service'(b03)
          'item_list.hsn_code'(b04)
          'item_list.bar_code'(b05)
          'item_list.quantity'(b06)
          'item_list.free_quantity'(b07)
          'item_list.unit'(b08)
          'item_list.unit_price'(b09)
          'item_list.total_amt'(b10)
          'item_list.pre_tax_value'(b11)
          'item_list.discount'(b12)
          'item_list.other_charge'(b13)
          'item_list.assessable_value'(b14)
          'item_list.gst_rate'(b15)
          'item_list.igst_amt'(b16)
          'item_list.cgst_amt'(b17)
          'item_list.sgst_amt'(b18)
          'item_list.cess_rate'(b19)
          'item_list.cess_amt'(b20)
          'item_list.cess_nonadvol_amt'(b21)
          'item_list.state_cess_rate'(b22)
          'item_list.state_cess_amt'(b23)
          'item_list.state_cess_nonadvol_amt'(b24)
          'item_list.total_item_value'(b25)
          'item_list.country_origin'(b26)
          'item_list.order_line_reference'(b27)
          'item_list.product_serial_num'(b28)
          'item_list.batch.name'(b29)
          'item_list.batch.expiry_date'(b30)
          'item_list.batch.warranty_date'(b31)
          'item_list.attribute.attribute_details'(b32)
          'item_list.attribute.attribute_value'(b33)
          'eway_supply_type'(b34)
          'sub_supply_type'(b35)
          'sub_supply_description'(b36)
          'transaction_type'(b37)
          'generate_type'(b38)

          INTO lw_text SEPARATED BY ','.
          INSERT lw_text INTO lt_text INDEX 1.
          CLEAR : lw_text.
        ENDIF.

        READ TABLE gt_download INTO lw_download INDEX 1.
        IF sy-subrc IS INITIAL.
          REPLACE ALL OCCURRENCES OF '"' IN lw_download-user_gstin WITH space.
          CONCATENATE 'EINVOICE_ALL_' lw_download-user_gstin '_' sy-datum '.csv' INTO lv_filename.
        ENDIF.


        CALL METHOD cl_gui_frontend_services=>file_save_dialog
          EXPORTING
            window_title      = 'File Directory'
            default_extension = '*.csv'
            default_file_name = lv_filename " SET THE DEFAULT FILE NAME HERE
          CHANGING
            filename          = lv_filename
            path              = lv_path
            fullpath          = lv_fullpath
            user_action       = lv_result.


        lv_filename = lv_fullpath.
        lv_file_name = lv_filename.

*Download CSV file
        CALL METHOD cl_gui_frontend_services=>gui_download
          EXPORTING
            filename                = lv_file_name "'c:.csv'
          CHANGING
            data_tab                = lt_text
          EXCEPTIONS
            file_write_error        = 1
            no_batch                = 2
            gui_refuse_filetransfer = 3
            invalid_type            = 4
            no_authority            = 5
            unknown_error           = 6
            header_not_allowed      = 7
            separator_not_allowed   = 8
            filesize_not_allowed    = 9
            header_too_long         = 10
            dp_error_create         = 11
            dp_error_send           = 12
            dp_error_write          = 13
            unknown_dp_error        = 14
            access_denied           = 15
            dp_out_of_memory        = 16
            disk_full               = 17
            dp_timeout              = 18
            file_not_found          = 19
            dataprovider_exception  = 20
            control_flush_error     = 21
            not_supported_by_gui    = 22
            error_no_gui            = 23
            OTHERS                  = 24.
        IF sy-subrc <> 0.

*          MESSAGE

        ENDIF.


      ENDIF.

    ENDIF.

  ENDIF.

  CLEAR gv_doc_download.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_EDOC_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_edoc_file .


  DATA:lv_val       TYPE xfeld,
       itab1        TYPE truxs_t_text_data,
       lt_text      TYPE truxs_t_text_data,
       lw_text      LIKE LINE OF lt_text,
       lv_file_name TYPE string,
       lv_file      TYPE string,
*       lt_file      TYPE rlgrap-filename,
       lv_filename  TYPE string,
       lv_path      TYPE string,
       lv_result    TYPE i,
       lv_fullpath  TYPE string.

  DATA:lt_file TYPE TABLE OF file_table,
       lw_file TYPE file_table.
  DATA : lv_useraction TYPE i,
         lv_rc         TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Edoc response'
*     default_extension       = c_xls
*     file_filter             = c_filter
      multiselection          = space
    CHANGING
      file_table              = lt_file
      rc                      = lv_rc
      user_action             = lv_useraction
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

*-->> If user cancels, exit
  IF lv_useraction <> cl_gui_frontend_services=>action_ok.
    EXIT.
  ENDIF.

*-->>   Get the filename in STRING format
  READ TABLE lt_file INTO lw_file INDEX 1.
  IF sy-subrc IS INITIAL.
    lv_filename = lw_file-filename.
    PERFORM upload_edoc  USING lv_filename.
  ENDIF.

ENDFORM.
*&      Form  UPLOAD EDOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
FORM upload_edoc USING i_filename.
  TYPES:
    BEGIN OF ty_data,
      gstin     TYPE string,
      docno     TYPE string,
      docdt     TYPE string,
      ackno     TYPE string,
      ackdt     TYPE string,
      irn       TYPE string,
      sqrcode   TYPE string,
      ebill     TYPE string,
      ebilldt   TYPE string,
      ebillval  TYPE string,
      qrcodeurl TYPE string,
      einvpdf   TYPE string,
      ewaypdf   TYPE string,
      status    TYPE string,
      alert     TYPE string,
      einverr   TYPE string,
      ewayerr   TYPE string,
      remarks   TYPE string,
      info      TYPE string,
      errcode   TYPE string,
      reqid     TYPE string,
    END OF ty_data.

  DATA: lt_data           TYPE TABLE OF ty_data,
        lw_data           TYPE ty_data,
        lt_ewaybill       TYPE TABLE OF j_1ig_ewaybill,
        lt_einvoice       TYPE TABLE OF j_1ig_invrefnum,
        lt_eway_transport TYPE TABLE OF zteway_transport,
        lt_einv_details   TYPE TABLE OF zteinv_details,
        lv_gjahr(4)       TYPE c,
        lv_fkart          TYPE vbrk-fkart,
        lv_fkdat          TYPE vbrk-fkdat,
        lv_date1          TYPE sy-datum,
        lv_docno          TYPE char10,
        lv_bukrs          TYPE bukrs,
        lv_change         TYPE xfeld,
        lv_mod            TYPE char2,
        lv_filename       TYPE localfile,
        lv_length         TYPE i,
        lv_werks          TYPE werks_d.

  DATA: lt_show_message TYPE esp1_message_tab_type,
        lw_show_message LIKE LINE OF lt_show_message.

  DATA:rt_docno TYPE RANGE OF vbrk-xblnr,
       rs_docno LIKE LINE OF rt_docno,
       rt_date  TYPE RANGE OF sy-datum,
       rs_date  LIKE LINE OF rt_date.

  DATA:lt_excel TYPE STANDARD TABLE OF zst_alsmex_tabline,
       lw_excel TYPE zst_alsmex_tabline.

  lv_filename = i_filename.
  CALL FUNCTION 'Z_ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = lv_filename
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 22
      i_end_row               = 999
    TABLES
      intern                  = lt_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


  LOOP AT lt_excel INTO lw_excel.
    CASE lw_excel-col.
      WHEN 1.
        lw_data-gstin   = lw_excel-value.
      WHEN 2.
        lw_data-docno = lw_excel-value.
      WHEN 3.
        lw_data-docdt    = lw_excel-value.
      WHEN 4.
        lw_data-ackno     = lw_excel-value.
      WHEN 5.
        lw_data-ackdt     = lw_excel-value.
        REPLACE ALL  OCCURRENCES OF '-' IN lw_data-ackdt WITH '/'.
      WHEN 6.
        lw_data-irn       = lw_excel-value.
      WHEN 7.
        lw_data-sqrcode   = lw_excel-value.
      WHEN 8.
        lw_data-ebill     = lw_excel-value.
      WHEN 9.
        lw_data-ebilldt   = lw_excel-value.
        REPLACE ALL  OCCURRENCES OF '-' IN lw_data-ebilldt WITH '/'.
      WHEN 10.
        lw_data-ebillval  = lw_excel-value.
        REPLACE ALL  OCCURRENCES OF '-' IN lw_data-ebillval WITH '/'.
      WHEN 11.
        lw_data-qrcodeurl = lw_excel-value.
      WHEN 12.
        lw_data-einvpdf   = lw_excel-value.
      WHEN 13.
        lw_data-ewaypdf   = lw_excel-value.
      WHEN 14.
        lw_data-status    = lw_excel-value.
      WHEN 15.
        lw_data-alert     = lw_excel-value.
      WHEN 16.
        lw_data-einverr    = lw_excel-value.
      WHEN 17.
        lw_data-ewayerr = lw_excel-value.
      WHEN 18.
        lw_data-remarks   = lw_excel-value.
      WHEN 19.
        lw_data-info      = lw_excel-value.
      WHEN 20.
        lw_data-errcode   = lw_excel-value.
      WHEN 21.
        lw_data-reqid     = lw_excel-value.
    ENDCASE.

    AT END OF row.

      IF lw_data-ackdt IS NOT INITIAL.
        PERFORM convert_date USING lw_data-ackdt CHANGING lw_data-ackdt.
      ENDIF.

      IF lw_data-ebilldt IS NOT INITIAL.
        PERFORM convert_date USING lw_data-ebilldt CHANGING lw_data-ebilldt.
      ENDIF.

      IF lw_data-ebillval IS NOT INITIAL.
        PERFORM convert_date USING lw_data-ebillval CHANGING lw_data-ebillval.
      ENDIF.

      APPEND lw_data TO lt_data.
      CLEAR lw_data.
    ENDAT.

    CLEAR lw_excel.

  ENDLOOP.

  IF lt_data IS NOT INITIAL.
    rs_docno-sign  = gc_sign.
    rs_docno-option = gc_equal.
    rs_date-sign  = gc_sign.
    rs_date-option = gc_equal.
    LOOP AT lt_data INTO DATA(ls_data).
      READ TABLE gt_final TRANSPORTING NO FIELDS WITH KEY odnno =  ls_data-docno.
      IF sy-subrc IS NOT INITIAL.
        rs_docno-low  = ls_data-docno.
        APPEND rs_docno TO rt_docno.
      ENDIF.
    ENDLOOP.

* Check the sales data
    IF p_mod = gc_sd.
      IF rt_docno IS NOT  INITIAL.
        SELECT vbeln,fkart,fkdat,gjahr,bukrs,xblnr FROM vbrk
        INTO TABLE @DATA(lt_vbrk) WHERE vbeln IN @rt_docno.
        IF sy-subrc IS INITIAL.

          SELECT  vbeln,posnr,werks FROM  vbrp INTO TABLE @DATA(lt_vbrp)
                FOR ALL ENTRIES IN @lt_vbrk
                WHERE vbeln = @lt_vbrk-vbeln.

          SELECT * FROM j_1ig_invrefnum APPENDING TABLE gt_invrefnum
          FOR ALL ENTRIES IN lt_vbrk
          WHERE bukrs = lt_vbrk-bukrs
          AND docno = lt_vbrk-vbeln
          AND doc_type = lt_vbrk-fkart.
          IF sy-subrc IS INITIAL.
            SELECT * FROM zteinv_details APPENDING TABLE gt_zteinv_details
            FOR ALL ENTRIES IN lt_einvoice
            WHERE bukrs = lt_einvoice-bukrs
            AND doctyp = lt_einvoice-doc_type
            AND docno = lt_einvoice-docno.
          ENDIF.

          SELECT * FROM j_1ig_ewaybill APPENDING TABLE gt_ewaybill_new
          FOR ALL ENTRIES IN lt_vbrk
          WHERE bukrs = lt_vbrk-bukrs
          AND docno = lt_vbrk-vbeln
          AND doctyp = lt_vbrk-fkart.
          IF sy-subrc IS INITIAL.
            SELECT * FROM zteway_transport APPENDING TABLE gt_zteway_transport
            FOR ALL ENTRIES IN lt_ewaybill
            WHERE bukrs = lt_ewaybill-bukrs
            AND doctyp = lt_ewaybill-doctyp
            AND docno = lt_ewaybill-docno.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.


    LOOP AT lt_data INTO DATA(lw_data1).

      CLEAR: lv_bukrs,lv_docno,lv_fkdat,lv_fkart,lv_bukrs,lv_change,lv_werks.

      READ TABLE gt_final ASSIGNING FIELD-SYMBOL(<lw_final>) WITH KEY vbeln = lw_data1-docno.
      IF sy-subrc IS INITIAL.

        lv_docno =  <lw_final>-vbeln.
        lv_bukrs =  <lw_final>-bukrs.
        lv_fkdat =  <lw_final>-fkdat.
        lv_fkart =  <lw_final>-fkart.
        lv_gjahr =  <lw_final>-gjahr.
        lv_mod   = p_mod.
        lv_werks = <lw_final>-werks.
      ELSE.

        READ TABLE lt_vbrk INTO DATA(lw_vbrk) WITH KEY vbeln = lw_data1-docno.
        IF sy-subrc IS INITIAL.
          lv_docno = lw_vbrk-vbeln.
          lv_bukrs = lw_vbrk-bukrs.
          lv_fkdat = lw_vbrk-fkdat.
          lv_fkart = lw_vbrk-fkart.
          lv_gjahr = lw_vbrk-gjahr.
          lv_mod   = gc_sd.
          READ TABLE lt_vbrp INTO DATA(lw_vbrp) WITH KEY vbeln = lw_vbrk-vbeln.
          IF sy-subrc IS INITIAL.
            lv_werks = lw_vbrp-werks.
          ENDIF.

          IF lv_gjahr IS INITIAL.

            CALL FUNCTION 'GM_GET_FISCAL_YEAR'
              EXPORTING
                i_date                     = lv_fkdat
                i_fyv                      = 'V3'
              IMPORTING
                e_fy                       = lv_gjahr
              EXCEPTIONS
                fiscal_year_does_not_exist = 1
                not_defined_for_date       = 2
                OTHERS                     = 3.
            IF sy-subrc <> 0.
*            MESSAGE 'Conversion error'(m03) TYPE 'I'.
            ENDIF.
          ENDIF.


        ENDIF.
      ENDIF.




* Fill the internal tables
      IF lv_docno IS NOT INITIAL.
        IF lv_mod =  gc_sd OR lv_mod = gc_fi.

          IF lw_data1-irn IS NOT INITIAL OR lw_data1-einverr IS NOT INITIAL.
            READ TABLE lt_einvoice INTO DATA(lw_einvoice) WITH KEY  bukrs = lv_bukrs
                  docno = lv_docno
                  doc_type = lv_fkart.
* Update e-invoice table
            IF sy-subrc IS NOT INITIAL.
              lw_einvoice-bukrs =  lv_bukrs.
              lw_einvoice-docno = lv_docno.
              lw_einvoice-doc_year = lv_gjahr.
              lw_einvoice-doc_type = lv_fkart.
              lw_einvoice-odn = lw_data1-docno.
              lw_einvoice-ernam = sy-uname.
              lw_einvoice-erdat = sy-datum.
              lw_einvoice-erzet = sy-uzeit.
            ELSE.
            ENDIF.

            IF lw_data1-irn IS NOT INITIAL.
              lw_einvoice-irn = lw_data1-irn.
              DATA(lv_ackno) = lw_data1-ackno.
              lv_length = strlen( lv_ackno ).
              lv_length = lv_length - 1.
              lw_einvoice-ack_no = lv_ackno+1(lv_length)."lw_data1-ackno.
              IF lw_data1-ackdt IS NOT INITIAL.
                CONCATENATE lw_data1-ackdt+0(2) '.'  lw_data1-ackdt+3(2)'.' lw_data1-ackdt+6(4)
                INTO lw_einvoice-ack_date.
              ENDIF.

              lw_einvoice-signed_qrcode = lw_data1-sqrcode.
              lw_einvoice-irn_status = 'ACT'.
            ELSE.
              lw_einvoice-irn_status = 'ERR'.
            ENDIF.

* Fill einvoice custom data
            READ TABLE lt_einv_details INTO DATA(lw_einv_detals) WITH KEY   bukrs = lv_bukrs
                  doctyp = lv_fkart
                  docno = lv_docno.

            IF sy-subrc IS NOT INITIAL.
              lw_einv_detals-bukrs = lv_bukrs.
              lw_einv_detals-doctyp = lv_fkart.
              lw_einv_detals-docno = lv_docno.
              lw_einv_detals-gjahr = lv_gjahr.
              lw_einv_detals-ernam = sy-uname.
              lw_einv_detals-erdat = sy-datum.
            ELSE.
              lw_einv_detals-aenam = sy-uname.
              lw_einv_detals-aedat = sy-datum.
            ENDIF.

            lw_einv_detals-einv_error = lw_data1-einverr.
            lw_einv_detals-qrcodeurl = lw_data1-qrcodeurl.
            lw_einv_detals-einv_pdf  = lw_data1-einvpdf.

* Save eway bill
            MODIFY j_1ig_invrefnum FROM lw_einvoice.
            IF sy-subrc IS INITIAL.

              MODIFY zteinv_details FROM lw_einv_detals.
              IF sy-subrc IS INITIAL.

                lw_show_message-msgid =  '01'.
                lw_show_message-msgty =  'S'.
                lw_show_message-msgno =  '319'.
                lw_show_message-msgv1 =  'E-invoice updated'.
                lw_show_message-msgv2 =  'for document'.
                lw_show_message-msgv3 =  lw_data1-docno.
                APPEND lw_show_message TO lt_show_message.
              ELSE.
                lw_show_message-msgid =  '01'.
                lw_show_message-msgty =  'E'.
                lw_show_message-msgno =  '319'.
                lw_show_message-msgv1 =  'Error while updating e-invoice'.
                lw_show_message-msgv2 =  'for document'.
                lw_show_message-msgv3 =  lw_data1-docno.
                APPEND lw_show_message TO lt_show_message.
              ENDIF.

* Update cockpit
              IF <lw_final> IS ASSIGNED.
                <lw_final>-irn =  lw_einvoice-irn.
                <lw_final>-ack_no =  lw_einvoice-ack_no.
                <lw_final>-ack_dt =  lw_einvoice-ack_date.
                <lw_final>-ernam = lw_einvoice-ernam.

                CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
                  EXPORTING
                    date_internal            = lw_einvoice-erdat
                  IMPORTING
                    date_external            = <lw_final>-erdat
                  EXCEPTIONS
                    date_internal_is_invalid = 1
                    OTHERS                   = 2.
                IF sy-subrc <> 0.
                  MESSAGE 'Date conversion error'(m02) TYPE 'I'.
                ENDIF.
                <lw_final>-erzet = lw_einvoice-erzet.
                IF lw_einvoice-irn_status = 'ACT'.
                  <lw_final>-status = 'Success'.
                  <lw_final>-icon    = '@08@'.
                ELSEIF  lw_einvoice-irn_status = 'ERR'.
                  <lw_final>-status = 'Error'.
                  <lw_final>-icon    = '@0A@'.
                  <lw_final>-einv_error = lw_einv_detals-einv_error.
                ENDIF.
              ENDIF.


            ENDIF.

          ENDIF.
        ENDIF.

* Eway bill.
        IF p_mod = gc_sd.
          IF lw_data1-ebill IS NOT INITIAL OR lw_data1-ewayerr IS NOT INITIAL.
            READ TABLE lt_ewaybill INTO DATA(lw_ewaybill) WITH KEY bukrs = lv_bukrs
                  docno = lv_docno
                  doctyp = lv_fkart.
            IF sy-subrc IS NOT INITIAL.
              lw_ewaybill-bukrs =  lv_bukrs.
              lw_ewaybill-docno = lv_docno.
              lw_ewaybill-gjahr = lv_gjahr.
              lw_ewaybill-doctyp = lv_fkart.
              lw_ewaybill-ernam = sy-uname.
              lw_ewaybill-erdat = sy-datum.
              lw_ewaybill-gjahr = lv_gjahr.
            ELSE.
              lw_ewaybill-aenam = sy-uname.
              lw_ewaybill-aedat = sy-datum.
            ENDIF.
            IF lw_data1-ebill IS NOT INITIAL.


              DATA(lv_ewaybill) = lw_data1-ebill.
              lv_length = strlen( lv_ewaybill ).
              lv_length = lv_length - 1.
              lw_ewaybill-ebillno = lv_ewaybill+1(lv_length).

              IF lw_data1-ebilldt IS NOT INITIAL.
                CONCATENATE lw_data1-ebilldt+6(4) lw_data1-ebilldt+3(2) lw_data1-ebilldt+0(2)
                INTO lw_ewaybill-egen_dat.
                lw_ewaybill-vdfmdate = lw_ewaybill-egen_dat.

                CONCATENATE lw_data1-ebilldt+11(2) lw_data1-ebilldt+14(2) '00'"lw_data1-ebilldt+17(2)
                INTO lw_ewaybill-egen_time.
                lw_ewaybill-vdfmtime = lw_ewaybill-egen_time.
              ENDIF.

              IF lw_data1-ebillval IS NOT INITIAL.
                CONCATENATE lw_data1-ebillval+6(4) lw_data1-ebillval+3(2) lw_data1-ebillval+0(2)
                INTO lw_ewaybill-vdtodate.

                CONCATENATE lw_data1-ebillval+11(2) lw_data1-ebillval+14(2) '00'"lw_data1-ebillval+17(2)
                INTO lw_ewaybill-vdtotime.
              ENDIF.
              lw_ewaybill-status = 'A'.
            ENDIF.

* eway bill custom table update
            READ TABLE lt_eway_transport INTO DATA(lw_eway_transport) WITH KEY bukrs = lv_bukrs
                  docno = lv_docno
                  doctyp = lv_fkart.

            IF sy-subrc IS NOT INITIAL.
              lw_eway_transport-bukrs  = lv_bukrs.
              lw_eway_transport-doctyp = lv_fkart.
              lw_eway_transport-docno = lv_docno.
              lw_eway_transport-gjahr = lv_gjahr.
              lw_eway_transport-ernam = sy-uname.
              lw_eway_transport-erdat = sy-datum.
              lw_eway_transport-gjahr = lv_gjahr.
            ELSE.
              lw_eway_transport-aenam = sy-uname.
              lw_eway_transport-aedat = sy-datum.
            ENDIF.

            lw_eway_transport-eway_error = lw_data1-ewayerr.
            lw_eway_transport-eway_print = lw_data1-ewaypdf.


* Save eway bill
            MODIFY j_1ig_ewaybill FROM lw_ewaybill.
            IF sy-subrc IS INITIAL.

              MODIFY zteway_transport FROM lw_eway_transport.
              IF sy-subrc IS INITIAL.

                lw_show_message-msgid =  '01'.
                lw_show_message-msgty =  'S'.
                lw_show_message-msgno =  '319'.
                lw_show_message-msgv1 =  'Eway bill updated'.
                lw_show_message-msgv2 =  'for document'.
                lw_show_message-msgv3 =  lw_data1-docno.
                APPEND lw_show_message TO lt_show_message.
              ELSE.
                lw_show_message-msgid =  '01'.
                lw_show_message-msgty =  'E'.
                lw_show_message-msgno =  '319'.
                lw_show_message-msgv1 =  'Error while updating ewaybill'.
                lw_show_message-msgv2 =  'for document'.
                lw_show_message-msgv3 =  lw_data1-docno.
                APPEND lw_show_message TO lt_show_message.
              ENDIF.
              IF <lw_final> IS ASSIGNED.

                <lw_final>-eway_num = lw_ewaybill-ebillno.

                CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
                  EXPORTING
                    date_internal            = lw_ewaybill-egen_dat
                  IMPORTING
                    date_external            = <lw_final>-eway_date
                  EXCEPTIONS
                    date_internal_is_invalid = 1
                    OTHERS                   = 2.
                IF sy-subrc <> 0.
                  MESSAGE 'Date conversion error'(m02) TYPE 'I'.
                ENDIF.

                <lw_final>-eway_ernam = lw_ewaybill-ernam.

                CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
                  EXPORTING
                    date_internal            = lw_ewaybill-erdat
                  IMPORTING
                    date_external            = <lw_final>-eway_erdat
                  EXCEPTIONS
                    date_internal_is_invalid = 1
                    OTHERS                   = 2.
                IF sy-subrc <> 0.
                  MESSAGE 'Date conversion error'(m02) TYPE 'I'.
                ENDIF.

                <lw_final>-eway_erzet = lw_ewaybill-egen_time.

                CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
                  EXPORTING
                    date_internal            = lw_ewaybill-vdtodate
                  IMPORTING
                    date_external            = <lw_final>-eway_v_to
                  EXCEPTIONS
                    date_internal_is_invalid = 1
                    OTHERS                   = 2.
                IF sy-subrc <> 0.
                  MESSAGE 'Date conversion error'(m02) TYPE 'I'.
                ENDIF.


                IF lw_ewaybill-status = 'A'.
                  <lw_final>-eway_status = 'Success'.
                  <lw_final>-eway_icon    = '@08@'.
                  <lw_final>-eway_error   = space.
                ELSEIF lw_ewaybill-status = 'E'.
                  <lw_final>-eway_status = 'Error'.
                  <lw_final>-eway_icon    = '@0A@'.
                  <lw_final>-eway_error   = lw_eway_transport-eway_error.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.


    IF lt_data IS NOT INITIAL.

      IF gref_alv_grid IS BOUND.
        CALL METHOD gref_alv_grid->refresh_table_display
          EXCEPTIONS
            finished = 1
            OTHERS   = 2.
        IF sy-subrc <> 0.
          MESSAGE 'Table refresh error'(012) TYPE 'I'.
        ENDIF.
      ENDIF.
    ENDIF.


    IF lt_show_message IS NOT INITIAL.
      CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
        TABLES
          i_message_tab = lt_show_message.
    ENDIF.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONVERT_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_DATA_EBILLDT  text
*      <--P_LW_DATA_EBILLDT  text
*----------------------------------------------------------------------*
FORM convert_date  USING     i_date
CHANGING  c_date.

  DATA:lv_val1    TYPE string,
       lv_val2    TYPE string,
       lv_val3    TYPE string,
       lv_val4    TYPE string,
       lv_val5    TYPE string,
       lv_length1 TYPE i.

  SPLIT i_date AT space INTO lv_val1 lv_val5.

  SPLIT lv_val1 AT '/' INTO lv_val2 lv_val3 lv_val4.


  IF lv_val2 IS NOT INITIAL.
    lv_length1 = strlen( lv_val2 ).
    IF lv_length1 < 2.
      CONCATENATE '0' lv_val2 INTO lv_val2.
    ENDIF.
  ENDIF.

  IF lv_val3 IS NOT INITIAL.
    lv_length1 = strlen( lv_val3 ).
    IF lv_length1 < 2.
      CONCATENATE '0' lv_val3 INTO lv_val3.
    ENDIF.
  ENDIF.

  CONCATENATE lv_val2 '/' lv_val3 '/' lv_val4 INTO c_date.
  CONCATENATE c_date lv_val5 INTO c_date SEPARATED BY space.


ENDFORM.
