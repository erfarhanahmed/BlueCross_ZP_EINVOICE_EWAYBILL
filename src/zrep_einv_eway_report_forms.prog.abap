*&---------------------------------------------------------------------*
*& Include          ZREP_EINV_EWAY_REPORT_FORMS
*&---------------------------------------------------------------------*
FORM get_data .

  DATA:lw_user_config TYPE ty_user_config.

* Get document related configuration data
  SELECT bukrs
  zmodule
  fkart
  vtext
  sup_type
  sub_type
  edoc_type
  einv
  eway
  accpost
  senderr
  instgen
  batch	 FROM zteinv_doctyp
  INTO  TABLE gt_doctyp
  WHERE bukrs EQ p_ccode AND
  zmodule EQ p_mod AND
  fkart IN s_docty AND
  batch EQ abap_true.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Batch generation config is empty for the given selection'(008) TYPE 'E'.
  ENDIF.


* Get generator/Approver data
  SELECT bukrs
  zmodule
  fkart
  werks
  prctr
  gen_apr
  blk_usr   FROM zteinv_appvr
  INTO TABLE gt_user_config
  WHERE bukrs EQ p_ccode AND
  zmodule EQ p_mod AND
  fkart IN s_docty AND
  werks IN s_werks AND
  prctr IN s_prctr AND
  gen_apr = sy-uname.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Config is empty for the given selection'(009) TYPE 'E'.
  ENDIF.

* get the API data
  SELECT apiid apiprov FROM zteinv_api INTO TABLE gt_api.
  SELECT * FROM ztt_state_code INTO TABLE @DATA(gt_state_code).
  IF p_mod = gc_sd.

* collect the billing type and plant details
    CLEAR:sl_fkart,sl_werks.
    IF gt_user_config IS NOT INITIAL.
      LOOP AT gt_user_config INTO lw_user_config  WHERE zmodule = gc_sd
      AND fkart IN s_docty.
        ss_fkart-sign = gc_sign.
        ss_fkart-option = gc_equal.
        ss_fkart-low = lw_user_config-fkart.
        APPEND ss_fkart TO sl_fkart.
        CLEAR:ss_fkart.

        ss_werks-sign = gc_sign.
        ss_werks-option = gc_equal.
        ss_werks-low = lw_user_config-werks.
        APPEND ss_werks TO sl_werks.
        CLEAR ss_werks.
      ENDLOOP.
      SORT sl_fkart.
      DELETE ADJACENT DUPLICATES FROM sl_fkart COMPARING ALL FIELDS.
      SORT sl_werks.
      DELETE ADJACENT DUPLICATES FROM sl_werks COMPARING ALL FIELDS.
    ENDIF.


    PERFORM process_sd_data.
*{   DELETE         MSDK900284                                        1
*\    PERFORM process_sd_data_delivery.
*}   DELETE

  ELSEIF p_mod = gc_fi.

* collect the billing type and plant details
    CLEAR:sl_fkart,sl_werks.
    IF gt_user_config IS NOT INITIAL.
      LOOP AT gt_user_config INTO lw_user_config  WHERE zmodule = gc_fi
      AND fkart IN s_docty.
        ss_fkart-sign = gc_sign.
        ss_fkart-option = gc_equal.
        ss_fkart-low = lw_user_config-fkart.
        APPEND ss_fkart TO sl_fkart.
        CLEAR:ss_fkart.

        ss_werks-sign = gc_sign.
        ss_werks-option = gc_equal.
        ss_werks-low = lw_user_config-werks.
        APPEND ss_werks TO sl_werks.
        CLEAR ss_werks.
      ENDLOOP.
      SORT sl_fkart.
      DELETE ADJACENT DUPLICATES FROM sl_fkart COMPARING ALL FIELDS.
      SORT sl_werks.
      DELETE ADJACENT DUPLICATES FROM sl_werks COMPARING ALL FIELDS.
    ENDIF.

    PERFORM process_fi_data.
  ELSEIF p_mod = gc_mm.

* collect the billing type and plant details
    CLEAR:sl_fkart,sl_werks.
    IF gt_user_config IS NOT INITIAL.
      LOOP AT gt_user_config INTO lw_user_config  WHERE zmodule = gc_mm
      AND fkart IN s_docty.
        ss_fkart-sign = gc_sign.
        ss_fkart-option = gc_equal.
        ss_fkart-low = lw_user_config-fkart.
        APPEND ss_fkart TO sl_fkart.
        CLEAR:ss_fkart.

        ss_werks-sign = gc_sign.
        ss_werks-option = gc_equal.
        ss_werks-low = lw_user_config-werks.
        APPEND ss_werks TO sl_werks.
        CLEAR ss_werks.
      ENDLOOP.
      SORT sl_fkart.
      DELETE ADJACENT DUPLICATES FROM sl_fkart COMPARING ALL FIELDS.
      SORT sl_werks.
      DELETE ADJACENT DUPLICATES FROM sl_werks COMPARING ALL FIELDS.
    ENDIF.

    PERFORM process_mm_data.

  ENDIF.

  PERFORM change_region_text.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  SET_MODULE_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_module_value .

  DATA:lt_values    TYPE TABLE OF   dd07v,
       ls_value     TYPE dd07v,
       lv_name      TYPE vrm_id,
       lt_list      TYPE vrm_values,
       lw_mod_value LIKE LINE OF lt_list.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZDOM_ZMODULE'
    TABLES
      values_tab      = lt_values
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc = 0.
    LOOP AT lt_values INTO ls_value.
      lw_mod_value-key =  ls_value-domvalue_l.
      lw_mod_value-text = ls_value-ddtext.
      APPEND lw_mod_value TO lt_list.
    ENDLOOP.

    lv_name = 'P_MOD'.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = lv_name
        values = lt_list.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FETCH_USER_CONFIG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fetch_user_config .

  DATA:lw_token    TYPE string,
       lw_return   TYPE string,
       wa_return   TYPE string,
       lt_messages TYPE bapiret2_t,
       lw_message  TYPE bapiret2.

* get approver details
  SELECT bukrs
  zmodule
  fkart
  werks
  prctr
  gen_apr
  blk_usr   FROM zteinv_appvr INTO TABLE gt_user_config WHERE gen_apr = sy-uname AND
  blk_usr EQ ''.
  IF sy-subrc IS NOT INITIAL.
* Through error message
    EXIT.
  ENDIF.

*** Fetch UOM Mapping
  SELECT edoc_uom meins FROM ztdoc_uom INTO TABLE gt_doc_uom WHERE edoc_uom IS NOT NULL.

* Get export port details.
  SELECT zport_code
  zport
  land1
  zport_state
  zport_address1
  zport_address2
  zport_place
  zport_pincode FROM ztedoc_export INTO TABLE gt_export WHERE land1 = gc_land1_in.

*** generate token
  CLEAR:gw_token,lw_return.
  SELECT SINGLE apiuri FROM zteinv_api INTO gw_token WHERE apiid = gc_apiid_token.
  IF sy-subrc IS NOT INITIAL.
    IF gw_token IS INITIAL.
      CALL FUNCTION 'ZFM_EINVOICE_OAUTH_API'
        IMPORTING
          ex_token    = gw_token
          ex_return   = lw_return
          et_messages = lt_messages.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SD_PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_sd_data .


  DATA: lw_invrefnum        TYPE j_1ig_invrefnum,
        lw_wb2_v_vbrk_vbrp2 TYPE ty_wb2_v_vbrk_vbrp2,
        lw_kna1             LIKE LINE OF gt_kna1,
        lw_gstin            LIKE LINE OF gt_gstin,
        lv_internal_date    TYPE sy-datum,
        lw_konv             TYPE ty_konv,
        lw_ewaybill         TYPE ty_ewaybill,
        ls_zteway_transport TYPE zteway_transport,
        ls_zteinv_details   TYPE zteinv_details,
        lw_vbpa             TYPE ty_vbpa,
        lw_kna1_d           TYPE ty_kna1,
        lw_lfa1             TYPE lfa1,
        lt_likp             TYPE TABLE OF ty_likp,
        lv_gjahr(4)         TYPE c,
        lt_vbrk_vbrp        TYPE TABLE OF ty_wb2_v_vbrk_vbrp2,
        lw_vbrk_vbrp2       TYPE ty_wb2_v_vbrk_vbrp2,
        lw_doctyp           TYPE ty_doctyp,
        lt_sd_header        TYPE TABLE OF ty_wb2_v_vbrk_vbrp2,
        lt_vbrk_export      TYPE TABLE OF ty_wb2_v_vbrk_vbrp2,
        lv_index            TYPE sy-tabix,
        lv_exp_curr         TYPE xfeld,
        lv_tdname           TYPE thead-tdname.

  DATA:rt_name TYPE RANGE OF rvari_vnam,
       rs_name LIKE LINE OF rt_name.

  CONSTANTS:lv_parvw TYPE parvw VALUE 'SP'.



*& Get Invocies for E-invocie and E-way bill
  IF s_prctr IS NOT INITIAL.


    SELECT vbeln  vbeln_i posnr_i fkart fktyp vbtyp waerk vkorg
    vtweg  knumv fkdat gjahr kurrf land1 bukrs netwr ernam kunrg
    kunag xblnr zuonr fksto bupla fkimg_i  meins_i kursk_i
    netwr_i vgbel_i  matnr_i arktx_i charg_i pstyv_i werks_i prctr_i mwsbp_i vrkme_i
    FROM wb2_v_vbrk_vbrp2
    INTO TABLE gt_wb2_v_vbrk_vbrp2
    WHERE fkart IN sl_fkart
    AND vbeln IN s_doc
    AND fkdat IN s_date
    AND bukrs = p_ccode
    AND werks_i  IN sl_werks
    AND prctr_i IN s_prctr.

  ELSE.


    SELECT vbeln  vbeln_i posnr_i fkart fktyp vbtyp waerk vkorg
    vtweg  knumv fkdat gjahr kurrf land1 bukrs netwr ernam kunrg
    kunag xblnr zuonr fksto bupla fkimg_i  meins_i kursk_i
    netwr_i vgbel_i  matnr_i arktx_i charg_i pstyv_i werks_i prctr_i mwsbp_i vrkme_i
    FROM wb2_v_vbrk_vbrp2
    INTO TABLE gt_wb2_v_vbrk_vbrp2
    WHERE fkart IN sl_fkart
    AND vbeln IN s_doc
    AND fkdat IN s_date
    AND bukrs = p_ccode
    AND werks_i  IN sl_werks.

  ENDIF.



  DELETE gt_wb2_v_vbrk_vbrp2 WHERE vbeln CS 'TMP'.
  IF gt_wb2_v_vbrk_vbrp2 IS NOT INITIAL.

    lt_sd_header = gt_wb2_v_vbrk_vbrp2.

    SORT lt_sd_header BY bukrs fkart vbeln.
    DELETE ADJACENT DUPLICATES FROM lt_sd_header COMPARING  bukrs fkart vbeln.

    IF lt_sd_header IS NOT INITIAL.

      SELECT knumv kposn  kschl kbetr kwert FROM prcd_elements INTO TABLE gt_konv
      FOR ALL ENTRIES IN lt_sd_header
      WHERE knumv = lt_sd_header-knumv
      AND kinak = abap_false.

      IF sy-subrc IS INITIAL.
        SORT gt_konv BY knumv kposn kschl.
      ENDIF.

* Eway bill
      SELECT bukrs
      doctyp
      docno
      gjahr
      ebillno
      egen_dat
      egen_time
      vdfmdate
      vdtodate
      vdtotime
      status
      ernam
      erdat
      aenam
      aedat FROM j_1ig_ewaybill INTO TABLE gt_ewaybill
      FOR ALL ENTRIES IN lt_sd_header
      WHERE bukrs =  lt_sd_header-bukrs
      AND doctyp = lt_sd_header-fkart
      AND docno  = lt_sd_header-vbeln.
      IF sy-subrc IS INITIAL.
        SORT gt_ewaybill DESCENDING BY egen_dat egen_time.
      ENDIF.

* Document Flow Details
      SELECT vbelv  posnv vbeln FROM vbfa
      INTO TABLE gt_vbfa
      FOR ALL ENTRIES IN lt_sd_header
      WHERE vbeln   = lt_sd_header-vbeln
      AND vbtyp_v = gc_vbtyp_v.

* GSTIN deatils
      SELECT  bukrs branch name adrnr gstin
      FROM j_1bbranch
      INTO TABLE gt_gstin FOR ALL ENTRIES IN lt_sd_header
      WHERE bukrs = lt_sd_header-bukrs
      AND branch = lt_sd_header-bupla.

* Get Transporter details from Z-Table
      SELECT * FROM zteway_transport INTO TABLE gt_zteway_transport
      FOR ALL ENTRIES IN lt_sd_header
      WHERE bukrs =  lt_sd_header-bukrs
      AND doctyp = lt_sd_header-fkart
      AND docno  = lt_sd_header-vbeln.
      IF sy-subrc IS INITIAL.
        SORT gt_zteway_transport BY bukrs doctyp docno.
      ENDIF.

      SELECT * FROM ztransport INTO TABLE @DATA(gt_transport) FOR ALL ENTRIES IN @lt_sd_header WHERE kunnr = @lt_sd_header-kunag.

* Get e-invoice details from Z-Table
      SELECT * FROM zteinv_details INTO TABLE gt_zteinv_details
      FOR ALL ENTRIES IN lt_sd_header
      WHERE bukrs =  lt_sd_header-bukrs
      AND doctyp = lt_sd_header-fkart
      AND docno  = lt_sd_header-vbeln.
      IF sy-subrc IS INITIAL.
        SORT gt_zteinv_details BY  bukrs docno doctyp.
      ENDIF.



* Select partners
      SELECT vbeln posnr parvw kunnr lifnr adrnr
      FROM vbpa INTO TABLE gt_vbpa
      FOR ALL ENTRIES IN lt_sd_header
      WHERE vbeln = lt_sd_header-vbeln.

*Buyer Details
      IF gt_vbpa IS NOT INITIAL.
        SORT gt_vbpa BY vbeln parvw.
        DATA(gt_vbpa_trans) = gt_vbpa.
        DELETE gt_vbpa_trans WHERE lifnr IS INITIAL AND parvw <> 'ZT'.
        IF gt_vbpa_trans IS NOT INITIAL.
          SELECT * FROM lfa1 INTO TABLE @DATA(gt_lfa1_trans) FOR ALL ENTRIES IN @gt_vbpa_trans WHERE lifnr = @gt_vbpa_trans-lifnr.
        ENDIF.
        SELECT * FROM adrc INTO TABLE gt_adrc_new FOR ALL ENTRIES IN gt_vbpa
        WHERE addrnumber = gt_vbpa-adrnr.



        SELECT  kunnr land1 name1 name2 ort01 pstlz
        regio stras telf1 adrnr stcd3
        FROM kna1 INTO  TABLE gt_kna1
        FOR ALL ENTRIES IN gt_vbpa
        WHERE kunnr = gt_vbpa-kunnr.
        IF sy-subrc EQ 0.
          SORT gt_kna1 BY kunnr.
          SELECT spras land1 bland bezei FROM t005u
          INTO TABLE gt_t005u
          FOR ALL ENTRIES IN gt_kna1
          WHERE land1 EQ gt_kna1-land1
          AND bland EQ gt_kna1-regio
          AND spras EQ sy-langu.

          SELECT addrnumber tel_number  FROM adrc
          INTO TABLE gt_adrc
          FOR ALL ENTRIES IN gt_kna1
          WHERE addrnumber = gt_kna1-adrnr.

          IF sy-subrc IS INITIAL.
            SELECT addrnumber smtp_addr   FROM adr6
            INTO TABLE gt_adr6
            FOR ALL ENTRIES IN gt_adrc
            WHERE addrnumber = gt_adrc-addrnumber.
          ENDIF.
        ENDIF.
      ENDIF.

      SELECT * FROM j_1ig_invrefnum
      INTO TABLE gt_invrefnum
      FOR ALL ENTRIES IN lt_sd_header
      WHERE bukrs EQ lt_sd_header-bukrs
      AND docno EQ lt_sd_header-vbeln
      AND doc_type EQ lt_sd_header-fkart.
      IF sy-subrc EQ 0.
        SORT  gt_invrefnum DESCENDING.
      ENDIF.



      lt_vbrk_export = lt_sd_header.
      DELETE lt_vbrk_export WHERE waerk = gc_curr_inr.

      IF lt_vbrk_export IS NOT INITIAL.
*        SELECT bukrs
*                    belnr
*                    gjahr
*                    blart
*                    bldat
*                    budat
*                    tcode
*                    xblnr
*                    bktxt
*                    awtyp
*                    awkey FROM bkpf INTO TABLE gt_bkpfm
*               FOR ALL ENTRIES IN lt_vbrk_export
*               WHERE bukrs = p_ccode
*               AND  xblnr = lt_vbrk_export-xblnr.
*        IF gt_bkpfm IS NOT INITIAL.
*          SELECT  bukrs
*                  belnr
*                  gjahr
*                  buzei
*                  buzid
*                  bschl
*                  koart
*                  shkzg
*                  dmbtr
*                  txgrp
*                  ktosl
*                  ebeln
*                  ebelp
*                  taxps  FROM bseg INTO TABLE gt_bseg
*             FOR ALL ENTRIES IN gt_bkpfm
*            WHERE bukrs = gt_bkpfm-bukrs
*            AND belnr = gt_bkpfm-belnr
*            AND gjahr = gt_bkpfm-gjahr.
*        ENDIF.
      ENDIF.
    ENDIF.

* Get material details for HSN, and other fields
    SELECT matnr werks steuc FROM marc INTO TABLE gt_marc
    FOR ALL ENTRIES IN gt_wb2_v_vbrk_vbrp2
    WHERE matnr = gt_wb2_v_vbrk_vbrp2-matnr_i
    AND werks = gt_wb2_v_vbrk_vbrp2-werks_i.
    IF sy-subrc IS INITIAL.
      SORT gt_marc BY matnr werks.
    ENDIF.



* Order Details
    IF gt_vbfa IS NOT INITIAL.
      SELECT  vbeln vbeln_i posnr_i audat bstnk bstdk ihrez bname
      FROM wb2_v_vbak_vbap2
      INTO TABLE gt_wb2_v_vbak_vbap2
      FOR ALL ENTRIES IN gt_vbfa
      WHERE vbeln = gt_vbfa-vbelv.
      IF sy-subrc IS INITIAL.
        SORT gt_wb2_v_vbak_vbap2 BY vbeln posnr_i.
      ENDIF.
    ENDIF.

* Dispatch and  Ship Details
    CLEAR:lt_sd_header.
    lt_sd_header =  gt_wb2_v_vbrk_vbrp2.
    SORT lt_sd_header BY vgbel_i.
    DELETE ADJACENT DUPLICATES FROM lt_sd_header COMPARING   vgbel_i.
    IF lt_sd_header IS NOT INITIAL.

      SELECT vbeln kunnr FROM likp
      INTO TABLE gt_likp
      FOR ALL ENTRIES IN lt_sd_header
      WHERE vbeln EQ  lt_sd_header-vgbel_i.

      IF gt_likp IS NOT INITIAL.


        SELECT * FROM likp INTO TABLE gt_likp_trn
        FOR ALL ENTRIES IN gt_likp
        WHERE vbeln = gt_likp-vbeln.
        IF sy-subrc IS INITIAL.
          SELECT * FROM vbpa INTO TABLE gt_vbpa_trn
          FOR ALL ENTRIES IN gt_likp
          WHERE vbeln = gt_likp-vbeln.
*                                AND parvw = 'SP'.

          IF gt_vbpa_trn IS NOT INITIAL.
            SELECT * FROM lfa1 INTO TABLE gt_lfa1_trn FOR ALL ENTRIES IN gt_vbpa_trn
            WHERE lifnr =  gt_vbpa_trn-lifnr.
          ENDIF.


          SELECT * FROM zcrac_inbound_dt INTO TABLE gt_inbound
          FOR ALL ENTRIES IN gt_likp_trn
          WHERE pono = gt_likp_trn-vbeln.


        ENDIF.



        SELECT kunnr land1 name1 name2 ort01 pstlz regio stras telf1 adrnr stcd3
        FROM kna1 INTO TABLE gt_kna1_d
        FOR ALL ENTRIES IN gt_likp
        WHERE kunnr = gt_likp-kunnr.
        IF sy-subrc EQ 0.
* Region text
          SELECT  spras land1 land1 bezei FROM t005u
          INTO TABLE gt_t005u_d
          FOR ALL ENTRIES IN gt_kna1_d
          WHERE land1 EQ gc_land1_in
          AND bland EQ gt_kna1_d-regio
          AND spras EQ sy-langu.

* Address details
          SELECT addrnumber tel_number FROM adrc
          INTO TABLE gt_adrc_d
          FOR ALL ENTRIES IN gt_kna1_d
          WHERE addrnumber = gt_kna1_d-adrnr.
        ENDIF.
      ENDIF.
    ENDIF.


*Seller Details
    CLEAR lt_sd_header.
    lt_sd_header = gt_wb2_v_vbrk_vbrp2.
    SORT lt_sd_header BY werks_i.

    DELETE ADJACENT DUPLICATES FROM lt_sd_header COMPARING werks_i.
    IF lt_sd_header IS NOT INITIAL.

      SELECT werks name1 name2 stras pstlz ort01 land1 regio
      counc cityc adrnr spras  zone1 j_1bbranch
      FROM t001w
      INTO TABLE gt_t001w
      FOR ALL ENTRIES IN lt_sd_header
      WHERE werks EQ lt_sd_header-werks_i.

      IF sy-subrc EQ 0.
        SORT gt_t001w BY werks.

* Region text
        SELECT spras land1 bland bezei FROM t005u
        INTO TABLE gt_t005u_s
        FOR ALL ENTRIES IN gt_t001w
        WHERE land1 EQ gt_t001w-land1
        AND bland EQ gt_t001w-regio
        AND spras EQ sy-langu.
* Address details
        SELECT addrnumber name1 tel_number  street str_suppl1 str_suppl2 str_suppl3 FROM adrc
        INTO CORRESPONDING FIELDS OF TABLE gt_adrc_s
        FOR ALL ENTRIES IN gt_t001w
        WHERE addrnumber = gt_t001w-adrnr.
        IF sy-subrc EQ 0.
          SELECT addrnumber smtp_addr
          FROM adr6
          INTO CORRESPONDING FIELDS OF TABLE gt_adr6_s
          FOR ALL ENTRIES IN gt_adrc_s
          WHERE addrnumber = gt_adrc_s-addrnumber.
        ENDIF.
      ENDIF.
    ENDIF.


* Export region
    IF gt_export IS NOT INITIAL.
      SELECT spras land1 bland bezei FROM t005u
      APPENDING TABLE gt_t005u
      FOR ALL ENTRIES IN gt_export
      WHERE land1 EQ gt_export-land1
      AND bland EQ gt_export-zport_state
      AND spras EQ sy-langu.
    ENDIF.

* get unit rates and discounts
    rs_name-sign = gc_sign.
    rs_name-option = gc_equal.
    rs_name-low = gc_var_unit.
    APPEND rs_name TO rt_name.
    rs_name-low = gc_var_disc.
    APPEND rs_name TO rt_name.
    SELECT  name type numb sign opti low high
    FROM tvarvc INTO TABLE gt_tvarvc
    WHERE name IN rt_name.



    SORT gt_wb2_v_vbrk_vbrp2 BY vbeln.
    lt_vbrk_vbrp = gt_wb2_v_vbrk_vbrp2.
    DELETE ADJACENT DUPLICATES FROM lt_vbrk_vbrp COMPARING vbeln.
    DATA: lv_diff TYPE kschl VALUE IS INITIAL.
    LOOP AT lt_vbrk_vbrp INTO lw_wb2_v_vbrk_vbrp2.

      CLEAR:lw_invrefnum.
      READ TABLE gt_invrefnum INTO lw_invrefnum WITH KEY bukrs = lw_wb2_v_vbrk_vbrp2-bukrs
      docno = lw_wb2_v_vbrk_vbrp2-vbeln
      doc_type = lw_wb2_v_vbrk_vbrp2-fkart.
      IF sy-subrc = 0.
        gw_final-ack_no = lw_invrefnum-ack_no.

        IF lw_invrefnum-ack_date IS NOT INITIAL.
          lv_internal_date = lw_invrefnum-ack_date.
          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
            EXPORTING
              date_internal            = lv_internal_date
            IMPORTING
              date_external            = gw_final-ack_dt
            EXCEPTIONS
              date_internal_is_invalid = 1
              OTHERS                   = 2.
          IF sy-subrc <> 0.
            MESSAGE 'Date conversion error'(m02) TYPE 'I'.
          ENDIF.
        ENDIF.

        gw_final-qr_code = lw_invrefnum-signed_qrcode.
        gw_final-irn = lw_invrefnum-irn.
        gw_final-sign_inv = lw_invrefnum-signed_inv.
        gw_final-ernam = lw_invrefnum-ernam.

        CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
          EXPORTING
            date_internal            = lw_invrefnum-erdat
          IMPORTING
            date_external            = gw_final-erdat
          EXCEPTIONS
            date_internal_is_invalid = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          MESSAGE 'Date conversion error'(m02) TYPE 'I'.
        ENDIF.

        gw_final-erzet = lw_invrefnum-erzet.

        IF lw_invrefnum-cancel_date IS NOT INITIAL.
          lv_internal_date = lw_invrefnum-cancel_date.
          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
            EXPORTING
              date_internal            = lv_internal_date
            IMPORTING
              date_external            = gw_final-canc_dt
            EXCEPTIONS
              date_internal_is_invalid = 1
              OTHERS                   = 2.
          IF sy-subrc <> 0.
            MESSAGE 'Date conversion error'(m02) TYPE 'I'.
          ENDIF.

        ENDIF.
        IF lw_invrefnum-irn_status EQ gc_irn_sts_act.
          gw_final-status = 'Success'(060).
          gw_final-icon    = gc_icon_08.
        ELSEIF lw_invrefnum-irn_status EQ gc_irn_sts_err.
          gw_final-status = 'Error'(061).
          gw_final-icon    = gc_icon_0a.
        ELSEIF lw_invrefnum-irn_status EQ gc_irn_sts_cnl.
          gw_final-status = 'Cancelled'(062).
          gw_final-icon    = gc_icon_0w.
        ENDIF.
      ENDIF.

      CLEAR ls_zteinv_details.
      READ TABLE gt_zteinv_details INTO ls_zteinv_details WITH KEY
      bukrs = lw_wb2_v_vbrk_vbrp2-bukrs
      docno = lw_wb2_v_vbrk_vbrp2-vbeln
      doctyp = lw_wb2_v_vbrk_vbrp2-fkart.
*                                                                     BINARY SEARCH.

      IF sy-subrc = 0.
        gw_final-einv_print = ls_zteinv_details-einv_pdf.
        IF gw_final-status = 'Error'(061).
          gw_final-einv_error     =  ls_zteinv_details-einv_error.
        ELSE.
          gw_final-eway_error     =  ' '.
        ENDIF.
      ENDIF.



      CLEAR:lw_ewaybill.
      READ TABLE gt_ewaybill INTO lw_ewaybill WITH KEY bukrs = lw_wb2_v_vbrk_vbrp2-bukrs
      docno = lw_wb2_v_vbrk_vbrp2-vbeln
      doctyp = lw_wb2_v_vbrk_vbrp2-fkart.
      IF sy-subrc = 0.
        gw_final-eway_num = lw_ewaybill-ebillno.

        CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
          EXPORTING
            date_internal            = lw_ewaybill-egen_dat
          IMPORTING
            date_external            = gw_final-eway_date
          EXCEPTIONS
            date_internal_is_invalid = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          MESSAGE 'Date conversion error'(m02) TYPE 'I'.
        ENDIF.

        gw_final-eway_ernam = lw_ewaybill-ernam.

        CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
          EXPORTING
            date_internal            = lw_ewaybill-erdat
          IMPORTING
            date_external            = gw_final-eway_erdat
          EXCEPTIONS
            date_internal_is_invalid = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          MESSAGE 'Date conversion error'(m02) TYPE 'I'.
        ENDIF.

        IF lw_ewaybill-status EQ  gc_eway_sts_c.
          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
            EXPORTING
              date_internal            = lw_ewaybill-aedat
            IMPORTING
              date_external            = gw_final-eway_canc_dt
            EXCEPTIONS
              date_internal_is_invalid = 1
              OTHERS                   = 2.
          IF sy-subrc <> 0.
            MESSAGE 'Date conversion error'(m02) TYPE 'I'.
          ENDIF.
        ELSE.
          gw_final-eway_canc_dt = 0.
        ENDIF.

        gw_final-eway_erzet = lw_ewaybill-egen_time.

* Valid from
        CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
          EXPORTING
            date_internal            = lw_ewaybill-vdfmdate
          IMPORTING
            date_external            = gw_final-eway_v_from
          EXCEPTIONS
            date_internal_is_invalid = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          MESSAGE 'Date conversion error'(m02) TYPE 'I'.
        ENDIF.

* Valid To
        CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
          EXPORTING
            date_internal            = lw_ewaybill-vdtodate
          IMPORTING
            date_external            = gw_final-eway_v_to
          EXCEPTIONS
            date_internal_is_invalid = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          MESSAGE 'Date conversion error'(m02) TYPE 'I'.
        ENDIF.
        gw_final-eway_v_time = lw_ewaybill-vdtotime.


        IF lw_ewaybill-status = gc_eway_sts_a.
          gw_final-eway_status = 'Success'(060).
          gw_final-eway_icon    = gc_icon_08.
        ELSEIF lw_ewaybill-status = gc_eway_sts_e.
          gw_final-eway_status = 'Error'(061).
          gw_final-eway_icon    = gc_icon_0a.
        ELSEIF lw_ewaybill-status = gc_eway_sts_c.
          gw_final-eway_status = 'Cancelled'(062).
          gw_final-eway_icon    = gc_icon_0w.
        ENDIF.
      ENDIF.

* Dont consider old cancelledrecords
      IF lw_wb2_v_vbrk_vbrp2-fksto = abap_true AND gw_final-eway_status IS INITIAL AND gw_final-status IS INITIAL .
        CONTINUE.
      ENDIF.



      READ TABLE gt_zteway_transport INTO ls_zteway_transport WITH KEY
      bukrs = lw_wb2_v_vbrk_vbrp2-bukrs
      docno = lw_wb2_v_vbrk_vbrp2-vbeln
      doctyp = lw_wb2_v_vbrk_vbrp2-fkart.
*                                                                BINARY SEARCH.
      IF sy-subrc = 0.
        gw_final-t_id       =  ls_zteway_transport-t_id.
        gw_final-t_name     =  ls_zteway_transport-t_name.
        gw_final-t_doc_no   =  ls_zteway_transport-t_doc_no.


        IF ls_zteway_transport-t_date IS NOT INITIAL.
          CONCATENATE ls_zteway_transport-t_date+6(2) '/'
          ls_zteway_transport-t_date+4(2) '/'
          ls_zteway_transport-t_date+0(4)
          INTO gw_final-t_date.
        ENDIF.

        gw_final-t_mode     =  ls_zteway_transport-t_mode.
        gw_final-t_distance =  ls_zteway_transport-t_distance.
        gw_final-v_number   =  ls_zteway_transport-v_number.
        gw_final-v_type     =  ls_zteway_transport-v_type.
        IF gw_final-v_type  IS INITIAL.
          gw_final-v_type     = gc_v_type_1..
        ENDIF.
        gw_final-eway_print     =  ls_zteway_transport-eway_print.
        IF gw_final-eway_status = 'Error'(061).
          gw_final-eway_error     =  ls_zteway_transport-eway_error.
        ELSE.
          gw_final-eway_error     =  ' '.
        ENDIF.
        gw_final-t_r_distance           = ls_zteway_transport-t_r_distance.
        gw_final-t_ext_valid_reason     = ls_zteway_transport-t_ext_valid_reason.
        gw_final-t_ext_valid_remarks    = ls_zteway_transport-t_ext_valid_remarks.
        gw_final-t_from_pin             = ls_zteway_transport-t_from_pin.
        gw_final-t_consignment_status   = ls_zteway_transport-t_consignment_status.
        gw_final-t_transit_type         = ls_zteway_transport-t_transit_type.
        gw_final-t_address1             = ls_zteway_transport-t_address1.
        gw_final-t_address2             = ls_zteway_transport-t_address2.
        gw_final-t_address3             = ls_zteway_transport-t_address3.

        gw_final-v_reason_code        = ls_zteway_transport-v_reason_code.
        gw_final-v_reason             = ls_zteway_transport-v_reason.
        gw_final-c_reason_code        = ls_zteway_transport-c_reason_code.
        gw_final-c_reason             = ls_zteway_transport-c_reason.
        gw_final-zport_code             = ls_zteway_transport-zport_code.
        gw_final-consignor_place      = ls_zteway_transport-consignor_place.
        gw_final-consignor_state      = ls_zteway_transport-consignor_state.
      ELSE.
* Customer specific logic

        IF lw_wb2_v_vbrk_vbrp2-fkdat IS NOT INITIAL.
          CONCATENATE lw_wb2_v_vbrk_vbrp2-fkdat+6(2) '/'
          lw_wb2_v_vbrk_vbrp2-fkdat+4(2) '/'
          lw_wb2_v_vbrk_vbrp2-fkdat+0(4)
          INTO gw_final-t_date.
        ENDIF.

        """"ADDED BY MANISHA 10/04/2024
*        IF lw_wb2_v_vbrk_vbrp2-fkart = 'ZSN'.
*          CLEAR lv_tdname.
*          lv_tdname = lw_wb2_v_vbrk_vbrp2-vbeln.
*          PERFORM readtext_zsn USING lv_tdname.
*          gw_final-v_type     = gc_v_type_1.
*          gw_final-t_mode     = '1'.
*        ELSEIF lw_wb2_v_vbrk_vbrp2-fkart = 'ZEXD'  OR lw_wb2_v_vbrk_vbrp2-fkart = 'ZEXL' OR lw_wb2_v_vbrk_vbrp2-fkart = 'ZEXR' OR
*        lw_wb2_v_vbrk_vbrp2-fkart = 'ZEXS'.
*          CLEAR lv_tdname.
*          lv_tdname = lw_wb2_v_vbrk_vbrp2-vbeln.
*          PERFORM readtext_exp USING lv_tdname.
*          gw_final-v_type     = gc_v_type_1.
*          gw_final-t_mode     = '1'.
*        ENDIF.
        """ENDED BY MANISHA

        IF lw_wb2_v_vbrk_vbrp2-fkart = 'ZTRD'.
          CLEAR lv_tdname.
          lv_tdname = lw_wb2_v_vbrk_vbrp2-vbeln.

          PERFORM readtext1 USING lv_tdname .
          gw_final-v_type     = gc_v_type_1.
          gw_final-t_mode     = '1'.

        ELSE.


          READ TABLE gt_likp_trn INTO DATA(ls_likp_trn) WITH KEY vbeln =    lw_wb2_v_vbrk_vbrp2-vgbel_i.
          IF sy-subrc IS INITIAL.
            gw_final-t_distance = '0'.
            gw_final-t_doc_no   = ls_likp_trn-bolnr.
*            IF ls_likp_trn-lfdat IS NOT INITIAL.
*              CONCATENATE ls_likp_trn-lfdat+6(2) '/'
*                          ls_likp_trn-lfdat+4(2) '/'
*                          ls_likp_trn-lfdat+0(4)
*                          INTO gw_final-t_date.
*            ENDIF.

            gw_final-t_mode     = '1'.
            CASE ls_likp_trn-vsart.
              WHEN '01' OR '02'.
                gw_final-t_mode     = '1'.
              WHEN '03'.
                gw_final-t_mode     = '2'.
              WHEN '04'.
                gw_final-t_mode     = '4'.
            ENDCASE.



            READ TABLE gt_vbpa_trn INTO DATA(ls_vbpa_trn) WITH KEY vbeln = ls_likp_trn-vbeln
                  parvw = lv_parvw.
            IF sy-subrc IS INITIAL.
              READ TABLE gt_lfa1_trn INTO DATA(ls_lfa1_trn) WITH KEY
                    lifnr = ls_vbpa_trn-lifnr.
              IF sy-subrc IS INITIAL.

                gw_final-t_id       =  ls_lfa1_trn-stcd3.
                gw_final-t_name     =  ls_lfa1_trn-name1.
              ENDIF.
            ENDIF.

*            IF lw_wb2_v_vbrk_vbrp2-fkart = 'ZDOM'.
*              IF lw_wb2_v_vbrk_vbrp2-matnr_i = 'PIG IRON(MOLTEN)' OR
*              lw_wb2_v_vbrk_vbrp2-matnr_i = 'PIG IRON(STEELGRA'.
*                gw_final-t_id = '27AAACU9269Q1ZC'.
*                gw_final-t_name = 'EVONITH METALLICS LIMITED'.
*                gw_final-v_number   = ls_likp_trn-traid..
*                gw_final-v_type     = gc_v_type_1.
*              ENDIF.
*            ENDIF.

            IF  gw_final-v_number IS INITIAL.
              READ TABLE gt_inbound INTO DATA(lS_inbound) WITH KEY pono = ls_likp_trn-vbeln.
              IF sy-subrc IS INITIAL.
                gw_final-v_number   = ls_inbound-truckno.
                gw_final-v_type     = gc_v_type_1.
              ENDIF.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.
      IF gw_final-t_id IS INITIAL.
        READ TABLE gt_vbpa INTO DATA(ls_vbpa) WITH KEY vbeln = lw_wb2_v_vbrk_vbrp2-vbeln parvw = 'ZT'.
        IF sy-subrc IS INITIAL.
          READ TABLE gt_lfa1_trans INTO DATA(ls_lfa1) WITH KEY lifnr = ls_vbpa-lifnr.
          IF sy-subrc IS INITIAL.
            gw_final-t_name = ls_lfa1-name1.
            gw_final-t_id = ls_lfa1-stcd3.
          ENDIF.
        ENDIF.
      ENDIF.

      IF gw_final-t_id IS INITIAL.
        READ TABLE gt_transport INTO DATA(gw_transport) WITH KEY kunnr = lw_wb2_v_vbrk_vbrp2-kunag.
        IF sy-subrc IS INITIAL.
          gw_final-t_name = gw_transport-trpname.
          gw_final-t_id = gw_transport-gstno.
        ENDIF.
      ENDIF.
      IF lw_wb2_v_vbrk_vbrp2-fkart = 'Z003' OR  lw_wb2_v_vbrk_vbrp2-fkart = 'Z004'.
        IF gw_final-zport_code IS INITIAL.
          READ TABLE gt_vbpa INTO ls_vbpa WITH KEY vbeln = lw_wb2_v_vbrk_vbrp2-vbeln parvw = 'ZL'.
          IF sy-subrc IS INITIAL.
            READ TABLE gt_KNA1 INTO DATA(ls_kna1) WITH KEY kunnr = ls_vbpa-kunnr.
            IF sy-subrc IS INITIAL.
              gw_final-zport_code = ls_kna1-pstlz.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.


      gw_final-vbeln      =        lw_wb2_v_vbrk_vbrp2-vbeln.
      gw_final-gjahr      =        lw_wb2_v_vbrk_vbrp2-gjahr.
      gw_final-fkart      =        lw_wb2_v_vbrk_vbrp2-fkart.
      gw_final-fktyp      =        lw_wb2_v_vbrk_vbrp2-fktyp.
      gw_final-vbtyp      =        lw_wb2_v_vbrk_vbrp2-vbtyp.
      gw_final-vkorg      =        lw_wb2_v_vbrk_vbrp2-vkorg.
      gw_final-vtweg     =        lw_wb2_v_vbrk_vbrp2-vtweg.
      IF lw_wb2_v_vbrk_vbrp2-fkdat IS NOT INITIAL.
        CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
          EXPORTING
            date_internal            = lw_wb2_v_vbrk_vbrp2-fkdat
          IMPORTING
            date_external            = gw_final-fkdat
          EXCEPTIONS
            date_internal_is_invalid = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          MESSAGE 'Date conversion error'(m02) TYPE 'I'.
        ENDIF.
        gw_final-fkdat_db = lw_wb2_v_vbrk_vbrp2-fkdat.
      ENDIF.

      gw_final-kunrg     =        lw_wb2_v_vbrk_vbrp2-kunrg.
      gw_final-werks     =        lw_wb2_v_vbrk_vbrp2-werks_i.
      gw_final-prctr     =        lw_wb2_v_vbrk_vbrp2-prctr_i.
      gw_final-doc_ernam =        lw_wb2_v_vbrk_vbrp2-ernam.
      gw_final-bupla     =        lw_wb2_v_vbrk_vbrp2-bupla.
      gw_final-odnno     =        lw_wb2_v_vbrk_vbrp2-xblnr.
      gw_final-bukrs     =        lw_wb2_v_vbrk_vbrp2-bukrs.

* Get Fiscl year
      CALL FUNCTION 'GM_GET_FISCAL_YEAR'
        EXPORTING
          i_date                     = lw_wb2_v_vbrk_vbrp2-fkdat
          i_fyv                      = gc_periv_v3
        IMPORTING
          e_fy                       = lv_gjahr
        EXCEPTIONS
          fiscal_year_does_not_exist = 1
          not_defined_for_date       = 2
          OTHERS                     = 3.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ELSE.
        gw_final-gjahr = lv_gjahr.
      ENDIF.


* Ship to details
      CLEAR:lw_vbpa.
      READ TABLE gt_vbpa INTO lw_vbpa WITH  KEY vbeln = lw_wb2_v_vbrk_vbrp2-vbeln
      parvw = gc_parvw_we
      BINARY SEARCH.
      IF sy-subrc EQ 0.
        CLEAR:lw_kna1_d.
        READ TABLE gt_kna1 INTO lw_kna1_d WITH  KEY kunnr = lw_vbpa-kunnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          gw_final-ship_to = lw_kna1_d-kunnr.
          gw_final-ship_to_name = lw_kna1_d-name1.
        ENDIF.
      ENDIF.


      READ TABLE gt_gstin INTO lw_gstin WITH KEY bukrs = gw_final-bukrs
      branch = gw_final-bupla.
      IF sy-subrc IS INITIAL.
        gw_final-sup_gstin = lw_gstin-gstin.
      ENDIF.

      READ TABLE gt_kna1 INTO lw_kna1 WITH KEY kunnr = gw_final-kunrg BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        gw_final-kunnr     =        lw_kna1-kunnr.
        gw_final-name1     =        lw_kna1-name1.
        gw_final-ship_gstin = lw_kna1-stcd3.
      ENDIF.

      READ TABLE gt_doctyp INTO lw_doctyp WITH KEY bukrs = p_ccode
      zmodule = gc_sd
      fkart   = gw_final-fkart.
      IF sy-subrc IS INITIAL.
        gw_final-einv = lw_doctyp-einv.
        gw_final-eway = lw_doctyp-eway.
      ENDIF.

      gw_final-mod = gc_sd.

      READ TABLE gt_wb2_v_vbrk_vbrp2 INTO lw_vbrk_vbrp2 WITH KEY vbeln = lw_wb2_v_vbrk_vbrp2-vbeln.
      IF sy-subrc = 0.
        lv_index = sy-tabix.
        LOOP AT gt_wb2_v_vbrk_vbrp2 INTO lw_vbrk_vbrp2 FROM lv_index.
          IF lw_vbrk_vbrp2-vbeln NE  lw_wb2_v_vbrk_vbrp2-vbeln.
            EXIT.
          ENDIF.


* check for export currency
          CLEAR lv_exp_curr.
          IF lw_vbrk_vbrp2-waerk NE  gc_curr_inr .
            lv_exp_curr = abap_true.
          ENDIF.

          gw_final-taxamt    =  gw_final-taxamt + lw_vbrk_vbrp2-netwr_i.
          gw_final-tax       =  gw_final-tax    + lw_vbrk_vbrp2-mwsbp_i.
          lv_diff = 'DIFF'.
          READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_vbrk_vbrp2-knumv
          kposn = lw_vbrk_vbrp2-posnr_i
          kschl = lv_diff
          BINARY SEARCH.
          IF sy-subrc EQ 0.
            gw_final-taxamt = gw_final-taxamt - lw_konv-kwert.
          ENDIF.
          IF lw_wb2_v_vbrk_vbrp2-fkart NE 'ZSTI'.
            "IGST Tax amount
            READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_vbrk_vbrp2-knumv
            kposn = lw_vbrk_vbrp2-posnr_i
            kschl = gc_kschl_joig
            BINARY SEARCH.
            IF sy-subrc IS NOT INITIAL.
              READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_vbrk_vbrp2-knumv
              kposn = lw_vbrk_vbrp2-posnr_i
              kschl = 'ZIGS'.
            ENDIF.
            IF sy-subrc EQ 0.
              gw_final-igst_amt  =  gw_final-igst_amt + lw_konv-kwert.
            ENDIF.
            "CGST Tax amount
            READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_vbrk_vbrp2-knumv
            kposn = lw_vbrk_vbrp2-posnr_i
            kschl = gc_kschl_jocg
            BINARY SEARCH.
            IF sy-subrc IS NOT INITIAL.
              READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_vbrk_vbrp2-knumv
              kposn = lw_vbrk_vbrp2-posnr_i
              kschl = 'ZCGS'.
            ENDIF   .
            IF sy-subrc EQ 0.
              gw_final-cgst_amt =  gw_final-cgst_amt + lw_konv-kwert.
            ENDIF.
            "SGST Tax amount
            READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_vbrk_vbrp2-knumv
            kposn = lw_vbrk_vbrp2-posnr_i
            kschl = gc_kschl_josg
            BINARY SEARCH.
            IF sy-subrc IS NOT INITIAL.
              READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_vbrk_vbrp2-knumv
              kposn = lw_vbrk_vbrp2-posnr_i
              kschl = 'ZSGS'.
            ENDIF.
            IF sy-subrc EQ 0.
              gw_final-sgst_amt =  gw_final-sgst_amt + lw_konv-kwert.
            ENDIF.
            "CESS Tax amount
            READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_vbrk_vbrp2-knumv
            kposn = lw_vbrk_vbrp2-posnr_i
            kschl = gc_kschl_cess
            BINARY SEARCH.
            IF sy-subrc EQ 0.
              gw_final-cess_amt =  gw_final-cess_amt + lw_konv-kwert.
            ENDIF.

          ENDIF.  " Not considering tax for ZSTI

        ENDLOOP.

        gw_final-invval =  gw_final-taxamt + gw_final-tax .

      ENDIF.


      IF lv_exp_curr IS NOT INITIAL.
        PERFORM convert_currency USING gw_final-invval lw_vbrk_vbrp2-kursk_i
        CHANGING gw_final-invval.
        PERFORM convert_currency USING gw_final-taxamt lw_vbrk_vbrp2-kursk_i
        CHANGING gw_final-taxamt.
        PERFORM convert_currency USING gw_final-igst_amt lw_vbrk_vbrp2-kursk_i
        CHANGING gw_final-igst_amt.
        PERFORM convert_currency USING gw_final-cgst_amt lw_vbrk_vbrp2-kursk_i
        CHANGING gw_final-cgst_amt.
        PERFORM convert_currency USING gw_final-sgst_amt lw_vbrk_vbrp2-kursk_i
        CHANGING gw_final-sgst_amt.
        PERFORM convert_currency USING gw_final-cess_amt lw_vbrk_vbrp2-kursk_i
        CHANGING gw_final-cess_amt.
      ENDIF.

      APPEND gw_final TO gt_final.
      CLEAR : gw_final.
    ENDLOOP.

  ENDIF.



ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PROCESS_SD_DATA_DELIVERY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_sd_data_delivery .


  DATA: lw_invrefnum        TYPE j_1ig_invrefnum,
        lw_wb2_v_likp_lips2 TYPE ty_wb2_v_likp_lips2,
        lw_kna1             LIKE LINE OF gt_kna1,
        lw_gstin            LIKE LINE OF gt_gstin.
  DATA: lv_internal_date TYPE sy-datum,
        lw_konv          TYPE ty_konv,
        lv_gjahr(4)      TYPE c,
        lw_likp          TYPE ty_likp,
        lw_lfa1          TYPE lfa1,
        lw_doctyp        TYPE ty_doctyp.
  DATA: lw_ewaybill         TYPE ty_ewaybill.
  DATA: ls_zteway_transport TYPE zteway_transport.
  DATA: ls_zteinv_details   TYPE zteinv_details.
  DATA:lt_likp_lips2      TYPE TABLE OF ty_wb2_v_likp_lips2,
       lw_likp_lips2      TYPE  ty_wb2_v_likp_lips2,
       lt_delivery_header TYPE TABLE OF ty_wb2_v_likp_lips2,
       lv_index           TYPE sy-tabix.


*& Get deliveries for E-Way Bill
  IF s_prctr IS NOT INITIAL.
    SELECT vbeln vbeln_i posnr_i ernam vkorg lfart lfdat vbtyp kunnr kunag
    fkdat knumv waerk lifnr bldat netwr werks xblnr  matnr_i matkl_i
    werks_i lgort_i charg_i lfimg_i meins_i vrkme_i ntgew_i brgew_i
    arktx_i vbelv_i posnv_i vgbel_i vgpos_i gsber_i  prctr_i
    netpr_i netwr_i lfgja_i
    FROM wb2_v_likp_lips2
    INTO TABLE gt_wb2_v_likp_lips2
    FOR ALL ENTRIES IN gt_user_config
    WHERE lfart = gt_user_config-fkart
    AND vbeln IN s_doc
    AND fkdat IN s_date
    AND werks_i = gt_user_config-werks
    AND prctr_i IN s_prctr.

  ELSE.
    SELECT vbeln vbeln_i posnr_i ernam vkorg lfart lfdat vbtyp kunnr kunag
    fkdat knumv waerk lifnr bldat netwr werks xblnr  matnr_i matkl_i
    werks_i lgort_i charg_i lfimg_i meins_i vrkme_i ntgew_i brgew_i
    arktx_i vbelv_i posnv_i vgbel_i vgpos_i gsber_i  prctr_i
    netpr_i netwr_i lfgja_i
    FROM wb2_v_likp_lips
    INTO TABLE gt_wb2_v_likp_lips2
    FOR ALL ENTRIES IN gt_user_config
    WHERE lfart = gt_user_config-fkart
    AND vbeln IN s_doc
    AND fkdat IN s_date
    AND werks_i = gt_user_config-werks.
  ENDIF.



  IF gt_wb2_v_likp_lips2 IS  NOT INITIAL.


    lt_delivery_header = gt_wb2_v_likp_lips2.
    SORT lt_delivery_header BY  lfart vbeln.
    DELETE ADJACENT DUPLICATES FROM lt_delivery_header COMPARING lfart vbeln.
    IF lt_delivery_header IS NOT INITIAL .

* Eway bill
      SELECT bukrs doctyp docno gjahr ebillno egen_dat egen_time vdfmdate vdtodate
      vdtotime status ernam erdat aenam aedat
      FROM j_1ig_ewaybill APPENDING TABLE gt_ewaybill
      FOR ALL ENTRIES IN lt_delivery_header
      WHERE bukrs = p_ccode
      AND doctyp = lt_delivery_header-lfart
      AND docno  = lt_delivery_header-vbeln.
      IF sy-subrc IS INITIAL.
        SORT gt_ewaybill DESCENDING BY egen_dat egen_time.
      ENDIF.

* Get Transporter details from Z-Table
      SELECT * FROM zteway_transport INTO TABLE gt_zteway_transport
      FOR ALL ENTRIES IN lt_delivery_header
      WHERE bukrs =  p_ccode
      AND doctyp = lt_delivery_header-lfart
      AND docno  = lt_delivery_header-vbeln.
      IF sy-subrc IS INITIAL.
        SORT gt_zteway_transport BY bukrs doctyp docno.
      ENDIF.
      SELECT * FROM ztransport INTO TABLE @DATA(gt_transport) FOR ALL ENTRIES IN @lt_delivery_header
                                                              WHERE kunnr = @lt_delivery_header-kunnr.
      IF sy-subrc IS INITIAL.
        SORT gt_transport BY kunnr.
      ENDIF.
* Select partners
      SELECT vbeln posnr parvw kunnr lifnr
      adrnr	 FROM vbpa APPENDING TABLE gt_vbpa FOR ALL ENTRIES IN lt_delivery_header
      WHERE vbeln = lt_delivery_header-vbeln.

*Buyer Details
      IF sy-subrc IS INITIAL.
        SORT gt_vbpa BY vbeln parvw.
        SELECT kunnr land1 name1 name2 ort01 pstlz regio
        stras telf1 adrnr stcd3
        FROM kna1
        APPENDING TABLE gt_kna1
        FOR ALL ENTRIES IN gt_vbpa
        WHERE kunnr = gt_vbpa-kunnr.
        IF sy-subrc EQ 0.
          SORT gt_kna1 BY kunnr.

          SELECT spras land1 bland bezei  FROM t005u
          APPENDING TABLE gt_t005u
          FOR ALL ENTRIES IN gt_kna1
          WHERE land1 EQ gt_kna1-land1
          AND bland EQ gt_kna1-regio
          AND spras EQ sy-langu.

          SELECT addrnumber tel_number FROM adrc
          APPENDING TABLE gt_adrc
          FOR ALL ENTRIES IN gt_kna1
          WHERE addrnumber = gt_kna1-adrnr.

          IF sy-subrc IS INITIAL.
            SELECT addrnumber smtp_addr   FROM adr6
            APPENDING  TABLE gt_adr6
            FOR ALL ENTRIES IN gt_adrc
            WHERE addrnumber = gt_adrc-addrnumber.
          ENDIF.
        ENDIF.
      ENDIF.

      SELECT * FROM j_1ig_invrefnum
      APPENDING TABLE gt_invrefnum
      FOR ALL ENTRIES IN lt_delivery_header
      WHERE bukrs EQ p_ccode
      AND docno EQ lt_delivery_header-vbeln
      AND doc_type EQ lt_delivery_header-lfart.
      IF sy-subrc EQ 0.
        SORT  gt_invrefnum BY bukrs docno doc_year doc_type.
      ENDIF.
    ENDIF.


* Get material details for HSN, and other fields

    SELECT matnr werks steuc FROM marc APPENDING TABLE gt_marc
    FOR ALL ENTRIES IN gt_wb2_v_likp_lips2
    WHERE matnr = gt_wb2_v_likp_lips2-matnr_i
    AND werks = gt_wb2_v_likp_lips2-werks_i.
    IF sy-subrc IS INITIAL.
      SORT gt_marc BY matnr werks.
    ENDIF.


* GSTIN deatils
    SELECT bukrs branch name adrnr gstin
    FROM j_1bbranch
    APPENDING TABLE gt_gstin
    WHERE bukrs = p_ccode.


    CLEAR:lt_delivery_header.
    lt_delivery_header = gt_wb2_v_likp_lips2.
    SORT lt_delivery_header BY kunnr.
    DELETE ADJACENT DUPLICATES FROM lt_delivery_header COMPARING kunnr.

    IF lt_delivery_header IS NOT INITIAL.
      SELECT kunnr land1 name1 name2 ort01 pstlz regio
      stras telf1 adrnr stcd3
      FROM kna1
      APPENDING TABLE gt_kna1_d
      FOR ALL ENTRIES IN gt_wb2_v_likp_lips2
      WHERE kunnr = gt_wb2_v_likp_lips2-kunnr.
      IF sy-subrc EQ 0.

        SELECT   spras
        land1
        bland
        bezei	 FROM t005u
        APPENDING TABLE gt_t005u_d
        FOR ALL ENTRIES IN gt_kna1_d
        WHERE land1 EQ gc_land1_in
        AND bland EQ gt_kna1_d-regio
        AND spras EQ sy-langu.

        SELECT addrnumber tel_number  FROM adrc
        APPENDING TABLE gt_adrc_d
        FOR ALL ENTRIES IN gt_kna1_d
        WHERE addrnumber = gt_kna1_d-adrnr.

      ENDIF.
    ENDIF.

*Seller Details

    CLEAR:lt_delivery_header.
    lt_delivery_header = gt_wb2_v_likp_lips2.
    SORT lt_delivery_header BY werks_i.
    DELETE ADJACENT DUPLICATES FROM lt_delivery_header COMPARING werks_i.

    IF lt_delivery_header IS NOT INITIAL.
      SELECT werks name1 name2 stras pstlz ort01 land1
      regio counc cityc adrnr spras zone1
      j_1bbranch FROM t001w
      APPENDING TABLE gt_t001w
      FOR ALL ENTRIES IN gt_wb2_v_likp_lips2
      WHERE werks EQ gt_wb2_v_likp_lips2-werks_i.
      IF sy-subrc EQ 0.
        SORT gt_t001w BY werks.
        SELECT spras land1 bland bezei FROM t005u
        APPENDING TABLE gt_t005u_s
        FOR ALL ENTRIES IN gt_t001w
        WHERE land1 EQ gt_t001w-land1
        AND bland EQ gt_t001w-regio
        AND spras EQ sy-langu.

        SELECT addrnumber name1 tel_number street str_suppl1 str_suppl2 str_suppl3  FROM adrc
        APPENDING CORRESPONDING FIELDS OF TABLE gt_adrc_s
        FOR ALL ENTRIES IN gt_t001w
        WHERE addrnumber = gt_t001w-adrnr.
        IF sy-subrc EQ 0.
          SELECT addrnumber smtp_addr   FROM adr6
          APPENDING TABLE  gt_adr6_s
          FOR ALL ENTRIES IN gt_adrc_s
          WHERE addrnumber = gt_adrc_s-addrnumber.
        ENDIF.
      ENDIF.
    ENDIF.




    SORT gt_wb2_v_likp_lips2 BY vbeln.
    lt_likp_lips2 = gt_wb2_v_likp_lips2.
    DELETE ADJACENT DUPLICATES FROM lt_likp_lips2 COMPARING vbeln.

    LOOP AT gt_wb2_v_likp_lips2 INTO lw_wb2_v_likp_lips2.

      CLEAR:lw_invrefnum.
      READ TABLE gt_invrefnum INTO lw_invrefnum WITH KEY bukrs = p_ccode
      docno = lw_wb2_v_likp_lips2-vbeln
      doc_type = lw_wb2_v_likp_lips2-lfart.
      IF sy-subrc = 0.
        gw_final-ack_no = lw_invrefnum-ack_no.

        IF lw_invrefnum-ack_date IS NOT INITIAL.
          lv_internal_date = lw_invrefnum-ack_date.
          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
            EXPORTING
              date_internal            = lv_internal_date
            IMPORTING
              date_external            = gw_final-ack_dt
            EXCEPTIONS
              date_internal_is_invalid = 1
              OTHERS                   = 2.
          IF sy-subrc <> 0.
            MESSAGE 'Date conversion error'(m02) TYPE 'I'.
          ENDIF.
        ENDIF.

        gw_final-qr_code = lw_invrefnum-signed_qrcode.
        gw_final-irn = lw_invrefnum-irn.
        gw_final-sign_inv = lw_invrefnum-signed_inv.
        gw_final-ernam = lw_invrefnum-ernam.

        CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
          EXPORTING
            date_internal            = lw_invrefnum-erdat
          IMPORTING
            date_external            = gw_final-erdat
          EXCEPTIONS
            date_internal_is_invalid = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          MESSAGE 'Date conversion error'(m02) TYPE 'I'.
        ENDIF.

        gw_final-erzet = lw_invrefnum-erzet.

        IF lw_invrefnum-cancel_date IS NOT INITIAL.
          lv_internal_date = lw_invrefnum-cancel_date.
          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
            EXPORTING
              date_internal            = lv_internal_date
            IMPORTING
              date_external            = gw_final-canc_dt
            EXCEPTIONS
              date_internal_is_invalid = 1
              OTHERS                   = 2.
          IF sy-subrc <> 0.
            MESSAGE 'Date conversion error'(m02) TYPE 'I'.
          ENDIF.

        ENDIF.
        IF lw_invrefnum-irn_status EQ gc_irn_sts_act.
          gw_final-status = 'Success'(060).
          gw_final-icon    = gc_icon_08.
        ELSEIF lw_invrefnum-irn_status EQ gc_irn_sts_err.
          gw_final-status = 'Error'(061).
          gw_final-icon    = gc_icon_0a.
        ELSEIF lw_invrefnum-irn_status EQ gc_irn_sts_cnl.
          gw_final-status = 'Cancelled'(062).
          gw_final-icon   = gc_icon_0w.
        ENDIF.
      ENDIF.


      CLEAR  ls_zteinv_details.
      READ TABLE gt_zteinv_details INTO ls_zteinv_details WITH KEY
      bukrs = p_ccode
      docno = lw_wb2_v_likp_lips2-vbeln
      doctyp = lw_wb2_v_likp_lips2-lfart.
      IF sy-subrc = 0.
        IF gw_final-status = 'Error'(061).
          gw_final-einv_error     =  ls_zteinv_details-einv_error.
        ELSE.
          gw_final-einv_error     =  ' '.
        ENDIF.
      ENDIF.



      CLEAR:lw_ewaybill.
      READ TABLE gt_ewaybill INTO lw_ewaybill WITH KEY bukrs = p_ccode
      docno = lw_wb2_v_likp_lips2-vbeln
      doctyp = lw_wb2_v_likp_lips2-lfart.
      IF sy-subrc = 0.
        gw_final-eway_num = lw_ewaybill-ebillno.

        CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
          EXPORTING
            date_internal            = lw_ewaybill-egen_dat
          IMPORTING
            date_external            = gw_final-eway_date
          EXCEPTIONS
            date_internal_is_invalid = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          MESSAGE 'Date conversion error'(m02) TYPE 'I'.
        ENDIF.

        gw_final-eway_ernam = lw_ewaybill-ernam.

        CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
          EXPORTING
            date_internal            = lw_ewaybill-erdat
          IMPORTING
            date_external            = gw_final-eway_erdat
          EXCEPTIONS
            date_internal_is_invalid = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          MESSAGE 'Date conversion error'(m02) TYPE 'I'.
        ENDIF.

        IF lw_ewaybill-status EQ  gc_eway_sts_c.
          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
            EXPORTING
              date_internal            = lw_ewaybill-aedat
            IMPORTING
              date_external            = gw_final-eway_canc_dt
            EXCEPTIONS
              date_internal_is_invalid = 1
              OTHERS                   = 2.
          IF sy-subrc <> 0.
            MESSAGE 'Date conversion error'(m02) TYPE 'I'.
          ENDIF.
        ELSE.
          gw_final-eway_canc_dt = 0.
        ENDIF.

        gw_final-eway_erzet = lw_ewaybill-egen_time.

* Valid from
        CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
          EXPORTING
            date_internal            = lw_ewaybill-vdfmdate
          IMPORTING
            date_external            = gw_final-eway_v_from
          EXCEPTIONS
            date_internal_is_invalid = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          MESSAGE 'Date conversion error'(m02) TYPE 'I'.
        ENDIF.

* Valid To
        CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
          EXPORTING
            date_internal            = lw_ewaybill-vdtodate
          IMPORTING
            date_external            = gw_final-eway_v_to
          EXCEPTIONS
            date_internal_is_invalid = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          MESSAGE 'Date conversion error'(m02) TYPE 'I'.
        ENDIF.
        gw_final-eway_v_time = lw_ewaybill-vdtotime.


        IF lw_ewaybill-status = gc_eway_sts_a.
          gw_final-eway_status = 'Success'(060).
          gw_final-eway_icon    = gc_icon_08.
        ELSEIF lw_ewaybill-status = gc_eway_sts_e.
          gw_final-eway_status = 'Error'(061).
          gw_final-eway_icon    = gc_icon_0a.
        ELSEIF lw_ewaybill-status = gc_eway_sts_c.
          gw_final-eway_status = 'Cancelled'(062).
          gw_final-eway_icon    = gc_icon_0w.
        ENDIF.
      ENDIF.
      READ TABLE gt_zteway_transport INTO ls_zteway_transport WITH KEY
      bukrs = p_ccode
      docno = lw_wb2_v_likp_lips2-vbeln
      doctyp = lw_wb2_v_likp_lips2-lfart
      BINARY SEARCH.
      IF sy-subrc = 0.
        gw_final-t_id       =  ls_zteway_transport-t_id.
        gw_final-t_name     =  ls_zteway_transport-t_name.
        gw_final-t_doc_no   =  ls_zteway_transport-t_doc_no.

        IF ls_zteway_transport-t_date IS NOT INITIAL.
          CONCATENATE ls_zteway_transport-t_date+6(2) '/'
          ls_zteway_transport-t_date+4(2) '/'
          ls_zteway_transport-t_date+0(4)
          INTO gw_final-t_date.
        ENDIF.

        gw_final-t_mode     =  ls_zteway_transport-t_mode.
        gw_final-t_distance =  ls_zteway_transport-t_distance.
        gw_final-v_number   =  ls_zteway_transport-v_number.
        gw_final-v_type     =  ls_zteway_transport-v_type.

        gw_final-t_r_distance           = ls_zteway_transport-t_r_distance.
        gw_final-t_ext_valid_reason     = ls_zteway_transport-t_ext_valid_reason.
        gw_final-t_ext_valid_remarks    = ls_zteway_transport-t_ext_valid_remarks.
        gw_final-t_from_pin             = ls_zteway_transport-t_from_pin.
        gw_final-t_consignment_status   = ls_zteway_transport-t_consignment_status.
        gw_final-t_transit_type         = ls_zteway_transport-t_transit_type.
        gw_final-t_address1             = ls_zteway_transport-t_address1.
        gw_final-t_address2             = ls_zteway_transport-t_address2.
        gw_final-t_address3             = ls_zteway_transport-t_address3.

        gw_final-v_reason_code        = ls_zteway_transport-v_reason_code.
        gw_final-v_reason             = ls_zteway_transport-v_reason.
        gw_final-c_reason_code        = ls_zteway_transport-c_reason_code.
        gw_final-c_reason             = ls_zteway_transport-c_reason.

        gw_final-zport_code               = ls_zteway_transport-zport_code.

      ELSE.

* Customer specific logic

      ENDIF.

      IF gw_final-t_id IS INITIAL.
        READ TABLE gt_transport INTO DATA(gw_transport) WITH KEY kunnr = lw_wb2_v_likp_lips2-kunnr.
        IF sy-subrc IS INITIAL.
          gw_final-t_id       =  gw_transport-gstno.
          gw_final-t_name     =  gw_transport-trpname.
        ENDIF.
      ENDIF.

      gw_final-vbeln      =        lw_wb2_v_likp_lips2-vbeln.
      gw_final-fkart      =        lw_wb2_v_likp_lips2-lfart.
      gw_final-vbtyp      =        lw_wb2_v_likp_lips2-vbtyp.
      gw_final-vkorg      =        lw_wb2_v_likp_lips2-vkorg.

      IF lw_wb2_v_likp_lips2-fkdat IS NOT INITIAL.
        CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
          EXPORTING
            date_internal            = lw_wb2_v_likp_lips2-fkdat
          IMPORTING
            date_external            = gw_final-fkdat
          EXCEPTIONS
            date_internal_is_invalid = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          MESSAGE 'Date conversion error'(m02) TYPE 'I'.
        ENDIF.
        gw_final-fkdat_db = lw_wb2_v_likp_lips2-fkdat.
      ENDIF.

      gw_final-kunrg     =        lw_wb2_v_likp_lips2-kunnr.
      gw_final-werks     =        lw_wb2_v_likp_lips2-werks_i.
      gw_final-prctr     =        lw_wb2_v_likp_lips2-prctr_i.
      gw_final-doc_ernam =        lw_wb2_v_likp_lips2-ernam.
      gw_final-odnno     =        lw_wb2_v_likp_lips2-xblnr.
      gw_final-bukrs     =        p_ccode.

* Get Fiscal year
      CALL FUNCTION 'GM_GET_FISCAL_YEAR'
        EXPORTING
          i_date                     = lw_wb2_v_likp_lips2-fkdat
          i_fyv                      = gc_periv_v3
        IMPORTING
          e_fy                       = lv_gjahr
        EXCEPTIONS
          fiscal_year_does_not_exist = 1
          not_defined_for_date       = 2
          OTHERS                     = 3.
      IF sy-subrc <> 0.
        MESSAGE 'Fiscal Year not found'(m03) TYPE 'I'.
      ELSE.
        gw_final-gjahr = lv_gjahr.
      ENDIF.



      READ TABLE gt_gstin INTO lw_gstin WITH KEY bukrs = gw_final-bukrs.

      IF sy-subrc IS INITIAL.
        gw_final-sup_gstin = lw_gstin-gstin.
      ENDIF.



      READ TABLE gt_kna1 INTO lw_kna1 WITH KEY kunnr = gw_final-kunrg BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        gw_final-kunnr     =        lw_kna1-kunnr.
        gw_final-name1     =        lw_kna1-name1.
        gw_final-ship_gstin = lw_kna1-stcd3.
      ENDIF.

      READ TABLE gt_doctyp INTO lw_doctyp WITH KEY bukrs = p_ccode
      zmodule = gc_sd
      fkart   = gw_final-fkart.
      IF sy-subrc IS INITIAL.
        gw_final-einv = lw_doctyp-einv.
        gw_final-eway = lw_doctyp-eway.
      ENDIF.
      gw_final-mod = gc_sd.


      READ TABLE gt_wb2_v_likp_lips2 INTO lw_likp_lips2 WITH KEY vbeln = lw_wb2_v_likp_lips2-vbeln.
      IF sy-subrc IS INITIAL.
        lv_index = sy-tabix.
        LOOP AT gt_wb2_v_likp_lips2 INTO lw_likp_lips2 FROM lv_index.
          IF lw_likp_lips2-vbeln NE lw_wb2_v_likp_lips2-vbeln.
            EXIT.
          ENDIF.

          gw_final-taxamt    =  gw_final-taxamt + lw_likp_lips2-netwr.
          "IGST Tax amount
          READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_likp_lips2-knumv
          kposn = lw_likp_lips2-posnr_i
          kschl = gc_kschl_joig
          BINARY SEARCH.
          IF sy-subrc EQ 0.
            gw_final-igst_amt  =  gw_final-igst_amt + lw_konv-kwert.
          ENDIF.
          "CGST Tax amount
          READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_likp_lips2-knumv
          kposn = lw_likp_lips2-posnr_i
          kschl = gc_kschl_jocg
          BINARY SEARCH.
          IF sy-subrc EQ 0.
            gw_final-cgst_amt =  gw_final-cgst_amt + lw_konv-kwert.
          ENDIF.
          "SGST Tax amount
          READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_likp_lips2-knumv
          kposn = lw_likp_lips2-posnr_i
          kschl = gc_kschl_josg
          BINARY SEARCH.
          IF sy-subrc EQ 0.
            gw_final-sgst_amt =  gw_final-sgst_amt + lw_konv-kwert.
          ENDIF.
          "CESS Tax amount
          READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_likp_lips2-knumv
          kposn = lw_likp_lips2-posnr_i
          kschl = gc_kschl_cess
          BINARY SEARCH.
          IF sy-subrc EQ 0.
            gw_final-cess_amt =  gw_final-cess_amt + lw_konv-kwert.
          ENDIF.
        ENDLOOP.
      ENDIF.


      gw_final-invval = gw_final-taxamt + gw_final-cgst_amt + gw_final-sgst_amt + gw_final-igst_amt + gw_final-cess_amt.

      APPEND gw_final TO gt_final.
      CLEAR : gw_final.

    ENDLOOP.

  ENDIF.




ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FETCH_DOMAIN_VALUES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fetch_domain_values .

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZDOM_SUP_TYPE'
    TABLES
      values_tab      = gt_supply_values
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc = 0.
  ENDIF.


  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZDOM_SUB_TYPE'
    TABLES
      values_tab      = gt_sub_supply_values
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc = 0.
  ENDIF.



  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZDOM_EDOC_TYPE'
    TABLES
      values_tab      = gt_doctyp_values
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc = 0.
  ENDIF.


  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZDOM_CANCEL_REASON'
    TABLES
      values_tab      = gt_cancel_values
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc = 0.
  ENDIF.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZDOM_T_MODE'
    TABLES
      values_tab      = gt_t_mode_values
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc = 0.
  ENDIF.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZDOM_EXT_REASON'
    TABLES
      values_tab      = gt_t_ext_reason_values
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc = 0.
  ENDIF.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZDOM_V_TYPE'
    TABLES
      values_tab      = gt_v_type_values
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc = 0.
  ENDIF.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZDOM_CONSIGN_STS'
    TABLES
      values_tab      = gt_t_consignment_status_values
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc = 0.
  ENDIF.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZDOM_TRANSIT_TYPE'
    TABLES
      values_tab      = gt_t_transit_type_values
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc = 0.
  ENDIF.


  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZDOM_VEH_REASON'
    TABLES
      values_tab      = gt_v_reason_values
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc = 0.
  ENDIF.



* Assign dn type gl details

  rs_hkont-sign = 'I'.
  rs_hkont-option = 'EQ'.
  rs_hkont-low = '0000365002'.
  APPEND rs_hkont TO rt_hkont.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_CONFIRM_WITH_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VL_PROGRAM  text
*      -->P_VL_FORMNAME  text
*      -->P_VL_POPUP_TITLE  text
*      -->P_VL_OKPUSHBUTTON  text
*      -->P_VL_BUTTONICON  text
*      -->P_LT_SVAL  text
*----------------------------------------------------------------------*
FORM popup_to_confirm_with_input  USING    p_vl_program TYPE sy-repid
      p_vl_formname TYPE char30
      p_vl_popup_title TYPE char30
      p_vl_okpushbutton TYPE svalbutton-buttontext
      p_vl_buttonicon TYPE icon-name
      p_lt_sval TYPE ty_sval.



  CALL FUNCTION 'POPUP_GET_VALUES_USER_BUTTONS'
    EXPORTING
      formname          = p_vl_formname
      programname       = p_vl_program
      popup_title       = p_vl_popup_title
      ok_pushbuttontext = p_vl_okpushbutton
      icon_ok_push      = p_vl_buttonicon
    TABLES
      fields            = p_lt_sval
    EXCEPTIONS
      error_in_fields   = 1
      OTHERS            = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
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
*&---------------------------------------------------------------------*
*&      Form  PROCESS_FI_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_fi_data .

  DATA:lt_bseg           TYPE TABLE OF ty_bseg,
       lv_index          TYPE sy-tabix,
       lw_kna1           TYPE ty_kna1,
       lw_lfa1           TYPE ty_lfa1,
       lw_gstin          TYPE ty_j_1bbranch,
       lw_invrefnum      TYPE j_1ig_invrefnum,
       lw_zteinv_details TYPE zteinv_details,
       lv_internal_date  TYPE sy-datum,
       lw_doctyp         TYPE ty_doctyp,
       lw_einv_hdr       TYPE zst_einv_api_struct,
       lw_einv_item      TYPE zst_einv_api_struct_itm.

  CLEAR:gt_bkpf,gt_bseg,gt_kna1,gt_lfa1,gt_gstin.

  SELECT bukrs
  belnr
  gjahr
  blart
  bldat
  budat
  tcode
  usnam
  xblnr
  bktxt
  awtyp
  awkey FROM bkpf INTO TABLE gt_bkpf
  WHERE bukrs = p_ccode
  AND belnr IN s_doc
  AND gjahr = p_fyear
  AND blart IN sl_fkart
  AND budat IN s_date.
  IF sy-subrc = 0.
    SELECT *
    FROM zdchallan
    INTO TABLE gt_challan
    FOR ALL ENTRIES IN gt_bkpf
    WHERE dcyear = gt_bkpf-gjahr
    AND belnr = gt_bkpf-belnr
    AND bukrs = gt_bkpf-bukrs.

    SELECT bukrs belnr gjahr buzei buzid bschl koart shkzg dmbtr ktosl sgtxt xauto
    kunnr lifnr matnr werks menge meins ebeln ebelp bupla secco taxps
    gst_part plc_sup hsn_sac hkont
    FROM bseg INTO TABLE gt_bseg
    FOR ALL ENTRIES IN gt_bkpf
    WHERE bukrs = gt_bkpf-bukrs
    AND belnr = gt_bkpf-belnr
    AND gjahr = gt_bkpf-gjahr
    AND prctr IN s_prctr.

    IF sy-subrc IS   INITIAL.
      lt_bseg = gt_bseg.
      DELETE lt_bseg WHERE bupla IS INITIAL.
      SORT lt_bseg BY bupla.
      DELETE ADJACENT DUPLICATES FROM lt_bseg COMPARING bupla.
      IF lt_bseg IS NOT INITIAL.

* GSTIN deatils
        SELECT  bukrs branch name adrnr gstin
        FROM j_1bbranch
        INTO TABLE gt_gstin FOR ALL ENTRIES IN lt_bseg
        WHERE bukrs = lt_bseg-bukrs
        AND branch = lt_bseg-bupla.

        IF sy-subrc = 0.
          SELECT         addrnumber
          name1
          name2
          city1
          post_code1
          street
          region      FROM adrc
          INTO TABLE gt_adrc1
          FOR ALL ENTRIES IN gt_gstin
          WHERE addrnumber = gt_gstin-adrnr.
          IF sy-subrc IS INITIAL.
            SELECT addrnumber smtp_addr FROM adr6 INTO TABLE gt_adr61
            FOR ALL ENTRIES IN gt_adrc1
            WHERE addrnumber = gt_adrc1-addrnumber.
          ENDIF.
        ENDIF.
      ENDIF.

* Custoer data
      lt_bseg = gt_bseg.
      DELETE lt_bseg WHERE kunnr IS INITIAL.
      SORT lt_bseg BY kunnr.
      DELETE ADJACENT DUPLICATES FROM lt_bseg COMPARING kunnr.
      IF lt_bseg IS NOT INITIAL.
        SELECT * FROM Ztransport INTO TABLE @DATA(gt_transport) FOR ALL ENTRIES IN @lt_bseg
                                                           WHERE kunnr = @lt_bseg-kunnr.
        SELECT  kunnr land1 name1 name2 ort01 pstlz
        regio stras telf1 adrnr stcd3
        FROM kna1 INTO  TABLE gt_kna1
        FOR ALL ENTRIES IN lt_bseg
        WHERE kunnr = lt_bseg-kunnr.
        IF sy-subrc EQ 0.
          SELECT addrnumber tel_number  FROM adrc
          APPENDING TABLE gt_adrc
          FOR ALL ENTRIES IN gt_kna1
          WHERE addrnumber = gt_kna1-adrnr.
          IF sy-subrc IS INITIAL.
            SELECT addrnumber smtp_addr FROM adr6 APPENDING TABLE gt_adr61
            FOR ALL ENTRIES IN gt_adrc1
            WHERE addrnumber = gt_adrc1-addrnumber.
          ENDIF.
        ENDIF.
      ENDIF.

* Vendor data

      lt_bseg = gt_bseg.
      DELETE lt_bseg WHERE lifnr IS INITIAL.
      SORT lt_bseg BY lifnr.
      DELETE ADJACENT DUPLICATES FROM lt_bseg COMPARING lifnr.
      IF lt_bseg IS NOT INITIAL.
        SELECT lifnr land1 name1 name2 name3 name4 ort01
        pstlz regio stras adrnr stcd3
        FROM lfa1 INTO TABLE gt_lfa1
        FOR ALL ENTRIES IN lt_bseg
        WHERE lifnr = lt_bseg-lifnr.
        IF sy-subrc EQ 0.
          SELECT addrnumber tel_number  FROM adrc
          APPENDING TABLE gt_adrc
          FOR ALL ENTRIES IN gt_kna1
          WHERE addrnumber = gt_kna1-adrnr.
          IF sy-subrc IS INITIAL.
            SELECT addrnumber smtp_addr FROM adr6 APPENDING TABLE gt_adr61
            FOR ALL ENTRIES IN gt_adrc1
            WHERE addrnumber = gt_adrc1-addrnumber.
          ENDIF.
        ENDIF.
      ENDIF.

* get region text
      SELECT spras land1 bland bezei FROM t005u INTO TABLE gt_t005u
      WHERE spras = sy-langu
      AND land1 = gc_land1_in.
    ENDIF.


** Get e-invoice details from Z-Table
    SELECT * FROM zteinv_details INTO TABLE gt_zteinv_details
    FOR ALL ENTRIES IN gt_bkpf
    WHERE bukrs =  gt_bkpf-bukrs
    AND docno  = gt_bkpf-belnr
    AND gjahr =  gt_bkpf-gjahr.
    SELECT * FROM j_1ig_invrefnum INTO TABLE gt_invrefnum
    FOR ALL ENTRIES IN gt_bkpf
    WHERE bukrs EQ gt_bkpf-bukrs
    AND docno EQ gt_bkpf-belnr
    AND doc_year EQ gt_bkpf-gjahr.
    IF sy-subrc EQ 0.
      SORT  gt_invrefnum DESCENDING.
    ENDIF.

    SORT gt_bkpf BY  bukrs belnr gjahr.
    SORT gt_bseg BY   bukrs belnr gjahr.


    LOOP AT gt_bkpf INTO gw_bkpf.
      gw_final-bukrs = gw_bkpf-bukrs.
      gw_final-vbeln = gw_bkpf-belnr.
      gw_final-gjahr = gw_bkpf-gjahr.
*      gw_final-fkdat = gw_bkpf-budat.
      IF gw_bkpf-budat IS NOT INITIAL.
        CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
          EXPORTING
            date_internal            = gw_bkpf-budat
          IMPORTING
            date_external            = gw_final-fkdat
          EXCEPTIONS
            date_internal_is_invalid = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          MESSAGE 'Date conversion error'(m02) TYPE 'I'.
        ENDIF.
        gw_final-fkdat_db = gw_bkpf-budat.
      ENDIF.

      gw_final-fkart = gw_bkpf-blart.
      gw_final-doc_ernam = gw_bkpf-usnam.
      IF gw_bkpf-blart EQ 'DN'.
        READ TABLE gt_challan INTO DATA(gw_challan) WITH KEY dcyear = gw_bkpf-gjahr
              belnr = gw_bkpf-belnr.
        IF sy-subrc EQ 0.
          DATA lv_gjahr TYPE gjahr.
          lv_gjahr = gw_challan-dcyear + 1.
          CONCATENATE gw_challan-dcyear '-' lv_gjahr+2(2) '/' gw_challan-dcno INTO gw_final-odnno.
        ELSE.
          gw_final-odnno = gw_bkpf-belnr.      " IF No Entry found in ZDCHALLAN.
        ENDIF.
      ELSE.
        gw_final-odnno = gw_bkpf-belnr.
      ENDIF.


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
*        lv_index = sy-tabix.
        IF gw_bseg-kunnr IS NOT INITIAL.
          CLEAR:lw_kna1.
          READ TABLE gt_kna1 INTO lw_kna1 WITH KEY kunnr = gw_bseg-kunnr.
          IF sy-subrc = 0.
            gw_final-kunnr = lw_kna1-kunnr.
            gw_final-name1 = lw_kna1-name1.
            gw_final-ship_gstin = lw_kna1-stcd3.
          ENDIF.
        ELSEIF gw_bseg-lifnr IS NOT INITIAL.
          CLEAR:lw_lfa1.
          READ TABLE gt_lfa1 INTO lw_lfa1 WITH KEY lifnr = gw_bseg-lifnr.
          IF sy-subrc = 0.
            gw_final-kunnr = lw_lfa1-lifnr.
            gw_final-name1 = lw_lfa1-name1.
            gw_final-ship_gstin = lw_lfa1-stcd3.
          ENDIF.
        ENDIF.
        CLEAR:lw_gstin.
        READ TABLE gt_gstin INTO lw_gstin WITH KEY bukrs = gw_bseg-bukrs
        branch = gw_bseg-bupla.
        IF sy-subrc = 0.
          gw_final-sup_gstin = lw_gstin-gstin.
          gw_final-werks = lw_gstin-branch.
        ENDIF.


***************item Data*****************************
        IF gw_bkpf-blart = 'DN'.
          DATA(lv_count) = 0.
          LOOP AT gt_bseg INTO gw_bseg WHERE bukrs = gw_bkpf-bukrs
          AND belnr = gw_bkpf-belnr
          AND gjahr = gw_bkpf-gjahr
          AND koart = 'S'.

            IF gw_bseg-hkont IN rt_hkont.

              ADD gw_bseg-dmbtr TO gw_final-taxamt.

            ELSE.
              lv_count = lv_count + 1.
              DATA(ls_bseg_gst)  = gw_bseg.
            ENDIF.
          ENDLOOP.
          IF lv_count > 1.
            gw_final-cgst_amt = ls_bseg_gst-dmbtr.
            gw_final-sgst_amt = ls_bseg_gst-dmbtr.
          ELSE.
            gw_final-igst_amt = ls_bseg_gst-dmbtr.
          ENDIF.

        ELSE.
          LOOP AT gt_bseg INTO gw_bseg WHERE bukrs = gw_bkpf-bukrs
          AND belnr = gw_bkpf-belnr
          AND gjahr = gw_bkpf-gjahr
          AND koart = 'S'.
*          IF gw_bseg-belnr NE gw_bkpf-belnr .
*            EXIT.
*          ENDIF.

            IF gw_bseg-buzid  IS NOT INITIAL.
              IF gw_bseg-ktosl = gc_ktosl_jos.
                ADD gw_bseg-dmbtr TO gw_final-sgst_amt.
              ELSEIF gw_bseg-ktosl = gc_ktosl_joc.
                ADD gw_bseg-dmbtr TO gw_final-cgst_amt.
              ELSEIF gw_bseg-ktosl = gc_ktosl_joi.
                ADD gw_bseg-dmbtr TO gw_final-igst_amt.
              ENDIF.
            ELSEIF gw_bseg-buzid IS INITIAL.
*            IF gw_bseg-koart NE 'D'.
              ADD gw_bseg-dmbtr TO gw_final-taxamt.
*            ENDIF.
            ENDIF.
            CLEAR:gw_bseg.
          ENDLOOP.
        ENDIF.



*************Einvoice Data***********************************
        READ TABLE gt_invrefnum INTO lw_invrefnum WITH KEY bukrs = gw_bkpf-bukrs
        docno = gw_bkpf-belnr
        doc_type = gw_bkpf-blart
        doc_year = gw_bkpf-gjahr.
        IF sy-subrc = 0.
          gw_final-ack_no = lw_invrefnum-ack_no.
          CLEAR:lv_internal_date.
          IF lw_invrefnum-ack_date IS NOT INITIAL.
            lv_internal_date = lw_invrefnum-ack_date.
            CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
              EXPORTING
                date_internal            = lv_internal_date
              IMPORTING
                date_external            = gw_final-ack_dt
              EXCEPTIONS
                date_internal_is_invalid = 1
                OTHERS                   = 2.
            IF sy-subrc <> 0.
              MESSAGE 'Date conversion error'(m02) TYPE 'I'.
            ENDIF.
          ENDIF.

          gw_final-qr_code = lw_invrefnum-signed_qrcode.
          gw_final-irn = lw_invrefnum-irn.
          gw_final-sign_inv = lw_invrefnum-signed_inv.
          gw_final-ernam = lw_invrefnum-ernam.

          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
            EXPORTING
              date_internal            = lw_invrefnum-erdat
            IMPORTING
              date_external            = gw_final-erdat
            EXCEPTIONS
              date_internal_is_invalid = 1
              OTHERS                   = 2.
          IF sy-subrc <> 0.
            MESSAGE 'Date conversion error'(m02) TYPE 'I'.
          ENDIF.

          gw_final-erzet = lw_invrefnum-erzet.
          CLEAR lv_internal_date.
          IF lw_invrefnum-cancel_date IS NOT INITIAL.
            lv_internal_date = lw_invrefnum-cancel_date.
            CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
              EXPORTING
                date_internal            = lv_internal_date
              IMPORTING
                date_external            = gw_final-canc_dt
              EXCEPTIONS
                date_internal_is_invalid = 1
                OTHERS                   = 2.
            IF sy-subrc <> 0.
              MESSAGE 'Date conversion error'(m02) TYPE 'I'.
            ENDIF.
          ENDIF.
          IF lw_invrefnum-irn_status EQ gc_irn_sts_act.
            gw_final-status = 'Success'(060).
            gw_final-icon    = gc_icon_08.
          ELSEIF lw_invrefnum-irn_status EQ gc_irn_sts_err.
            gw_final-status = 'Error'(061).
            gw_final-icon    = gc_icon_0a.
          ELSEIF lw_invrefnum-irn_status EQ gc_irn_sts_cnl.
            gw_final-status = 'Cancelled'(062).
            gw_final-icon    = gc_icon_0w.
          ENDIF.
        ENDIF.
        CLEAR lw_zteinv_details.
        READ TABLE gt_zteinv_details INTO lw_zteinv_details WITH KEY bukrs = gw_bkpf-bukrs
        docno = gw_bkpf-belnr
        doctyp = gw_bkpf-blart
        gjahr = gw_bkpf-gjahr.

        IF sy-subrc = 0.
          IF gw_final-status = 'Error'(061).
            gw_final-einv_error     =  lw_zteinv_details-einv_error.
          ELSE.
            gw_final-eway_error     =  ' '.
          ENDIF.
        ENDIF.

        READ TABLE gt_doctyp INTO lw_doctyp WITH KEY bukrs = p_ccode
        zmodule = gc_fi
        fkart   = gw_final-fkart.
        IF sy-subrc IS INITIAL.
          gw_final-einv = lw_doctyp-einv.
          gw_final-eway = lw_doctyp-eway.
        ENDIF.


*********************************************************************
        gw_final-invval = gw_final-taxamt +
        gw_final-cgst_amt +
        gw_final-sgst_amt +
        gw_final-igst_amt.
        IF gw_final-hsn_sac IS INITIAL.
          gw_final-hsn_sac = '9917'.
        ENDIF.

        IF gw_final-sgst_amt IS NOT INITIAL OR
        gw_final-cgst_amt IS NOT INITIAL OR
        gw_final-igst_amt IS NOT INITIAL.
          APPEND gw_final TO gt_final.
        ENDIF.
      ENDIF.
      CLEAR: gw_final,gw_bkpf.
    ENDLOOP.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHANGE_REGION_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_region_text .
  FIELD-SYMBOLS <lw_t005u> TYPE ty_t005u.
* Change the region text region 26
  LOOP AT gt_t005u ASSIGNING <lw_t005u>.
    IF <lw_t005u>-bland = '26'.
      <lw_t005u>-bezei = 'DADAR AND NAGAR HAVELI & DAMAN AND DIU'.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_t005u_s ASSIGNING <lw_t005u>.
    IF <lw_t005u>-bland = '26'.
      <lw_t005u>-bezei = 'DADAR AND NAGAR HAVELI & DAMAN AND DIU'.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_t005u_d ASSIGNING <lw_t005u>.
    IF <lw_t005u>-bland = '26'.
      <lw_t005u>-bezei = 'DADAR AND NAGAR HAVELI & DAMAN AND DIU'.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form readtext1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_TDNAME
*&---------------------------------------------------------------------*
FORM readtext1  USING    p_lv_tdname TYPE tdobname.

  DATA: lw_lines TYPE tline,
        lt_lines TYPE TABLE OF tline.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = 'Z016'
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
    CONCATENATE   '' lw_lines-tdline ''  INTO gw_final-t_name.
  ENDIF.


  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = 'Z017'
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
    CONCATENATE  '' lw_lines-tdline '' INTO gw_final-v_number.
  ENDIF.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = 'Z018'
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
    CONCATENATE  '' lw_lines-tdline '' INTO gw_final-t_doc_no.
  ENDIF.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = 'Z019'
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
    CONCATENATE  '' lw_lines-tdline '' INTO gw_final-t_id.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form readtext_zsn
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_TDNAME
*&---------------------------------------------------------------------*
FORM readtext_zsn  USING    p_lv_tdname.

  DATA: lw_lines TYPE tline,
        lt_lines TYPE TABLE OF tline.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = 'Z038'
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
    CONCATENATE   '' lw_lines-tdline ''  INTO gw_final-t_name.
  ENDIF.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = 'Z017'
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
    CONCATENATE  '' lw_lines-tdline '' INTO gw_final-v_number.
  ENDIF.


  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = 'Z018'
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
    CONCATENATE  '' lw_lines-tdline '' INTO gw_final-t_doc_no.
  ENDIF.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = 'Z030'
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
    CONCATENATE  '' lw_lines-tdline '' INTO gw_final-t_date .
  ENDIF.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = 'Z047'
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
    CONCATENATE  '' lw_lines-tdline '' INTO gw_final-t_mode.

  ENDIF.


  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = 'Z031'
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
    CONCATENATE  '' lw_lines-tdline '' INTO gw_final-t_id.
  ENDIF.
  """Ship to details.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = 'Z032'
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
    CONCATENATE  '' lw_lines-tdline '' INTO gw_final-ship_to_name.
  ENDIF.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = 'Z033'
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
    CONCATENATE  '' lw_lines-tdline '' INTO gw_final-ship_to.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form readtext_exp
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_TDNAME
*&---------------------------------------------------------------------*
FORM readtext_exp  USING    p_lv_tdname.
  DATA: lw_lines TYPE tline,
        lt_lines TYPE TABLE OF tline.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = 'Z011'
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
    CONCATENATE  '' lw_lines-tdline '' INTO gw_final-zport_code.
  ENDIF.

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

*&---------------------------------------------------------------------*
*& Form process_mm_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM process_mm_data .


  DATA:lt_bseg             TYPE TABLE OF ty_bseg,
       lw_kna1             TYPE ty_kna1,
       lw_lfa1             TYPE ty_lfa1,
       lw_gstin            TYPE ty_j_1bbranch,
       lv_xblnr            TYPE xblnr,
       lv_gjahr            TYPE gjahr,
       lw_ewaybill         TYPE ty_ewaybill,
       ls_zteway_transport TYPE zteway_transport.


  SELECT * FROM zdchallan INTO TABLE gt_challan
  WHERE  bukrs = p_ccode
  AND dcno IN s_doc
  AND dcyear = p_fyear
  AND dcdate IN s_date.
  IF sy-subrc IS INITIAL.
    CLEAR:gt_bkpf,gt_bseg,gt_kna1,gt_lfa1,gt_gstin.

    SELECT bukrs
    belnr
    gjahr
    blart
    bldat
    budat
    tcode
    usnam
    xblnr
    bktxt
    awtyp
    awkey
    xreversing
    xreversed FROM bkpf INTO TABLE gt_bkpf
    FOR ALL ENTRIES IN gt_challan
    WHERE bukrs = p_ccode
    AND belnr = gt_challan-belnr
    AND gjahr = gt_challan-gjahr
    AND blart IN sl_fkart.
    IF sy-subrc IS INITIAL.
      DELETE gt_bkpf WHERE xreversing IS NOT INITIAL.
      DELETE gt_bkpf WHERE xreversed IS NOT INITIAL.
    ENDIF.

    IF gt_bkpf IS NOT INITIAL.
      SELECT bukrs belnr gjahr buzei buzid bschl koart shkzg dmbtr ktosl sgtxt xauto
      kunnr lifnr matnr werks menge meins ebeln ebelp bupla secco taxps
      gst_part plc_sup hsn_sac hkont txgrp mwskz
      FROM bseg INTO TABLE gt_bseg
      FOR ALL ENTRIES IN gt_bkpf
      WHERE bukrs = gt_bkpf-bukrs
      AND belnr = gt_bkpf-belnr
      AND gjahr = gt_bkpf-gjahr
      AND prctr IN s_prctr.

      IF sy-subrc IS   INITIAL.
        lt_bseg = gt_bseg.
        DELETE lt_bseg WHERE bupla IS INITIAL.
        SORT lt_bseg BY bupla.
        DELETE ADJACENT DUPLICATES FROM lt_bseg COMPARING bupla.
        IF lt_bseg IS NOT INITIAL.

* GSTIN deatils
          SELECT  bukrs branch name adrnr gstin
          FROM j_1bbranch
          INTO TABLE gt_gstin FOR ALL ENTRIES IN lt_bseg
          WHERE bukrs = lt_bseg-bukrs
          AND branch = lt_bseg-bupla.

          IF sy-subrc = 0.
            SELECT         addrnumber
            name1
            name2
            city1
            post_code1
            street
            region      FROM adrc
            INTO TABLE gt_adrc1
            FOR ALL ENTRIES IN gt_gstin
            WHERE addrnumber = gt_gstin-adrnr.
            IF sy-subrc IS INITIAL.
              SELECT addrnumber smtp_addr FROM adr6 INTO TABLE gt_adr61
              FOR ALL ENTRIES IN gt_adrc1
              WHERE addrnumber = gt_adrc1-addrnumber.
            ENDIF.
          ENDIF.
        ENDIF.

* Get plant details for address

        lt_bseg = gt_bseg.
        DELETE lt_bseg WHERE werks IS INITIAL.
        SORT lt_bseg BY werks.
        DELETE ADJACENT DUPLICATES FROM lt_bseg COMPARING werks.
        IF lt_bseg IS NOT INITIAL.

* GSTIN deatils
          SELECT werks name1 name2 stras pstlz ort01 land1 regio
          counc cityc adrnr spras  zone1 j_1bbranch
          FROM t001w
          INTO TABLE gt_t001w_mm
          FOR ALL ENTRIES IN lt_bseg
          WHERE werks EQ lt_bseg-werks.

          IF sy-subrc = 0.
            SELECT         addrnumber
            name1
            name2
            city1
            post_code1
            street
            region      FROM adrc
            APPENDING TABLE gt_adrc1
            FOR ALL ENTRIES IN gt_t001w_mm
            WHERE addrnumber = gt_t001w_mm-adrnr.
            IF sy-subrc IS INITIAL.
              SELECT addrnumber smtp_addr FROM adr6 APPENDING TABLE gt_adr61
              FOR ALL ENTRIES IN gt_adrc1
              WHERE addrnumber = gt_adrc1-addrnumber.
            ENDIF.
          ENDIF.
        ENDIF.


* Material description
        lt_bseg = gt_bseg.
        DELETE lt_bseg WHERE matnr IS INITIAL.
        SORT lt_bseg BY matnr.
        DELETE ADJACENT DUPLICATES FROM lt_bseg COMPARING matnr.
        IF lt_bseg IS NOT INITIAL.
          SELECT * FROM makt INTO TABLE gt_makt
          FOR ALL ENTRIES IN lt_bseg
          WHERE matnr = lt_bseg-matnr
          AND spras = 'EN'.
        ENDIF.

* Custoer data
*        lt_bseg = gt_bseg.
*        DELETE lt_bseg WHERE kunnr IS INITIAL.
*        SORT lt_bseg BY kunnr.
*        DELETE ADJACENT DUPLICATES FROM lt_bseg COMPARING kunnr.
*        IF lt_bseg IS NOT INITIAL.
*          SELECT  kunnr land1 name1 name2 ort01 pstlz
*                          regio stras telf1 adrnr stcd3
*                           FROM kna1 INTO  TABLE gt_kna1
*                           FOR ALL ENTRIES IN lt_bseg
*                           WHERE kunnr = lt_bseg-kunnr.
*          IF sy-subrc EQ 0.
*            SELECT addrnumber
*            name1
*            name2
*            city1
*            post_code1
*            street
*            region    FROM adrc
*              APPENDING TABLE gt_adrc1
*              FOR ALL ENTRIES IN gt_kna1
*              WHERE addrnumber = gt_kna1-adrnr.
*            IF sy-subrc IS INITIAL.
*              SELECT addrnumber smtp_addr FROM adr6 APPENDING TABLE gt_adr61
*                 FOR ALL ENTRIES IN gt_adrc1
*                WHERE addrnumber = gt_adrc1-addrnumber.
*            ENDIF.
*          ENDIF.
*        ENDIF.

* Vendor data

        lt_bseg = gt_bseg.
        DELETE lt_bseg WHERE lifnr IS INITIAL.
        SORT lt_bseg BY lifnr.
        DELETE ADJACENT DUPLICATES FROM lt_bseg COMPARING lifnr.
        IF lt_bseg IS NOT INITIAL.


          SELECT lifnr land1 name1 name2 name3 name4 ort01 ort02
          pstlz regio sortl stras adrnr stcd3
          FROM lfa1 INTO TABLE gt_lfa1
          FOR ALL ENTRIES IN lt_bseg
          WHERE lifnr = lt_bseg-lifnr.
          IF sy-subrc EQ 0.
            SELECT addrnumber
            name1
            name2
            city1
            post_code1
            street
            region   FROM adrc
            APPENDING TABLE gt_adrc1
            FOR ALL ENTRIES IN gt_lfa1
            WHERE addrnumber = gt_lfa1-adrnr.
            IF sy-subrc IS INITIAL.
              SELECT addrnumber smtp_addr FROM adr6 APPENDING TABLE gt_adr61
              FOR ALL ENTRIES IN gt_adrc1
              WHERE addrnumber = gt_adrc1-addrnumber.
            ENDIF.
          ENDIF.
        ENDIF.

* get region text
        SELECT spras land1 bland bezei FROM t005u INTO TABLE gt_t005u
        WHERE spras = sy-langu
        AND land1 = gc_land1_in.



* Eway bill
        SELECT bukrs
        doctyp
        docno
        gjahr
        ebillno
        egen_dat
        egen_time
        vdfmdate
        vdtodate
        vdtotime
        status
        ernam
        erdat
        aenam
        aedat FROM j_1ig_ewaybill INTO TABLE gt_ewaybill
        FOR ALL ENTRIES IN gt_bkpf
        WHERE bukrs =  gt_bkpf-bukrs
        AND docno  = gt_bkpf-belnr
        AND gjahr  = gt_bkpf-gjahr.
        IF sy-subrc IS INITIAL.
          SORT gt_ewaybill DESCENDING BY egen_dat egen_time.
        ENDIF.



* Get material details for HSN, and other fields
        lt_bseg = gt_bseg.
        DELETE lt_bseg WHERE matnr IS INITIAL.
        IF lt_bseg IS NOT  INITIAL.
          SELECT matnr werks steuc FROM marc INTO TABLE gt_marc
          FOR ALL ENTRIES IN lt_bseg
          WHERE matnr = lt_bseg-matnr
          AND werks = lt_bseg-werks.
          IF sy-subrc IS INITIAL.
            SORT gt_marc BY matnr werks.
          ENDIF.
        ENDIF.

        SELECT * FROM bset INTO TABLE gt_bset FOR ALL ENTRIES IN gt_bkpf
        WHERE bukrs = gt_bkpf-bukrs
        AND belnr  = gt_bkpf-belnr
        AND gjahr  = gt_bkpf-gjahr.


      ENDIF.


      SORT gt_bkpf BY  bukrs belnr gjahr.
      SORT gt_bseg BY   bukrs belnr gjahr.



      LOOP AT gt_challan INTO DATA(ls_challan).
        READ TABLE gt_bkpf INTO gw_bkpf WITH KEY bukrs = p_ccode
        belnr = ls_challan-belnr
        gjahr = ls_challan-gjahr.

        IF sy-subrc IS INITIAL.
          gw_final-bukrs = gw_bkpf-bukrs.
          gw_final-vbeln = gw_bkpf-belnr.
          gw_final-gjahr = gw_bkpf-gjahr.

          IF gw_bkpf-budat IS NOT INITIAL.
            CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
              EXPORTING
                date_internal            = gw_bkpf-budat
              IMPORTING
                date_external            = gw_final-fkdat
              EXCEPTIONS
                date_internal_is_invalid = 1
                OTHERS                   = 2.
            IF sy-subrc <> 0.
              MESSAGE 'Date conversion error'(m02) TYPE 'I'.
            ENDIF.
            gw_final-fkdat_db = gw_bkpf-budat.
          ENDIF.

          gw_final-fkart = gw_bkpf-blart.
          gw_final-doc_ernam = gw_bkpf-usnam.

          lv_gjahr = ls_challan-dcYEAR + 1.
          CONCATENATE ls_challan-dcyear '-' lv_gjahr '/' ls_challan-dcno INTO gw_final-odnno.

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
            IF gw_bseg-kunnr IS NOT INITIAL.
              CLEAR:lw_kna1.
              READ TABLE gt_kna1 INTO lw_kna1 WITH KEY kunnr = gw_bseg-kunnr.
              IF sy-subrc = 0.
                gw_final-kunnr = lw_kna1-kunnr.
                gw_final-name1 = lw_kna1-name1.
                gw_final-ship_gstin = lw_kna1-stcd3.
              ENDIF.
            ELSEIF gw_bseg-lifnr IS NOT INITIAL.
              CLEAR:lw_lfa1.
              READ TABLE gt_lfa1 INTO lw_lfa1 WITH KEY lifnr = gw_bseg-lifnr.
              IF sy-subrc = 0.
                gw_final-kunnr = lw_lfa1-lifnr.
                gw_final-name1 = lw_lfa1-name1.
                gw_final-ship_gstin = lw_lfa1-stcd3.
              ENDIF.
            ENDIF.
            CLEAR:lw_gstin.
            READ TABLE gt_gstin INTO lw_gstin WITH KEY bukrs = gw_bseg-bukrs
            branch = gw_bseg-bupla.
            IF sy-subrc = 0.
              gw_final-sup_gstin = lw_gstin-gstin.
            ENDIF.
          ENDIF.


          CLEAR:lw_ewaybill.
          READ TABLE gt_ewaybill INTO lw_ewaybill WITH KEY bukrs = gw_bkpf-bukrs
          docno = gw_bkpf-belnr
          doctyp = gw_bkpf-blart.
          IF sy-subrc = 0.
            gw_final-eway_num = lw_ewaybill-ebillno.

            CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
              EXPORTING
                date_internal            = lw_ewaybill-egen_dat
              IMPORTING
                date_external            = gw_final-eway_date
              EXCEPTIONS
                date_internal_is_invalid = 1
                OTHERS                   = 2.
            IF sy-subrc <> 0.
              MESSAGE 'Date conversion error'(m02) TYPE 'I'.
            ENDIF.

            gw_final-eway_ernam = lw_ewaybill-ernam.

            CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
              EXPORTING
                date_internal            = lw_ewaybill-erdat
              IMPORTING
                date_external            = gw_final-eway_erdat
              EXCEPTIONS
                date_internal_is_invalid = 1
                OTHERS                   = 2.
            IF sy-subrc <> 0.
              MESSAGE 'Date conversion error'(m02) TYPE 'I'.
            ENDIF.

            IF lw_ewaybill-status EQ  gc_eway_sts_c.
              CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
                EXPORTING
                  date_internal            = lw_ewaybill-aedat
                IMPORTING
                  date_external            = gw_final-eway_canc_dt
                EXCEPTIONS
                  date_internal_is_invalid = 1
                  OTHERS                   = 2.
              IF sy-subrc <> 0.
                MESSAGE 'Date conversion error'(m02) TYPE 'I'.
              ENDIF.
            ELSE.
              gw_final-eway_canc_dt = 0.
            ENDIF.

            gw_final-eway_erzet = lw_ewaybill-egen_time.

* Valid from
            CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
              EXPORTING
                date_internal            = lw_ewaybill-vdfmdate
              IMPORTING
                date_external            = gw_final-eway_v_from
              EXCEPTIONS
                date_internal_is_invalid = 1
                OTHERS                   = 2.
            IF sy-subrc <> 0.
              MESSAGE 'Date conversion error'(m02) TYPE 'I'.
            ENDIF.

* Valid To
            CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
              EXPORTING
                date_internal            = lw_ewaybill-vdtodate
              IMPORTING
                date_external            = gw_final-eway_v_to
              EXCEPTIONS
                date_internal_is_invalid = 1
                OTHERS                   = 2.
            IF sy-subrc <> 0.
              MESSAGE 'Date conversion error'(m02) TYPE 'I'.
            ENDIF.
            gw_final-eway_v_time = lw_ewaybill-vdtotime.


            IF lw_ewaybill-status = gc_eway_sts_a.
              gw_final-eway_status = 'Success'(060).
              gw_final-eway_icon    = gc_icon_08.
            ELSEIF lw_ewaybill-status = gc_eway_sts_e.
              gw_final-eway_status = 'Error'(061).
              gw_final-eway_icon    = gc_icon_0a.
            ELSEIF lw_ewaybill-status = gc_eway_sts_c.
              gw_final-eway_status = 'Cancelled'(062).
              gw_final-eway_icon    = gc_icon_0w.
            ENDIF.
          ENDIF.



          READ TABLE gt_zteway_transport INTO ls_zteway_transport WITH KEY
          bukrs = gw_bkpf-bukrs
          docno = gw_bkpf-belnr
          gjahr  = gw_bkpf-gjahr.

          IF sy-subrc = 0.
            gw_final-t_id       =  ls_zteway_transport-t_id.
            gw_final-t_name     =  ls_zteway_transport-t_name.
            gw_final-t_doc_no   =  ls_zteway_transport-t_doc_no.


            IF ls_zteway_transport-t_date IS NOT INITIAL.
              CONCATENATE ls_zteway_transport-t_date+6(2) '/'
              ls_zteway_transport-t_date+4(2) '/'
              ls_zteway_transport-t_date+0(4)
              INTO gw_final-t_date.
            ENDIF.

            gw_final-t_mode     =  ls_zteway_transport-t_mode.
            gw_final-t_distance =  ls_zteway_transport-t_distance.
            gw_final-v_number   =  ls_zteway_transport-v_number.
            gw_final-v_type     =  ls_zteway_transport-v_type.
            IF gw_final-v_type  IS INITIAL.
              gw_final-v_type     = gc_v_type_1..
            ENDIF.
            gw_final-eway_print     =  ls_zteway_transport-eway_print.
            IF gw_final-eway_status = 'Error'(061).
              gw_final-eway_error     =  ls_zteway_transport-eway_error.
            ELSE.
              gw_final-eway_error     =  ' '.
            ENDIF.
            gw_final-t_r_distance           = ls_zteway_transport-t_r_distance.
            gw_final-t_ext_valid_reason     = ls_zteway_transport-t_ext_valid_reason.
            gw_final-t_ext_valid_remarks    = ls_zteway_transport-t_ext_valid_remarks.
            gw_final-t_from_pin             = ls_zteway_transport-t_from_pin.
            gw_final-t_consignment_status   = ls_zteway_transport-t_consignment_status.
            gw_final-t_transit_type         = ls_zteway_transport-t_transit_type.
            gw_final-t_address1             = ls_zteway_transport-t_address1.
            gw_final-t_address2             = ls_zteway_transport-t_address2.
            gw_final-t_address3             = ls_zteway_transport-t_address3.

            gw_final-v_reason_code        = ls_zteway_transport-v_reason_code.
            gw_final-v_reason             = ls_zteway_transport-v_reason.
            gw_final-c_reason_code        = ls_zteway_transport-c_reason_code.
            gw_final-c_reason             = ls_zteway_transport-c_reason.
            gw_final-zport_code           = ls_zteway_transport-zport_code.
            gw_final-consignor_place      = ls_zteway_transport-consignor_place.
            gw_final-consignor_state      = ls_zteway_transport-consignor_state.
          ELSE.
* Customer specific logic


            gw_final-t_id       =  ls_challan-name5.
            gw_final-t_name     =  ls_challan-transporter.
            gw_final-t_doc_no   =  ls_challan-lrno.
            IF ls_challan-lrdate IS NOT INITIAL.
              CONCATENATE ls_challan-lrdate+6(2) '/'
              ls_challan-lrdate+4(2) '/'
              ls_challan-lrdate+0(4)
              INTO gw_final-t_date.
            ENDIF.
            gw_final-t_mode     = '1'.
            gw_final-v_number  = ls_challan-truckno.
            gw_final-v_type    = gc_v_type_1.

          ENDIF.



          READ TABLE gt_doctyp INTO DATA(ls_doctyp) WITH KEY bukrs = p_ccode
                zmodule = gc_mm
                fkart   = gw_final-fkart.
          IF sy-subrc IS INITIAL.
            gw_final-einv = ls_doctyp-einv.
            gw_final-eway = ls_doctyp-eway.
          ENDIF.

          LOOP AT gt_bseg INTO gw_bseg WHERE bukrs = gw_bkpf-bukrs
          AND belnr = gw_bkpf-belnr
          AND gjahr = gw_bkpf-gjahr
          AND koart = 'S'
          AND buzid <> 'T'.


            IF gw_bseg-mwskz = 'V0'.
              CONTINUE.
            ENDIF.
            gw_final-werks = gw_bseg-werks.
            CLEAR gw_final_temp.
            gw_final_temp = gw_final.
            gw_final_temp-matnr = gw_bseg-matnr.
            gw_final_temp-menge = gw_bseg-menge.
            gw_final_temp-meins = gw_bseg-meins.
            gw_final_temp-hsn_sac = gw_bseg-hsn_sac.
            READ TABLE gt_bset INTO DATA(ls_bset) WITH KEY  bukrs = gw_bkpf-bukrs
                  belnr = gw_bkpf-belnr
                  gjahr  = gw_bkpf-gjahr
                  txgrp  = gw_bseg-txgrp
                  kschl =  gc_igst.
            IF sy-subrc IS INITIAL.
              gw_final_temp-taxamt = ls_bset-hwbas.
              gw_final_temp-igst_amt = ls_bset-hwste.
              gw_final_temp-igst_rate = ls_bset-kbetr / 10.
            ELSE.
              READ TABLE gt_bset INTO DATA(ls_bset1) WITH KEY  bukrs = gw_bkpf-bukrs
                    belnr = gw_bkpf-belnr
                    gjahr  = gw_bkpf-gjahr
                    txgrp  = gw_bseg-txgrp
                    kschl =  gc_cgst.
              IF sy-subrc IS INITIAL.
                gw_final_temp-taxamt = ls_bset1-hwbas.
                gw_final_temp-cgst_amt = ls_bset1-hwste.
                gw_final_temp-sgst_amt = ls_bset1-hwste.
                gw_final_temp-cgst_rate = ls_bset1-kbetr / 10.
                gw_final_temp-sgst_rate = ls_bset1-kbetr / 10.
              ENDIF.
            ENDIF.

            gw_final_temp-invval = gw_final_temp-taxamt +
            gw_final_temp-cgst_amt +
            gw_final_temp-sgst_amt +
            gw_final_temp-igst_amt.



            gw_final-taxamt = gw_final-taxamt + gw_final_temp-taxamt.
            gw_final-invval = gw_final-invval + gw_final_temp-invval.
            gw_final-cgst_amt = gw_final-cgst_amt + gw_final_temp-cgst_amt.
            gw_final-sgst_amt = gw_final-sgst_amt + gw_final_temp-sgst_amt.
            gw_final-igst_amt = gw_final-igst_amt + gw_final_temp-igst_amt.

            IF gw_final_temp-hsn_sac IS INITIAL.
              gw_final_temp-hsn_sac = '9917'.
            ENDIF.
            APPEND gw_final_temp TO gt_final_temp.
          ENDLOOP.



          IF gw_final-hsn_sac IS INITIAL.
            gw_final-hsn_sac = '9917'.
          ENDIF.
          APPEND gw_final TO gt_final.
          CLEAR: gw_final,gw_bkpf.

        ENDIF.
      ENDLOOP.

    ENDIF.
  ENDIF.
ENDFORM.
