FUNCTION ZFM_EWAY_BILL_GENERATE_AUTO.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_VBELN) TYPE  VBELN
*"  EXPORTING
*"     VALUE(EX_EWAY_BILLNO) TYPE  J_1IG_DOCNO
*"  TABLES
*"      XVBRK STRUCTURE  VBRKVB
*"      XVBRP STRUCTURE  VBRPVB
*"      XKOMV STRUCTURE  KOMV
*"----------------------------------------------------------------------
  DATA:gt_invrefnum        TYPE STANDARD TABLE OF j_1ig_invrefnum,
        gt_bkpf             TYPE STANDARD TABLE OF bkpf,
        gt_kna1             TYPE STANDARD TABLE OF kna1,
        gt_adrc             TYPE STANDARD TABLE OF adrc,
        gt_adr6             TYPE  TABLE OF adr6,
        gt_zteway_transport TYPE TABLE OF zteway_transport,
        gt_api              TYPE TABLE OF zteinv_api,
        gt_api_ddic         TYPE TABLE OF zteinv_api_ddic,
        gt_appvr            TYPE TABLE OF zteinv_appvr,
        gt_doctyp           TYPE TABLE OF zteinv_doctyp,
        gt_user_config      TYPE TABLE OF zteinv_appvr,
        gw_user_config      TYPE  zteinv_appvr,
        gt_vbfa             TYPE TABLE OF vbfa,
        gt_konv             TYPE TABLE OF konv,
        gt_ewaybill         TYPE TABLE OF j_1ig_ewaybill,
        gt_marc             TYPE TABLE OF marc,
        gt_mara             TYPE TABLE OF mara,
        gt_t005u            TYPE STANDARD TABLE OF t005u,
        gt_doc_uom          TYPE STANDARD TABLE OF ztdoc_uom,
        gt_export           TYPE TABLE OF ztedoc_export,

*Seller Details
        gt_t001w            TYPE STANDARD TABLE OF t001w,
        gt_adrc_s           TYPE STANDARD TABLE OF adrc,
        gt_adr6_s           TYPE STANDARD TABLE OF adr6,
        gt_t005u_s          TYPE STANDARD TABLE OF t005u,
        gt_gstin            TYPE TABLE OF j_1bbranch.

  DATA: gt_supply_values               TYPE TABLE OF   dd07v,
        gt_sub_supply_values           TYPE TABLE OF   dd07v,
        gt_doctyp_values               TYPE TABLE OF   dd07v,
        gt_cancel_values               TYPE TABLE OF   dd07v,
        gt_t_mode_values               TYPE TABLE OF   dd07v,
        gt_v_type_values               TYPE TABLE OF   dd07v,
        gt_t_ext_reason_values         TYPE TABLE OF   dd07v,
        gt_t_consignment_status_values TYPE TABLE OF   dd07v,
        gt_t_transit_type_values       TYPE TABLE OF   dd07v,
        gt_v_reason_values             TYPE TABLE OF   dd07v.


*Dispatch and Ship Details
  DATA : gt_likp    TYPE STANDARD TABLE OF likp,
        gt_kna1_d  TYPE STANDARD TABLE OF kna1,
        gt_t005u_d TYPE STANDARD TABLE OF t005u,
        gt_adrc_d  TYPE STANDARD TABLE OF adrc,
        gt_vbpa    TYPE TABLE OF vbpa,
        gt_lfa1    TYPE TABLE OF lfa1.

  DATA: gt_einv_api_hdr TYPE ztt_einv_api_struct,
        gt_einv_api_itm TYPE ztt_einv_api_struct_itm.

  DATA: gt_eway_api_hdr TYPE ztt_eway_api_struct,
        gt_eway_api_itm TYPE ztt_eway_api_struct_itm.

  DATA: gv_date  TYPE sydatum,
        gv_doc   TYPE j_1ig_docno,
        gv_odn   TYPE xblnr_alt,
        gv_bp    TYPE bupla,
        gv_stat  TYPE j_1ig_irn_status,
        gv_werks TYPE werks_d,
        gv_docty TYPE zde_docty,
        gv_prctr TYPE prctr.

  DATA:gw_token    TYPE string,
        gw_return   TYPE string,
        ga_return   TYPE string,
        gt_messages TYPE bapiret2_t,
        gw_message  TYPE bapiret2.

  DATA: gv_e_comm TYPE sy-ucomm.

  DATA:gt_index TYPE lvc_t_row,
        gt_row   TYPE lvc_t_roid.


* Constant declarations
  CONSTANTS:
  gc_sd(2) TYPE C VALUE 'SD',
  gc_mm(2) TYPE C VALUE 'MM',
  gc_fi(2) TYPE C VALUE 'FI'.


  DATA:lt_vbrk TYPE STANDARD TABLE OF vbrkvb,
        lw_vbrk TYPE vbrkvb,
        lt_vbrp TYPE STANDARD TABLE OF vbrpvb,
        lw_vbrp TYPE vbrpvb.


  DATA:lw_index  TYPE lvc_s_row,
        lv_val    TYPE xfeld,
        lv_answer TYPE C.

  DATA:lv_index        TYPE sy-tabix.

  DATA:lw_token      TYPE string,
        lw_return     TYPE string,
        wa_return     TYPE string,
        lt_messages   TYPE bapiret2_t,
        lw_message    TYPE bapiret2,
        lw_etransport TYPE zteway_transport.

  DATA:lt_ewaybill TYPE TABLE OF j_1ig_ewaybill,
        lw_ewaybill TYPE j_1ig_ewaybill.

  FIELD-SYMBOLS: <lw_ewaybill> TYPE j_1ig_ewaybill.

  DATA :ls_doc_uom TYPE ztdoc_uom,
        ls_doctyp  TYPE zveinv_doctyp.

  DATA: lv_output_uom TYPE meins.

  DATA: lt_api_hdr TYPE ztt_eway_api_struct , "ztenv_api_struct,
        lt_api_itm TYPE ztt_eway_api_struct_itm.


  DATA:lw_eway_api_hdr TYPE zst_eway_api_struct,
        lw_eway_api_itm TYPE zst_eway_api_struct_itm.


  DATA: lw_t001w   TYPE t001w,
        lw_t005u_s TYPE t005u.
  DATA :lw_kna1   TYPE kna1,
        lw_t005u  TYPE t005u,
        lw_adrc   TYPE adrc,
        lw_adr6   TYPE adr6,
        lw_marc   TYPE marc,
        ls_export TYPE ztedoc_export,
        lw_vbpa   TYPE vbpa.


  DATA:lw_usrgstin TYPE zteinv_api,
        lw_konv     TYPE konv.

  DATA: lv_supply_type     TYPE char10 VALUE 'O',
        lv_document_type   TYPE char10 VALUE 1,
        lv_sub_supply_type TYPE char10 VALUE 1,
        lv_document_number TYPE I VALUE 1.


  DATA: lv_total_invoice_value   TYPE netwr,
        lv_total_sgst_value      TYPE netwr,
        lv_total_cgst_value      TYPE netwr,
        lv_total_igst_value      TYPE netwr,
        lv_total_cess_value      TYPE netwr,
        lv_total_cessnoval_value TYPE netwr,
        lv_total_taxable_value   TYPE netwr.

  DATA: ls_values TYPE dd07v,
        lv_error  TYPE xfeld,
        lv_string TYPE string,
        lv_vbeln  TYPE vbrk-vbeln.


  CLEAR: lt_messages,lt_ewaybill,lt_api_hdr,lt_api_itm,gt_eway_api_hdr,gt_eway_api_itm.

  lt_vbrk =      xvbrk[] .
  lt_vbrp =      xvbrp[]   .
*  lt_vbr =      xkomv[]   .

*** Reset header value for every document
  lv_total_invoice_value   = 0.
  lv_total_sgst_value      = 0.
  lv_total_cgst_value      = 0.
  lv_total_igst_value      = 0.
  lv_total_cess_value      = 0.
  lv_total_cessnoval_value = 0.
  lv_total_taxable_value    = 0.

*Fetch the detaisl
  SELECT *
  FROM zteinv_api
  INTO TABLE gt_api.
  IF sy-subrc EQ 0.
  ENDIF.

* Get document related configuration data
  SELECT *
  FROM zteinv_doctyp
  INTO  TABLE gt_doctyp
  FOR ALL ENTRIES IN lt_vbrk
  WHERE bukrs EQ lt_vbrk-bukrs AND
*         zmodule EQ p_mod AND
  fkart EQ lt_vbrk-fkart AND
  batch EQ 'X'.
  IF sy-subrc EQ 0.
  ENDIF.

  SELECT * FROM t001w
  INTO CORRESPONDING FIELDS OF TABLE gt_t001w
  FOR ALL ENTRIES IN lt_vbrp
  WHERE werks EQ lt_vbrp-werks.
  IF sy-subrc EQ 0.

    SELECT * FROM t005u
    INTO CORRESPONDING FIELDS OF TABLE gt_t005u_s
    FOR ALL ENTRIES IN gt_t001w
    WHERE land1 EQ gt_t001w-land1
    AND bland EQ gt_t001w-regio
    AND spras EQ 'EN'.
    IF sy-subrc EQ 0.
    ENDIF.

    SELECT * FROM adrc
    INTO CORRESPONDING FIELDS OF TABLE gt_adrc_s
    FOR ALL ENTRIES IN gt_t001w
    WHERE addrnumber = gt_t001w-adrnr.
    IF sy-subrc EQ 0.

      SELECT * FROM adr6
      INTO CORRESPONDING FIELDS OF TABLE gt_adr6_s
      FOR ALL ENTRIES IN gt_adrc_s
      WHERE addrnumber = gt_adrc_s-addrnumber.
      IF sy-subrc EQ 0.
      ENDIF.

    ENDIF.
  ENDIF.

  SELECT * FROM ztedoc_export INTO TABLE gt_export.


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

* Select partners
  SELECT * FROM vbpa
  INTO TABLE gt_vbpa
  FOR ALL ENTRIES IN lt_vbrk
  WHERE vbeln = lt_vbrk-vbeln.
  IF sy-subrc EQ 0.
*Buyer Details
    SELECT * FROM kna1
    INTO CORRESPONDING FIELDS OF TABLE gt_kna1
    FOR ALL ENTRIES IN gt_vbpa
    WHERE kunnr = gt_vbpa-kunnr.
    IF sy-subrc EQ 0.

      SELECT * FROM t005u
      INTO CORRESPONDING FIELDS OF TABLE gt_t005u
      FOR ALL ENTRIES IN gt_kna1
      WHERE land1 EQ gt_kna1-land1
      AND bland EQ gt_kna1-regio
      AND spras EQ 'EN'.

      SELECT * FROM adrc
      INTO CORRESPONDING FIELDS OF TABLE gt_adrc
      FOR ALL ENTRIES IN gt_kna1
      WHERE addrnumber = gt_kna1-adrnr.

      IF sy-subrc EQ 0.
        SELECT * FROM adr6
        INTO CORRESPONDING FIELDS OF TABLE gt_adr6
        FOR ALL ENTRIES IN gt_adrc
        WHERE addrnumber = gt_adrc-addrnumber.
        IF sy-subrc EQ 0.
        ENDIF.
      ENDIF.
    ENDIF.




    LOOP AT lt_vbrp INTO lw_vbrp .

      CLEAR:lw_vbrk.
      READ TABLE lt_vbrk INTO lw_vbrk WITH  KEY vbeln = lw_vbrp-vbeln.
      IF sy-subrc EQ 0.

********************************* Begin of Fill header derails   ***********************************************
        lw_eway_api_hdr-bukrs       = lw_vbrk-bukrs.

        READ TABLE gt_api  INTO lw_usrgstin WITH KEY apiid = 'E_USRGSTIN'.
        IF sy-subrc EQ 0.
          CONCATENATE '"' lw_usrgstin-apiprov '"' INTO lw_eway_api_hdr-user_gstin.
        ELSE.
*      CONCATENATE '"' gw_final-sup_gstin '"' INTO lw_eway_api_hdr-user_gstin.
        ENDIF.

        READ TABLE gt_doctyp INTO ls_doctyp WITH KEY fkart = lw_vbrk-fkart.
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

        CONCATENATE '"' lw_vbrk-gjahr '"' INTO lw_eway_api_hdr-doc_year.

        CASE lv_document_number.
        WHEN 1.   "Billing Number as same as ODN
          CONCATENATE '"' lw_vbrk-vbeln '"' INTO lw_eway_api_hdr-document_number.
        WHEN 2.   "ODN
          CONCATENATE '"' lw_vbrk-xblnr '"' INTO lw_eway_api_hdr-document_number.
        ENDCASE.
        lw_eway_api_hdr-vbeln = lw_vbrp-vbeln.

        CONCATENATE '"'
        lw_vbrk-fkdat+6(2) '/'
        lw_vbrk-fkdat+4(2) '/'
        lw_vbrk-fkdat+0(4)
        '"' INTO lw_eway_api_hdr-document_date.

*** Supplier details
        CLEAR: lw_t001w. ", lw_adrc_s, lw_adr6_s.
        READ TABLE gt_t001w INTO lw_t001w WITH  KEY werks = lw_vbrp-werks.
        IF sy-subrc EQ 0.

          READ TABLE gt_api  INTO lw_usrgstin WITH KEY apiid = 'E_SUPGSTIN'.
          IF sy-subrc EQ 0.
            CONCATENATE '"' lw_usrgstin-apiprov '"' INTO lw_eway_api_hdr-gstin_of_consignor.
          ELSE.
*        CONCATENATE '"' gw_final-sup_gstin '"' INTO lw_eway_api_hdr-gstin_of_consignor.
          ENDIF.

*                  CONCATENATE '"' gw_final-sup_gstin '"' INTO lw_eway_api_hdr-gstin_of_consignor.
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
        READ TABLE gt_kna1 INTO lw_kna1 WITH  KEY kunnr = lw_vbrk-kunag.
        IF sy-subrc EQ 0.

          READ TABLE gt_api  INTO lw_usrgstin WITH KEY apiid = 'E_BUYGSTIN'.
          IF sy-subrc EQ 0.
            CONCATENATE '"' lw_usrgstin-apiprov '"' INTO lw_eway_api_hdr-gstin_of_consignee.
          ELSE.
            IF lw_kna1-stcd3 IS NOT INITIAL.
              CONCATENATE '"' lw_kna1-stcd3 '"' INTO lw_eway_api_hdr-gstin_of_consignee.
            ELSE.
              lw_eway_api_hdr-gstin_of_consignee = '"URP"'.
            ENDIF.
          ENDIF.

          READ TABLE gt_vbpa INTO lw_vbpa WITH  KEY vbeln = lw_vbrp-vbeln
          parvw = 'WE'.
          IF sy-subrc EQ 0.
            READ TABLE gt_kna1 INTO lw_kna1 WITH KEY kunnr = lw_vbpa-kunnr.
            IF sy-subrc EQ 0.
              READ TABLE gt_adrc INTO lw_adrc WITH  KEY addrnumber = lw_kna1-adrnr.
              READ TABLE gt_adr6 INTO lw_adr6 WITH  KEY addrnumber = lw_adrc-addrnumber.
              CONCATENATE '"' lw_kna1-name1 '"' INTO lw_eway_api_hdr-legal_name_of_consignee.
              CONCATENATE '"' lw_kna1-stras '"' INTO lw_eway_api_hdr-address1_of_consignee.
              CONCATENATE '"' lw_kna1-name2 '"' INTO lw_eway_api_hdr-address2_of_consignee.
              CONCATENATE '"' lw_kna1-ort01 '"' INTO lw_eway_api_hdr-place_of_consignee.
              CONCATENATE '"' lw_kna1-pstlz '"' INTO lw_eway_api_hdr-pincode_of_consignee.
              CLEAR:lw_t005u.
              READ TABLE gt_t005u INTO lw_t005u WITH  KEY bland = lw_kna1-regio.
              IF sy-subrc EQ 0.
                TRANSLATE lw_t005u-bezei TO UPPER CASE.
                CONCATENATE '"' lw_t005u-bezei '"' INTO lw_eway_api_hdr-state_of_supply.
              ENDIF.

              CONCATENATE '"' lw_t005u-bezei '"' INTO lw_eway_api_hdr-actual_to_state_name.

              IF lw_eway_api_hdr-sub_supply_type        = '"EXPORT"'.
                lw_eway_api_hdr-state_of_supply       = '"OTHER COUNTRY"'.
                lw_eway_api_hdr-gstin_of_consignee    = '"URP"'.
                READ TABLE gt_export INTO ls_export WITH KEY zport = ''." gw_final-zport.
                IF sy-subrc EQ 0..
                  CONCATENATE '"' ls_export-zport_address1 '"' INTO lw_eway_api_hdr-address1_of_consignee.
                  CONCATENATE '"' ls_export-zport_address2 '"' INTO lw_eway_api_hdr-address2_of_consignee.
                  CONCATENATE '"' ls_export-zport_place '"' INTO lw_eway_api_hdr-place_of_consignee.
                  CONCATENATE '"' ls_export-zport_pincode '"' INTO lw_eway_api_hdr-pincode_of_consignee.

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

*    CONCATENATE '"' gw_final-t_id '"' INTO lw_eway_api_hdr-transporter_id.
*    CONCATENATE '"' gw_final-t_name '"' INTO lw_eway_api_hdr-transporter_name.

*    READ TABLE gt_t_mode_values INTO ls_values WITH KEY domvalue_l = gw_final-t_mode.
        IF sy-subrc EQ 0.
          TRANSLATE ls_values-ddtext TO UPPER CASE.
          CONCATENATE '"' ls_values-ddtext '"' INTO lw_eway_api_hdr-transportation_mode.
        ENDIF.
*    CONCATENATE '"' gw_final-t_distance '"' INTO lw_eway_api_hdr-transportation_distance.

*    CONCATENATE '"' gw_final-t_doc_no '"' INTO lw_eway_api_hdr-transporter_document_number.
*    CONCATENATE '"' gw_final-t_date '"' INTO lw_eway_api_hdr-transporter_document_date.

*    CONCATENATE '"' gw_final-v_number '"' INTO lw_eway_api_hdr-vehicle_number.
*    READ TABLE gt_v_type_values INTO ls_values WITH KEY domvalue_l = gw_final-v_type.
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

        CONCATENATE '"' lw_vbrp-matnr '"' INTO lw_eway_api_itm-product_name.
        CONCATENATE '"' lw_vbrp-arktx '"' INTO lw_eway_api_itm-product_description.

        READ TABLE gt_marc INTO lw_marc WITH KEY matnr = lw_vbrp-matnr
        werks = lw_vbrp-werks.
        IF sy-subrc EQ 0.
          CONCATENATE '"' lw_marc-steuc '"' INTO lw_eway_api_itm-hsn_code.
        ENDIF.

        lw_eway_api_itm-quantity         = lw_vbrp-fkimg.

        CLEAR lv_output_uom.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          INPUT          = lw_vbrp-meins
        IMPORTING
          OUTPUT         = lv_output_uom
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
          lw_eway_api_itm-unit_of_product = '"PCS"'.
        ENDIF.
        "Item Amount
        lw_eway_api_itm-taxable_amount = lw_vbrp-netwr.

        "CGST Tax amount
*    READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_vbrk-knumv
*                                             kposn = lw_vbrp-posnr
*                                             kschl = 'JOCG'.
*    IF sy-subrc EQ 0.
*      lw_eway_api_itm-cgst_rate = lw_konv-kbetr / 10.
*      lv_total_cgst_value = lv_total_cgst_value + lw_konv-kwert.
*    ENDIF.
        "SGST Tax amount
*    READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_vbrp-knumv
*                                             kposn = lw_vbrp-posnr
*                                             kschl = 'JOSG'.
*    IF sy-subrc EQ 0.
*      lw_eway_api_itm-sgst_rate = lw_konv-kbetr / 10.
*      lv_total_sgst_value = lv_total_sgst_value + lw_konv-kwert.
*    ENDIF.
*    "IGST Tax amount
*    READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_vbrp-knumv
*                                             kposn = lw_vbrp-posnr
*                                             kschl = 'JOIG'.
*    IF sy-subrc EQ 0.
*      lw_eway_api_itm-igst_rate = lw_konv-kbetr / 10.
*      lv_total_igst_value = lv_total_igst_value + lw_konv-kwert.
*    ENDIF.
*    "CESS Tax amount
*    READ TABLE gt_konv INTO lw_konv WITH KEY knumv = lw_wb2_v_vbrk_vbrp2-knumv
*                                             kposn = lw_vbrp2-posnr_i
*                                             kschl = 'CESS'.
*    IF sy-subrc EQ 0.
*      lw_eway_api_itm-cess_rate = lw_konv-kbetr / 10.
*      lv_total_cess_value = lv_total_cess_value + lw_konv-kwert.
*    ENDIF.
        lw_eway_api_itm-cessnonadvol = 0.
          lv_total_cessnoval_value = lv_total_cessnoval_value + 0.

          lv_total_taxable_value  = lv_total_taxable_value  + lw_vbrp-netwr.
          APPEND lw_eway_api_itm TO gt_eway_api_itm.
          CLEAR:lw_eway_api_itm.

***********************************  End of Fill item derails   ************************************************


**** Header total values

          lw_eway_api_hdr-total_invoice_value  = lv_total_taxable_value + "lw_wb2_v_vbrk_vbrp2-netwr +
          lv_total_cgst_value +
          lv_total_sgst_value +
          lv_total_igst_value +
          lv_total_cess_value +
          lv_total_cessnoval_value.

          lw_eway_api_hdr-other_value          = 0.
          lw_eway_api_hdr-taxable_amount       = lv_total_taxable_value. "lw_wb2_v_vbrk_vbrp2-netwr.
          lw_eway_api_hdr-cgst_amount          = lv_total_cgst_value.
          lw_eway_api_hdr-sgst_amount          = lv_total_sgst_value.
          lw_eway_api_hdr-igst_amount          = lv_total_igst_value.
          lw_eway_api_hdr-cess_amount          = lv_total_cess_value.
          lw_eway_api_hdr-cess_nonadvol_value  = lv_total_cessnoval_value.

          APPEND lw_eway_api_hdr TO gt_eway_api_hdr.
          CLEAR:lw_eway_api_hdr.


        ENDIF.

      ENDLOOP.


      IF gt_eway_api_hdr IS NOT INITIAL.
        SORT gt_eway_api_hdr .
        SORT gt_eway_api_itm .

        DELETE ADJACENT DUPLICATES FROM   gt_eway_api_hdr  COMPARING ALL FIELDS.
        DELETE ADJACENT DUPLICATES FROM  gt_einv_api_itm  COMPARING ALL FIELDS.

        lt_api_hdr = gt_eway_api_hdr .
        lt_api_itm = gt_eway_api_itm .
        CLEAR lv_error.
        CALL FUNCTION 'ZFM_EWAY_BILL_VALIDATION'
        EXPORTING
          im_ucomm      = gv_e_comm
          im_api_header = gt_eway_api_hdr
*         IM_API_ITEM   =
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
              im_token      = lw_token
              im_api_header = lt_api_hdr
              im_api_item   = lt_api_itm
            IMPORTING
              ex_return     = lw_return
              ex_messages   = lt_messages
              ex_ewaybill   = lt_ewaybill.

            IF lt_ewaybill IS NOT INITIAL."lw_return EQ 'S'.
              LOOP AT lt_ewaybill ASSIGNING <lw_ewaybill>.
*      READ TABLE gt_final INTO gw_final WITH KEY vbeln = <lw_ewaybill>-docno.
                IF sy-subrc = 0.
*        <lw_ewaybill>-gjahr = gw_final-gjahr.
*        <lw_ewaybill>-doctyp = gw_final-fkart.
                ENDIF.
              ENDLOOP.
              MODIFY j_1ig_ewaybill FROM TABLE lt_ewaybill.
              IF sy-subrc EQ 0.
                COMMIT WORK.

                LOOP AT lt_ewaybill INTO lw_ewaybill.
*        READ TABLE gt_final ASSIGNING <gw_final> WITH KEY vbeln = lw_ewaybill-docno.
                  IF sy-subrc EQ 0.
*          <gw_final>-eway_num = lw_ewaybill-ebillno.

*          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
*            EXPORTING
*              date_internal            = lw_ewaybill-egen_dat
*            IMPORTING
*              date_external            = <gw_final>-eway_date
*            EXCEPTIONS
*              date_internal_is_invalid = 1
*              OTHERS                   = 2.
*          IF sy-subrc <> 0.
*            MESSAGE 'Date conversion error'(m02) TYPE 'I'.
*          ENDIF.

*          <gw_final>-eway_ernam = lw_ewaybill-ernam.

*          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
*            EXPORTING
*              date_internal            = lw_ewaybill-erdat
*            IMPORTING
*              date_external            = <gw_final>-eway_erdat
*            EXCEPTIONS
*              date_internal_is_invalid = 1
*              OTHERS                   = 2.
*          IF sy-subrc <> 0.
*            MESSAGE 'Date conversion error'(m02) TYPE 'I'.
*          ENDIF.

*          <gw_final>-eway_erzet = lw_ewaybill-egen_time.


                  ENDIF.
                ENDLOOP.

*            CLEAR lt_show_message.
*            IF  lt_messages IS NOT INITIAL.
*              LOOP AT gt_index INTO lw_index.

*          READ TABLE gt_final ASSIGNING <gw_final> INDEX lw_index-index.
*                IF sy-subrc EQ 0.

*                  CLEAR: lv_string,lw_etransport.
*            LOOP AT lt_messages INTO lw_message WHERE message_v3 = <gw_final>-vbeln.
*                  IF lw_message-type = 'S'.
*                <gw_final>-eway_print = lw_message-message_v4.
*                UPDATE zteway_transport SET eway_print = lw_message-message_v4
*                                            WHERE  bukrs = <gw_final>-bukrs AND
*                                                   gjahr = <gw_final>-gjahr AND
*                                                   docno = lw_message-message_v3 AND
*                                                   doctyp = <gw_final>-fkart.
*                IF sy-subrc IS NOT INITIAL.
*                  MOVE-CORRESPONDING <gw_final> TO lw_etransport.
*                  lw_etransport-t_date = <gw_final>-fkdat_db.
*                  lw_etransport-doctyp = <gw_final>-fkart.
*                  lw_etransport-docno = <gw_final>-vbeln.
*                  lw_etransport-eway_print = lw_message-message_v4.
*                  lw_etransport-ernam = sy-uname.
*                  lw_etransport-erdat = sy-datum.
*
*                  MODIFY zteway_transport FROM lw_etransport.
*                  IF sy-subrc IS NOT INITIAL.
*                    MESSAGE 'Data update error'(022) TYPE 'I'.
*                  ENDIF.
*                ENDIF.
*                CONTINUE.
*                  ELSEIF lw_message-type = 'E'.

*                    CONCATENATE lv_string lw_message-message INTO lv_string SEPARATED BY '||'.

*                  ENDIF.

*                ENDLOOP.
*                IF lv_string IS NOT INITIAL.
*                  <gw_final>-eway_error = lv_string.
*                  UPDATE zteway_transport SET eway_error = lv_string
*                                              WHERE  bukrs = <gw_final>-bukrs AND
*                                                     gjahr = <gw_final>-gjahr AND
*                                                     docno = lw_message-message_v3 AND
*                                                     doctyp = <gw_final>-fkart.
*                  IF sy-subrc IS NOT INITIAL.
*                    MOVE-CORRESPONDING <gw_final> TO lw_etransport.
*                    lw_etransport-t_date = <gw_final>-fkdat_db.
*                    lw_etransport-doctyp = <gw_final>-fkart.
*                    lw_etransport-docno = <gw_final>-vbeln.
*                    lw_etransport-ernam = sy-uname.
*                    lw_etransport-erdat = sy-datum.
*                    MODIFY zteway_transport FROM lw_etransport.
*                    IF sy-subrc IS NOT INITIAL.
*
*                    ENDIF.
*                  ENDIF.
*                ENDIF.

*              ENDIF.
*            ENDLOOP.
              ENDIF.


            ENDIF.

          ENDIF.


        ELSE.

        ENDIF.

      ENDIF.
    ENDIF.




ENDFUNCTION.
