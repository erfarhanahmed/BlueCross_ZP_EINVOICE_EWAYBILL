FUNCTION ZFM_EWAY_BILL_AUTO_GENERATE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_INVOICE) TYPE  VBELN
*"----------------------------------------------------------------------

  TYPES lr_fkart_type TYPE RANGE OF fkart.
  DATA : lr_fkart TYPE lr_fkart_type.

  DATA:lw_vbrk     TYPE ty_vbrk,
        lt_vbrp     TYPE TABLE OF ty_vbrp,
        lt_konv     TYPE TABLE OF ty_konv,
        lt_marc     TYPE TABLE OF ty_marc,
        lt_vbpa     TYPE TABLE OF ty_vbpa,
        lt_kna1     TYPE TABLE OF ty_kna1,
        lt_t001w    TYPE TABLE OF ty_t001w1,
        lt_t005u    TYPE TABLE OF ty_t005u,
        lt_adrc     TYPE TABLE OF ty_adrc,
        lt_adr6     TYPE TABLE OF ty_adr6,
        lt_api      TYPE TABLE OF ty_api,
        lt_doc_uom  TYPE TABLE OF ty_doc_uom,
        lt_export   TYPE TABLE OF ty_export,
        lw_konv     TYPE ty_konv,
        lw_doc_uom  TYPE ty_doc_uom,
        lw_marc     TYPE ty_marc,
        lw_gstin    TYPE  ty_j_1bbranch,
        lw_docty    TYPE  ty_doctyp,
        lw_usrgstin TYPE ty_api,
        lw_t001w    TYPE ty_t001w1,
        lw_t005u    TYPE ty_t005u,
        lw_adrc     TYPE ty_adrc,
        lw_vbrp     TYPE ty_vbrp,
        lw_adr6     TYPE ty_adr6,
        lw_kna1     TYPE ty_kna1,
        lw_vbpa     TYPE ty_vbpa,
        lw_export   TYPE ty_export,
        lv_export   TYPE xfeld,
        lv_exp_curr TYPE xfeld.

  DATA: lv_supply_type     TYPE char10,
        lv_document_type   TYPE char10,
        lv_sub_supply_type TYPE char10,
        lv_document_number TYPE I VALUE 1.

  DATA: lv_total_invoice_value   TYPE netwr,
        lv_total_sgst_value      TYPE netwr,
        lv_total_cgst_value      TYPE netwr,
        lv_total_igst_value      TYPE netwr,
        lv_total_cess_value      TYPE netwr,
        lv_total_cessnoval_value TYPE netwr,
        lv_total_taxable_value   TYPE netwr,
        lv_total_other_value     TYPE netwr,
        lv_diff_value            TYPE netwr,
        lv_output_uom            TYPE meins,
        lv_error                 TYPE xfeld,
        lw_token                 TYPE string.

  DATA:lt_show_message TYPE esp1_message_tab_type,
        lw_show_message LIKE LINE OF lt_show_message,
        lw_return       TYPE string,
        lt_messages     TYPE bapiret2_t,
        lw_message      TYPE bapiret2,
        lv_gjahr(4)     TYPE C.


  DATA:lt_supply_values     TYPE TABLE OF   dd07v,
        lt_sub_supply_values TYPE TABLE OF   dd07v,
        lt_doctyp_values     TYPE TABLE OF   dd07v,
        lt_t_mode_values     TYPE TABLE OF   dd07v,
        lt_v_type_values     TYPE TABLE OF   dd07v,
        ls_values            TYPE dd07v,
        lv_bill_ship         TYPE xfeld,
        lv_dispatch          TYPE xfeld.


  DATA: lt_eway_api_hdr TYPE ztt_eway_api_struct,
        lt_eway_api_itm TYPE ztt_eway_api_struct_itm.

  DATA:lw_eway_api_hdr TYPE zst_eway_api_struct,
        lw_eway_api_itm TYPE zst_eway_api_struct_itm.

  DATA:lt_ewaybill   TYPE TABLE OF j_1ig_ewaybill,
        lt_etransport TYPE TABLE OF zteway_transport,
        lw_etransport TYPE zteway_transport,
        lv_eway_error TYPE string.

  DATA:lt_print_data TYPE TABLE OF zteway_transport.

  DATA:lt_vbpa_trn TYPE TABLE OF vbpa,
        lt_Adrc_new TYPE TABLE OF adrc,
        lt_lfa1_trn TYPE TABLE OF lfa1,
        lt_inbound  TYPE TABLE OF zcrac_inbound_dt,
        lt_likp_trn TYPE TABLE OF likp,
        lv_tdname   TYPE thead-tdname,
        lv_tdline   TYPE tdline,
        lv_tdline1  TYPE tdline.


  DATA: lw_lines TYPE tline,
        lt_lines TYPE TABLE OF tline.
  RANGES : rg_diff FOR prcd_elements-kschl.

  SELECT * FROM tvarvc INTO TABLE @DATA(it_diff) WHERE name = 'EDOC_ROUND_OFF'.

  LOOP AT it_diff INTO DATA(wa_diff) .
    rg_diff-low = wa_diff-low.
    rg_diff-option = 'EQ'.
    rg_diff-SIGN = 'I'.
    APPEND rg_diff.
  ENDLOOP.


  CONSTANTS: gc_tobject_vbbk TYPE tdobject VALUE 'VBBK',
  gc_v_type_1     TYPE zde_v_type VALUE '1'.

  CONSTANTS lv_parvw TYPE parvw VALUE 'SP'.
  FIELD-SYMBOLS: <lw_ewaybill> TYPE j_1ig_ewaybill.

  lr_fkart = VALUE lr_fkart_type(
  LET s = 'I'
  O = 'EQ'
  IN SIGN   = s
  option = O
  ( low = 'ZDOM' )
  ( low = 'ZEXS' )
  ( low = 'ZEXR' )
  ( low = 'ZEXL' )
  ( low = 'ZEXD' )
  ( low = 'ZSEZ' )
  ).

  IF im_invoice IS NOT INITIAL.
    CLEAR:lt_eway_api_hdr,lt_eway_api_itm.
    SELECT SINGLE vbeln fkart waerk vkorg vtweg
    knumv fkdat kurrf bukrs netwr ernam kunrg
    kunag xblnr mwsbk bupla  FROM vbrk INTO lw_vbrk WHERE vbeln = im_invoice.
    IF sy-subrc IS INITIAL.
* Check this document is valid for e-way bill instant creation
      SELECT SINGLE bukrs
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
      batch FROM zteinv_doctyp INTO lw_docty WHERE bukrs = lw_vbrk-bukrs
      AND zmodule = gc_sd
      AND fkart = lw_vbrk-fkart
      AND eway = abap_true
      AND instgen = abap_true.
      IF sy-subrc IS INITIAL.

* Select Item details
        SELECT vbeln posnr fkimg meins fklmg ntgew brgew
        gsber	kursk
        netwr
        vbelv
        posnv
        vgbel
        vgpos
        vgtyp
        aubel
        matnr
        arktx
        charg
        matkl
        pstyv
        posar
        werks
        kostl
        shkzg
        ernam
        lgort
        prctr
        mwsbp	 vrkme FROM vbrp INTO TABLE lt_vbrp WHERE vbeln = lw_vbrk-vbeln.

        IF lt_vbrp  IS NOT INITIAL.


* Get transporter details
          DATA(lt_vbrp_temp) = lt_vbrp.
          SORT lt_vbrp_temp BY vgbel.
          DELETE ADJACENT DUPLICATES FROM lt_vbrp_temp COMPARING vgbel.

          IF lt_vbrp_temp IS NOT INITIAL.
            SELECT * FROM likp INTO TABLE lt_likp_trn
            FOR ALL ENTRIES IN lt_vbrp_temp
            WHERE vbeln = lt_vbrp_temp-vgbel.
            IF sy-subrc IS INITIAL.
              SELECT * FROM vbpa INTO TABLE lt_vbpa_trn
              FOR ALL ENTRIES IN lt_likp_trn
              WHERE vbeln = lt_likp_trn-vbeln.
*                                    AND parvw = 'SP'.

              IF lt_vbpa_trn IS NOT INITIAL.
                SELECT * FROM lfa1 INTO TABLE lt_lfa1_trn FOR ALL ENTRIES IN lt_vbpa_trn
                WHERE lifnr =  lt_vbpa_trn-lifnr.
              ENDIF.

              SELECT * FROM  zcrac_inbound_dt INTO TABLE lt_inbound
              FOR ALL ENTRIES IN lt_likp_trn
              WHERE pono = lt_likp_trn-vbeln.


            ENDIF.
          ENDIF.

* get the API data

          SELECT apiid apiprov  FROM zteinv_api INTO TABLE lt_api.

*** Fetch UOM Mapping
          SELECT edoc_uom meins  FROM ztdoc_uom INTO TABLE lt_doc_uom WHERE edoc_uom IS NOT NULL.

* Get export port details.
          SELECT zport_code
          zport
          land1
          zport_state
          zport_address1
          zport_address2
          zport_place
          zport_pincode FROM ztedoc_export INTO TABLE lt_export
          WHERE land1 = gc_land1_in.


          SELECT SINGLE bukrs branch name adrnr gstin FROM j_1bbranch
          INTO lw_gstin
          WHERE bukrs = lw_vbrk-bukrs
          AND branch = lw_vbrk-bupla.


          SELECT werks name1 name2 stras pstlz ort01 land1 regio
          counc cityc adrnr spras  zone1 j_1bbranch FROM t001w
          INTO TABLE lt_t001w
          FOR ALL ENTRIES IN lt_vbrp
          WHERE werks EQ lt_vbrp-werks.

          IF sy-subrc IS INITIAL.

            SELECT spras land1 bland bezei FROM t005u
            INTO TABLE lt_t005u
            FOR ALL ENTRIES IN lt_t001w
            WHERE land1 EQ lt_t001w-land1
            AND bland EQ lt_t001w-regio
            AND spras EQ sy-langu.
            IF sy-subrc EQ 0.
              SELECT addrnumber tel_number FROM adrc
              INTO TABLE lt_adrc
              FOR ALL ENTRIES IN lt_t001w
              WHERE addrnumber = lt_t001w-adrnr.
              IF sy-subrc EQ 0.
                SELECT addrnumber smtp_addr  FROM adr6
                INTO TABLE lt_adr6
                FOR ALL ENTRIES IN lt_adrc
                WHERE addrnumber = lt_adrc-addrnumber.
              ENDIF.
            ENDIF.
          ENDIF.


*  Condition value
          SELECT knumv kposn  kschl kbetr kwert FROM prcd_elements INTO TABLE lt_konv
          WHERE knumv = lw_vbrk-knumv
          AND kinak = abap_false.



* Get material details for HSN, and other fields
          SELECT matnr werks steuc FROM marc INTO TABLE lt_marc
          FOR ALL ENTRIES IN lt_vbrp
          WHERE matnr = lt_vbrp-matnr
          AND werks = lt_vbrp-werks.


*  Select partners
          SELECT vbeln posnr parvw kunnr lifnr adrnr FROM vbpa INTO TABLE lt_vbpa
          WHERE vbeln = lw_vbrk-vbeln.

          IF lt_vbpa IS NOT INITIAL.

            SELECT * FROM adrc INTO TABLE lt_adrc_new FOR ALL ENTRIES IN lt_vbpa
            WHERE addrnumber = lt_vbpa-adrnr.

            SELECT kunnr land1 name1 name2 ort01 pstlz
            regio stras telf1 adrnr stcd3 FROM kna1
            INTO TABLE lt_kna1
            FOR ALL ENTRIES IN lt_vbpa
            WHERE kunnr = lt_vbpa-kunnr.

            IF lt_kna1 IS NOT INITIAL.
              SELECT spras land1 bland bezei FROM t005u
              APPENDING TABLE lt_t005u
              FOR ALL ENTRIES IN lt_kna1
              WHERE land1 EQ lt_kna1-land1
              AND bland EQ lt_kna1-regio
              AND spras EQ sy-langu.
              IF sy-subrc EQ 0.

                SELECT addrnumber tel_number FROM adrc   APPENDING TABLE lt_adrc
                FOR ALL ENTRIES IN lt_t001w
                WHERE addrnumber = lt_t001w-adrnr.

                IF sy-subrc EQ 0.
                  SELECT  addrnumber smtp_addr FROM adr6
                  APPENDING TABLE lt_adr6
                  FOR ALL ENTRIES IN lt_adrc
                  WHERE addrnumber = lt_adrc-addrnumber.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.


* Set all the values
        lw_eway_api_hdr-bukrs       = lw_vbrk-bukrs.
        READ TABLE lt_api  INTO lw_usrgstin WITH KEY apiid = gc_apiid_eusrgstin.
        IF sy-subrc IS INITIAL.
          lw_eway_api_hdr-user_gstin = lw_usrgstin-apiprov.
        ELSE.
          lw_eway_api_hdr-user_gstin = lw_gstin-gstin.
        ENDIF.
        CONCATENATE '"' lw_eway_api_hdr-user_gstin '"' INTO lw_eway_api_hdr-user_gstin.

        lv_supply_type = lw_docty-sup_type.
        lv_sub_supply_type = lw_docty-sub_type.
        lv_document_type = lw_docty-edoc_type.

        CALL FUNCTION 'GET_DOMAIN_VALUES'
        EXPORTING
          domname         = 'ZDOM_SUP_TYPE'
        TABLES
          values_tab      = lt_supply_values
        EXCEPTIONS
          no_values_found = 1
          OTHERS          = 2.
        IF sy-subrc = 0.

          READ TABLE lt_supply_values INTO ls_values WITH KEY domvalue_l = lv_supply_type.
          IF sy-subrc EQ 0.
            TRANSLATE ls_values-ddtext TO UPPER CASE.
            lw_eway_api_hdr-supply_type = ls_values-ddtext.
            CONCATENATE '"' lw_eway_api_hdr-supply_type '"' INTO lw_eway_api_hdr-supply_type.
          ENDIF.

        ENDIF.


        CALL FUNCTION 'GET_DOMAIN_VALUES'
        EXPORTING
          domname         = 'ZDOM_SUB_TYPE'
        TABLES
          values_tab      = lt_sub_supply_values
        EXCEPTIONS
          no_values_found = 1
          OTHERS          = 2.
        IF sy-subrc = 0.
          READ TABLE lt_sub_supply_values INTO ls_values WITH KEY domvalue_l = lv_sub_supply_type.
          IF sy-subrc EQ 0.
            TRANSLATE ls_values-ddtext TO UPPER CASE.
            CONCATENATE '"' ls_values-ddtext '"' INTO ls_values-ddtext.
            lw_eway_api_hdr-sub_supply_type = ls_values-ddtext.
            lw_eway_api_hdr-sub_supply_description = ls_values-ddtext.
          ENDIF.
        ENDIF.

        CALL FUNCTION 'GET_DOMAIN_VALUES'
        EXPORTING
          domname         = 'ZDOM_EDOC_TYPE'
        TABLES
          values_tab      = lt_doctyp_values
        EXCEPTIONS
          no_values_found = 1
          OTHERS          = 2.
        IF sy-subrc = 0.

          READ TABLE lt_doctyp_values INTO ls_values WITH KEY domvalue_l = lv_document_type.
          IF sy-subrc EQ 0.
            TRANSLATE ls_values-ddtext TO UPPER CASE.
            CONCATENATE '"' ls_values-ddtext '"' INTO ls_values-ddtext.
            lw_eway_api_hdr-document_type = ls_values-ddtext.
          ENDIF.

        ENDIF.

* Get Fiscl year
        CALL FUNCTION 'GM_GET_FISCAL_YEAR'
        EXPORTING
          i_date                     = lw_vbrk-fkdat
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
          lw_eway_api_hdr-doc_year = lv_gjahr.
        ENDIF.


        CASE lv_document_number.
        WHEN 1.   "Billing Number as same as ODN
          lw_eway_api_hdr-document_number = lw_vbrk-vbeln.
        WHEN 2.   "ODN
          lw_eway_api_hdr-document_number =  lw_vbrk-xblnr.
        ENDCASE.
        CONCATENATE '"' lw_eway_api_hdr-document_number '"' INTO lw_eway_api_hdr-document_number.
        lw_eway_api_hdr-vbeln = lw_vbrk-vbeln.

        CONCATENATE '"'
        lw_vbrk-fkdat+6(2) '/'
        lw_vbrk-fkdat+4(2) '/'
        lw_vbrk-fkdat+0(4) '"'
        INTO lw_eway_api_hdr-document_date.

*** Supplier details
        CLEAR: lw_t001w. ", lw_adrc_s, lw_adr6_s.
        READ TABLE lt_vbrp INTO lw_vbrp INDEX 1.
        IF sy-subrc IS INITIAL.
          READ TABLE lt_t001w INTO lw_t001w WITH  KEY werks = lw_vbrp-werks.
          IF sy-subrc EQ 0.

            READ TABLE lt_api  INTO lw_usrgstin WITH KEY apiid = gc_apiid_esupgstin.
            IF sy-subrc IS INITIAL.
              lw_eway_api_hdr-gstin_of_consignor =  lw_usrgstin-apiprov.
            ELSE.
              lw_eway_api_hdr-gstin_of_consignor = lw_gstin-gstin.
            ENDIF.
            CONCATENATE '"' lw_eway_api_hdr-gstin_of_consignor '"' INTO lw_eway_api_hdr-gstin_of_consignor.


            CONCATENATE '"'  gc_name1 '"' INTO lw_eway_api_hdr-legal_name_of_consignor.
            CONCATENATE '"' lw_t001w-stras '"' INTO lw_eway_api_hdr-address1_of_consignor.
*            CONCATENATE '"' lw_t001w-name2 '"' INTO lw_eway_api_hdr-address2_of_consignor.
            lw_eway_api_hdr-address2_of_consignor = '""'.
            CONCATENATE '"' lw_t001w-ort01 '"' INTO lw_eway_api_hdr-place_of_consignor.
            CONCATENATE '"' lw_t001w-pstlz '"' INTO lw_eway_api_hdr-pincode_of_consignor.


            CLEAR:lw_t005u.
            READ TABLE lt_t005u INTO lw_t005u WITH  KEY bland = lw_t001w-regio.
            IF sy-subrc EQ 0.
              TRANSLATE lw_t005u-bezei TO UPPER CASE.
              CONCATENATE '"' lw_t005u-bezei '"' INTO  lw_t005u-bezei.
              lw_eway_api_hdr-state_of_consignor = lw_t005u-bezei.
            ENDIF.
            lw_eway_api_hdr-actual_from_state_name = lw_t005u-bezei.

            CLEAR lv_dispatch.
            IF lw_vbrk-fkart = 'ZTRD'.
              lv_tdname  = lw_vbrp-vbeln.
              PERFORM readtext_value USING lv_tdname 'Z023' CHANGING lv_TDLINE1.
              PERFORM readtext_value USING lv_tdname 'Z024' CHANGING lv_TDLINE.
              IF lv_tdline IS NOT INITIAL.
                CONCATENATE lv_tdline1 lv_TDLINE INTO lw_eway_api_hdr-address1_of_consignor SEPARATED BY space.
*                lw_eway_api_hdr-address1_of_consignor = lv_tdline.
                lw_eway_api_hdr-address2_of_consignor = '""'.
                lv_dispatch = abap_true.
              ENDIF.
              PERFORM readtext_value USING lv_tdname 'Z025' CHANGING lv_TDLINE.
              IF lv_tdline IS NOT INITIAL.
                lw_eway_api_hdr-place_of_consignor = lv_tdline.
              ENDIF.
              PERFORM readtext_value USING lv_tdname 'Z026' CHANGING lv_TDLINE.
              IF lv_tdline IS NOT INITIAL.
                lw_eway_api_hdr-pincode_of_consignor = lv_tdline.
              ENDIF.
              PERFORM readtext_value USING lv_tdname 'Z027' CHANGING lv_TDLINE.
              IF lv_tdline IS NOT INITIAL.
                lw_eway_api_hdr-actual_from_state_name = lv_tdline.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

* Buyer details

        CLEAR:lw_kna1, lw_adrc, lw_adr6,lv_bill_ship.
        READ TABLE lt_vbpa INTO lw_vbpa WITH KEY vbeln = lw_vbrk-vbeln
        parvw = gc_parvw_re.
        IF sy-subrc IS INITIAL.
          READ TABLE lt_vbpa INTO DATA(lw_vbpa_ship) WITH KEY vbeln = lw_vbrk-vbeln
                parvw = gc_parvw_we.
          IF sy-subrc IS INITIAL.
            IF lw_vbpa-kunnr NE lw_vbpa_ship-kunnr.
              lv_bill_ship = abap_true.
            ENDIF.

            READ TABLE lt_adrc_new INTO DATA(ls_adrc_re) WITH KEY addrnumber  = lw_vbpa-adrnr.
            IF sy-subrc = 0.
              READ TABLE lt_adrc_new INTO DATA(ls_adrc_we) WITH KEY addrnumber  = lw_vbpa_ship-adrnr.
              IF sy-subrc IS INITIAL.
                IF ls_adrc_re-post_code1 NE ls_adrc_we-post_code1.
                  lv_bill_ship = abap_true.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
          READ TABLE lt_kna1 INTO lw_kna1 WITH  KEY kunnr = lw_vbpa-kunnr.
          IF sy-subrc EQ 0.

            READ TABLE lt_api  INTO lw_usrgstin WITH KEY apiid = gc_apiid_ebuygstin.
            IF sy-subrc IS INITIAL.
              lw_eway_api_hdr-gstin_of_consignee = lw_usrgstin-apiprov.
            ELSE.
              lw_eway_api_hdr-gstin_of_consignee = lw_kna1-stcd3.
            ENDIF.

            CONCATENATE '"' lw_eway_api_hdr-gstin_of_consignee '"' INTO lw_eway_api_hdr-gstin_of_consignee.

            CONCATENATE '"' lw_kna1-name1 '"' INTO lw_eway_api_hdr-legal_name_of_consignee.


            CLEAR:lw_t005u.
            READ TABLE lt_t005u INTO lw_t005u WITH  KEY bland = lw_kna1-regio.
            IF sy-subrc EQ 0.
              TRANSLATE lw_t005u-bezei TO UPPER CASE.
              CONCATENATE '"' lw_t005u-bezei '"' INTO lw_eway_api_hdr-state_of_supply.
            ENDIF.


            READ TABLE lt_vbpa INTO lw_vbpa WITH  KEY vbeln = lw_vbrk-vbeln
            parvw = gc_parvw_we.
            IF sy-subrc IS INITIAL.
              READ TABLE lt_kna1 INTO lw_kna1 WITH KEY kunnr = lw_vbpa-kunnr.
              IF sy-subrc IS INITIAL.
*              lw_eway_api_hdr-address1_of_consignee   = lw_kna1-name1.
*              CONCATENATE '"' lw_eway_api_hdr-address1_of_consignee '"' INTO lw_eway_api_hdr-address1_of_consignee.
*              lw_eway_api_hdr-address2_of_consignee   = lw_kna1-stras.
*              CONCATENATE '"' lw_eway_api_hdr-address2_of_consignee '"' INTO lw_eway_api_hdr-address2_of_consignee.
*              CONCATENATE '"' lw_kna1-ort01 '"' INTO lw_eway_api_hdr-place_of_consignee.
*              lw_eway_api_hdr-pincode_of_consignee    = lw_kna1-pstlz.
*              CONCATENATE '"' lw_eway_api_hdr-pincode_of_consignee  '"' INTO lw_eway_api_hdr-pincode_of_consignee.


                READ TABLE lt_adrc INTO lw_adrc WITH  KEY addrnumber = lw_kna1-adrnr.
                READ TABLE lt_adr6 INTO lw_adr6 WITH  KEY addrnumber = lw_adrc-addrnumber.

*              CLEAR:lw_t005u.
*              READ TABLE lt_t005u INTO lw_t005u WITH  KEY bland = lw_kna1-regio.
*              IF sy-subrc EQ 0.
*                TRANSLATE lw_t005u-bezei TO UPPER CASE.
*                CONCATENATE '"' lw_t005u-bezei '"' INTO lw_eway_api_hdr-actual_to_state_name .
*              ENDIF.


                READ TABLE lt_adrc_new INTO DATA(ls_adrc_1) WITH KEY addrnumber  =  lw_vbpa-adrnr.
                IF sy-subrc IS INITIAL.
                  CONCATENATE ls_adrc_1-name1 ls_adrc_1-street INTO lw_eway_api_hdr-address1_of_consignee.
                  CONCATENATE '"' lw_eway_api_hdr-address1_of_consignee '"' INTO lw_eway_api_hdr-address1_of_consignee.
                  lw_eway_api_hdr-address2_of_consignee   = ls_adrc_1-str_suppl3.
                  CONCATENATE '"' lw_eway_api_hdr-address2_of_consignee '"' INTO lw_eway_api_hdr-address2_of_consignee.
                  CONCATENATE '"' ls_adrc_1-city1 '"' INTO lw_eway_api_hdr-place_of_consignee.
                  lw_eway_api_hdr-pincode_of_consignee    = ls_adrc_1-post_code1.
                  CONCATENATE '"' lw_eway_api_hdr-pincode_of_consignee  '"' INTO lw_eway_api_hdr-pincode_of_consignee.

                  CLEAR:lw_t005u.
                  READ TABLE lt_t005u INTO lw_t005u WITH  KEY bland = ls_adrc_1-region.
                  IF sy-subrc EQ 0.
                    TRANSLATE lw_t005u-bezei TO UPPER CASE.
                    CONCATENATE '"' lw_t005u-bezei '"' INTO lw_eway_api_hdr-actual_to_state_name .
                  ENDIF.
                ENDIF.


                CLEAR lv_export.
                IF lw_t001w-land1 NE lw_kna1-land1.
                  lv_export = abap_true.
                ENDIF.

                IF lw_eway_api_hdr-sub_supply_type      = gc_sub_suptyp_exp OR lv_export IS NOT INITIAL..
                  lw_eway_api_hdr-state_of_supply       = gc_export_place.
                  lw_eway_api_hdr-gstin_of_consignee    = gc_gstin_urp.
                  READ TABLE lt_export INTO lw_export WITH KEY zport = 'Dummy'.
                  IF sy-subrc IS INITIAL..
                    CONCATENATE '"' lw_export-zport_address1 '"' INTO lw_eway_api_hdr-address1_of_consignee.
                    CONCATENATE '"' lw_export-zport_address2 '"' INTO lw_eway_api_hdr-address2_of_consignee.
                    CONCATENATE '"' lw_export-zport_place '"' INTO lw_eway_api_hdr-place_of_consignee.
                    CONCATENATE '"' lw_export-zport_pincode '"' INTO lw_eway_api_hdr-pincode_of_consignee.
                    CLEAR:lw_t005u.
                    READ TABLE lt_t005u INTO lw_t005u WITH  KEY bland = lw_export-zport_state.
                    IF sy-subrc EQ 0.
                      TRANSLATE lw_t005u-bezei TO UPPER CASE.
                      CONCATENATE '"' lw_t005u-bezei '"' INTO lw_eway_api_hdr-actual_to_state_name.
                    ENDIF.
                  ENDIF.
                ENDIF.

*                IF ( ( lw_eway_api_hdr-state_of_consignor = lw_eway_api_hdr-actual_from_state_name ) AND
*                    ( lw_eway_api_hdr-state_of_supply = lw_eway_api_hdr-actual_to_state_name ) ).
*                  lw_eway_api_hdr-transaction_type = 1.
*                ELSEIF ( ( lw_eway_api_hdr-state_of_consignor = lw_eway_api_hdr-actual_from_state_name ) AND
*                   ( lw_eway_api_hdr-state_of_supply NE lw_eway_api_hdr-actual_to_state_name ) ).
*                  lw_eway_api_hdr-transaction_type = 2.
*                ELSEIF ( ( lw_eway_api_hdr-state_of_consignor NE lw_eway_api_hdr-actual_from_state_name ) AND
*                   ( lw_eway_api_hdr-state_of_supply = lw_eway_api_hdr-actual_to_state_name ) ).
*                  lw_eway_api_hdr-transaction_type = 3.
*                ELSEIF ( ( lw_eway_api_hdr-state_of_consignor NE lw_eway_api_hdr-actual_from_state_name ) AND
*                  ( lw_eway_api_hdr-state_of_supply NE lw_eway_api_hdr-actual_to_state_name ) ).
*                  lw_eway_api_hdr-transaction_type = 4.
*                ENDIF.
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
                lw_eway_api_hdr-data_source =  gc_dsource_erp.
                CONCATENATE '"' sy-uname '"' INTO lw_eway_api_hdr-user_ref.
                CONCATENATE '"' lw_kna1-ort01 '"' INTO lw_eway_api_hdr-location_code.


*& Generate E-Way Bill Part A & Part B.
                lw_eway_api_hdr-eway_bill_status = gc_ewaysts_abc.

*& Auto print E-way Bill

                lw_eway_api_hdr-auto_print = '" "'."gc_eway_print'.
                lw_eway_api_hdr-email = '""'."lw_adr6-smtp_addr'.
*                CONCATENATE '"' lw_eway_api_hdr-email '"' INTO lw_eway_api_hdr-email.


                IF lw_vbrk-fkdat IS NOT INITIAL.
                  CONCATENATE lw_vbrk-fkdat+6(2) '/'
                  lw_vbrk-fkdat+4(2) '/'
                  lw_vbrk-fkdat+0(4)
                  INTO lw_eway_api_hdr-transporter_document_date.
                ENDIF.


                IF lw_vbrk-fkart = 'ZTRD'.

                  CLEAR lv_tdname.
                  lv_tdname =  lw_vbrp-vbeln.


                  CALL FUNCTION 'READ_TEXT'
                  EXPORTING
                    CLIENT                  = sy-mandt
                    ID                      = 'Z016'
                    LANGUAGE                = sy-langu
                    name                    = lv_tdname
                    object                  = gc_tobject_vbbk
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
                    lw_eway_api_hdr-transporter_name = lw_lines-tdline.
                    lw_etransport-t_name  = lw_eway_api_hdr-transporter_name.
*                    CONCATENATE   ''  ''  INTO .
                  ENDIF.


                  CALL FUNCTION 'READ_TEXT'
                  EXPORTING
                    CLIENT                  = sy-mandt
                    ID                      = 'Z017'
                    LANGUAGE                = sy-langu
                    name                    = lv_tdname
                    object                  = gc_tobject_vbbk
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
                    lw_eway_api_hdr-vehicle_number = lw_lines-tdline.
                    lw_etransport-v_number = lw_eway_api_hdr-vehicle_number.
*                    CONCATENATE  ''  '' INTO .
                  ENDIF.

                  CALL FUNCTION 'READ_TEXT'
                  EXPORTING
                    CLIENT                  = sy-mandt
                    ID                      = 'Z018'
                    LANGUAGE                = sy-langu
                    name                    = lv_tdname
                    object                  = gc_tobject_vbbk
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
                    lw_eway_api_hdr-transporter_document_number = lw_lines-tdline .
                    lw_etransport-t_doc_no = lw_eway_api_hdr-transporter_document_number.
*                    CONCATENATE  '' '' INTO .
                  ENDIF.

                  CALL FUNCTION 'READ_TEXT'
                  EXPORTING
                    CLIENT                  = sy-mandt
                    ID                      = 'Z019'
                    LANGUAGE                = sy-langu
                    name                    = lv_tdname
                    object                  = gc_tobject_vbbk
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
                    lw_eway_api_hdr-transporter_id  =  lw_lines-tdline.
                    lw_etransport-t_id    = lw_eway_api_hdr-transporter_id.
*                    CONCATENATE  '' '' INTO  .
                  ENDIF.
                  lw_etransport-t_date  = lw_vbrk-fkdat.

*                  PERFORM readtext1 USING lv_tdname.
                  lw_eway_api_hdr-vehicle_type  = gc_v_type_1.
                  lw_etransport-v_type = lw_eway_api_hdr-vehicle_type.
                  lw_eway_api_hdr-t_mode     = '1'.
                  lw_etransport-t_mode  = lw_eway_api_hdr-transportation_mode.

                  CALL FUNCTION 'GET_DOMAIN_VALUES'
                  EXPORTING
                    domname         = 'ZDOM_V_TYPE'
                  TABLES
                    values_tab      = lt_v_type_values
                  EXCEPTIONS
                    no_values_found = 1
                    OTHERS          = 2.
                  IF sy-subrc = 0.
                    READ TABLE lt_v_type_values INTO ls_values WITH KEY domvalue_l = lw_eway_api_hdr-vehicle_type.
                    IF sy-subrc IS INITIAL.
                      TRANSLATE ls_values-ddtext TO UPPER CASE.
                      lw_eway_api_hdr-vehicle_type = ls_values-ddtext.
                    ENDIF.
                  ENDIF.

                  CALL FUNCTION 'GET_DOMAIN_VALUES'
                  EXPORTING
                    domname         = 'ZDOM_T_MODE'
                  TABLES
                    values_tab      = lt_t_mode_values
                  EXCEPTIONS
                    no_values_found = 1
                    OTHERS          = 2.
                  IF sy-subrc = 0.
                    READ TABLE lt_t_mode_values INTO ls_values WITH KEY domvalue_l = lw_eway_api_hdr-transportation_mode."lw_vbrk-zztraty.
                    IF sy-subrc EQ 0.

                      TRANSLATE ls_values-ddtext TO UPPER CASE.
                      lw_eway_api_hdr-transportation_mode = ls_values-ddtext.
                    ENDIF.
                  ENDIF.


                ELSE.
                  READ TABLE lt_likp_trn INTO DATA(ls_likp_trn) WITH KEY vbeln =    lw_vbrp-vgbel.
                  IF sy-subrc IS INITIAL.
                    lw_eway_api_hdr-transporter_document_number   = ls_likp_trn-bolnr.


                    lw_etransport-t_doc_no = lw_eway_api_hdr-transporter_document_number.
                    lw_etransport-t_date  = lw_vbrk-fkdat.


                    lw_eway_api_hdr-transportation_mode    = '1'.
                    CASE ls_likp_trn-vsart.
                    WHEN '01' OR '02'.
                      lw_eway_api_hdr-transportation_mode    = '1'.
                    WHEN '03'.
                      lw_eway_api_hdr-transportation_mode    = '2'.
                    WHEN '04'.
                      lw_eway_api_hdr-transportation_mode    = '4'.
                    ENDCASE.
                    lw_etransport-t_mode  = lw_eway_api_hdr-transportation_mode.

                    CALL FUNCTION 'GET_DOMAIN_VALUES'
                    EXPORTING
                      domname         = 'ZDOM_T_MODE'
                    TABLES
                      values_tab      = lt_t_mode_values
                    EXCEPTIONS
                      no_values_found = 1
                      OTHERS          = 2.
                    IF sy-subrc = 0.
                      READ TABLE lt_t_mode_values INTO ls_values WITH KEY domvalue_l = lw_eway_api_hdr-transportation_mode."lw_vbrk-zztraty.
                      IF sy-subrc EQ 0.

                        TRANSLATE ls_values-ddtext TO UPPER CASE.
                        lw_eway_api_hdr-transportation_mode = ls_values-ddtext.
                      ENDIF.
                    ENDIF.

                    READ TABLE lt_vbpa_trn INTO DATA(ls_vbpa_trn) WITH KEY vbeln = ls_likp_trn-vbeln
                          parvw = lv_parvw.
                    IF sy-subrc IS INITIAL.
                      READ TABLE lt_lfa1_trn INTO DATA(ls_lfa1_trn) WITH KEY
                            lifnr = ls_vbpa_trn-lifnr.
                      IF sy-subrc IS INITIAL.

                        lw_eway_api_hdr-transporter_id       =  ls_lfa1_trn-stcd3.
                        lw_eway_api_hdr-transporter_name     =  ls_lfa1_trn-name1.
                        lw_etransport-t_id    = lw_eway_api_hdr-transporter_id.
                        lw_etransport-t_name  = lw_eway_api_hdr-transporter_name.
                      ENDIF.
                    ENDIF.

                    IF lw_vbrk-fkart = 'ZDOM'.
                      IF lw_vbrp-matnr = 'PIG IRON(MOLTEN)' OR
                      lw_vbrp-matnr = 'PIG IRON(STEELGRA'.
                        lw_eway_api_hdr-transporter_id       =  '27AAACU9269Q1ZC'.
                        lw_eway_api_hdr-transporter_name     =  'EVONITH METALLICS LIMITED'.
                        lw_eway_api_hdr-vehicle_number   = ls_likp_trn-traid.
                        lw_eway_api_hdr-vehicle_type     = '1'.

                        lw_etransport-t_id    = lw_eway_api_hdr-transporter_id.
                        lw_etransport-t_name  = lw_eway_api_hdr-transporter_name.
                        lw_etransport-v_number = lw_eway_api_hdr-vehicle_number.
                        lw_etransport-v_type = lw_eway_api_hdr-vehicle_type.

                        CALL FUNCTION 'GET_DOMAIN_VALUES'
                        EXPORTING
                          domname         = 'ZDOM_V_TYPE'
                        TABLES
                          values_tab      = lt_v_type_values
                        EXCEPTIONS
                          no_values_found = 1
                          OTHERS          = 2.
                        IF sy-subrc = 0.
                          READ TABLE lt_v_type_values INTO ls_values WITH KEY domvalue_l = lw_eway_api_hdr-vehicle_type.
                          IF sy-subrc IS INITIAL.
                            TRANSLATE ls_values-ddtext TO UPPER CASE.
                            lw_eway_api_hdr-vehicle_type = ls_values-ddtext.
                          ENDIF.
                        ENDIF.

                      ENDIF.
                    ENDIF.


                    READ TABLE lt_inbound INTO DATA(lS_inbound) WITH KEY pono = ls_likp_trn-vbeln.
                    IF sy-subrc IS INITIAL.
                      lw_eway_api_hdr-vehicle_number   = ls_inbound-truckno.
                      lw_eway_api_hdr-vehicle_type     = '1'.
                      lw_etransport-v_number = lw_eway_api_hdr-vehicle_number.
                      lw_etransport-v_type = lw_eway_api_hdr-vehicle_type.

                      CALL FUNCTION 'GET_DOMAIN_VALUES'
                      EXPORTING
                        domname         = 'ZDOM_V_TYPE'
                      TABLES
                        values_tab      = lt_v_type_values
                      EXCEPTIONS
                        no_values_found = 1
                        OTHERS          = 2.
                      IF sy-subrc = 0.
                        READ TABLE lt_v_type_values INTO ls_values WITH KEY domvalue_l = lw_eway_api_hdr-vehicle_type.
                        IF sy-subrc IS INITIAL.
                          TRANSLATE ls_values-ddtext TO UPPER CASE.
                          lw_eway_api_hdr-vehicle_type = ls_values-ddtext.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.


                IF lw_eway_api_hdr-transporter_name = 'SELF TRANSPORT'.
                  lv_error = abap_true.
                ENDIF.

                IF lw_eway_api_hdr-transportation_distance IS INITIAL.
                  lw_eway_api_hdr-transportation_distance = '0'.
                  lw_etransport-t_distance = lw_eway_api_hdr-transportation_distance.
                ENDIF.
                CONCATENATE '"' lw_eway_api_hdr-transporter_id '"' INTO lw_eway_api_hdr-transporter_id.
                CONCATENATE '"' lw_eway_api_hdr-transporter_name '"' INTO lw_eway_api_hdr-transporter_name.
                CONCATENATE '"' lw_eway_api_hdr-transportation_mode '"' INTO lw_eway_api_hdr-transportation_mode.
                CONCATENATE '"' lw_eway_api_hdr-transporter_document_number '"' INTO lw_eway_api_hdr-transporter_document_number.
                CONCATENATE '"'  lw_eway_api_hdr-transporter_document_date '"' INTO lw_eway_api_hdr-transporter_document_date.
                CONCATENATE '"'  lw_eway_api_hdr-transportation_distance '"' INTO lw_eway_api_hdr-transportation_distance.
                CONCATENATE '"'  lw_eway_api_hdr-vehicle_number '"' INTO lw_eway_api_hdr-vehicle_number.
                CONCATENATE '"'  lw_eway_api_hdr-vehicle_type '"' INTO lw_eway_api_hdr-vehicle_type.


* check for export currency
                CLEAR lv_exp_curr.
                IF lw_vbrk-waerk NE  'INR' .
                  lv_exp_curr = abap_true.
                ENDIF.


                "default value
                lw_eway_api_hdr-other_value            = '0'.
                lw_eway_api_hdr-total_invoice_value    = '0'.
                lw_eway_api_hdr-taxable_amount         = '0'.
                lw_eway_api_hdr-cgst_amount            = '0'.
                lw_eway_api_hdr-sgst_amount            = '0'.
                lw_eway_api_hdr-igst_amount            = '0'.
                lw_eway_api_hdr-cess_amount            = '0'.
                lw_eway_api_hdr-cess_nonadvol_value    = '0'.



                LOOP AT lt_vbrp INTO lw_vbrp.

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

                  REPLACE ALL OCCURRENCES OF '"' IN lw_vbrp-arktx WITH space.
                  CONCATENATE '"' lw_vbrp-arktx '"' INTO lw_eway_api_itm-product_name.
                  CONCATENATE '"' lw_vbrp-arktx '"' INTO lw_eway_api_itm-product_description.
***** ---logic for hsncode based on billing type----added by raghu on 27.03.2023****

                  IF lw_vbrk-fkart IN lr_fkart.
                    lv_tdname  = lw_vbrp-vbeln.
                    CALL FUNCTION 'READ_TEXT'
                    EXPORTING
                      CLIENT                  = sy-mandt
                      ID                      = 'Z021'
                      LANGUAGE                = sy-langu
                      name                    = lv_tdname
                      object                  = gc_tobject_vbbk
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
                    IF sy-subrc = 0.
                      READ TABLE lt_lines INTO lw_lines INDEX 1.
                      IF lw_lines-tdline IS NOT INITIAL.
                        CONCATENATE  '"' lw_lines-tdline '"' INTO lw_eway_api_itm-hsn_code.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                  IF lw_eway_api_itm-hsn_code IS INITIAL.
                    READ TABLE lt_marc INTO lw_marc WITH KEY matnr = lw_vbrp-matnr
                    werks = lw_vbrp-werks.
                    IF sy-subrc IS INITIAL.
                      CONCATENATE '"' lw_marc-steuc  '"' INTO lw_eway_api_itm-hsn_code.

                    ENDIF.
                  ENDIF.
******------------end-----------------------------*****
                  lw_eway_api_itm-quantity         = lw_vbrp-fkimg.

                  CLEAR lv_output_uom.
                  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                  EXPORTING
                    INPUT          = lw_vbrp-vrkme
                  IMPORTING
                    OUTPUT         = lv_output_uom
                  EXCEPTIONS
                    unit_not_found = 1
                    OTHERS         = 2.
                  IF sy-subrc = 0.
                    READ TABLE lt_doc_uom INTO lw_doc_uom WITH KEY meins = lv_output_uom.
                    IF sy-subrc EQ 0.
                      CONCATENATE '"'  lw_doc_uom-edoc_uom '"' INTO lw_eway_api_itm-unit_of_product.
                    ELSE.
                      READ TABLE lt_doc_uom INTO lw_doc_uom WITH KEY meins = lw_vbrp-vrkme.
                      IF sy-subrc EQ 0.
                        CONCATENATE '"' lw_doc_uom-edoc_uom   '"' INTO lw_eway_api_itm-unit_of_product.
                      ENDIF.
                    ENDIF.
                  ENDIF.


                  IF lw_vbrk-fkart = 'ZSAM'.
                    READ TABLE lt_konv INTO lw_konv WITH KEY knumv = lw_vbrk-knumv
                    kposn = lw_vbrp-posnr
                    kschl = 'ZSAM'.
                    IF sy-subrc IS INITIAL.
                      lw_vbrp-netwr = lw_konv-kwert.
                    ENDIF.
                  ENDIF.


                  READ TABLE lt_konv INTO lw_konv WITH KEY knumv = lw_vbrk-knumv
                  kposn = lw_vbrp-posnr
                  kschl = 'ZCES'.
                  IF sy-subrc IS INITIAL.
                    lw_vbrp-netwr = lw_vbrp-netwr - lw_konv-kwert.

                  ENDIF.
                  LOOP AT lt_konv INTO lw_konv WHERE knumv = lw_vbrk-knumv
                  AND kposn = lw_vbrp-posnr
                  AND kschl = 'DIFF'.

                    lw_vbrp-netwr = lw_vbrp-netwr - lw_konv-kwert.
                    lv_diff_value = lv_diff_value + lw_konv-kwert.
                  ENDLOOP.


                  "Item Amount
                  lw_eway_api_itm-taxable_amount = lw_vbrp-netwr.
                  IF lv_exp_curr IS NOT INITIAL.
                    PERFORM convert_currency USING lw_eway_api_itm-taxable_amount lw_vbrk-kurrf
                    CHANGING lw_eway_api_itm-taxable_amount.
                  ENDIF.

                  "CGST Tax amount
                  READ TABLE lt_konv INTO lw_konv WITH KEY knumv = lw_vbrk-knumv
                  kposn = lw_vbrp-posnr
                  kschl = gc_kschl_jocg.
                  IF sy-subrc IS NOT INITIAL.
                    READ TABLE lt_konv INTO lw_konv WITH KEY knumv = lw_vbrk-knumv
                    kposn = lw_vbrp-posnr
                    kschl = 'ZCGS'.
                  ENDIF.
                  IF sy-subrc EQ 0.
                    IF lv_exp_curr IS NOT INITIAL.
                      PERFORM convert_currency USING lw_konv-kwert lw_vbrk-kurrf
                      CHANGING lw_konv-kwert.
                    ENDIF.
                    lw_eway_api_itm-cgst_rate = lw_konv-kbetr.
                    lv_total_cgst_value = lv_total_cgst_value + lw_konv-kwert.
                  ENDIF.
                  "SGST Tax amount
                  READ TABLE lt_konv INTO lw_konv WITH KEY knumv = lw_vbrk-knumv
                  kposn = lw_vbrp-posnr
                  kschl = gc_kschl_josg.

                  IF sy-subrc IS NOT INITIAL.
                    READ TABLE lt_konv INTO lw_konv WITH KEY knumv = lw_vbrk-knumv
                    kposn = lw_vbrp-posnr
                    kschl = gc_kschl_joug.
                    IF sy-subrc IS NOT INITIAL.
                      READ TABLE lt_konv INTO lw_konv WITH KEY knumv = lw_vbrk-knumv
                      kposn = lw_vbrp-posnr
                      kschl = 'ZSGS'.
                    ENDIF.
                  ENDIF.
                  IF sy-subrc EQ 0.
                    IF lv_exp_curr IS NOT INITIAL.
                      PERFORM convert_currency USING lw_konv-kwert lw_vbrk-kurrf
                      CHANGING lw_konv-kwert.
                    ENDIF.
                    lw_eway_api_itm-sgst_rate = lw_konv-kbetr.
                    lv_total_sgst_value = lv_total_sgst_value + lw_konv-kwert.
                  ENDIF.
                  "IGST Tax amount
                  READ TABLE lt_konv INTO lw_konv WITH KEY knumv = lw_vbrk-knumv
                  kposn = lw_vbrp-posnr
                  kschl = gc_kschl_joig.
                  IF sy-subrc IS NOT INITIAL.
                    READ TABLE lt_konv INTO lw_konv WITH KEY knumv = lw_vbrk-knumv
                    kposn = lw_vbrp-posnr
                    kschl = 'ZIGS'.
                  ENDIF.
                  IF sy-subrc EQ 0.
                    IF lv_exp_curr IS NOT INITIAL.
                      PERFORM convert_currency USING lw_konv-kwert lw_vbrk-kurrf
                      CHANGING lw_konv-kwert.
                    ENDIF.
                    lw_eway_api_itm-igst_rate = lw_konv-kbetr.
                    lv_total_igst_value = lv_total_igst_value + lw_konv-kwert.
                  ENDIF.
                  "CESS Tax amount
                  READ TABLE lt_konv INTO lw_konv WITH KEY knumv = lw_vbrk-knumv
                  kposn = lw_vbrp-posnr
                  kschl = gc_kschl_cess.
                  IF sy-subrc EQ 0.
                    IF lv_exp_curr IS NOT INITIAL.
                      PERFORM convert_currency USING lw_konv-kwert lw_vbrk-kurrf
                      CHANGING lw_konv-kwert.
                    ENDIF.
                    lw_eway_api_itm-cess_rate = lw_konv-kbetr.
                    lv_total_cess_value = lv_total_cess_value + lw_konv-kwert.
                  ENDIF.

                  "CESS non addl Tax amount
                  READ TABLE lt_konv INTO lw_konv WITH KEY knumv = lw_vbrk-knumv
                  kposn = lw_vbrp-posnr
                  kschl = 'ZCES'.
                  IF sy-subrc EQ 0.
                    IF lv_exp_curr IS NOT INITIAL.
                      PERFORM convert_currency USING lw_konv-kwert lw_vbrk-kurrf
                      CHANGING lw_konv-kwert.
                    ENDIF.
                    lw_eway_api_itm-cessnonadvol = lw_konv-kbetr.
                    lv_total_cessnoval_value = lv_total_cessnoval_value + lw_konv-kwert.
                  ENDIF.

                  "TCS
                  LOOP AT lt_konv INTO lw_konv WHERE knumv = lw_vbrk-knumv
                  AND kposn = lw_vbrp-posnr
                  AND ( kschl EQ 'JTC1' OR kschl EQ 'ZTCS' ).
                    IF lv_exp_curr IS NOT INITIAL.
                      PERFORM convert_currency USING lw_konv-kwert lw_vbrk-kurrf
                      CHANGING lw_konv-kwert.
                    ENDIF.
                    lv_total_other_value = lv_total_other_value + lw_konv-kwert.
                  ENDLOOP.

                  lv_total_taxable_value  = lv_total_taxable_value  + lw_eway_api_itm-taxable_amount.
                  APPEND lw_eway_api_itm TO lt_eway_api_itm.
                  CLEAR:lw_eway_api_itm.

                ENDLOOP.

**** Header total values
*                lv_total_taxable_value = lv_total_taxable_value - lv_diff_value.
                lw_eway_api_hdr-total_invoice_value  = lv_total_taxable_value +
                lv_total_cgst_value +
                lv_total_sgst_value +
                lv_total_igst_value +
                lv_total_cess_value +
                lv_total_cessnoval_value +
                lv_total_other_value.

                lw_eway_api_hdr-other_value          = lv_total_other_value + lv_diff_value.
                lw_eway_api_hdr-taxable_amount       = lv_total_taxable_value.
                lw_eway_api_hdr-cgst_amount          = lv_total_cgst_value.
                lw_eway_api_hdr-sgst_amount          = lv_total_sgst_value.
                lw_eway_api_hdr-igst_amount          = lv_total_igst_value.
                lw_eway_api_hdr-cess_amount          = lv_total_cess_value.
                lw_eway_api_hdr-cess_nonadvol_value  = lv_total_cessnoval_value.

                APPEND lw_eway_api_hdr TO lt_eway_api_hdr.
                CLEAR:lw_eway_api_hdr.


                IF lv_error IS INITIAL.
                  SELECT SINGLE apiuri FROM zteinv_api INTO lw_token WHERE apiid = gc_apiid_token.
                  IF lw_token IS INITIAL.
                    CALL FUNCTION 'ZFM_EINVOICE_OAUTH_API'
                    IMPORTING
                      ex_token = lw_token.
                  ENDIF.
                  IF lw_token IS NOT INITIAL.

                    CALL FUNCTION 'ZFM_EWAY_BILL_GENERATE_API'
                    EXPORTING
                      im_token          = lw_token
                      im_api_header     = lt_eway_api_hdr
                      im_api_item       = lt_eway_api_itm
                    IMPORTING
                      ex_return         = lw_return
                      ex_messages       = lt_messages
                      ex_ewaybill       = lt_ewaybill
                      ex_eway_transport = lt_print_data.

*                      ex_error      = lv_eway_error.

                    IF lt_ewaybill IS NOT INITIAL.
                      READ TABLE lt_ewaybill ASSIGNING <lw_ewaybill> INDEX 1.
                      IF <lw_ewaybill> IS ASSIGNED.

                        <lw_ewaybill>-gjahr = lv_gjahr.
                        <lw_ewaybill>-doctyp = lw_vbrk-fkart.


* Add entry to eway bill table
                        MODIFY j_1ig_ewaybill FROM TABLE lt_ewaybill.

* Add entry to transport table
                        READ TABLE lt_messages INTO lw_message INDEX 1.
                        IF sy-subrc IS INITIAL.

                          lw_etransport-bukrs   = lw_vbrk-bukrs.
                          lw_etransport-doctyp  = lw_vbrk-fkart.
                          lw_etransport-docno   = lw_vbrk-vbeln.
                          lw_etransport-gjahr   = lv_gjahr.

* This code to be replaced with client specific code
*                          lw_etransport-t_id    = lw_eway_api_hdr-transporter_id.
*                        lw_etransport-t_name  = lw_vbrk-zztran_name.
*                        lw_etransport-t_doc_no = lw_vbrk-zztran_num.
*                        lw_etransport-t_date  = lw_vbrk-zztran_date.
*                        lw_etransport-t_mode  = lw_vbrk-zztraty.
*                        lw_etransport-t_distance = lw_vbrk-zzdist.
*                        lw_etransport-v_number = lw_vbrk-zzvehicle_num.
*                        IF lw_vbrk-zzvehicle_typ = 'R'.
*                          lw_etransport-v_type = '1'.
*                        ELSEIF lw_vbrk-zzvehicle_typ = 'O'.
*                          lw_etransport-v_type = '2'.
*                        ENDIF.
                          lw_etransport-eway_print = lw_message-message_v4.
                          lw_etransport-eway_error = lv_eway_error.
                          lw_etransport-ernam = sy-uname.
                          lw_etransport-erdat = sy-datum.

* add the print url from new export table
                          READ TABLE lt_print_data INTO DATA(ls_print_data) INDEX 1.
                          IF sy-subrc IS INITIAL AND ls_print_data-eway_print IS NOT INITIAL.
                            lw_etransport-eway_print = ls_print_data-eway_print.
                          ENDIF.


                          MODIFY zteway_transport FROM lw_etransport.
                          IF sy-subrc IS INITIAL.
                            COMMIT WORK.
                          ENDIF.

                        ENDIF.

                      ENDIF.


                    ENDIF.


                  ENDIF.



                ENDIF.
              ENDIF.
            ENDIF.

          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.



ENDFUNCTION.
