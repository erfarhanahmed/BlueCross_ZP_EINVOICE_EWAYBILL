FUNCTION ZFM_EINVOICE_AUTO_GENERATE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_INVOICE) TYPE  VBELN
*"----------------------------------------------------------------------

  IF im_invoice IS NOT INITIAL.

    TYPES lr_fkart_type TYPE RANGE OF fkart.
    DATA : lr_fkart TYPE lr_fkart_type.

    DATA:lv_tdname   TYPE thead-tdname.
    DATA:lv_tdline   TYPE tdline.
    DATA : lt_lines TYPE STANDARD TABLE OF tline.
    DATA : ls_lines TYPE tline.

    DATA: lt_wb2_v_vbrk_vbrp2          TYPE TABLE OF ty_wb2_v_vbrk_vbrp2,
          ls_wb2_v_vbrk_vbrp2          TYPE ty_wb2_v_vbrk_vbrp2,
          lt_vbrk_vbrp                 TYPE TABLE OF ty_wb2_v_vbrk_vbrp2,
          ls_vbrk1_vbrp                TYPE ty_wb2_v_vbrk_vbrp2,
          lt_konv                      TYPE TABLE OF ty_konv,
          ls_konv                      TYPE ty_konv,
          lt_marc                      TYPE TABLE OF ty_marc,
          ls_marc                      TYPE ty_marc,
          lt_vbfa                      TYPE TABLE OF ty_vbfa,
          ls_vbfa                      TYPE ty_vbfa,
          lt_api                       TYPE TABLE OF ty_api,
          ls_api                       TYPE ty_api,
          lt_doctyp                    TYPE TABLE OF ty_doctyp,
          ls_doctyp                    TYPE ty_doctyp,
          im_vbrk                      TYPE ty_vbrk,
          lt_wb2_v_vbak_vbap2          TYPE TABLE OF ty_wb2_v_vbak_vbap2,
          ls_wb2_v_vbak_vbap2          TYPE ty_wb2_v_vbak_vbap2,
          lt_gstin                     TYPE TABLE OF ty_j_1bbranch,
          ls_gstin                     TYPE ty_j_1bbranch,
          lt_kna1_d                    TYPE TABLE OF ty_kna1,
          ls_kna1_d                    TYPE ty_kna1,
          lt_likp                      TYPE TABLE OF ty_likp,
          ls_likp                      TYPE ty_likp,
          lt_adrc_s                    TYPE TABLE OF ty_adrc,
          ls_adrc_s                    TYPE ty_adrc,
          lt_t005u_s                   TYPE TABLE OF ty_t005u,
          ls_t005u_s                   TYPE ty_t005u,
          lt_t001w                     TYPE TABLE OF ty_t001w1,
          ls_t001w                     TYPE ty_t001w1,
          lt_adrc_d                    TYPE TABLE OF ty_adrc,
          ls_adrc_d                    TYPE ty_adrc,
          lt_t005u_d                   TYPE TABLE OF ty_t005u,
          ls_t005u_d                   TYPE ty_t005u,
          lt_export                    TYPE TABLE OF ty_export,
          ls_export                    TYPE ty_export,
          lt_zteinv_details            TYPE TABLE OF zteinv_details,
          ls_zteinv_details            TYPE zteinv_details,
          lt_kna1                      TYPE TABLE OF ty_kna1,
          ls_kna1                      TYPE ty_kna1,
          lt_vbpa                      TYPE TABLE OF ty_vbpa,
          ls_vbpa                      TYPE ty_vbpa,
          ls_vbpa_ship                 TYPE ty_vbpa,
          lt_adr6_s                    TYPE TABLE OF ty_adr6,
          ls_adr6_s                    TYPE ty_adr6,
          lt_t005u                     TYPE TABLE OF ty_t005u,
          ls_t005u                     TYPE ty_t005u,
          lt_t005u_all                 TYPE TABLE OF ty_t005u,
          lt_adrc                      TYPE TABLE OF ty_adrc,
          ls_adrc                      TYPE ty_adrc,
          lt_adr6                      TYPE TABLE OF ty_adr6,
          ls_adr6                      TYPE ty_adr6,
          ls_bkpf                      TYPE ty_bkpf1,
          lt_bseg                      TYPE TABLE OF ty_bseg1,
          ls_bseg                      TYPE ty_bseg1,
          lt_invrefnum                 TYPE TABLE OF j_1ig_invrefnum,
          ls_invrefnum                 TYPE j_1ig_invrefnum,
          lt_api_hdr                   TYPE ztt_einv_api_struct,
          lt_api_itm                   TYPE ztt_einv_api_struct_itm,
          ls_token                     TYPE string,
          ls_return                    TYPE string,
          lt_invref                    TYPE TABLE OF j_1ig_invrefnum,
          ls_invref                    TYPE  j_1ig_invrefnum,
          lt_einv_details              TYPE TABLE OF zteinv_details,
          ls_einv_details              TYPE zteinv_details,
          lt_likp_lips2                TYPE TABLE OF wb2_v_likp_lips2,
          ls_likp_lips2                TYPE wb2_v_likp_lips2,
          lt_messages                  TYPE bapiret2_t,
          ls_message                   TYPE bapiret2,
          lt_tvarvc                    TYPE TABLE OF ty_tvarvc,
          ls_tvarvc                    TYPE ty_tvarvc,
          lt_einv_api_hdr              TYPE ztt_einv_api_struct,
          lt_einv_api_itm              TYPE ztt_einv_api_struct_itm,
          lv_total_assessable_value    TYPE wertv13,
          lv_total_cgst_value          TYPE wertv13,
          lv_total_sgst_value          TYPE wertv13,
          lv_total_igst_value          TYPE wertv13,
          lv_total_invoice_value       TYPE wertv13,
          lv_total_cess_value          TYPE wertv13,
          lv_total_cess_nonadvol_value TYPE wertv13,
          lv_total_tcs_value           TYPE wertv13,
          lv_total_roundoff            TYPE wertv13,
          lv_item_roundoff             TYPE wertv13,
          ls_einv_api_hdr              TYPE zst_einv_api_struct,
          ls_einv_api_itm              TYPE zst_einv_api_struct_itm,
          lv_reverse_charge            TYPE I VALUE 1,
          lv_supply_type               TYPE char10 VALUE 'O',
          lv_document_type             TYPE char10 VALUE 1,
          lv_sub_supply_type           TYPE char10 VALUE 1,
          lv_document_number           TYPE I VALUE 1,
          lv_gjahr(4)                  TYPE C, "gjahr,
          lv_internal_date             TYPE datum,
          ls_doc_uom                   TYPE ty_doc_uom,
          lt_doc_uom                   TYPE TABLE OF ty_doc_uom,
          lv_dmbtr                     TYPE dmbtr,
          lv_rate                      TYPE dmbtr,
          lv_item_amt                  TYPE dmbtr,
          lv_roundoff                  TYPE dmbtr,
          lv_exp_curr                  TYPE xfeld,
          lw_gstrate                   TYPE kbetr,
          lv_output_uom                TYPE meins,
          lv_export                    TYPE xfeld,
          lv_posnr                     TYPE posnr,
          lw_lines                     TYPE tline,
          lv_hsn                       TYPE steuc,
          lt_Adrc_new                  TYPE TABLE OF adrc.

    lr_fkart  = VALUE lr_fkart_type(
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

    CONSTANTS lc_tobject_vbbk    TYPE tdobject VALUE 'VBBK'.

    FIELD-SYMBOLS                <lw_t005u> TYPE ty_t005u.

    RANGES : rg_diff FOR prcd_elements-kschl.

    SELECT * FROM tvarvc INTO TABLE @DATA(it_diff) WHERE name = 'EDOC_ROUND_OFF'.

    LOOP AT it_diff INTO DATA(wa_diff) .
      rg_diff-low = wa_diff-low.
      rg_diff-option = 'EQ'.
      rg_diff-SIGN = 'I'.
      APPEND rg_diff.
    ENDLOOP.


    DATA:rt_name TYPE RANGE OF rvari_vnam,
          rs_name LIKE LINE OF rt_name.

    CLEAR:lt_api[],ls_api,im_vbrk,lt_doctyp,lt_wb2_v_vbak_vbap2[],
    ls_wb2_v_vbak_vbap2,im_vbrk,
    lt_wb2_v_vbrk_vbrp2,ls_wb2_v_vbrk_vbrp2,lv_posnr.

    SELECT SINGLE  bukrs  vbeln fkart  fktyp  vbtyp  vkorg  vtweg  fkdat kunrg xblnr
    FROM vbrk INTO im_vbrk WHERE vbeln = im_invoice.
    IF  sy-subrc = 0.
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
      INTO  TABLE lt_doctyp
      WHERE bukrs EQ im_vbrk-bukrs AND
      zmodule EQ gc_sd AND
      fkart = im_vbrk-fkart AND
      einv = abap_true AND
      instgen = abap_true.

      IF sy-subrc IS INITIAL.

* get the API data

        SELECT apiid apiprov  FROM zteinv_api INTO TABLE lt_api.

        SELECT vbeln  vbeln_i posnr_i fkart fktyp vbtyp waerk vkorg
        vtweg  knumv fkdat gjahr kurrf land1 bukrs netwr ernam kunrg
        kunag xblnr zuonr fksto bupla fkimg_i  meins_i kursk_i
        netwr_i vgbel_i  matnr_i arktx_i charg_i pstyv_i werks_i prctr_i mwsbp_i vrkme_i
        FROM wb2_v_vbrk_vbrp2
        INTO TABLE lt_wb2_v_vbrk_vbrp2
        FOR ALL ENTRIES IN lt_doctyp
        WHERE fkart = lt_doctyp-fkart
        AND vbeln = im_vbrk-vbeln
        AND fkdat = im_vbrk-fkdat
        AND bukrs = lt_doctyp-bukrs.

        IF sy-subrc = 0.

          READ TABLE lt_wb2_v_vbrk_vbrp2 TRANSPORTING NO FIELDS WITH KEY waerk = gc_curr_inr.

          CLEAR:lt_konv[],ls_konv.
          "Condition value
          SELECT knumv kposn  kschl kbetr kwert FROM prcd_elements INTO TABLE lt_konv
          FOR ALL ENTRIES IN lt_wb2_v_vbrk_vbrp2
          WHERE knumv = lt_wb2_v_vbrk_vbrp2-knumv
          AND kinak = abap_false.

          CLEAR:lt_marc[],ls_marc,lt_vbfa[],ls_vbfa.
* Get material details for HSN, and other fields
          SELECT matnr werks steuc FROM marc INTO TABLE lt_marc
          FOR ALL ENTRIES IN lt_wb2_v_vbrk_vbrp2
          WHERE matnr = lt_wb2_v_vbrk_vbrp2-matnr_i
          AND werks = lt_wb2_v_vbrk_vbrp2-werks_i.

* Document Flow Details
          SELECT vbelv  posnv vbeln FROM vbfa
          INTO TABLE lt_vbfa
          FOR ALL ENTRIES IN lt_wb2_v_vbrk_vbrp2
          WHERE vbeln   = lt_wb2_v_vbrk_vbrp2-vbeln
          AND vbtyp_v =   gc_vbtyp_v.
* Order Details
          IF lt_vbfa IS NOT INITIAL.
            SELECT vbeln vbeln_i posnr_i audat bstnk bstdk ihrez bname
            FROM wb2_v_vbak_vbap2
            INTO TABLE lt_wb2_v_vbak_vbap2
            FOR ALL ENTRIES IN lt_vbfa
            WHERE vbeln = lt_vbfa-vbelv.
          ENDIF.

* GSTIN deatils
          SELECT bukrs branch name adrnr gstin FROM j_1bbranch
          INTO TABLE lt_gstin
          WHERE bukrs = im_vbrk-bukrs.


          CLEAR:lt_likp[],ls_likp.
* Dispatch and  Ship Details
          SELECT vbeln kunnr FROM likp
          INTO TABLE lt_likp
          FOR ALL ENTRIES IN lt_wb2_v_vbrk_vbrp2
          WHERE vbeln EQ  lt_wb2_v_vbrk_vbrp2-vgbel_i.
          IF sy-subrc EQ 0.

            CLEAR:lt_kna1_d,ls_kna1_d.
            SELECT  kunnr land1 name1 name2 ort01 pstlz regio stras telf1 adrnr stcd3
            FROM kna1 INTO TABLE lt_kna1_d
            FOR ALL ENTRIES IN lt_likp
            WHERE kunnr = lt_likp-kunnr.
            IF sy-subrc EQ 0.

              CLEAR:lt_t005u_d[],ls_t005u_d.
              SELECT spras land1 land1 bezei FROM t005u
              INTO TABLE lt_t005u_d
              FOR ALL ENTRIES IN lt_kna1_d
              WHERE land1 EQ   gc_land1_in
              AND bland EQ lt_kna1_d-regio
              AND spras EQ sy-langu.
              IF sy-subrc EQ 0.
              ENDIF.

              CLEAR:lt_adrc_d,ls_adrc_d.
              SELECT addrnumber tel_number FROM adrc
              INTO TABLE lt_adrc_d
              FOR ALL ENTRIES IN lt_kna1_d
              WHERE addrnumber = lt_kna1_d-adrnr.

            ENDIF.
          ENDIF.
*Seller Details

          CLEAR:lt_t001w[],ls_t001w.
          SELECT werks name1 name2 stras pstlz ort01 land1 regio
          counc cityc adrnr spras  zone1 j_1bbranch FROM t001w
          INTO TABLE lt_t001w
          FOR ALL ENTRIES IN lt_wb2_v_vbrk_vbrp2
          WHERE werks EQ lt_wb2_v_vbrk_vbrp2-werks_i.
          IF sy-subrc EQ 0.

            CLEAR:lt_t005u_s[],ls_t005u_s.
            SELECT spras land1 bland bezei FROM t005u
            INTO TABLE lt_t005u_s
            FOR ALL ENTRIES IN lt_t001w
            WHERE land1 EQ lt_t001w-land1
            AND bland EQ lt_t001w-regio
            AND spras EQ sy-langu.

            CLEAR:lt_adrc_s[],ls_adrc_s.
            SELECT addrnumber tel_number FROM adrc
            INTO  TABLE lt_adrc_s
            FOR ALL ENTRIES IN lt_t001w
            WHERE addrnumber = lt_t001w-adrnr.
            IF sy-subrc EQ 0.

              CLEAR:lt_adr6_s[],ls_adr6_s.
              SELECT addrnumber smtp_addr FROM adr6
              INTO TABLE lt_adr6_s
              FOR ALL ENTRIES IN lt_adrc_s
              WHERE addrnumber = lt_adrc_s-addrnumber.
            ENDIF.
          ENDIF.


* Select partners

          CLEAR:lt_vbpa[],ls_vbpa.
          SELECT vbeln posnr parvw kunnr lifnr adrnr
          FROM vbpa INTO TABLE lt_vbpa
          WHERE vbeln = im_vbrk-vbeln.

*Buyer Details
          CLEAR:lt_kna1[],ls_kna1.
          IF sy-subrc IS INITIAL.

            SELECT * FROM adrc INTO TABLE lt_adrc_new FOR ALL ENTRIES IN lt_vbpa
            WHERE addrnumber = lt_vbpa-adrnr.


            SELECT kunnr land1 name1 name2 ort01 pstlz
            regio stras telf1 adrnr stcd3
            FROM kna1 INTO  TABLE lt_kna1
            FOR ALL ENTRIES IN lt_vbpa
            WHERE kunnr = lt_vbpa-kunnr.
            IF sy-subrc EQ 0.

              CLEAR:lt_t005u[],ls_t005u.
              SELECT spras land1 bland bezei FROM t005u
              INTO TABLE lt_t005u
              FOR ALL ENTRIES IN lt_kna1
              WHERE land1 EQ lt_kna1-land1
              AND bland EQ lt_kna1-regio
              AND spras EQ sy-langu.

              CLEAR:lt_adrc[],ls_adrc.
              SELECT addrnumber tel_number  FROM adrc
              INTO  TABLE lt_adrc
              FOR ALL ENTRIES IN lt_kna1
              WHERE addrnumber = lt_kna1-adrnr.

              IF sy-subrc IS INITIAL.
                SELECT  addrnumber smtp_addr FROM adr6
                INTO TABLE lt_adr6
                FOR ALL ENTRIES IN lt_adrc
                WHERE addrnumber = lt_adrc-addrnumber.
              ENDIF.
            ENDIF.
          ENDIF.

          SELECT edoc_uom meins FROM ztdoc_uom INTO TABLE lt_doc_uom WHERE edoc_uom IS NOT NULL.

* Get export port details.
          SELECT zport_code
          zport
          land1
          zport_state
          zport_address1
          zport_address2
          zport_place
          zport_pincode FROM ztedoc_export INTO TABLE lt_export WHERE land1 = gc_land1_in.


* Select the region text and chnge
          SELECT spras land1 bland bezei FROM t005u
          INTO TABLE lt_t005u_all
          WHERE land1 EQ gc_land1_in
          AND spras EQ sy-langu.
          IF sy-subrc IS INITIAL.
            LOOP AT lt_t005u_all ASSIGNING <lw_t005u>.
              IF <lw_t005u>-bland = '26'.
                <lw_t005u>-bezei = 'DADAR AND NAGAR HAVELI & DAMAN AND DIU'.
              ENDIF.
            ENDLOOP.
            lt_t005u = lt_t005u_all.
            lt_t005u_s = lt_t005u_all.
            lt_t005u_d = lt_t005u_all.
          ENDIF.



* get unit rates and discounts
          rs_name-SIGN = gc_sign.
          rs_name-option = gc_equal.
          rs_name-low = gc_var_unit.
          APPEND rs_name TO rt_name.
          rs_name-low = gc_var_disc.
          APPEND rs_name TO rt_name.
          SELECT  name TYPE numb SIGN opti low high
          FROM tvarvc INTO TABLE lt_tvarvc
          WHERE name IN rt_name.

        ENDIF.

        IF lt_wb2_v_vbrk_vbrp2 IS NOT INITIAL.
          CLEAR: lt_messages,lt_invrefnum,lt_api_hdr,lt_api_itm,lt_einv_api_hdr,lt_einv_api_itm.
          CLEAR:ls_einv_api_hdr-total_assessable_value,
          ls_einv_api_hdr-total_cgst_value,
          ls_einv_api_hdr-total_sgst_value,
          ls_einv_api_hdr-total_igst_value,
          ls_einv_api_hdr-total_invoice_value,
          ls_einv_api_hdr-total_cess_value,
          ls_einv_api_hdr-total_cess_nonadvol_value,
          ls_einv_api_hdr-total_other_charge.

          CLEAR: lv_total_assessable_value,lv_total_igst_value,
          lv_total_cgst_value,lv_total_sgst_value,
          lv_total_cess_value,
          lv_total_cess_nonadvol_value,
          lv_total_tcs_value,
          lv_total_roundoff,
          lv_total_invoice_value,ls_einv_api_hdr.

          LOOP AT lt_wb2_v_vbrk_vbrp2 INTO ls_wb2_v_vbrk_vbrp2 .
            ls_einv_api_hdr-vbeln  = ls_wb2_v_vbrk_vbrp2-vbeln.
            ls_einv_api_hdr-bukrs  = ls_wb2_v_vbrk_vbrp2-bukrs.
* Get Fiscl year
            CALL FUNCTION 'GM_GET_FISCAL_YEAR'
            EXPORTING
              i_date                     = ls_wb2_v_vbrk_vbrp2-fkdat
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
              ls_einv_api_hdr-doc_year = lv_gjahr.
            ENDIF.
            ls_einv_api_hdr-doc_type = ls_wb2_v_vbrk_vbrp2-fkart.

            READ TABLE lt_gstin INTO ls_gstin WITH KEY bukrs = ls_wb2_v_vbrk_vbrp2-bukrs
            branch = ls_wb2_v_vbrk_vbrp2-bupla.
            IF sy-subrc IS INITIAL.
              ls_einv_api_hdr-user_gstin = ls_gstin-gstin.
            ENDIF.

            READ TABLE lt_api  INTO ls_api WITH KEY apiid = gc_apiid_usrgstin.
            IF sy-subrc IS INITIAL.
              ls_einv_api_hdr-user_gstin   = ls_api-apiprov.
            ENDIF.

            CONCATENATE  '"' ls_einv_api_hdr-user_gstin '"' INTO ls_einv_api_hdr-user_gstin.


            ls_einv_api_hdr-supply_type   = gc_sup_type_b2b .

            ls_einv_api_hdr-charge_type = gc_charge_n.

            ls_einv_api_hdr-ecommerce_gstin = '""'.

            IF ls_wb2_v_vbrk_vbrp2-fkart  = 'ZSEZ'.
              ls_einv_api_hdr-supply_type   = gc_sup_type_sezwp.
            ENDIF.

            IF ls_wb2_v_vbrk_vbrp2-fkart  = 'ZEXD'.
              ls_einv_api_hdr-supply_type   = gc_sup_type_dexp.
            ENDIF.

            CASE lv_document_number.
            WHEN 1.
              ls_einv_api_hdr-document_number = ls_wb2_v_vbrk_vbrp2-vbeln.
            WHEN 2.
              ls_einv_api_hdr-document_number = ls_wb2_v_vbrk_vbrp2-xblnr.
            ENDCASE.
            SHIFT ls_einv_api_hdr-document_number LEFT DELETING LEADING '0'.

            IF ls_wb2_v_vbrk_vbrp2-fkdat IS NOT INITIAL.
              CONCATENATE  ls_wb2_v_vbrk_vbrp2-fkdat+6(2) '/'
              ls_wb2_v_vbrk_vbrp2-fkdat+4(2) '/'
              ls_wb2_v_vbrk_vbrp2-fkdat+0(4)
              INTO ls_einv_api_hdr-document_date.
            ENDIF.


            READ TABLE lt_doctyp INTO ls_doctyp WITH KEY fkart = ls_wb2_v_vbrk_vbrp2-fkart
            bukrs = ls_wb2_v_vbrk_vbrp2-bukrs
            zmodule = gc_sd.
            IF sy-subrc = 0.
              ls_einv_api_hdr-document_type = ls_doctyp-edoc_type.
            ELSE.
              ls_einv_api_hdr-document_type = gc_docty_inv.
            ENDIF.


            CONCATENATE '"' ls_einv_api_hdr-document_type '"' INTO  ls_einv_api_hdr-document_type.
            CONCATENATE '"' ls_einv_api_hdr-document_number '"' INTO  ls_einv_api_hdr-document_number.
            CONCATENATE '"' ls_einv_api_hdr-document_date '"' INTO  ls_einv_api_hdr-document_date.

**Begin of seller_details
            CLEAR: ls_t001w, ls_adrc_s, ls_adr6_s.
            READ TABLE lt_t001w INTO ls_t001w WITH  KEY werks = ls_wb2_v_vbrk_vbrp2-werks_i.
            IF sy-subrc EQ 0.
              READ TABLE lt_adrc_s INTO ls_adrc_s WITH  KEY addrnumber = ls_t001w-adrnr.
              READ TABLE lt_adr6_s INTO ls_adr6_s WITH  KEY addrnumber = ls_adrc_s-addrnumber.

              READ TABLE lt_gstin INTO ls_gstin WITH KEY bukrs = ls_wb2_v_vbrk_vbrp2-bukrs
              branch = ls_wb2_v_vbrk_vbrp2-bupla.
              IF sy-subrc IS INITIAL.
                ls_einv_api_hdr-gstin_s = ls_gstin-gstin.
              ENDIF.

              READ TABLE lt_api  INTO ls_api WITH KEY apiid = gc_apiid_usrgstin.
              IF sy-subrc IS INITIAL.
                ls_einv_api_hdr-gstin_s   = ls_api-apiprov.
              ENDIF.


              CONCATENATE '"' ls_einv_api_hdr-gstin_s '"' INTO ls_einv_api_hdr-gstin_s.

              CONCATENATE '"'   gc_name1  '"' INTO   ls_einv_api_hdr-legal_name_s   .
              CONCATENATE '"'   gc_name1  '"' INTO   ls_einv_api_hdr-trade_name_s    .
              CONCATENATE '"'   ls_t001w-stras  '"' INTO   ls_einv_api_hdr-address1_s.
*              CONCATENATE '"'   ls_t001w-name2  '"' INTO   ls_einv_api_hdr-address2_s.
              ls_einv_api_hdr-address2_s = '""'.
              CONCATENATE '"'   ls_t001w-ort01  '"' INTO   ls_einv_api_hdr-location_s       .
              ls_einv_api_hdr-pincode_s =  ls_t001w-pstlz    .
              CLEAR:ls_t005u_s.
              READ TABLE lt_t005u_s INTO ls_t005u_s WITH  KEY bland = ls_t001w-regio.
              IF sy-subrc EQ 0.
                TRANSLATE ls_t005u_s-bezei TO UPPER CASE.
                CONCATENATE '"'   ls_t005u_s-bezei  '"' INTO  ls_einv_api_hdr-state_code_s .
              ENDIF.

              CONCATENATE '"'  ls_adrc_s-tel_number+0(10)  '"' INTO  ls_einv_api_hdr-phone_number_s.
              CONCATENATE '"'    ls_adr6_s-smtp_addr '"' INTO    ls_einv_api_hdr-email_s.
            ENDIF.
**End of seller_details

**Begin of buyer_details
            CLEAR:ls_kna1, ls_adrc, ls_adr6.
            READ TABLE lt_vbpa INTO ls_vbpa WITH KEY vbeln = ls_wb2_v_vbrk_vbrp2-vbeln
            parvw = gc_parvw_re.
            IF sy-subrc = 0.
              READ TABLE lt_vbpa INTO ls_vbpa_ship WITH KEY vbeln = ls_wb2_v_vbrk_vbrp2-vbeln
              parvw = gc_parvw_we.
              IF sy-subrc IS INITIAL.
                IF ls_vbpa-kunnr NE ls_vbpa_ship-kunnr.
                  ls_einv_api_hdr-ship_to = abap_true.
                ENDIF.
              ENDIF.

              READ TABLE lt_kna1 INTO ls_kna1 WITH  KEY kunnr = ls_vbpa-kunnr.
              IF sy-subrc EQ 0.
                READ TABLE lt_adrc INTO ls_adrc WITH  KEY addrnumber = ls_kna1-adrnr.
                READ TABLE lt_adr6 INTO ls_adr6 WITH  KEY addrnumber = ls_adrc-addrnumber.
                CONCATENATE '"'  ls_kna1-stcd3 '"' INTO          ls_einv_api_hdr-gstin_b .
                CONCATENATE '"'  ls_kna1-name1 '"' INTO          ls_einv_api_hdr-legal_name_b  .
                CONCATENATE '"' ls_kna1-name1 '"' INTO ls_einv_api_hdr-trade_name_b  .
*                CONCATENATE '"' ls_kna1-stras '"' INTO          ls_einv_api_hdr-address1_b       .
*                CONCATENATE '"' ls_kna1-name2 '"' INTO  ls_einv_api_hdr-address2_b.
*                CONCATENATE '"'  ls_kna1-ort01 '"'  INTO           ls_einv_api_hdr-location_b      .
*                ls_einv_api_hdr-pincode_b  = ls_kna1-pstlz.
*                ls_einv_api_hdr-place_of_supply_b = ls_kna1-regio.
*                CLEAR:ls_t005u.
*                READ TABLE lt_t005u INTO ls_t005u WITH  KEY bland = ls_kna1-regio.
*                IF sy-subrc EQ 0.
*                  TRANSLATE ls_t005u-bezei TO UPPER CASE.
*                  CONCATENATE '"' ls_t005u-bezei '"'  INTO  ls_einv_api_hdr-state_code_b.
*                  ls_einv_api_hdr-place_of_supply_b   =   ls_einv_api_hdr-state_code_b.
*                ENDIF.
*                CONCATENATE '"' ls_adrc-tel_number+0(10) '"' INTO   ls_einv_api_hdr-phone_number_b.
*                CONCATENATE '"'   ls_adr6-smtp_addr '"' INTO  ls_einv_api_hdr-email_b.
*

                READ TABLE lt_adrc_new INTO DATA(ls_adrc_1) WITH KEY addrnumber = ls_vbpa-adrnr.
                IF sy-subrc IS INITIAL.

                  CONCATENATE '"' ls_adrc_1-street '"' INTO          ls_einv_api_hdr-address1_b       .
                  CONCATENATE '"' ls_Adrc_1-str_suppl3 '"' INTO  ls_einv_api_hdr-address2_b.
                  CONCATENATE '"'  ls_Adrc_1-city1 '"'  INTO           ls_einv_api_hdr-location_b      .
                  ls_einv_api_hdr-pincode_b  = ls_Adrc_1-post_code1.
                  ls_einv_api_hdr-place_of_supply_b = ls_adrc_1-region.
                  CLEAR:ls_t005u.
                  READ TABLE lt_t005u_all INTO ls_t005u WITH  KEY bland = ls_adrc_1-region.
                  IF sy-subrc EQ 0.
                    TRANSLATE ls_t005u-bezei TO UPPER CASE.
                    CONCATENATE '"' ls_t005u-bezei '"'  INTO  ls_einv_api_hdr-state_code_b.
                    ls_einv_api_hdr-place_of_supply_b   =   ls_einv_api_hdr-state_code_b.
                  ENDIF.
                  CONCATENATE '"' ls_adrc-tel_number+0(10) '"' INTO   ls_einv_api_hdr-phone_number_b.
                  CONCATENATE '"'   ls_adr6-smtp_addr '"' INTO  ls_einv_api_hdr-email_b.

                ENDIF.


              ENDIF.

              IF ls_t001w-land1 NE ls_kna1-land1.
                ls_einv_api_hdr-gstin_b = gc_gstin_urp.
*                READ TABLE lt_export INTO ls_export WITH KEY zport_code = gw_final-zport_code.
*                IF sy-subrc IS INITIAL.
*                  CONCATENATE '"' ls_export-zport_address1 '"' INTO ls_einv_api_hdr-address1_b.
*                  CONCATENATE '"' ls_export-zport_address2 '"' INTO ls_einv_api_hdr-address2_b.
*                  CONCATENATE '"' ls_export-zport_place '"' INTO ls_einv_api_hdr-location_b.
*                  ls_einv_api_hdr-pincode_b = ls_export-zport_pincode.
*
*                  CLEAR:ls_t005u.
*                  READ TABLE lt_t005u INTO ls_t005u WITH  KEY bland = ls_export-zport_state.
*                  IF sy-subrc EQ 0.
*                    TRANSLATE ls_t005u-bezei TO UPPER CASE.
*                    CONCATENATE '"' ls_t005u-bezei '"' INTO ls_einv_api_hdr-state_code_b.
*                  ENDIF.
*                ENDIF.
                ls_einv_api_hdr-pincode_b = gc_export_pincode.
                ls_einv_api_hdr-place_of_supply_b = gc_export_place.
                ls_einv_api_hdr-state_code_b = gc_exp_state.

              ENDIF.

            ENDIF.
**End of buyer_details


*begin of dispatch_details
            CONCATENATE '"' ls_t001w-name1 '"' INTO ls_einv_api_hdr-comapny_name_d.
            CONCATENATE '"' ls_t001w-name2 '"' INTO   ls_einv_api_hdr-address1_d .
            CONCATENATE '"' ls_t001w-stras '"' INTO  ls_einv_api_hdr-address2_d .
            CONCATENATE '"' ls_t001w-ort01 '"'  INTO     ls_einv_api_hdr-location_d .
            IF ls_t001w-pstlz IS  NOT INITIAL.
              ls_einv_api_hdr-pincode_d = ls_t001w-pstlz .
            ENDIF.
            CLEAR:ls_t005u_d.
            READ TABLE lt_t005u_s INTO ls_t005u_s WITH  KEY bland = ls_t001w-regio.
            IF sy-subrc EQ 0.
              TRANSLATE ls_t005u_s-bezei TO UPPER CASE.
              CONCATENATE '"' ls_t005u_s-bezei '"'  INTO ls_einv_api_hdr-state_code_d .
            ENDIF.

            IF ls_wb2_v_vbrk_vbrp2-fkart = 'ZTRD'.
              lv_tdname = ls_wb2_v_vbrk_vbrp2-vbeln.
              PERFORM readtext_value USING lv_tdname 'Z023' CHANGING lv_TDLINE.
              IF lv_tdline IS NOT INITIAL.
                ls_einv_api_hdr-comapny_name_d = lv_tdline.
                ls_einv_api_hdr-dispatch = abap_true.
              ENDIF.
              PERFORM readtext_value USING lv_tdname 'Z024' CHANGING lv_TDLINE.
              IF lv_tdline IS NOT INITIAL.
                ls_einv_api_hdr-address1_d = lv_tdline.
              ENDIF.
              ls_einv_api_hdr-address2_d = '" "'.
              PERFORM readtext_value USING lv_tdname 'Z025' CHANGING lv_TDLINE.
              IF lv_tdline IS NOT INITIAL.
                ls_einv_api_hdr-location_d = lv_tdline.
              ENDIF.
              PERFORM readtext_value USING lv_tdname 'Z026' CHANGING lv_TDLINE.
              IF lv_tdline IS NOT INITIAL.
                ls_einv_api_hdr-pincode_d = lv_tdline.
              ENDIF.
              PERFORM readtext_value USING lv_tdname 'Z027' CHANGING lv_TDLINE.
              IF lv_tdline IS NOT INITIAL.
                ls_einv_api_hdr-state_code_d = lv_tdline.
              ENDIF.
            ENDIF.
*begin of dispatch_details.
*
**Begin of ship_details

            CLEAR:ls_vbpa.
            READ TABLE lt_vbpa INTO ls_vbpa WITH  KEY vbeln = ls_wb2_v_vbrk_vbrp2-vbeln
            parvw = gc_parvw_we.
            IF sy-subrc EQ 0.
              CLEAR:ls_kna1_d.
              READ TABLE lt_kna1 INTO ls_kna1_d WITH  KEY kunnr = ls_vbpa-kunnr.
              IF sy-subrc EQ 0.
                CONCATENATE '"' ls_kna1_d-stcd3 '"' INTO ls_einv_api_hdr-gstin_sh .
                CONCATENATE '"' ls_kna1_d-name1 '"' INTO  ls_einv_api_hdr-legal_name_sh.
                CONCATENATE '"' ls_kna1_d-name1 '"' INTO  ls_einv_api_hdr-trade_name_sh.
*                CONCATENATE '"' ls_kna1_d-name2 '"' INTO  ls_einv_api_hdr-address2_sh .
*                CONCATENATE '"' ls_kna1_d-stras '"' INTO  ls_einv_api_hdr-address1_sh .
*                IF ls_kna1_d-name2 IS  NOT INITIAL AND ls_kna1_d-stras IS INITIAL.
*                  ls_einv_api_hdr-address1_sh = ls_einv_api_hdr-address2_sh .
*                ENDIF.
*                CONCATENATE '"'  ls_kna1_d-ort01 '"'  INTO     ls_einv_api_hdr-location_sh .
*                IF ls_kna1_d-pstlz IS  NOT INITIAL.
*                  ls_einv_api_hdr-pincode_sh = ls_kna1_d-pstlz .
*                ENDIF.
*                CLEAR:ls_t005u_d.
*                READ TABLE lt_t005u INTO ls_t005u_d WITH  KEY bland = ls_kna1_d-regio.
*                IF sy-subrc EQ 0.
*                  TRANSLATE ls_t005u_d-bezei TO UPPER CASE.
*                  CONCATENATE '"' ls_t005u_d-bezei '"' INTO ls_einv_api_hdr-state_code_sh.
*                ELSE.
*                  ls_einv_api_hdr-state_code_sh  = ls_einv_api_hdr-state_code_b.
*                ENDIF.


                READ TABLE lt_adrc_new INTO DATA(ls_adrc_2) WITH KEY addrnumber =  ls_vbpa-adrnr.
                IF sy-subrc IS INITIAL.

                  CONCATENATE '"' ls_adrc_2-street '"' INTO  ls_einv_api_hdr-address1_sh .
                  CONCATENATE '"' ls_adrc_2-str_suppl3 '"' INTO   ls_einv_api_hdr-address2_sh .
                  CONCATENATE '"'  ls_adrc_2-city1 '"'  INTO     ls_einv_api_hdr-location_sh .
                  IF ls_adrc_2-post_code1 IS  NOT INITIAL.
                    ls_einv_api_hdr-pincode_sh = ls_adrc_2-post_code1.
                  ENDIF.
                  CLEAR:ls_t005u_d.
                  READ TABLE lt_t005u_all INTO ls_t005u_d WITH  KEY bland = ls_Adrc_2-region.
                  IF sy-subrc EQ 0.
                    TRANSLATE ls_t005u_d-bezei TO UPPER CASE.
                    CONCATENATE '"' ls_t005u_d-bezei '"' INTO ls_einv_api_hdr-state_code_sh.
                  ELSE.
                    ls_einv_api_hdr-state_code_sh  = ls_einv_api_hdr-state_code_b.
                  ENDIF.

                ENDIF.

              ENDIF.



              CLEAR lv_export.
              IF ls_t001w-land1 NE ls_kna1_d-land1.
                lv_export = abap_true.
                ls_einv_api_hdr-gstin_sh = gc_gstin_urp.
                READ TABLE lt_export INTO ls_export WITH KEY zport_code = 'Dummy'.
                IF sy-subrc IS INITIAL.
                  CONCATENATE '"' ls_export-zport_address1 '"' INTO ls_einv_api_hdr-address1_sh.
                  CONCATENATE '"' ls_export-zport_address2 '"' INTO ls_einv_api_hdr-address2_sh.
                  CONCATENATE '"' ls_export-zport_place '"' INTO ls_einv_api_hdr-location_sh.
                  ls_einv_api_hdr-pincode_sh = ls_export-zport_pincode.

                  CLEAR:ls_t005u_d.
                  READ TABLE lt_t005u INTO ls_t005u_d WITH  KEY bland = ls_export-zport_state.
                  IF sy-subrc EQ 0.
                    TRANSLATE ls_t005u_d-bezei TO UPPER CASE.
                    CONCATENATE '"' ls_t005u_d-bezei '"' INTO ls_einv_api_hdr-state_code_sh.
                  ENDIF.
                ENDIF.
              ENDIF.

            ENDIF.

*End of  ship_details

* If ship to and buil d to is diferent
            IF ls_kna1-kunnr NE ls_kna1_d-kunnr .
              ls_einv_api_hdr-transaction_type = gc_ttype_shg.
          ELSEIF ls_kna1-kunnr EQ ls_kna1_d-kunnr.
              ls_einv_api_hdr-transaction_type = gc_ttype_reg.
            ENDIF.



*Begin of export_details
            IF ls_t001w-land1 NE ls_kna1-land1.
              ls_einv_api_hdr-supply_type   = gc_sup_type_expwp.
              ls_einv_api_hdr-ship_bill_number = ls_einv_api_hdr-document_number.
              ls_einv_api_hdr-ship_bill_date = ls_einv_api_hdr-document_date.
              CONCATENATE '"' ls_kna1-land1             '"' INTO ls_einv_api_hdr-country_code.
              CONCATENATE '"' ls_wb2_v_vbrk_vbrp2-waerk '"' INTO ls_einv_api_hdr-foreign_currency.
              CONCATENATE '"' ' '                       '"' INTO ls_einv_api_hdr-refund_claim.
              CONCATENATE '"' ' '                       '"' INTO ls_einv_api_hdr-port_code  .
            ELSE.
              CONCATENATE  '"' ' ' '"' INTO ls_einv_api_hdr-ship_bill_number.
              CONCATENATE  '"' ' ' '"' INTO ls_einv_api_hdr-ship_bill_date.
              CONCATENATE  '"' ' ' '"' INTO ls_einv_api_hdr-country_code.
              CONCATENATE  '"' ' ' '"' INTO ls_einv_api_hdr-foreign_currency.
              CONCATENATE  '"' ' ' '"' INTO ls_einv_api_hdr-refund_claim.
              CONCATENATE  '"' ' ' '"' INTO ls_einv_api_hdr-port_code  .
            ENDIF.
*End of export_details

**Begin of payment_details
            CONCATENATE '"' ' ' '"' INTO ls_einv_api_hdr-bank_account_number.               "   type char50, "String (50) Account Details   N
            CONCATENATE ' ' '0' ' ' INTO ls_einv_api_hdr-paid_balance_amount.         " type n, "Number (0-17) Balance Amount to be paid   N
            CONCATENATE ' ' '0' ' ' INTO ls_einv_api_hdr-credit_days.                  " type n, "Number (0-3)  Credit Days   N
            CONCATENATE '"' ' ' '"' INTO ls_einv_api_hdr-credit_transfer.               "   type char50, "String (50) Credit Transfer   N
            CONCATENATE '"' ' ' '"' INTO ls_einv_api_hdr-direct_debit.                   "  type char50, "String (50) Direct Debit    N
            CONCATENATE '"' ' ' '"' INTO ls_einv_api_hdr-branch_or_ifsc.                  " type char11, " String (max 11) Branch or IFSC code   N
            CONCATENATE '"' ' ' '"' INTO ls_einv_api_hdr-payment_mode.                     "type char6, "String (max 6)  Mode of Payment "CASH","EPAY", "DIRDBT","OTH" N
            CONCATENATE '"' ' ' '"' INTO ls_einv_api_hdr-payee_name.                       "type char100, "String (0-100)  Payee Name    N
            CONCATENATE '"' ' ' '"' INTO ls_einv_api_hdr-payment_due_date.                 "type char10, "  String (max 10) Due Date of Payment [2][0][1-2][0-9]-[0-1][0-9]-[0-3][0-9]  N
            CONCATENATE '"' ' ' '"' INTO ls_einv_api_hdr-payment_instruction .             "type char200, "String (max 200)  Payment Instruction   N
            CONCATENATE '"' ' ' '"' INTO ls_einv_api_hdr-payment_term.                     "type char200, "String (max 200)  Terms of Payment    N

**End of payment_details


            lv_tdname  = ls_wb2_v_vbak_vbap2-vbeln.

            CALL FUNCTION 'READ_TEXT'
            EXPORTING
              CLIENT                  = sy-mandt
              ID                      = gc_tdid_0002
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
              CONCATENATE  '"' '' '"' INTO ls_einv_api_hdr-invoice_remarks. "Required
            ELSE.
              READ TABLE lt_lines INTO ls_lines INDEX 1.
              CONCATENATE  '"' ls_lines-tdline '"' INTO ls_einv_api_hdr-invoice_remarks. "Required

            ENDIF.

*Begin of reference_details
            ls_einv_api_hdr-invoice_period_start_date = ls_einv_api_hdr-document_date.
            ls_einv_api_hdr-invoice_period_end_date   = ls_einv_api_hdr-document_date.

**Begin of preceding_document_details
            ls_einv_api_hdr-reference_of_original_invoice = ls_einv_api_hdr-document_number.
            ls_einv_api_hdr-preceding_invoice_date        = ls_einv_api_hdr-document_date.
            ls_einv_api_hdr-other_reference = '""'.
*End of preceding_document_details
*End of reference_details

*Begin of contract Details,
            CLEAR: ls_vbfa, ls_wb2_v_vbak_vbap2.
            READ TABLE lt_vbfa INTO ls_vbfa WITH KEY vbeln = ls_wb2_v_vbak_vbap2-vbeln.
            IF sy-subrc EQ 0.
              READ TABLE lt_wb2_v_vbak_vbap2 INTO ls_wb2_v_vbak_vbap2 WITH KEY vbeln = ls_vbfa-vbelv
              posnr_i = ls_vbfa-posnv.
              IF sy-subrc EQ 0.
                CONCATENATE '"' ls_wb2_v_vbak_vbap2-vbeln  '"' INTO    ls_einv_api_hdr-receipt_advice_number .     "Required
                IF ls_wb2_v_vbak_vbap2-audat IS NOT INITIAL.
                  CONCATENATE '"' ls_wb2_v_vbak_vbap2-audat+6(2) '/'
                  ls_wb2_v_vbak_vbap2-audat+4(2) '/'
                  ls_wb2_v_vbak_vbap2-audat+0(4)
                  '"' INTO    ls_einv_api_hdr-receipt_advice_date .     "Required
                ENDIF.
                CONCATENATE '"' ls_wb2_v_vbrk_vbrp2-charg_i '"' INTO    ls_einv_api_hdr-batch_reference_number.    "Required
                CONCATENATE '"' ls_wb2_v_vbak_vbap2-bname '"' INTO    ls_einv_api_hdr-contract_reference_number.  "Required
                CONCATENATE '"' ls_wb2_v_vbak_vbap2-ihrez '"' INTO    ls_einv_api_hdr-other_reference_c.       "Required
                ls_einv_api_hdr-project_reference_number = '""'.
                CONCATENATE '"' ls_wb2_v_vbak_vbap2-bstnk '"' INTO    ls_einv_api_hdr-vendor_po_reference_number.   "Required

                IF ls_wb2_v_vbak_vbap2-bstdk IS NOT INITIAL.
                  CONCATENATE '"' ls_wb2_v_vbak_vbap2-bstdk+6(2) '/'
                  ls_wb2_v_vbak_vbap2-bstdk+4(2) '/'
                  ls_wb2_v_vbak_vbap2-bstdk+0(4)
                  '"' INTO    ls_einv_api_hdr-vendor_po_reference_date.      ""Required
                ENDIF.
                "End of contract_details
              ENDIF.

            ELSE.

              ls_einv_api_hdr-receipt_advice_number = '""'.
              ls_einv_api_hdr-receipt_advice_date  = '""'.
              ls_einv_api_hdr-batch_reference_number = '""'.
              ls_einv_api_hdr-contract_reference_number = '""'.
              ls_einv_api_hdr-project_reference_number = '""'.
              ls_einv_api_hdr-vendor_po_reference_number = '""'.
              ls_einv_api_hdr-vendor_po_reference_date = '""'.
              ls_einv_api_hdr-other_reference_c = '""'.

            ENDIF.
*Begin of'"additional_document_details"
            CONCATENATE  '"' ls_einv_api_hdr-supporting_document_url '"' INTO ls_einv_api_hdr-supporting_document_url. "",'
            CONCATENATE  '"' ls_einv_api_hdr-supporting_document '"' INTO ls_einv_api_hdr-supporting_document. "india",'
            CONCATENATE  '"' ls_einv_api_hdr-additional_information '"' INTO ls_einv_api_hdr-additional_information. "india"'
*End of'"additional_document_details"


********************* dafault all values to 0 ******************************
            ls_einv_api_hdr-total_assessable_value          = '0'.
            ls_einv_api_hdr-total_cgst_value                = '0'.
            ls_einv_api_hdr-total_sgst_value                = '0'.
            ls_einv_api_hdr-total_igst_value                = '0'.
            ls_einv_api_hdr-total_cess_value                = '0'.
            ls_einv_api_hdr-total_cess_nonadvol_value       = '0'.
            ls_einv_api_hdr-total_invoice_value             = '0'.
            ls_einv_api_hdr-total_cess_value_of_state       = '0'.
            ls_einv_api_hdr-round_off_amount                = '0'.
            ls_einv_api_hdr-total_invoice_value_additional  = '0'.


****************************************************************************************************************************************************

* check for export currency
            CLEAR lv_exp_curr.
            IF ls_wb2_v_vbrk_vbrp2-waerk NE  gc_curr_inr .
              lv_exp_curr = abap_true.
            ENDIF.


*Begin of item_list


********************* dafault all values to 0 ******************************
            ls_einv_api_itm-quantity                    = '0'.
            ls_einv_api_itm-free_quantity               = '0'.
            ls_einv_api_itm-unit_price                  = '0'.
            ls_einv_api_itm-total_amount                = '0'.
            ls_einv_api_itm-pre_tax_value               = '0'.
            ls_einv_api_itm-discount                    = '0'.
            ls_einv_api_itm-other_charge                = '0'.
            ls_einv_api_itm-gst_rate                    = '0'.
            ls_einv_api_itm-igst_amount                 = '0'.
            ls_einv_api_itm-cgst_amount                 = '0'.
            ls_einv_api_itm-sgst_amount                 = '0'.
            ls_einv_api_itm-cess_rate                   = '0'.
            ls_einv_api_itm-cess_amount                 = '0'.
            ls_einv_api_itm-cess_nonadvol_value         = '0'.
            ls_einv_api_itm-state_cess_rate             = '0'.
            ls_einv_api_itm-state_cess_amount           = '0'.
            ls_einv_api_itm-state_cess_nonadvol_amount  = '0'.
            ls_einv_api_itm-total_item_value            = '0'.


********************* dafault all values to 0 ******************************

            ls_einv_api_itm-bukrs  = ls_wb2_v_vbrk_vbrp2-bukrs.
            ls_einv_api_itm-doc_year = ls_einv_api_hdr-doc_year.
            ls_einv_api_itm-doc_type = ls_wb2_v_vbrk_vbrp2-fkart.
            ls_einv_api_itm-document_type = ls_wb2_v_vbrk_vbrp2-fkart  .
            ls_einv_api_itm-document_number = ls_wb2_v_vbrk_vbrp2-vbeln   .

            SHIFT ls_einv_api_itm-document_number LEFT DELETING LEADING '0'.
            IF ls_wb2_v_vbrk_vbrp2-fkdat IS NOT INITIAL.
              CONCATENATE  ls_wb2_v_vbrk_vbrp2-fkdat+6(2) '/'
              ls_wb2_v_vbrk_vbrp2-fkdat+4(2) '/'
              ls_wb2_v_vbrk_vbrp2-fkdat+0(4)
              INTO ls_einv_api_itm-document_date.
            ENDIF.
            CONCATENATE '"' ls_einv_api_itm-document_type '"' INTO  ls_einv_api_itm-document_type.
            CONCATENATE '"' ls_einv_api_itm-document_number '"' INTO  ls_einv_api_itm-document_number.
            CONCATENATE '"' ls_einv_api_itm-document_date '"' INTO  ls_einv_api_itm-document_date.
            lv_posnr = lv_posnr + 1.
            CONCATENATE '"'  lv_posnr '"' INTO ls_einv_api_itm-item_serial_number .
            REPLACE ALL OCCURRENCES OF '"' IN ls_wb2_v_vbrk_vbrp2-arktx_i WITH space .
            CONCATENATE '"'  ls_wb2_v_vbrk_vbrp2-arktx_i '"' INTO ls_einv_api_itm-product_description.

***** ---logic for hsncode based on billing type----added by raghu on 27.03.2023****
            CLEAR lv_hsn.
            IF ls_wb2_v_vbrk_vbrp2-fkart IN lr_fkart.
              lv_tdname  = ls_wb2_v_vbrk_vbrp2-vbeln.
              CALL FUNCTION 'READ_TEXT'
              EXPORTING
                CLIENT                  = sy-mandt
                ID                      = 'Z021'
                LANGUAGE                = sy-langu
                name                    = lv_tdname
                object                  = lc_tobject_vbbk
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
                  CONCATENATE  '"' lw_lines-tdline '"' INTO ls_einv_api_itm-hsn_code.
                  lv_hsn = lw_lines-tdline.
                ENDIF.
              ENDIF.
            ENDIF.


            IF ls_einv_api_itm-hsn_code IS INITIAL.
              READ TABLE lt_marc INTO ls_marc WITH KEY matnr = ls_wb2_v_vbrk_vbrp2-matnr_i
              werks = ls_wb2_v_vbrk_vbrp2-werks_i
              BINARY SEARCH.
              IF sy-subrc EQ 0.
                CONCATENATE '"'  ls_marc-steuc  '"' INTO ls_einv_api_itm-hsn_code.
                lv_hsn = ls_marc-steuc.
              ENDIF.
            ENDIF.


            IF ls_wb2_v_vbrk_vbrp2-pstyv_i EQ gc_pstyv_tad.
              ls_einv_api_itm-is_service = gc_service_y.
          ELSEIF  lv_hsn+0(2) = '99'.
              ls_einv_api_itm-is_service = gc_service_y.
            ELSE.
              ls_einv_api_itm-is_service = gc_service_n.
            ENDIF.
******------------end-----------------------------*****
*            IF ls_wb2_v_vbrk_vbrp2-pstyv_i EQ gc_pstyv_tad.
*              ls_einv_api_itm-is_service = gc_service_y.
*            ELSE.
*              ls_einv_api_itm-is_service = gc_service_n.
*            ENDIF.

*            READ TABLE lt_marc INTO ls_marc WITH KEY matnr = ls_wb2_v_vbrk_vbrp2-matnr_i
*                                                     werks = ls_wb2_v_vbrk_vbrp2-werks_i.
*            IF sy-subrc EQ 0.
*              CONCATENATE '"'  ls_marc-steuc  '"' INTO ls_einv_api_itm-hsn_code.
*              IF  ls_marc-steuc+0(2) = '99'.
*                ls_einv_api_itm-is_service = gc_service_y.
*              ENDIF.
*            ENDIF.
            ls_einv_api_itm-bar_code =  '" "'.
            ls_einv_api_itm-quantity = ls_wb2_v_vbrk_vbrp2-fkimg_i . "'1'.
            ls_einv_api_itm-free_quantity = '0'.
*Unit
            CLEAR lv_output_uom.
            CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
            EXPORTING
              INPUT          = ls_wb2_v_vbrk_vbrp2-vrkme_i
            IMPORTING
              OUTPUT         = lv_output_uom
            EXCEPTIONS
              unit_not_found = 1
              OTHERS         = 2.
            IF sy-subrc <> 0.
* Implement suitable error handling here
            ENDIF.

            READ TABLE lt_doc_uom INTO ls_doc_uom WITH KEY meins = lv_output_uom.
            IF sy-subrc EQ 0.
              CONCATENATE '"' ls_doc_uom-edoc_uom '"' INTO ls_einv_api_itm-UNIT.
            ELSE.
              READ TABLE lt_doc_uom INTO ls_doc_uom WITH KEY meins =  ls_wb2_v_vbrk_vbrp2-vrkme_i.
              IF sy-subrc IS INITIAL.
                CONCATENATE '"' ls_doc_uom-edoc_uom '"' INTO ls_einv_api_itm-UNIT.
              ENDIF.
            ENDIF.

* Unit Price
            IF ls_wb2_v_vbrk_vbrp2-fkimg_i IS NOT INITIAL  .
              "Unit Price
*            LOOP AT lt_tvarvc INTO ls_tvarvc WHERE name = gc_var_unit.
*              READ TABLE lt_konv INTO ls_konv WITH KEY knumv = ls_wb2_v_vbrk_vbrp2-knumv
*                                                                         kposn = ls_wb2_v_vbrk_vbrp2-posnr_i
*                                                                         kschl = ls_tvarvc-low.
*              IF sy-subrc IS INITIAL.
*                EXIT.
*              ENDIF.
*            ENDLOOP.
*            IF ls_konv  IS NOT INITIAL.
*              CLEAR:lv_rate,lv_dmbtr,lv_item_amt.
*              IF lv_exp_curr IS INITIAL.
*                lv_rate = ls_konv-kbetr.
*                ls_einv_api_itm-unit_price = lv_rate.
*                lv_dmbtr = ls_wb2_v_vbrk_vbrp2-fkimg_i * lv_rate.
*                ls_einv_api_itm-total_amount = lv_dmbtr.
*                lv_item_amt = ls_wb2_v_vbrk_vbrp2-netwr_i + ls_wb2_v_vbrk_vbrp2-mwsbp_i.
*              ELSE.

*                  lv_rate = ls_konv-kbetr * ls_wb2_v_vbrk_vbrp2-kursk_i.
*                  ls_einv_api_itm-unit_price = lv_dmbtr.
*                  lv_dmbtr =  ls_wb2_v_vbrk_vbrp2-fkimg_i * lv_rate.
*                  ls_einv_api_itm-total_amount = lv_dmbtr.
*                  lv_item_amt =  ls_bseg-dmbtr.
*              ENDIF.
*
*            ENDIF.

              IF ls_wb2_v_vbrk_vbrp2-fkart = 'ZSAM'.
                READ TABLE lt_konv INTO ls_konv WITH KEY knumv = ls_wb2_v_vbrk_vbrp2-knumv
                kposn = ls_wb2_v_vbrk_vbrp2-posnr_i
                kschl = 'ZSAM'.
                IF sy-subrc IS INITIAL.
                  ls_wb2_v_vbrk_vbrp2-netwr_i = ls_konv-kwert.
                ENDIF.
              ENDIF.


              READ TABLE lt_konv INTO ls_konv WITH KEY knumv = ls_wb2_v_vbrk_vbrp2-knumv
              kposn = ls_wb2_v_vbrk_vbrp2-posnr_i
              kschl = 'ZCES'.
              IF sy-subrc IS INITIAL.
                ls_wb2_v_vbrk_vbrp2-netwr_i  = ls_wb2_v_vbrk_vbrp2-netwr_i  -  ls_konv-kwert.
              ENDIF.
              CLEAR lv_item_roundoff.
              LOOP AT lt_konv INTO ls_konv WHERE knumv = ls_wb2_v_vbrk_vbrp2-knumv
              AND kposn = ls_wb2_v_vbrk_vbrp2-posnr_i
              AND kschl IN rg_diff.
*              IF sy-subrc IS INITIAL.
                ls_wb2_v_vbrk_vbrp2-netwr_i  = ls_wb2_v_vbrk_vbrp2-netwr_i  -  ls_konv-kwert.
                lv_total_roundoff = lv_total_roundoff + ls_konv-kwert.
                lv_item_roundoff = ls_konv-kwert.
              ENDLOOP.


              IF lv_exp_curr IS NOT INITIAL.

                lv_rate = ls_wb2_v_vbrk_vbrp2-netwr_i / ls_wb2_v_vbrk_vbrp2-fkimg_i.
                ls_einv_api_itm-unit_price = lv_rate.

                PERFORM convert_currency USING ls_einv_api_itm-unit_price ls_wb2_v_vbrk_vbrp2-kurrf
                CHANGING ls_einv_api_itm-unit_price.

                ls_einv_api_itm-total_amount = ls_wb2_v_vbrk_vbrp2-netwr_i.

                PERFORM convert_currency USING ls_einv_api_itm-total_amount ls_wb2_v_vbrk_vbrp2-kurrf
                CHANGING ls_einv_api_itm-total_amount.

                lv_item_amt = ls_wb2_v_vbrk_vbrp2-netwr_i + ls_wb2_v_vbrk_vbrp2-mwsbp_i.

                PERFORM convert_currency USING lv_item_amt ls_wb2_v_vbrk_vbrp2-kurrf
                CHANGING lv_item_amt.


              ELSE.
                lv_rate = ls_wb2_v_vbrk_vbrp2-netwr_i / ls_wb2_v_vbrk_vbrp2-fkimg_i.
                ls_einv_api_itm-unit_price = lv_rate.
                ls_einv_api_itm-total_amount = ls_wb2_v_vbrk_vbrp2-netwr_i.
                lv_item_amt = ls_wb2_v_vbrk_vbrp2-netwr_i + ls_wb2_v_vbrk_vbrp2-mwsbp_i.
              ENDIF.

              CONDENSE:ls_einv_api_itm-unit_price,ls_einv_api_itm-total_amount.

            ENDIF.



            ls_einv_api_itm-pre_tax_value =  '0'.
            CONDENSE:ls_einv_api_itm-pre_tax_value.

            "Discount amount
            LOOP AT lt_tvarvc INTO ls_tvarvc WHERE name = gc_var_disc.
              READ TABLE lt_konv INTO ls_konv WITH KEY knumv = ls_wb2_v_vbrk_vbrp2-knumv
              kposn = ls_wb2_v_vbrk_vbrp2-posnr_i
              kschl = ls_tvarvc-low
              BINARY SEARCH.

              IF sy-subrc EQ 0.
                ls_konv-kwert = ls_konv-kwert * -1.
                ls_einv_api_itm-discount =  ls_einv_api_itm-discount + ls_konv-kwert.
              ENDIF.
            ENDLOOP.

            IF ls_einv_api_itm-discount IS NOT INITIAL AND lv_exp_curr IS NOT INITIAL.
              PERFORM convert_currency USING ls_einv_api_itm-discount ls_wb2_v_vbrk_vbrp2-kurrf
              CHANGING ls_einv_api_itm-discount.
            ENDIF.

            CONDENSE:ls_einv_api_itm-discount.

            ls_einv_api_itm-other_charge =  '0'.
            ls_einv_api_itm-assessable_value = ls_einv_api_itm-total_amount -
            ls_einv_api_itm-discount.

            CONDENSE:ls_einv_api_itm-assessable_value,ls_einv_api_itm-other_charge.
            lv_total_assessable_value = lv_total_assessable_value  + ls_einv_api_itm-assessable_value.

            CLEAR:lw_gstrate .
            "IGST Tax amount
            READ TABLE lt_konv INTO ls_konv WITH KEY knumv = ls_wb2_v_vbrk_vbrp2-knumv
            kposn = ls_wb2_v_vbrk_vbrp2-posnr_i
            kschl = gc_kschl_joig.
            IF sy-subrc IS NOT INITIAL.
              READ TABLE lt_konv INTO ls_konv WITH KEY knumv = ls_wb2_v_vbrk_vbrp2-knumv
              kposn = ls_wb2_v_vbrk_vbrp2-posnr_i
              kschl = 'ZIGS'.
            ENDIF.
            IF sy-subrc EQ 0.
              IF lv_exp_curr IS NOT INITIAL.
                PERFORM convert_currency USING ls_konv-kwert ls_wb2_v_vbrk_vbrp2-kurrf
                CHANGING ls_konv-kwert.
              ENDIF.

              lw_gstrate  =  ls_konv-kbetr.
              ls_einv_api_itm-igst_amount =  ls_konv-kwert.
              CONDENSE:ls_einv_api_itm-gst_rate,ls_einv_api_itm-igst_amount.
              lv_total_igst_value = lv_total_igst_value  + ls_einv_api_itm-igst_amount.
            ENDIF.
            "CGST Tax amount
            READ TABLE lt_konv INTO ls_konv WITH KEY knumv = ls_wb2_v_vbrk_vbrp2-knumv
            kposn = ls_wb2_v_vbrk_vbrp2-posnr_i
            kschl = gc_kschl_jocg.
            IF sy-subrc IS NOT INITIAL.
              READ TABLE lt_konv INTO ls_konv WITH KEY knumv = ls_wb2_v_vbrk_vbrp2-knumv
              kposn = ls_wb2_v_vbrk_vbrp2-posnr_i
              kschl = 'ZCGS'.
            ENDIF.
            IF sy-subrc EQ 0.
              IF lv_exp_curr IS NOT INITIAL.
                PERFORM convert_currency USING ls_konv-kwert ls_wb2_v_vbrk_vbrp2-kurrf
                CHANGING ls_konv-kwert.
              ENDIF.
              ls_einv_api_itm-gst_rate =  ls_konv-kbetr.
              CONDENSE ls_einv_api_itm-gst_rate.
              ADD ls_einv_api_itm-gst_rate TO lw_gstrate .
              ls_einv_api_itm-cgst_amount =  ls_konv-kwert.
              CONDENSE:ls_einv_api_itm-gst_rate ,ls_einv_api_itm-cgst_amount.
              lv_total_cgst_value = lv_total_cgst_value  + ls_einv_api_itm-cgst_amount.
            ENDIF.

            "SGST Tax amount
            READ TABLE lt_konv INTO ls_konv WITH KEY knumv = ls_wb2_v_vbrk_vbrp2-knumv
            kposn = ls_wb2_v_vbrk_vbrp2-posnr_i
            kschl = gc_kschl_josg.
            IF sy-subrc IS NOT INITIAL.
              READ TABLE lt_konv INTO ls_konv WITH KEY knumv = ls_wb2_v_vbrk_vbrp2-knumv
              kposn = ls_wb2_v_vbrk_vbrp2-posnr_i
              kschl = gc_kschl_joug.
              IF sy-subrc IS NOT INITIAL.
                READ TABLE lt_konv INTO ls_konv WITH KEY knumv = ls_wb2_v_vbrk_vbrp2-knumv
                kposn = ls_wb2_v_vbrk_vbrp2-posnr_i
                kschl = 'ZSGS'.
              ENDIF.
            ENDIF.
            IF sy-subrc EQ 0.
              IF lv_exp_curr IS NOT INITIAL.
                PERFORM convert_currency USING ls_konv-kwert ls_wb2_v_vbrk_vbrp2-kurrf
                CHANGING ls_konv-kwert.
              ENDIF.
              ls_einv_api_itm-gst_rate =  ls_konv-kbetr. "Required
              CONDENSE ls_einv_api_itm-gst_rate.
              ADD ls_einv_api_itm-gst_rate TO lw_gstrate.
              ls_einv_api_itm-sgst_amount =  ls_konv-kwert.
              CONDENSE:ls_einv_api_itm-gst_rate,ls_einv_api_itm-sgst_amount.
              lv_total_sgst_value = lv_total_sgst_value  + ls_einv_api_itm-sgst_amount.
            ENDIF.

            "CESS
            READ TABLE lt_konv INTO ls_konv WITH KEY knumv = ls_wb2_v_vbrk_vbrp2-knumv
            kposn = ls_wb2_v_vbrk_vbrp2-posnr_i
            kschl = gc_kschl_cess.
            IF sy-subrc EQ 0.
              IF lv_exp_curr IS NOT INITIAL.
                PERFORM convert_currency USING ls_konv-kwert ls_wb2_v_vbrk_vbrp2-kurrf
                CHANGING ls_konv-kwert.
              ENDIF.
              ls_einv_api_itm-gst_rate =  ls_konv-kbetr. "Required
              CONDENSE ls_einv_api_itm-gst_rate.
              ADD ls_einv_api_itm-gst_rate TO lw_gstrate.
              ls_einv_api_itm-cess_amount =  ls_konv-kwert.
              CONDENSE:ls_einv_api_itm-cess_rate,ls_einv_api_itm-cess_amount.
              lv_total_cess_value = lv_total_cess_value  + ls_einv_api_itm-cess_amount.
            ENDIF.

            "cess_nonadvol_amount
            READ TABLE lt_konv INTO ls_konv WITH KEY knumv = ls_wb2_v_vbrk_vbrp2-knumv
            kposn = ls_wb2_v_vbrk_vbrp2-posnr_i
            kschl = 'ZCES'.
            IF sy-subrc EQ 0.
              IF lv_exp_curr IS NOT INITIAL.
                PERFORM convert_currency USING ls_konv-kwert ls_wb2_v_vbrk_vbrp2-kurrf
                CHANGING ls_konv-kwert.
              ENDIF.
              ls_einv_api_itm-cess_nonadvol_value =  ls_konv-kwert.
              CONDENSE:ls_einv_api_itm-cess_nonadvol_value.
              lv_total_cess_nonadvol_value = lv_total_cess_nonadvol_value  + ls_einv_api_itm-cess_nonadvol_value.
            ENDIF.


            "TCS Charges JTC1

            LOOP AT lt_konv INTO ls_konv WHERE knumv = ls_wb2_v_vbrk_vbrp2-knumv
            AND kposn = ls_wb2_v_vbrk_vbrp2-posnr_i
            AND ( kschl = gc_kschl_jtc1 OR kschl = gc_kschl_ztcs ).
              lv_total_tcs_value = lv_total_tcs_value + ls_konv-kwert.
            ENDLOOP.



            ls_einv_api_itm-total_item_value =  ( ls_einv_api_itm-assessable_value +
            ls_einv_api_itm-cgst_amount +
            ls_einv_api_itm-sgst_amount +
            ls_einv_api_itm-igst_amount +
            ls_einv_api_itm-cess_amount +
            ls_einv_api_itm-state_cess_amount +
            ls_einv_api_itm-cess_nonadvol_value ).

            CONDENSE:ls_einv_api_itm-total_item_value.

* round off value
            CLEAR lv_roundoff.
*            IF lv_item_amt NE ls_einv_api_itm-total_item_value.
*              lv_roundoff = lv_item_amt - ls_einv_api_itm-total_item_value.
*            ENDIF.


            lv_total_invoice_value = lv_total_invoice_value + ls_einv_api_itm-total_item_value + lv_roundoff.


            CONCATENATE '"' ls_wb2_v_vbrk_vbrp2-land1 '"' INTO ls_einv_api_itm-country_origin.
            "get billing item text object VBBP
            CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
            EXPORTING
              INPUT  = ls_wb2_v_vbrk_vbrp2-matnr_i
            IMPORTING
              OUTPUT = ls_wb2_v_vbrk_vbrp2-matnr_i.

            CONCATENATE '"' ls_wb2_v_vbrk_vbrp2-matnr_i '"' INTO ls_einv_api_itm-order_line_reference.
            CONCATENATE '"' ls_wb2_v_vbrk_vbrp2-matnr_i '"' INTO ls_einv_api_itm-product_serial_number.

            CONCATENATE '"' ls_wb2_v_vbrk_vbrp2-charg_i '"' INTO ls_einv_api_itm-name.
            ls_einv_api_itm-expiry_date =  '" "'.
            ls_einv_api_itm-warranty_date = '" "'.

*Begin of  attribute_details"
            ls_einv_api_itm-item_attribute_details = '" "'.
            ls_einv_api_itm-item_attribute_value = '""'.
*End of of  attribute_details"
*End of item_list
            ls_einv_api_itm-gst_rate = lw_gstrate.
            CONDENSE:ls_einv_api_itm-gst_rate.
            APPEND ls_einv_api_itm TO lt_einv_api_itm.
            CLEAR:ls_einv_api_itm .
          ENDLOOP.

          IF lv_total_roundoff < 0.
            lv_total_roundoff  = lv_total_roundoff * -1.
            ls_einv_api_hdr-round_off_amount  = lv_total_roundoff.
            CONCATENATE '-' ls_einv_api_hdr-round_off_amount INTO ls_einv_api_hdr-round_off_amount.
          ELSE.
            ls_einv_api_hdr-round_off_amount  = lv_total_roundoff.
          ENDIF.
          CONDENSE ls_einv_api_hdr-round_off_amount NO-GAPS.


* Set supply type
          IF lv_export IS NOT INITIAL AND lv_total_igst_value IS INITIAL.
            ls_einv_api_hdr-supply_type = gc_sup_type_expwop.
          ENDIF.


          IF  ls_einv_api_hdr-supply_type   = gc_sup_type_sezwp.
            IF lv_total_igst_value IS INITIAL AND lv_total_cgst_value IS INITIAL AND
            lv_total_sgst_value IS INITIAL.
              ls_einv_api_hdr-supply_type   = gc_sup_type_sezwop.
            ENDIF.
          ENDIF.

* Change the charge type for rcm
          READ TABLE lt_konv INTO ls_konv WITH KEY knumv = ls_wb2_v_vbrk_vbrp2-knumv
          kposn = ls_wb2_v_vbrk_vbrp2-posnr_i
          kschl = 'ZIGS'.
          IF sy-subrc IS NOT INITIAL.
            READ TABLE lt_konv INTO ls_konv WITH KEY knumv = ls_wb2_v_vbrk_vbrp2-knumv
            kposn = ls_wb2_v_vbrk_vbrp2-posnr_i
            kschl = 'ZCGS'.
          ENDIF.
          IF sy-subrc IS INITIAL.
            ls_einv_api_hdr-charge_type = gc_charge_y.
          ENDIF.



          ls_einv_api_hdr-total_assessable_value = lv_total_assessable_value.
          ls_einv_api_hdr-total_cgst_value =       lv_total_cgst_value.
          ls_einv_api_hdr-total_sgst_value =  lv_total_sgst_value.
          ls_einv_api_hdr-total_igst_value =   lv_total_igst_value .
          ls_einv_api_hdr-total_cess_value =  lv_total_cess_value.
          ls_einv_api_hdr-total_cess_nonadvol_value =  lv_total_cess_nonadvol_value.
          ls_einv_api_hdr-total_other_charge  = lv_total_tcs_value.

*          ls_einv_api_hdr-total_invoice_value =  lv_total_invoice_value + ls_einv_api_hdr-total_other_charge.
          ls_einv_api_hdr-total_invoice_value =  lv_total_invoice_value + ls_einv_api_hdr-total_other_charge + ls_einv_api_hdr-round_off_amount .

          CONDENSE:ls_einv_api_hdr-total_assessable_value,ls_einv_api_hdr-total_cgst_value,ls_einv_api_hdr-total_sgst_value,ls_einv_api_hdr-total_igst_value,
          ls_einv_api_hdr-total_invoice_value,ls_einv_api_hdr-total_cess_value,ls_einv_api_hdr-total_cess_nonadvol_value,ls_einv_api_hdr-total_other_charge.
          APPEND ls_einv_api_hdr TO lt_einv_api_hdr.
          CLEAR:ls_einv_api_hdr.
        ENDIF.
        CLEAR:ls_wb2_v_vbrk_vbrp2.
        IF lt_einv_api_hdr IS NOT INITIAL.
          SORT lt_einv_api_hdr .
          SORT lt_einv_api_itm .
          DELETE ADJACENT DUPLICATES FROM   lt_einv_api_hdr  COMPARING ALL FIELDS.
          DELETE ADJACENT DUPLICATES FROM  lt_einv_api_itm  COMPARING ALL FIELDS.
          lt_api_hdr = lt_einv_api_hdr .
          lt_api_itm = lt_einv_api_itm .
          CLEAR:ls_token,ls_return.
          SELECT SINGLE apiuri FROM zteinv_api INTO ls_token WHERE apiid = gc_apiid_token.
          IF ls_token IS NOT INITIAL.
            ls_token = ls_token.
          ELSE.
            CALL FUNCTION 'ZFM_EINVOICE_OAUTH_API'
            IMPORTING
              ex_token    = ls_token
              ex_return   = ls_return
              et_messages = lt_messages.
          ENDIF.
          IF ls_token IS NOT INITIAL." AND LS_return EQ 'S'.
            CALL FUNCTION 'ZFM_EINVOICE_GENERATE_API'
            EXPORTING
              im_token        = ls_token
              im_api_hdr      = lt_api_hdr
              im_api_itm      = lt_api_itm
            IMPORTING
              ex_return       = ls_return
              ex_invref       = lt_invref
              ex_messages     = lt_messages
              ex_einv_details = lt_einv_details.

            IF lt_invref IS NOT INITIAL."LS_return EQ 'S'.
              MODIFY j_1ig_invrefnum FROM TABLE lt_invref.

              IF sy-subrc EQ 0.
                READ TABLE lt_invref INTO ls_invref INDEX 1.
                IF sy-subrc IS INITIAL AND ls_invref-irn IS NOT INITIAL.

                ELSE.
*                  ex_error = abap_true.

                ENDIF.
                MODIFY zteinv_details FROM TABLE lt_einv_details.
                COMMIT WORK.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.



ENDFUNCTION.
