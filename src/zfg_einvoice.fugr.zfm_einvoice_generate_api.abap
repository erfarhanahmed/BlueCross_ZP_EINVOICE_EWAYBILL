FUNCTION ZFM_EINVOICE_GENERATE_API.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_TOKEN) TYPE  STRING
*"     VALUE(IM_API_HDR) TYPE  ZTT_EINV_API_STRUCT
*"     VALUE(IM_API_ITM) TYPE  ZTT_EINV_API_STRUCT_ITM
*"     VALUE(IM_API_DWN) TYPE  XFELD OPTIONAL
*"  EXPORTING
*"     VALUE(EX_RETURN) TYPE  STRING
*"     VALUE(EX_INVREF) TYPE  ZTT_J_1IG_INVREFNUM
*"     VALUE(EX_MESSAGES) TYPE  BAPIRET2_T
*"     VALUE(EX_ERROR) TYPE  STRING
*"     VALUE(EX_EINV_DETAILS) TYPE  ZTT_EINV_DETAILS
*"----------------------------------------------------------------------
  TYPES:BEGIN OF ty_message,
    ackno         TYPE string,
    ackdt         TYPE string,
    irn           TYPE string,
    signedinvoice TYPE string,
    signedqrcode  TYPE string,
    ewbno         TYPE string,
    ewbdt         TYPE string,
    ewbvalidtill  TYPE string,
    qrcodeurl     TYPE string,
    einvoicepdf   TYPE string,
    status        TYPE string,
    alert         TYPE string,
    error         TYPE string,
  END OF ty_message,
  BEGIN OF ty_output,
    MESSAGE      TYPE ty_message,
    errormessage TYPE string,
    infodtls     TYPE string,
    status       TYPE string,
    CODE         TYPE string,
  END OF ty_output.

  DATA:ls_output TYPE ty_output.

  DATA lw_message TYPE bapiret2.
  DATA:lt_api_hdr TYPE STANDARD TABLE OF zst_einv_api_struct,
        lw_api_hdr TYPE zst_einv_api_struct.

  DATA:lt_api_itm TYPE STANDARD TABLE OF zst_einv_api_struct_itm,
        lw_api_itm TYPE zst_einv_api_struct_itm.

  DATA : lv_ew  TYPE string,
        lv_ew1 TYPE string.
  DATA : lv_ew2      TYPE string,
        lv_ew3      TYPE string,
        lv_ew4      TYPE string,
        lv_ew5      TYPE string,
        lv_ew6      TYPE string,
        lv_ew7      TYPE string,
        lv_ew8      TYPE string,
        lv_ew9      TYPE string,
        lv_ew10     TYPE string,
        lv_irn      TYPE string,
        alert       TYPE string,
        http_status TYPE string.

  DATA: "wa_einv TYPE zeinv_gen,
        lv_date TYPE dats.

  DATA : lc_doc TYPE string.

  DATA: lo_http_client     TYPE REF TO if_http_client,
        lv_service         TYPE string,
        lv_result          TYPE string,
        lo_ixml            TYPE REF TO if_ixml,
        lo_streamfactory   TYPE REF TO if_ixml_stream_factory,
        lo_istream         TYPE REF TO if_ixml_istream,
        lo_document        TYPE REF TO if_ixml_document,
        lo_parser          TYPE REF TO if_ixml_parser,
        lo_weather_element TYPE REF TO if_ixml_element,
        lo_weather_nodes   TYPE REF TO if_ixml_node_list,
        lo_curr_node       TYPE REF TO if_ixml_node,
        lv_value           TYPE string,
        lv_node_length     TYPE I,
        lv_node_index      TYPE I,
        lv_node_name       TYPE string,
        lv_node_value      TYPE string.
  DATA : lo_rest_client TYPE REF TO if_rest_client,
        lo_rest_entity TYPE REF TO if_rest_entity.
  DATA: ls_match        TYPE match_result,
        lv_offset_start TYPE sy-tabix,
        lv_offset_end   TYPE sy-tabix,
        lv_string       TYPE string,
        lv_model_code   TYPE char12,
        lv_length       TYPE sy-tabix,
        lv_post         TYPE  string,
        lc_string       TYPE string,
        lv_token        TYPE string,
        lv_length1      TYPE string.

  DATA lr_rest_exception TYPE REF TO cx_rest_client_exception.

  DATA:lw_inref          TYPE  j_1ig_invrefnum,
        lw_zteinv_details TYPE zteinv_details,
        lw_config         TYPE zteinv_api,
        lv_url            TYPE string.
  DATA:it_temp TYPE TABLE OF zst_einv_api_struct_itm.

  CLEAR ex_messages.

  lv_token = im_token.

  lt_api_hdr = im_api_hdr.
  lt_api_itm = im_api_itm.

  SORT  lt_api_hdr BY document_number.
  SORT  lt_api_itm BY document_number.


  SELECT SINGLE * FROM zteinv_api INTO lw_config WHERE apiid = 'EINVGEN'.
  IF sy-subrc IS INITIAL.
    lv_url = lw_config-apiuri.
  ENDIF.


  LOOP AT lt_api_hdr INTO  lw_api_hdr.

    lv_doc = lw_api_hdr-document_number.
    REPLACE ALL OCCURRENCES OF  '"' IN lv_doc WITH space.
    CLEAR lv_post.

    CONCATENATE : '{'
    '"access_token":' lv_token','
    '"user_gstin":' lw_api_hdr-user_gstin ','
    '"data_source": "erp",'
    INTO  lv_post SEPARATED BY ' ' .

    CONCATENATE : lv_post '"transaction_details":' '{'
    '"supply_type":' lw_api_hdr-supply_type ',' "B2B",'
    '"charge_type":' lw_api_hdr-charge_type ',' "N",'
    '"ecommerce_gstin":' lw_api_hdr-ecommerce_gstin  ""'
    '},' INTO  lv_post SEPARATED BY ' '.

    CONCATENATE : lv_post '"document_details":' '{'
    '"document_type":'  lw_api_hdr-document_type ',' "INV"' ','   Y
    '"document_number":' lw_api_hdr-document_number ','          " Y
    '"document_date":' lw_api_hdr-document_date "09/03/2020"'     Y
    '},' INTO lv_post SEPARATED BY ' '.

    IF lw_api_hdr-pincode_s IS INITIAL.
      lw_api_hdr-pincode_s = '0'.
    ENDIF.

    CONCATENATE : lv_post  '"seller_details":' '{'
    '"gstin":' lw_api_hdr-gstin_s ','
    '"legal_name":' lw_api_hdr-legal_name_s ','
    '"trade_name":' lw_api_hdr-trade_name_s ','
    '"address1":' lw_api_hdr-address1_s ','"VILA",'                           Y
    '"address2":' lw_api_hdr-address2_s ',' "VILA",'
    '"location":' lw_api_hdr-location_s ',' "Noida",'                         Y
    '"pincode":' lw_api_hdr-pincode_s ','" 201301 ','                         Y
    '"state_code":' lw_api_hdr-state_code_s ',' "UTTAR PRADESH",'             Y
*                           '"phone_number":' lw_api_hdr-phone_number_s ',' "9876543231,'
    '"email":' lw_api_hdr-email_s "abc@xyz.com"'
    '},' INTO lv_post SEPARATED BY ' '.


    IF lw_api_hdr-pincode_b IS INITIAL.
      lw_api_hdr-pincode_b = '0'.
    ENDIF.

    CONCATENATE : lv_post '"buyer_details":' '{'
    '"gstin":' lw_api_hdr-gstin_b ',' "05AAAPG7885R002",'                  Y
    '"legal_name":' lw_api_hdr-legal_name_b ',' "MastersIndia UT",'        Y
    '"trade_name":' lw_api_hdr-trade_name_b ',' "MastersIndia UT",'
    '"address1":' lw_api_hdr-address1_b ',' "KILA",'                       Y
    '"address2":' lw_api_hdr-address2_b ',' "KILA",'
    '"location":' lw_api_hdr-location_b ',' "Nainital",'                   Y
    '"pincode":' lw_api_hdr-pincode_b  ',' "110010,'
    '"place_of_supply":' lw_api_hdr-place_of_supply_b ',' "9",'          Y
    '"state_code":' lw_api_hdr-state_code_b ',' "UTTARAKHAND",'
*                          '"phone_number":' lw_api_hdr-phone_number_b ',' " ,'
    '"email":'  lw_api_hdr-email_b  "abc@xyz.com"'
    '},' INTO lv_post SEPARATED BY ' '.


    IF lw_api_hdr-pincode_d IS INITIAL.
      lw_api_hdr-pincode_d = '0'.
    ENDIF.
*& dispatch detailed commented as until specifically required by customer
    IF lw_api_hdr-dispatch = abap_true.
      CONCATENATE : lv_post '"dispatch_details":' '{'
      '"company_name":' lw_api_hdr-comapny_name_d ',' "MastersIndia UP",'     Y
      '"address1":' lw_api_hdr-address1_d ',' "VILA",'                        Y
      '"address2":' lw_api_hdr-address2_d ',' "VILA",'
      '"location":' lw_api_hdr-location_d ',' "Noida",'                      Y
      '"pincode":' lw_api_hdr-pincode_d ',' "201301,'                        Y
      '"state_code":' lw_api_hdr-state_code_d  "UTTAR PRADESH"'               Y
      '},' INTO lv_post SEPARATED BY ' '.
    ENDIF.

*& dispatch detailed commented as until specifically required by customer
    IF lw_api_hdr-pincode_sh IS INITIAL.
      lw_api_hdr-pincode_sh = '0'.
    ENDIF.


    IF lw_api_hdr-supply_type   NE '"EXPWP"' AND  lw_api_hdr-supply_type NE '"EXPWOP"'.
*      IF lw_api_hdr-gstin_sh NE lw_api_hdr-gstin_b AND lw_api_hdr-address1_sh NE lw_api_hdr-address1_b.
      IF lw_api_hdr-ship_to = abap_true.
        CONCATENATE : lv_post '"ship_details":' '{'
        '"gstin":' lw_api_hdr-gstin_sh ','
        '"legal_name":' lw_api_hdr-legal_name_sh ','
        '"trade_name":' lw_api_hdr-trade_name_sh ','
        '"address1":' lw_api_hdr-address1_sh ','
        '"address2":' lw_api_hdr-address1_sh ','
        '"location":' lw_api_hdr-location_sh ','
        '"pincode":' lw_api_hdr-pincode_sh ','
        '"state_code":' lw_api_hdr-state_code_sh
        '},' INTO lv_post SEPARATED BY ' '.

      ENDIF.
    ENDIF.



    IF lw_api_hdr-supply_type   = '"EXPWP"' OR lw_api_hdr-supply_type = '"EXPWOP"'.

      CONCATENATE : lv_post '"export_details":' '{'
      '"ship_bill_number":' lw_api_hdr-ship_bill_number ','
      '"ship_bill_date":' lw_api_hdr-ship_bill_date ','
      '"country_code":' lw_api_hdr-country_code ','
      '"foreign_currency":' lw_api_hdr-foreign_currency ','
      '"refund_claim":' lw_api_hdr-refund_claim ','
      '"port_code":' lw_api_hdr-port_code
      '},'  INTO lv_post SEPARATED BY ' '.

    ENDIF.

    CONCATENATE : lv_post '"payment_details": {'
    '"bank_account_number":' lw_api_hdr-bank_account_number ','
    '"paid_balance_amount":' lw_api_hdr-paid_balance_amount ','
    '"credit_days":' lw_api_hdr-credit_days ','
    '"credit_transfer":' lw_api_hdr-credit_transfer ',' "credit_transfer",'
    '"direct_debit":' lw_api_hdr-direct_debit ',' "Direct Debit",'
    '"branch_or_ifsc":' lw_api_hdr-branch_or_ifsc ',' "KKK000180",'
    '"payment_mode":' lw_api_hdr-payment_mode ',' "CASH",'
    '"payee_name":' lw_api_hdr-payee_name ',' "Payee Name",'
    '"payment_due_date":' lw_api_hdr-payment_due_date ',' "09/03/2020",'
    '"payment_instruction":' lw_api_hdr-payment_instruction ',' "Payment Instruction",'
    '"payment_term":' lw_api_hdr-payment_term "Terms of Payment"'
    '},' INTO lv_post SEPARATED BY ' '.

    IF lw_api_hdr-document_type EQ '"CRN"' OR lw_api_hdr-document_type EQ '"DBN"'.

      CONCATENATE : lv_post '"reference_details":' '{'
      '"invoice_remarks":' lw_api_hdr-invoice_remarks ',' "Invoice Remarks",'          y
      '"document_period_details":'  '{'
      '"invoice_period_start_date":' lw_api_hdr-invoice_period_start_date ',' "2020-02-01",'                y
      '"invoice_period_end_date":' lw_api_hdr-invoice_period_end_date "',' "2020-02-30",'                       y
      '},'
      '"preceding_document_details":' '[' '{'
      '"reference_of_original_invoice":' lw_api_hdr-reference_of_original_invoice ',' "CFRT/0006",'    y
      '"preceding_invoice_date":' lw_api_hdr-preceding_invoice_date ','  "09/03/2020",'                 y
      '"other_reference":' lw_api_hdr-other_reference "2334"'                                           y
      '}'
      ']},'   INTO lv_post SEPARATED BY ' '.

    ELSE.

      CONCATENATE : lv_post '"reference_details":' '{'
      '"invoice_remarks":' lw_api_hdr-invoice_remarks ',' "Invoice Remarks",'          y
      '"document_period_details":'  '{'
      '"invoice_period_start_date":' lw_api_hdr-invoice_period_start_date ',' "2020-02-01",'                y
      '"invoice_period_end_date":' lw_api_hdr-invoice_period_end_date  "2020-02-30",'                       y
      '}'
      '},' INTO lv_post SEPARATED BY ' '.

    ENDIF.

*    CONCATENATE : lv_post '"contract_details":' '[' '{'
*                          '"receipt_advice_number":' lw_api_hdr-receipt_advice_number ',' "aaa",'                                 y
*                          '"receipt_advice_date":' lw_api_hdr-receipt_advice_date ',' "09/03/2020",'                              y
*                          '"batch_reference_number":' lw_api_hdr-batch_reference_number ',' "2334",'                              y
*                          '"contract_reference_number":' lw_api_hdr-contract_reference_number ',' "2334",'                        y
*                          '"other_reference":' lw_api_hdr-other_reference_c ',' "2334" ,'                                         y
*                          '"project_reference_number":' lw_api_hdr-project_reference_number ',' "2334",'                          y
*                          '"vendor_po_reference_number":' lw_api_hdr-vendor_po_reference_number ',' "233433454545",'              y
*                          '"vendor_po_reference_date":' lw_api_hdr-vendor_po_reference_date "09/03/2020"'                         y
*                          '}'
*                          ']'
*                          '},'  INTO lv_post SEPARATED BY ' '.

*    CONCATENATE: lv_post '"additional_document_details":' '['  '{'
*                         '"supporting_document_url":' lw_api_hdr-supporting_document_url ',' "",'
*                         '"supporting_document":' lw_api_hdr-supporting_document ',' "india",'
*                         '"additional_information":' lw_api_hdr-additional_information  "',' "india"' " changed
*                         '}'
*                         '],'  INTO lv_post SEPARATED BY ' '.

    CONCATENATE : lv_post '"value_details":' '{'
    '"total_assessable_value":' lw_api_hdr-total_assessable_value ',' "1,'                    y
    '"total_cgst_value":' lw_api_hdr-total_cgst_value ',' " 0,'
    '"total_sgst_value":' lw_api_hdr-total_sgst_value ',' "0,'
    '"total_igst_value":' lw_api_hdr-total_igst_value ',' " 0.01,'
    '"total_cess_value":' lw_api_hdr-total_cess_value ',' "0,'
    '"total_cess_nonadvol_value":' lw_api_hdr-total_cess_nonadvol_value ','" 0,'
    '"total_other_charge":' lw_api_hdr-total_other_charge ','
    '"total_invoice_value":' lw_api_hdr-total_invoice_value ',' "1.01,'                      y
    '"total_cess_value_of_state":' lw_api_hdr-total_cess_value_of_state ',' "0,'
    '"round_off_amount":' lw_api_hdr-round_off_amount ',' "0,'
    '"total_invoice_value_additional_currency":' lw_api_hdr-total_invoice_value_additional " 0'
    '},'  INTO lv_post SEPARATED BY ' '.

*    CONCATENATE : lv_post '"ewaybill_details":' '{'
*                          '"transporter_id":' lw_api_hdr-transporter_id ',' "05AAABB0639G1Z8",'          y
*                          '"transporter_name":' lw_api_hdr-transporter_name ',' "Jay Trans",'            y
*                          '"transportation_mode":' lw_api_hdr-transportation_mode ',' "1",'                        y
*                          '"transportation_distance":' lw_api_hdr-transportation_distance ',' "120",'               y
*                          '"transporter_document_number":' lw_api_hdr-transporter_document_number ',' "1230",'    y
*                          '"transporter_document_date":' lw_api_hdr-transporter_document_date ',' "09/03/2020",'   y
*                          '"vehicle_number":' lw_api_hdr-vehicle_number ',' "PQR1234",'        y
*                          '"vehicle_type":' lw_api_hdr-vehicle_type  "R"'            y
*                          '},' INTO lv_post SEPARATED BY ' '.


    REFRESH it_temp.
    it_temp  = lt_api_itm.
    DELETE it_temp  WHERE document_number NE lw_api_hdr-document_number.


    CONCATENATE : lv_post '"item_list":' '['
    INTO lv_post SEPARATED BY ' '.


    LOOP AT it_temp INTO lw_api_itm WHERE  document_number EQ lw_api_hdr-document_number.

      IF lw_api_itm-UNIT IS INITIAL.
        lw_api_itm-UNIT = '" "'.
      ENDIF.

      IF sy-tabix NE 1.
        CONCATENATE lv_post ',' INTO lv_post.
      ENDIF.
      CONCATENATE : lv_post  '{'
      '"item_serial_number":' lw_api_itm-item_serial_number ',' "8965",'                y
      '"product_description":' lw_api_itm-product_description ',' "Wheat desc",'
      '"is_service":' lw_api_itm-is_service ',' "N",'                                    y
      '"hsn_code":' lw_api_itm-hsn_code ',' "1001",'                                     y
**********                             '"bar_code":' lw_api_itm-bar_code ',' "1212",'
      '"quantity":' lw_api_itm-quantity ',' "1,'
*                             '"free_quantity":' lw_api_itm-free_quantity ',' "0,'
      '"unit":' lw_api_itm-UNIT ',' "KGS",'                                              Y
      '"unit_price":' lw_api_itm-unit_price ',' "1,'                                     y
      '"total_amount":' lw_api_itm-total_amount ',' "1,'                                 y
      '"pre_tax_value":' lw_api_itm-pre_tax_value ',' "0,'
      '"discount":' lw_api_itm-discount ',' "0,'
      '"other_charge":' lw_api_itm-other_charge ',' "0,'
      '"assessable_value":' lw_api_itm-assessable_value ',' "1,'                           y
      '"gst_rate":' lw_api_itm-gst_rate ',' "0,'                                           y
      '"igst_amount":' lw_api_itm-igst_amount ',' "0,'
      '"cgst_amount":' lw_api_itm-cgst_amount ',' "0,'
      '"sgst_amount":' lw_api_itm-sgst_amount ',' "1,'
      '"cess_rate":' lw_api_itm-cess_rate ',' "0,'
      '"cess_amount":' lw_api_itm-cess_amount ',' "0,'
      '"cess_nonadvol_amount":' lw_api_itm-cess_nonadvol_value ',' " 0,'
      '"state_cess_rate":' lw_api_itm-state_cess_rate ',' " 0,'
      '"state_cess_amount":' lw_api_itm-state_cess_amount ',' " 0,'
      '"state_cess_nonadvol_amount":' lw_api_itm-state_cess_nonadvol_amount ',' "0,'
      '"total_item_value":' lw_api_itm-total_item_value  "1,'                             y
*                             '"country_origin":' lw_api_itm-country_origin ',' "52",'
*                             '"order_line_reference":' lw_api_itm-order_line_reference ',' "5236",'
*                             '"product_serial_number":' lw_api_itm-product_serial_number ',' "14785",'
*                             '"batch details":' '{'
*                             '"name":' lw_api_itm-name
*                             '"expiry_date":' lw_api_itm-expiry_date ',' "2020-02-10",'
*                             '"warranty_date":' lw_api_itm-warranty_date  "2020-02-20"'
*                             '},'
*                             '"attribute_details":' '[' '{'
*                             '"item_attribute_details":' lw_api_itm-item_attribute_details ',' "aaa",'
*                             '"item_attribute_value":' lw_api_itm-item_attribute_value "147852"'
*                             '}'
*                             ']'
      '}'

      INTO lv_post SEPARATED BY ' '.


    ENDLOOP.

    CONCATENATE : lv_post ']' INTO lv_post SEPARATED BY ' '.
    CONCATENATE : lv_post   '}' INTO lv_post SEPARATED BY ' '.

    IF im_api_dwn = 'X'.
      CALL FUNCTION 'ZFM_EDOC_DOWNLOAD'
      EXPORTING
        im_data = lv_post.
      EXIT.
    ENDIF.

    cl_http_client=>create_by_url(
    EXPORTING
      url = lv_url "'https://clientbasic.mastersindia.co/generateEinvoice'
    IMPORTING
      CLIENT = lo_http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active = 2
      internal_error = 3
      OTHERS = 4 ).
    IF sy-subrc = 0.
* Create REST Client
      CREATE OBJECT lo_rest_client TYPE cl_rest_http_client
      EXPORTING
        io_http_client = lo_http_client.
* Create REST entity Object
      lo_rest_entity = lo_rest_client->create_request_entity( ).


      lo_rest_entity->set_content_type( if_rest_media_type=>gc_appl_json ).

      CALL METHOD lo_rest_entity->set_string_data
      EXPORTING
        iv_data = lv_post.
      TRY.
* POST request ( or call the web service )
        lo_rest_client->post( lo_rest_entity ).
      CATCH cx_rest_client_exception INTO lr_rest_exception.

      ENDTRY.
* Get Response entiry
      lo_rest_entity = lo_rest_client->get_response_entity( ).

*Get Respose XML as string
      lv_string = lo_rest_entity->get_string_data( ).


**  http return status
      http_status  = lo_rest_entity->get_header_field( '~status_code' ).

      IF  http_status EQ '200'.
        ex_return = 'S'.


        SPLIT lv_string AT ':' INTO lv_ew1 lv_ew2.

        REPLACE FIRST OCCURRENCE OF '{' IN lv_ew2 WITH space.
        lv_length1 = STRLEN( lv_ew2 ).
        lv_length1 = lv_length1 - 1.
        lv_ew2 = lv_ew2+0(lv_length1).


        CALL METHOD cl_fdt_json=>json_to_data
        EXPORTING
          iv_json = lv_ew2
        CHANGING
          ca_data = ls_output.



        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = lv_doc
        IMPORTING
          OUTPUT = lv_doc.


        IF ls_output-CODE = '204'.

          lw_message-TYPE = 'E'.
          lw_message-message_v1 = ls_output-errormessage.
          lw_message-message_v2 = 'for document'.
          lw_message-message_v3 = lv_doc.

          APPEND lw_message TO ex_messages.

          ex_return =  'E'.
          lw_inref-irn_status = 'ERR'.

          lw_inref-ernam = sy-uname.
          lw_inref-erdat = sy-datum.
          lw_inref-erzet = sy-uzeit.

          lw_inref-bukrs  = lw_api_hdr-bukrs.
          lw_inref-doc_year = lw_api_hdr-doc_year.
          lw_inref-doc_type = lw_api_hdr-doc_type.
          lw_inref-docno = lw_api_hdr-vbeln.
          lw_inref-odn = lv_doc.
          APPEND lw_inref TO ex_invref.
          CLEAR lw_inref.

* Custom table update
          lw_zteinv_details-bukrs  = lw_api_hdr-bukrs.
          lw_zteinv_details-gjahr = lw_api_hdr-doc_year.
          lw_zteinv_details-doctyp = lw_api_hdr-doc_type.
          lw_zteinv_details-docno = lw_api_hdr-vbeln.
          lw_zteinv_details-einv_error = ls_output-errormessage.
          lw_zteinv_details-ernam = sy-uname.
          lw_zteinv_details-erdat = sy-datum.
          APPEND lw_zteinv_details TO ex_einv_details.
          CONTINUE.


      ELSEIF ls_output-CODE = '200'.

          lw_inref-ack_no  = ls_output-MESSAGE-ackno. " Ack no

* Date
          SPLIT  ls_output-MESSAGE-ackdt AT space INTO lv_ew1 lv_ew2.
          CONCATENATE lv_ew1+0(4) lv_ew1+5(2) lv_ew1+8(2) INTO lv_date.
          lw_inref-ack_date = lv_date.
          lw_inref-irn = ls_output-MESSAGE-irn.
          lw_inref-signed_inv = ls_output-MESSAGE-signedinvoice.
          lw_inref-signed_qrcode = ls_output-MESSAGE-signedqrcode.
******* extract status
          lw_inref-irn_status = 'ACT'.

          lw_inref-ernam = sy-uname.
          lw_inref-erdat = sy-datum.
          lw_inref-erzet = sy-uzeit.

          lw_inref-bukrs  = lw_api_hdr-bukrs.
          lw_inref-doc_year = lw_api_hdr-doc_year.
          lw_inref-doc_type = lw_api_hdr-doc_type.
          lw_inref-docno = lw_api_hdr-vbeln.
          lw_inref-odn = lv_doc.


          lw_message-TYPE = 'S'.
          lw_message-message_v1 = 'IRN successfully generated for document'.
          lw_message-message_v2 = lv_doc.
          APPEND lw_message TO ex_messages.

          APPEND lw_inref TO ex_invref.
          CLEAR:lw_inref.

* Custom table update
          lw_zteinv_details-bukrs  = lw_api_hdr-bukrs.
          lw_zteinv_details-gjahr = lw_api_hdr-doc_year.
          lw_zteinv_details-doctyp = lw_api_hdr-doc_type.
          lw_zteinv_details-docno = lw_api_hdr-vbeln.
          lw_zteinv_details-ernam = sy-uname.
          lw_zteinv_details-erdat = sy-datum.
          lw_zteinv_details-qrcodeurl = ls_output-MESSAGE-qrcodeurl.
          lw_zteinv_details-einv_pdf = ls_output-MESSAGE-einvoicepdf.

          APPEND lw_zteinv_details TO ex_einv_details.
          CLEAR lw_zteinv_details.


        ENDIF.



*****        SPLIT  lv_string AT ',' INTO :  lv_ew lv_ew1 lv_ew2 lv_ew3 lv_ew4 lv_ew5 lv_ew6 lv_ew7 lv_ew8 lv_ew9 lv_ew10.
*****        CLEAR: lv_ew5, lv_ew7,lv_ew8.
*****        IF lv_ew1 CA 'errorMessage'.
*****          SPLIT lv_ew1 AT ':' INTO lv_ew5 lv_ew7.
******          SPLIT lv_ew7 AT ':' INTO lv_ew8 lv_ew9.
*****
*****
*****          lw_message-type = 'E'.
*****          lw_message-message_v1 = lv_ew7.
*****          lw_message-message_v2 = 'for document'.
*****          lw_message-message_v3 = lv_doc.
*****
*****          APPEND lw_message TO ex_messages.
*****
*****          ex_return =  'E'.
*****          lw_inref-irn_status = 'ERR'.
*****
*****          lw_inref-ernam = sy-uname.
*****          lw_inref-erdat = sy-datum.
*****          lw_inref-erzet = sy-uzeit.
*****
*****          lw_inref-bukrs  = lw_api_hdr-bukrs.
*****          lw_inref-doc_year = lw_api_hdr-doc_year.
*****          lw_inref-doc_type = lw_api_hdr-doc_type.
*****          lw_inref-docno = lw_api_hdr-vbeln.
*****          lw_inref-odn = lv_doc.
*****          APPEND lw_inref TO ex_invref.
*****          CLEAR lw_inref.
*****
****** Custom table update
*****          lw_zteinv_details-bukrs  = lw_api_hdr-bukrs.
*****          lw_zteinv_details-gjahr = lw_api_hdr-doc_year.
*****          lw_zteinv_details-doctyp = lw_api_hdr-doc_type.
*****          lw_zteinv_details-docno = lw_api_hdr-vbeln.
*****          lw_zteinv_details-einv_error = lv_ew1.
*****          lw_zteinv_details-ernam = sy-uname.
*****          lw_zteinv_details-erdat = sy-datum.
*****          APPEND lw_zteinv_details TO ex_einv_details.
*****          CONTINUE.
*****
*****        ENDIF.
*****
*****        IF lv_ew6 CA 'alert'.
*****          REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lv_ew6 WITH ''.
*****          SPLIT lv_ew6 AT ':' INTO lv_ew5 lv_ew7.
*****          SPLIT lv_ew7 AT '.' INTO alert lv_ew8.
*****          IF alert = 'IRN already generated on this document number'.
*****
******          MESSAGE 'IRN is already generated' TYPE 'E' DISPLAY LIKE 'E'.
******            lw_inref-irn_status = 'Duplicate IRN'.
******            lw_inref-ernam = sy-uname.
******            lw_inref-erdat = sy-datum.
******            lw_inref-erzet = sy-uzeit.
******
******            APPEND lw_inref TO ex_invref.
******            CLEAR:lw_inref.
*******          CONTINUE.
*****            EXIT.
*****
*****          ENDIF.
*****        ENDIF.
*****
*****        CLEAR: lv_ew5, lv_ew6, lv_ew7,lv_ew8.
*******          EXTRACT irn.
*****        SPLIT  lv_ew2 AT ':' INTO  : lv_ew5 lv_ew6.
*****
*****        REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lv_ew6 WITH ''.
*****        lw_inref-irn = lv_ew6.
*****        lv_irn = lv_ew6.
*****        CLEAR: lv_ew5,  lv_ew6.
*******           EXTRACT ACKNO
*****        SPLIT  lv_ew AT ':' INTO : lv_ew5 lv_ew6 lv_ew8.
*****        CLEAR: lv_ew5, lv_ew6.
*****        SPLIT lv_ew8 AT ':' INTO lv_ew5 lv_ew6.
*****        REPLACE ALL OCCURENCES OF SUBSTRING '"' IN lv_ew6 WITH ''.
*****
*****
*****        lw_inref-docno = lv_doc.
******        REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lv_token WITH ''.
******      ex_invref-accs_token = lv_token.
*****        lw_inref-ack_no  = lv_ew6. " Ack no
*****
*****        CLEAR: lv_ew5, lv_ew6, lv_ew8.
*****        "EXTRACT ACTDT
*****        REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lv_ew1 WITH ''.
*****        SPLIT lv_ew1 AT ' ' INTO lv_ew5 lv_ew6.
*****        CLEAR lv_ew6.
*****        SPLIT lv_ew5 AT ':' INTO lv_ew6 lv_ew8.
******           REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lv_ew8 WITH ''.
*****        CONCATENATE lv_ew8+8(2) '.' lv_ew8+5(2) '.' lv_ew8(4) INTO lv_ew8.
*****
*****
*****
*****        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
*****          EXPORTING
*****            date_external            = lv_ew8
******           ACCEPT_INITIAL_DATE      =
*****          IMPORTING
*****            date_internal            = lv_date
*****          EXCEPTIONS
*****            date_external_is_invalid = 1
*****            OTHERS                   = 2.
*****        IF sy-subrc <> 0.
*****          MESSAGE 'Data Convertion is incomplete' TYPE 'E'.
*****        ENDIF.
*****
*****        lw_inref-ack_date = lv_date.
******
*****        CLEAR: lv_ew5,lv_ew6,lv_ew8.
************  extract signed invoice
*****        REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lv_ew3 WITH ''.
*****        SPLIT lv_ew3 AT ':' INTO :  lv_ew5 lv_ew8.
*****        lw_inref-signed_inv = lv_ew8.
*****        CLEAR: lv_ew5, lv_ew8.
*****
***********  extract QR code
*****        REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lv_ew4 WITH ''.
*****        SPLIT lv_ew4 AT ':' INTO : lv_ew5 lv_ew6.
*****        lw_inref-signed_qrcode = lv_ew6.
*****        CLEAR: lv_ew5, lv_ew6.
*****
************ extract status
*****        lw_inref-irn_status = 'ACT'.
*****
*****        lw_inref-ernam = sy-uname.
*****        lw_inref-erdat = sy-datum.
*****        lw_inref-erzet = sy-uzeit.
*****
*****        lw_inref-bukrs  = lw_api_hdr-bukrs.
*****        lw_inref-doc_year = lw_api_hdr-doc_year.
*****        lw_inref-doc_type = lw_api_hdr-doc_type.
*****        lw_inref-docno = lw_api_hdr-vbeln.
*****        lw_inref-odn = lv_doc.
*****
*****
*****
*****        lw_message-type = 'S'.
*****        lw_message-message_v1 = 'IRN successfully generated for document'.
*****        lw_message-message_v2 = lv_doc.
*****        APPEND lw_message TO ex_messages.
*****
*****        APPEND lw_inref TO ex_invref.
*****        CLEAR:lw_inref.
*****
****** Custom table update
*****        lw_zteinv_details-bukrs  = lw_api_hdr-bukrs.
*****        lw_zteinv_details-gjahr = lw_api_hdr-doc_year.
*****        lw_zteinv_details-doctyp = lw_api_hdr-doc_type.
*****        lw_zteinv_details-docno = lw_api_hdr-vbeln.
*****        lw_zteinv_details-ernam = sy-uname.
*****        lw_zteinv_details-erdat = sy-datum.
*****
*****
*****        APPEND lw_zteinv_details TO ex_einv_details.
*****        CLEAR lw_zteinv_details.


      ELSE.
        ex_return =  'E'.




      ENDIF.
    ENDIF.
*    ENDIF.
  ENDLOOP.




ENDFUNCTION.
