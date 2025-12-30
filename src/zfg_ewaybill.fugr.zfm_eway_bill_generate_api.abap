FUNCTION zfm_eway_bill_generate_api.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_TOKEN) TYPE  STRING
*"     REFERENCE(IM_API_HEADER) TYPE  ZTT_EWAY_API_STRUCT
*"     REFERENCE(IM_API_ITEM) TYPE  ZTT_EWAY_API_STRUCT_ITM
*"     VALUE(IM_API_DWN) TYPE  XFELD OPTIONAL
*"  EXPORTING
*"     REFERENCE(EX_RETURN) TYPE  STRING
*"     REFERENCE(EX_MESSAGES) TYPE  BAPIRET2_T
*"     REFERENCE(EX_EWAYBILL) TYPE  ZTT_J_1IG_EWAYBILL
*"     REFERENCE(EX_EWAY_TRANSPORT) TYPE  ZTT_EWAY_TRANSPORT
*"----------------------------------------------------------------------
  TYPES: BEGIN OF ty_message,
           message TYPE bapi_msg,
         END OF ty_message.

  DATA:lw_eway_trasnport TYPE zteway_transport.
  DATA:lt_api_data TYPE ztt_eway_api_struct,
       lw_api_data TYPE zst_eway_api_struct.
  DATA:lt_api_data_itm TYPE ztt_eway_api_struct_itm,
       lw_api_data_itm TYPE zst_eway_api_struct_itm.
  DATA:it_temp TYPE ztt_eway_api_struct_itm,
       wa_temp TYPE zst_eway_api_struct_itm.
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
        lv_node_length     TYPE i,
        lv_node_index      TYPE i,
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
        gv_downld       TYPE string.
  DATA:wa_message TYPE bapiret2.
  DATA:it_texttab TYPE TABLE OF string,
       wa_texttab TYPE string.
  DATA:taxable_amount TYPE  string,
       cgst_amount    TYPE string,
       sgst_amount    TYPE string,
       igst_amount    TYPE string,
       cess_amount    TYPE string.
  DATA:taxable_amount1 TYPE string,
       cgst_rate       TYPE string,
       sgst_rate       TYPE string,
       igst_rate       TYPE string,
       cess_rate       TYPE string.
  DATA : lv_token   TYPE string,
         lt_message TYPE TABLE OF ty_message,
         lw_message TYPE ty_message.
  TYPES:BEGIN OF ty_ebill,
          ewaybillno   TYPE string,
          ewaybilldate TYPE string,
          alert        TYPE string,
          error        TYPE string,
          url          TYPE string,
        END OF ty_ebill.
  DATA: ls_ebill TYPE ty_ebill.
  CLEAR ex_messages.

  DATA lr_rest_exception TYPE REF TO cx_rest_client_exception.

  DATA:ls_config    TYPE zteinv_api,
       ls_config1   TYPE zteinv_api,
       lv_url       TYPE string,
       lv_url1      TYPE string,
       lv_print_url TYPE string,
       http_status  TYPE string.

  DATA:lv_return1         TYPE string,
       lt_messages1       TYPE bapiret2_t,
       lt_ewaybill1       TYPE ztt_j_1ig_ewaybill,
       lt_eway_trasnport1 TYPE ztt_eway_transport.

  DATA:ls_ewaybill TYPE j_1ig_ewaybill.

  DATA : lc_doc TYPE string.
  DATA : lv_doc TYPE string.
  DATA :  lv_tok  TYPE  string.

  DATA:lt_irn TYPE TABLE OF j_1ig_invrefnum,
       lw_irn TYPE j_1ig_invrefnum.


  SHIFT lc_doc LEFT DELETING LEADING '0'.
  lv_token = im_token.
  lt_api_data = im_api_header.
  lt_api_data_itm = im_api_item.

  IF lt_api_data IS NOT INITIAL.
    SELECT * FROM j_1ig_invrefnum INTO TABLE lt_irn FOR ALL ENTRIES IN lt_api_data
    WHERE bukrs = lt_api_data-bukrs
    AND docno = lt_api_data-vbeln.

    IF sy-subrc IS INITIAL.
      DELETE lt_irn WHERE irn_status NE 'ACT'.
    ENDIF.
  ENDIF.

  CLEAR lv_url1.
  SELECT SINGLE * FROM zteinv_api INTO ls_config1 WHERE apiid = 'EWAY_IRN'.
  IF sy-subrc IS INITIAL.
    lv_url1 = ls_config1-apiuri.
  ENDIF.


  CLEAR lv_url.
  SELECT SINGLE * FROM zteinv_api INTO ls_config WHERE apiid = 'EWAY_GEN'.
  IF sy-subrc IS INITIAL.
    lv_url = ls_config-apiuri.
  ENDIF.

  IF lv_url IS NOT INITIAL OR lv_url1 IS NOT INITIAL.
    lv_token = im_token.
    LOOP AT lt_api_data INTO lw_api_data.
      CLEAR:lv_doc.
      lc_doc  =  lw_api_data-vbeln."lw_api_data-document_number.
      REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lc_doc WITH ''.
      CONDENSE lc_doc NO-GAPS.
      lv_doc  =  lc_doc.

*
*      READ TABLE lt_irn INTO lw_irn WITH KEY bukrs = lw_api_data-bukrs
*                                             docno = lw_api_data-vbeln.
*
*      IF sy-subrc IS INITIAL.
*
*
*        CLEAR:lv_return1,lt_messages1,lt_ewaybill1,lt_eway_trasnport1.
*        CALL FUNCTION 'ZFM_EWAY_BILL_GENERATE_IRN_API'
*          EXPORTING
*            im_token          = im_token
*            im_api_header     = lw_api_data
*            im_url            = lv_url1
*            im_irn            = lw_irn-irn
*            im_doc            = lv_doc
*            im_api_dwn        = im_api_dwn
*          IMPORTING
*            ex_return         = lv_return1
*            ex_messages       = lt_messages1
*            ex_ewaybill       = lt_ewaybill1
*            ex_eway_transport = lt_eway_trasnport1.
*        ex_return = lv_return1.
*        APPEND LINES OF lt_messages1 TO ex_messages.
*        APPEND LINES OF lt_ewaybill1 TO ex_ewaybill.
*        APPEND LINES OF  lt_eway_trasnport1 TO ex_eway_transport.
*        CONTINUE.
*      ENDIF.


      IF lw_api_data-pincode_of_consignor IS INITIAL.
        lw_api_data-pincode_of_consignor = '0'.
      ENDIF.

      IF lw_api_data-pincode_of_consignee IS INITIAL.
        lw_api_data-pincode_of_consignee = '0'.
      ENDIF.

*      IF sy-sysid = 'DS4'.
*        lw_api_data-user_gstin = '"05AAABB0639G1Z8"'.
*        lw_api_data-gstin_of_consignor = '"05AAABB0639G1Z8"'.
*        lw_api_data-gstin_of_consignee = '"05AAABC0181E1ZE"'.
*      endif.
      CONCATENATE : '{' '"access_token":' lv_token ','
      '"userGstin": ' lw_api_data-user_gstin ','
      '"supply_type":' lw_api_data-supply_type  ','
      INTO  lv_post SEPARATED BY ' ' .
      CONCATENATE : lv_post  '"sub_supply_type":' lw_api_data-sub_supply_type ','
      '"sub_supply_description":' lw_api_data-sub_supply_description ','
      '"document_type":' lw_api_data-document_type ','
      '"document_number":' lw_api_data-document_number','
      INTO  lv_post SEPARATED BY ' '.
      CONCATENATE : lv_post ' "document_date":' lw_api_data-document_date','
      '"gstin_of_consignor":' lw_api_data-gstin_of_consignor ','
      '"legal_name_of_consignor":' lw_api_data-legal_name_of_consignor ','
      INTO lv_post SEPARATED BY ' '.
      CONCATENATE : lv_post '"address1_of_consignor":' lw_api_data-address1_of_consignor  ','
      '"address2_of_consignor":' lw_api_data-address2_of_consignor  ','
      INTO lv_post SEPARATED BY ' '.
      CONCATENATE : lv_post '"place_of_consignor":' lw_api_data-place_of_consignor ','
      '"pincode_of_consignor":' lw_api_data-pincode_of_consignor ','
      INTO lv_post SEPARATED BY ' '.
      CONCATENATE : lv_post '"state_of_consignor":' lw_api_data-state_of_consignor ','
      '"actual_from_state_name":' lw_api_data-actual_from_state_name ','
      INTO lv_post SEPARATED BY ' '.
      CONCATENATE : lv_post  '"gstin_of_consignee":' lw_api_data-gstin_of_consignee  ','
      '"legal_name_of_consignee":' lw_api_data-legal_name_of_consignee ','
      '"address1_of_consignee": ' lw_api_data-address1_of_consignee ','
      '"address2_of_consignee": ' lw_api_data-address2_of_consignee ','
      '"place_of_consignee": ' lw_api_data-place_of_consignee ','
      '"pincode_of_consignee":' lw_api_data-pincode_of_consignee ','
      '"state_of_supply":' lw_api_data-state_of_supply  ','
      '"actual_to_state_name":' lw_api_data-actual_to_state_name ','
      INTO lv_post SEPARATED BY ' '.
      CONCATENATE : lv_post '"transaction_type":' lw_api_data-transaction_type ','
      '"other_value":' lw_api_data-other_value ','
      '"total_invoice_value":' lw_api_data-total_invoice_value ','
      '"taxable_amount":' lw_api_data-taxable_amount ','
      '"cgst_amount":' lw_api_data-cgst_amount ','
      '"sgst_amount":' lw_api_data-sgst_amount ','
      INTO lv_post SEPARATED BY ' ' .
      CONCATENATE : lv_post '"igst_amount":' lw_api_data-igst_amount ','
      '"cess_amount":' lw_api_data-cess_amount ','
      '"cess_nonadvol_value":' lw_api_data-cess_nonadvol_value ','
      INTO lv_post SEPARATED BY ' '.

      CONCATENATE : lv_post '"transporter_id":' lw_api_data-transporter_id  ','
      '"transporter_name":' lw_api_data-transporter_name ','
      '"transporter_document_number":' lw_api_data-transporter_document_number ','
      '"transporter_document_date":' lw_api_data-transporter_document_date ','
      '"transportation_mode":' lw_api_data-transportation_mode ','
      '"transportation_distance":' lw_api_data-transportation_distance ','
      INTO lv_post SEPARATED BY ' '.
      CONCATENATE : lv_post '"vehicle_number":' lw_api_data-vehicle_number ','
      '"vehicle_type":' lw_api_data-vehicle_type ','

      '"generate_status":' lw_api_data-generate_status ','
      '"data_source":' lw_api_data-data_source ','
      INTO lv_post SEPARATED BY ' '.
      CONCATENATE : lv_post '"user_ref":' lw_api_data-user_ref','
      '"location_code":' lw_api_data-location_code ','
      '"eway_bill_status":' lw_api_data-eway_bill_status ','
      '"auto_print":' lw_api_data-auto_print ','
      '"email":' lw_api_data-email ','
      INTO lv_post SEPARATED BY ' '.
      REFRESH it_temp.
      DATA:lv_count TYPE i.
      it_temp  = lt_api_data_itm.
      DELETE it_temp  WHERE document_number NE lw_api_data-document_number.
      CLEAR:lv_count.
      DESCRIBE TABLE it_temp LINES lv_count.
      CONCATENATE : lv_post '"itemList":' '[' INTO lv_post SEPARATED BY ' '.
      LOOP AT it_temp  INTO wa_temp  WHERE document_number EQ lw_api_data-document_number.


        IF wa_temp-unit_of_product IS INITIAL.
          wa_temp-unit_of_product = '" "'.
        ENDIF.

        IF sy-tabix NE 1.
          CONCATENATE lv_post ',' INTO lv_post.
        ENDIF.
        CONCATENATE : lv_post "'"itemList"' ':' '['
        '{'
        '"product_name":' wa_temp-product_name ','
        '"product_description":' wa_temp-product_description ','
        '"hsn_code": ' wa_temp-hsn_code ','
        '"quantity":' wa_temp-quantity ','
        '"unit_of_product":' wa_temp-unit_of_product ','
        '"cgst_rate":' wa_temp-cgst_rate ','
        '"sgst_rate":' wa_temp-sgst_rate ','
        '"igst_rate":' wa_temp-igst_rate','
        '"cess_rate":' wa_temp-cess_rate ','
        '"cessNonAdvol":'  wa_temp-cessnonadvol ','
        '"taxable_amount":' wa_temp-taxable_amount  '}'
        INTO lv_post SEPARATED BY ' '.
      ENDLOOP.
      CONCATENATE  lv_post ']'  INTO lv_post SEPARATED BY ' '.

      CONCATENATE : lv_post   '}' INTO lv_post SEPARATED BY ' '.

      IF im_api_dwn = 'X'.
        CALL FUNCTION 'ZFM_EDOC_DOWNLOAD'
          EXPORTING
            im_data = lv_post.
        EXIT.
      ENDIF.

      cl_http_client=>create_by_url(
      EXPORTING
        url = lv_url "'https://clientbasic.mastersindia.co/ewayBillsGenerate'
      IMPORTING
        client = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active = 2
        internal_error = 3
        OTHERS = 4 ).

      IF sy-subrc = 0.
        CREATE OBJECT lo_rest_client TYPE cl_rest_http_client
          EXPORTING
            io_http_client = lo_http_client.

        lo_rest_entity = lo_rest_client->create_request_entity( ).

        lo_rest_entity->set_content_type( if_rest_media_type=>gc_appl_json ).

        CALL METHOD lo_rest_entity->set_string_data
          EXPORTING
            iv_data = lv_post.
        TRY.

            lo_rest_client->post( lo_rest_entity ).
          CATCH cx_rest_client_exception INTO lr_rest_exception.


            ex_return = 'E'.

*            LOOP AT lt_message INTO lw_message.
            wa_message-type = 'E'.
            wa_message-message_v3 = lv_doc.
            wa_message-message    = 'Too much load on NIC server'.
            wa_message-message_v1 = 'Too much load on NIC server'.
            APPEND wa_message TO ex_messages.
            CLEAR wa_message.
*            ENDLOOP.

            ls_ewaybill-status = 'E'.
            ls_ewaybill-bukrs  = lw_api_data-bukrs.
            ls_ewaybill-docno = lv_doc.
            ls_ewaybill-ernam = sy-uname.
            ls_ewaybill-erdat = sy-datum.
            ls_ewaybill-gjahr = lw_api_data-doc_year.


            APPEND ls_ewaybill TO ex_ewaybill.
            CLEAR:ls_ewaybill.


            lw_eway_trasnport-bukrs = lw_api_data-bukrs.
            lw_eway_trasnport-docno = lw_api_data-vbeln.
            lw_eway_trasnport-gjahr  = lw_api_data-doc_year.
            lw_eway_trasnport-eway_error = 'Posting Error'.
            APPEND lw_eway_trasnport TO ex_eway_transport.
            CLEAR lw_eway_trasnport.


            CONTINUE.

        ENDTRY.

        lo_rest_entity = lo_rest_client->get_response_entity( ).


        lv_string = lo_rest_entity->get_string_data( ).
        http_status   = lo_rest_entity->get_header_field( '~status_code' ).

        IF  http_status EQ '200'.
          ex_return = lv_string ."'S'.

          DATA : lv_ew  TYPE string,
                 lv_ew1 TYPE string.
          DATA : lv_ew2   TYPE string,
                 lv_ew3   TYPE string,
                 lv_ew4   TYPE string,
                 lv_ew5   TYPE string,
                 lv_ew6   TYPE string,
                 lv_ew7   TYPE string,
                 lv_ew8   TYPE string,
                 lv_ew9   TYPE string,
                 lv_ew10  TYPE string,
                 alert    TYPE string,
                 lv_data  TYPE string,
                 lv_time1 TYPE string,
                 lv_time2 TYPE string.

          DATA:lv_date     TYPE dats,
               lv_date_gen TYPE dats,
               lv_time_gen TYPE sy-uzeit.

          CLEAR:lv_time_gen,lv_time_gen.
          SPLIT  lv_string AT ',' INTO :  lv_ew lv_ew1 lv_ew2 lv_ew3 lv_ew4 lv_ew5 lv_ew6 lv_ew7 lv_ew8 lv_ew9 lv_ew10.
          lv_print_url = lv_ew6.

          CLEAR: lv_ew5,lv_ew8.
          SPLIT  lv_ew AT ':' INTO : lv_ew lv_ew6 lv_ew8.
          REPLACE ALL OCCURRENCES OF '"' IN lv_ew8 WITH ' '.
          CALL FUNCTION 'SOTR_SERV_STRING_TO_TABLE'
            EXPORTING
              text        = lv_ew8
*             FLAG_NO_LINE_BREAKS       = 'X'
              line_length = '60'
              langu       = sy-langu
            TABLES
              text_tab    = it_texttab.

          DATA : lvtemp1 TYPE string,
                 lvtemp3 TYPE string,
                 lvtemp2 TYPE string.

          SPLIT lv_string AT 'error' INTO lvtemp1 lvtemp2.
          SPLIT lvtemp2 AT ',' INTO lvtemp1 lvtemp3.
          REPLACE ALL OCCURRENCES OF ',' IN lvtemp1 WITH ''.
          REPLACE ALL OCCURRENCES OF ':' IN lvtemp1 WITH ''.
          REPLACE ALL OCCURRENCES OF '"' IN lvtemp1 WITH ''.
          CONDENSE lvtemp1 NO-GAPS.

          IF lv_ew1 CS 'no content' OR lvtemp1 = 'true'.
*            SPLIT lv_ew8 AT '||' INTO lv_ew8 lv_ew5.
            SPLIT lv_ew8 AT '||' INTO TABLE lt_message.
            ex_return = 'E'.
            IF lvtemp1 EQ 'true'.
              SPLIT lv_string AT 'errorMessage' INTO lvtemp1 lvtemp2.
              SPLIT lvtemp2 AT ',' INTO lvtemp1 lvtemp3.
              REPLACE ALL OCCURRENCES OF ',' IN lvtemp1 WITH ''.
              REPLACE ALL OCCURRENCES OF ':' IN lvtemp1 WITH ''.
              REPLACE ALL OCCURRENCES OF '"' IN lvtemp1 WITH ''.
              CONDENSE lvtemp1.
              wa_message-type = 'E'.
              wa_message-message_v3 = lv_doc.
              wa_message-message    = lvtemp1.
              wa_message-message_v1 = lvtemp1.
              APPEND wa_message TO ex_messages.
            ENDIF.


            LOOP AT lt_message INTO lw_message.
              wa_message-type = 'E'.
              wa_message-message_v3 = lv_doc.
              wa_message-message    = lw_message-message.
              wa_message-message_v1 = lw_message-message.
              APPEND wa_message TO ex_messages.
              CLEAR wa_message.
            ENDLOOP.

            ls_ewaybill-status = 'E'.
            ls_ewaybill-bukrs  = lw_api_data-bukrs.
            ls_ewaybill-docno = lv_doc.
            ls_ewaybill-ernam = sy-uname.
            ls_ewaybill-erdat = sy-datum.
            ls_ewaybill-gjahr = lw_api_data-doc_year.


            APPEND ls_ewaybill TO ex_ewaybill.
            CLEAR:ls_ewaybill.


            lw_eway_trasnport-bukrs = lw_api_data-bukrs.
            lw_eway_trasnport-docno = lw_api_data-vbeln.
            lw_eway_trasnport-gjahr  = lw_api_data-doc_year.
            lw_eway_trasnport-eway_error = lv_ew8.
            APPEND lw_eway_trasnport TO ex_eway_transport.
            CLEAR lw_eway_trasnport.


            CONTINUE.
          ELSEIF lv_ew2 CS 'No content'.
*            SPLIT lv_ew8 AT '||' INTO lv_ew8 lv_ew5.
            SPLIT lv_ew8 AT '||' INTO TABLE lt_message.
            ex_return = 'E'.
            wa_message-type = 'E'.
            LOOP AT lt_message INTO lw_message.
              wa_message-type = 'E'.
              wa_message-message_v3 = lv_doc.
              wa_message-message    = lw_message-message.
              wa_message-message_v1 = lw_message-message.
              APPEND wa_message TO ex_messages.
              CLEAR wa_message.
            ENDLOOP.


            ls_ewaybill-status = 'E'.
            ls_ewaybill-bukrs  = lw_api_data-bukrs.
            ls_ewaybill-docno = lv_doc.
            ls_ewaybill-ernam = sy-uname.
            ls_ewaybill-erdat = sy-datum.
            ls_ewaybill-gjahr = lw_api_data-doc_year.


            APPEND ls_ewaybill TO ex_ewaybill.
            CLEAR:ls_ewaybill.

            lw_eway_trasnport-bukrs = lw_api_data-bukrs.
            lw_eway_trasnport-docno = lw_api_data-vbeln.
            lw_eway_trasnport-gjahr  = lw_api_data-doc_year.
            lw_eway_trasnport-eway_error = lv_ew8.
            APPEND lw_eway_trasnport TO ex_eway_transport.
            CLEAR lw_eway_trasnport.



            CONTINUE.
          ELSE.
            IF lv_ew3 CS 'alert' ."and sy-subrc = 0.
              REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lv_ew3 WITH ''.
              SPLIT lv_ew3 AT ':' INTO lv_ew5 lv_ew7.
              SPLIT lv_ew7 AT '.' INTO alert lv_ew10.
              IF alert = 'E-way bill number already generated on this document number'.
                ex_return = alert.

*              CONTINUE.
              ENDIF.
            ENDIF.
            CLEAR: lv_ew5, lv_ew6, lv_ew7,lv_ew10.
            SPLIT lv_string AT '{' INTO lv_ew5 lv_ew9 lv_ew10 lv_data.
            SPLIT lv_data AT '}' INTO lv_data lv_ew5.
            CONCATENATE '{' lv_data '}' INTO lv_data.
            CALL METHOD cl_fdt_json=>json_to_data
              EXPORTING
                iv_json = lv_data
              CHANGING
                ca_data = ls_ewaybill.

            CALL METHOD cl_fdt_json=>json_to_data
              EXPORTING
                iv_json = lv_data
              CHANGING
                ca_data = ls_ebill.
            lv_print_url = ls_ebill-url.
            SPLIT  lv_ew2 AT ':' INTO  : lv_ew5 lv_ew6.

            REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lv_ew6 WITH ''.

            SPLIT  lv_ew6 AT ' ' INTO  : lv_ew5 lv_ew6.

            CLEAR: lv_ew5,  lv_ew6.

            SPLIT  lv_ew AT ':' INTO : lv_ew5 lv_ew6 .
            CLEAR: lv_ew5, lv_ew6.
            SPLIT lv_ew8 AT ':' INTO lv_ew5 lv_ew6.
            REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lv_ew6 WITH ''.


            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = lv_doc
              IMPORTING
                output = lv_doc.
            ls_ewaybill-docno = lv_doc.

            ls_ewaybill-ebillno  = lv_ew6.

            CLEAR: lv_ew5, lv_ew6, lv_ew8.

            REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lv_ew1 WITH ''.
            SPLIT lv_ew1 AT ' ' INTO lv_ew5 lv_ew6.
            CLEAR:lv_time1,lv_time2.
            SPLIT lv_ew6 AT ' ' INTO lv_time1 lv_time2.
            REPLACE ALL OCCURRENCES OF ':' IN lv_time1 WITH ' '.
            CALL FUNCTION 'CONVERT_TIME_INPUT'
              EXPORTING
                input                     = lv_time1
                plausibility_check        = 'X'
              IMPORTING
                output                    = lv_time_gen
              EXCEPTIONS
                plausibility_check_failed = 1
                wrong_format_in_input     = 2
                OTHERS                    = 3.
            IF sy-subrc = 0.
              PERFORM convert_time USING lv_time2 CHANGING lv_time_gen.
            ENDIF.


            SPLIT lv_ew5 AT ':' INTO lv_ew6 lv_ew8.
            REPLACE ALL OCCURRENCES OF SUBSTRING '\/' IN lv_ew8 WITH ''.

**            CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'      " -- By Deep
**              EXPORTING
**                date_external            = lv_ew8
***               ACCEPT_INITIAL_DATE      =
**              IMPORTING
**                date_internal            = lv_date_gen
**              EXCEPTIONS
**                date_external_is_invalid = 1
**                OTHERS                   = 2.
**            IF sy-subrc <> 0.
*** Implement suitable error handling here
**            ENDIF.

*****            ++ By Deep FOR e-way Date
            TRY .
                CLEAR : lv_ew5 , lv_ew6.
                SPLIT lv_string AT 'ewayBillDate' INTO lv_ew5 lv_ew6.
                CLEAR lv_ew5.
                SPLIT lv_ew6 AT ',' INTO lv_ew5 lv_ew4.
                CLEAR lv_ew6.
                lv_ew6 = lv_ew5.
                REPLACE ALL OCCURRENCES OF '"' IN lv_ew6 WITH ''.
                REPLACE ALL OCCURRENCES OF ':' IN lv_ew6 WITH ''.
                REPLACE ALL OCCURRENCES OF '/' IN lv_ew6 WITH ''.
                REPLACE ALL OCCURRENCES OF SUBSTRING '\/' IN lv_ew6 WITH ''.
                IF lv_ew6 IS NOT INITIAL.
                  CONDENSE lv_ew6.
                  CLEAR lv_date_gen.
                  CONCATENATE lv_ew6+4(4) lv_ew6+2(2) lv_ew6+0(2) INTO lv_date_gen.
                ENDIF.
              CATCH cx_sy_range_out_of_bounds.

            ENDTRY.

*****

*
            CLEAR: lv_ew5,lv_ew6,lv_ew8.

            REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lv_ew3 WITH ''.
            SPLIT lv_ew3 AT ':' INTO :  lv_ew5 lv_ew8.

            CLEAR: lv_ew5, lv_ew8.

            REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lv_ew4 WITH ''.
            SPLIT lv_ew4 AT ':' INTO : lv_ew5 lv_ew6.
            CLEAR: lv_ew5, lv_ew6.
            SPLIT lv_ew2 AT ':' INTO lv_ew5 lv_ew6.
            CLEAR:lv_ew2,lv_ew5.
            SPLIT lv_ew6 AT ' ' INTO : lv_ew2 lv_ew5.
            REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lv_ew2 WITH ' '.
            REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lv_ew5 WITH ' '.
            REPLACE ALL OCCURRENCES OF SUBSTRING '\/' IN lv_ew2 WITH ' '.
            CLEAR lv_ew6.
            SPLIT lv_ew5 AT ' ' INTO lv_ew6 lv_ew5.
            CLEAR lv_date.




*            CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'   "-- BY deep On 19.04.2024
*              EXPORTING
*                date_external            = lv_ew2
**               ACCEPT_INITIAL_DATE      =
*              IMPORTING
*                date_internal            = lv_date
*              EXCEPTIONS
*                date_external_is_invalid = 1
*                OTHERS                   = 2.
*            IF sy-subrc <> 0.
** Implement suitable error handling here
*            ENDIF.
            TRY .
                CLEAR : lv_ew5 , lv_ew6.
                SPLIT lv_string AT 'validUpto' INTO lv_ew5 lv_ew6.
                CLEAR lv_ew5.
                SPLIT lv_ew6 AT ',' INTO lv_ew5 lv_ew4.
                CLEAR lv_ew6.
                lv_ew6 = lv_ew5.
                REPLACE ALL OCCURRENCES OF '"' IN lv_ew6 WITH ''.
                REPLACE ALL OCCURRENCES OF ':' IN lv_ew6 WITH ''.
                REPLACE ALL OCCURRENCES OF '/' IN lv_ew6 WITH ''.
                REPLACE ALL OCCURRENCES OF SUBSTRING '\/' IN lv_ew6 WITH ''.
                IF lv_ew6 IS NOT INITIAL.
                  CONDENSE lv_ew6.
                  CLEAR lv_date.
                  CONCATENATE lv_ew6+4(4) lv_ew6+2(2) lv_ew6+0(2) INTO lv_date.
                ENDIF.
              CATCH cx_sy_range_out_of_bounds.

            ENDTRY.


            DATA:lv_time TYPE sy-uzeit.
            REPLACE ALL OCCURRENCES OF ':' IN lv_ew6 WITH ' '.
            CALL FUNCTION 'CONVERT_TIME_INPUT'
              EXPORTING
                input                     = lv_ew6
                plausibility_check        = 'X'
              IMPORTING
                output                    = lv_time
              EXCEPTIONS
                plausibility_check_failed = 1
                wrong_format_in_input     = 2
                OTHERS                    = 3.
            IF sy-subrc = 0.
              PERFORM convert_time USING lv_ew5 CHANGING lv_time.

            ENDIF.

            ls_ewaybill-status = 'A'.
            ls_ewaybill-ernam = sy-uname.
            ls_ewaybill-erdat = sy-datum.
            ls_ewaybill-egen_dat = lv_date_gen.
            ls_ewaybill-egen_time = lv_time_gen.
            ls_ewaybill-vdfmdate = lv_date_gen.
*            ls_ewaybill-vdfmtime = lv_time_gen.
            ls_ewaybill-vdtodate = lv_date.
            ls_ewaybill-vdtotime = lv_time.
            ls_ewaybill-bukrs  = lw_api_data-bukrs.
            ls_ewaybill-gjahr = lw_api_data-doc_year.
            ls_ewaybill-docno = lv_doc.

            APPEND ls_ewaybill TO ex_ewaybill.

            CLEAR:ls_ewaybill.


            lw_eway_trasnport-bukrs = lw_api_data-bukrs.
            lw_eway_trasnport-docno = lw_api_data-vbeln.
            lw_eway_trasnport-gjahr  = lw_api_data-doc_year.
            lw_eway_trasnport-eway_print = ls_ebill-url.
            APPEND lw_eway_trasnport TO ex_eway_transport.
            CLEAR lw_eway_trasnport.


            wa_message-type = 'S'.
            wa_message-message_v1 = 'E-Way Bill is successfully generated'.
            wa_message-message_v2 = 'for document'.
            wa_message-message_v3 = lv_doc.
* Find print URL
            " SPLIT lv_print_url AT ':' INTO lv_ew1 lv_ew2.
            "lv_print_url = lv_ew2.
            SPLIT lv_print_url AT '/' INTO lv_ew1 lv_ew2 lv_ew3 lv_ew4 lv_ew5 lv_ew6.
            REPLACE ALL OCCURRENCES OF '"}' IN lv_ew4 WITH space.
            lv_print_url = lv_ew4.
            CONCATENATE '/' lv_print_url INTO lv_print_url.
* Find print URL
            wa_message-message_v4 = lv_print_url.

            APPEND wa_message TO ex_messages.
            CLEAR wa_message.
            ex_return = 'S'.

          ENDIF.
        ELSE.
          ex_return =  'E'.

        ENDIF.


      ENDIF.

      CLEAR:lv_post,lv_string.

*      ENDIF.
    ENDLOOP.
  ENDIF.
*endif.




ENDFUNCTION.
