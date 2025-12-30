FUNCTION ZFM_EWAY_BILL_GENERATE_IRN_API.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_TOKEN) TYPE  STRING
*"     REFERENCE(IM_API_HEADER) TYPE  ZST_EWAY_API_STRUCT
*"     REFERENCE(IM_URL) TYPE  STRING
*"     REFERENCE(IM_IRN) TYPE  J_1IG_IRN
*"     REFERENCE(IM_DOC) TYPE  STRING
*"     VALUE(IM_API_DWN) TYPE  XFELD OPTIONAL
*"  EXPORTING
*"     REFERENCE(EX_RETURN) TYPE  STRING
*"     REFERENCE(EX_MESSAGES) TYPE  BAPIRET2_T
*"     REFERENCE(EX_EWAYBILL) TYPE  ZTT_J_1IG_EWAYBILL
*"     REFERENCE(EX_EWAY_TRANSPORT) TYPE  ZTT_EWAY_TRANSPORT
*"----------------------------------------------------------------------

  TYPES:BEGIN OF ty_message,
    ewbno        TYPE string,
    ewbdt        TYPE string,
    ewbvalidtill TYPE string,
    remarks      TYPE string,
    qrcodeurl    TYPE string,
    einvoicepdf  TYPE string,
    ewaybillpdf  TYPE string,
  END OF ty_message,
  BEGIN OF ty_output,
    MESSAGE      TYPE ty_message,
    errormessage TYPE string,
    infodtls     TYPE string,
    status       TYPE string,
    CODE         TYPE string,
  END OF ty_output,
  BEGIN OF ty_message1,
    MESSAGE TYPE bapi_msg,
  END OF ty_message1.
  DATA:lw_output         TYPE ty_output,
        lw_api_data       TYPE zst_eway_api_struct,
        lv_token          TYPE string,
        lv_url            TYPE string,
        lv_irn            TYPE string,
        lv_post           TYPE string,
        lwa_message       TYPE bapiret2,
        lw_ewaybill       TYPE j_1ig_ewaybill,
        lw_eway_trasnport TYPE zteway_transport,
        lv_doc            TYPE string,
        lv_string         TYPE string,
        http_status       TYPE string,
        lv_ew1            TYPE string,
        lv_ew2            TYPE string,
        lv_ew3            TYPE string,
        lv_ew4            TYPE string,
        lv_length1        TYPE string,
        lt_message        TYPE TABLE OF ty_message1,
        lw_message        TYPE ty_message1,
        lv_print_url      TYPE string.

  DATA: lo_http_client    TYPE REF TO if_http_client,
        lo_rest_client    TYPE REF TO if_rest_client,
        lo_rest_entity    TYPE REF TO if_rest_entity,
        lr_rest_exception TYPE REF TO cx_rest_client_exception.


  IF im_token IS NOT INITIAL.
    lv_token = im_token.
  ENDIF.
  IF im_api_header IS NOT INITIAL.
    lw_api_data = im_api_header.
  ENDIF.

  IF im_url IS NOT INITIAL.
    lv_url = im_url.
  ENDIF.

  IF im_doc IS NOT INITIAL.
    lv_doc = im_doc.
  ENDIF .

  IF im_irn IS NOT INITIAL.
    CONCATENATE '"' im_irn '"' INTO lv_irn.
  ENDIF.

  CLEAR  lv_post.
  CONCATENATE : '{' '"access_token":' lv_token ','
  '"user_gstin": ' lw_api_data-user_gstin_irn ','
  '"irn":' lv_irn  ','
  '"transporter_id":' lw_api_data-transporter_id ','
  '"transportation_mode":' lw_api_data-t_mode ','
  '"transporter_document_number":' lw_api_data-transporter_document_number ','
  '"transporter_document_date":' lw_api_data-transporter_document_date ','
  '"vehicle_number":' lw_api_data-vehicle_number ','
  '"distance":' lw_api_data-transportation_distance ','
  '"vehicle_type":' lw_api_data-v_type ','
  '"transporter_name":' lw_api_data-transporter_name ','
  '"data_source":' lw_api_data-data_source
  INTO lv_post SEPARATED BY ' '.

  IF lw_api_data-sub_supply_type = '"EXPORT"'.
    CONCATENATE lv_post ',' INTO lv_post SEPARATED BY ' '.
    CONCATENATE: lv_post '"ship_details":' '{'
    '"address1":' lw_api_data-address1_of_consignee ','
    '"address2":' lw_api_data-address2_of_consignee ','
    '"location":' lw_api_data-place_of_consignee ','
    '"pincode":' lw_api_data-pincode_of_consignee ','
    '"state_code":' lw_api_data-actual_to_state_name
    '}' INTO lv_post SEPARATED BY ' '.

  ENDIF.


  IF lw_api_data-company_name_d IS NOT INITIAL.
    CONCATENATE lv_post ',' INTO lv_post SEPARATED BY ' '.
    CONCATENATE: lv_post '"dispatch_details":' '{'
    '"company_name":' lw_api_data-company_name_d ','
    '"address1":' lw_api_data-address1_d ','
    '"address2":' lw_api_data-address2_d ','
    '"location":' lw_api_data-place_d ','
    '"pincode":' lw_api_data-pincode_d ','
    '"state_code":' lw_api_data-state_name_d
    '}' INTO lv_post SEPARATED BY ' '.
  ENDIF.

  CONCATENATE lv_post '}' INTO lv_post SEPARATED BY ' '.

  IF im_api_dwn = 'X'.
    CALL FUNCTION 'ZFM_EDOC_DOWNLOAD'
    EXPORTING
      im_data = lv_post.
    EXIT.
  ENDIF.

  cl_http_client=>create_by_url(
  EXPORTING
    url = lv_url
  IMPORTING
    CLIENT = lo_http_client
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

      lwa_message-TYPE = 'E'.
      lwa_message-message_v3 = lv_doc.
      lwa_message-MESSAGE    = 'Too much load on NIC server'.
      lwa_message-message_v1 = 'Too much load on NIC server'.
      APPEND lwa_message TO ex_messages.
      CLEAR lwa_message.

      lw_ewaybill-status = 'E'.
      lw_ewaybill-bukrs  = lw_api_data-bukrs.
      lw_ewaybill-docno = lv_doc.
      lw_ewaybill-ernam = sy-uname.
      lw_ewaybill-erdat = sy-datum.
      lw_ewaybill-gjahr = lw_api_data-doc_year.
      APPEND lw_ewaybill TO ex_ewaybill.
      CLEAR:lw_ewaybill.

      lw_eway_trasnport-bukrs = lw_api_data-bukrs.
      lw_eway_trasnport-docno = lw_api_data-vbeln.
      lw_eway_trasnport-gjahr  = lw_api_data-doc_year.
      lw_eway_trasnport-eway_error = 'Posting Error'.
      APPEND lw_eway_trasnport TO ex_eway_transport.
      CLEAR lw_eway_trasnport.
      EXIT.
    ENDTRY.

    lo_rest_entity = lo_rest_client->get_response_entity( ).


    lv_string = lo_rest_entity->get_string_data( ).
    http_status   = lo_rest_entity->get_header_field( '~status_code' ).


    IF  http_status EQ '200'.

      SPLIT lv_string AT ':' INTO lv_ew1 lv_ew2.

      REPLACE FIRST OCCURRENCE OF '{' IN lv_ew2 WITH space.
      lv_length1 = STRLEN( lv_ew2 ).
      lv_length1 = lv_length1 - 1.
      lv_ew2 = lv_ew2+0(lv_length1).

      CALL METHOD cl_fdt_json=>json_to_data
      EXPORTING
        iv_json = lv_ew2
      CHANGING
        ca_data = lw_output.

      IF lw_output-CODE = '204'.

        ex_return = 'E'.

        SPLIT lw_output-errormessage AT '||' INTO TABLE lt_message.

        LOOP AT lt_message INTO lw_message.
          lwa_message-TYPE = 'E'.
          lwa_message-message_v3 = lv_doc.
          lwa_message-MESSAGE    = lw_message-MESSAGE.
          lwa_message-message_v1 = lw_message-MESSAGE.
          APPEND lwa_message TO ex_messages.
          CLEAR lwa_message.
        ENDLOOP.


        lw_ewaybill-status = 'E'.
        lw_ewaybill-bukrs  = lw_api_data-bukrs.
        lw_ewaybill-docno = lv_doc.
        lw_ewaybill-ernam = sy-uname.
        lw_ewaybill-erdat = sy-datum.
        lw_ewaybill-gjahr = lw_api_data-doc_year.
        APPEND lw_ewaybill TO ex_ewaybill.
        CLEAR:lw_ewaybill.

        lw_eway_trasnport-bukrs = lw_api_data-bukrs.
        lw_eway_trasnport-docno = lw_api_data-vbeln.
        lw_eway_trasnport-gjahr  = lw_api_data-doc_year.
        lw_eway_trasnport-eway_error = lw_output-errormessage.
        APPEND lw_eway_trasnport TO ex_eway_transport.
        CLEAR lw_eway_trasnport.

      ELSE.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = lv_doc
        IMPORTING
          OUTPUT = lv_doc.

        lw_ewaybill-docno = lv_doc.
        lw_ewaybill-ebillno  = lw_output-MESSAGE-ewbno.
        lw_ewaybill-status = 'A'.
        lw_ewaybill-ernam = sy-uname.
        lw_ewaybill-erdat = sy-datum.

        CONCATENATE lw_output-MESSAGE-ewbdt+0(4) lw_output-MESSAGE-ewbdt+5(2) lw_output-MESSAGE-ewbdt+8(2) INTO  lw_ewaybill-egen_dat.
        CONCATENATE lw_output-MESSAGE-ewbdt+11(2) lw_output-MESSAGE-ewbdt+14(2) lw_output-MESSAGE-ewbdt+17(2) INTO  lw_ewaybill-egen_time.

        lw_ewaybill-vdfmdate = lw_ewaybill-egen_dat.
        IF lw_output-MESSAGE-ewbvalidtill IS NOT INITIAL.
          CONCATENATE lw_output-MESSAGE-ewbvalidtill+0(4) lw_output-MESSAGE-ewbvalidtill+5(2) lw_output-MESSAGE-ewbvalidtill+8(2) INTO  lw_ewaybill-vdtodate.
          CONCATENATE lw_output-MESSAGE-ewbvalidtill+11(2) lw_output-MESSAGE-ewbvalidtill+14(2) lw_output-MESSAGE-ewbvalidtill+17(2) INTO  lw_ewaybill-vdtotime.
        ENDIF.
        lw_ewaybill-bukrs  = lw_api_data-bukrs.
        lw_ewaybill-gjahr = lw_api_data-doc_year.
        APPEND lw_ewaybill TO ex_ewaybill.


        lw_eway_trasnport-bukrs = lw_api_data-bukrs.
        lw_eway_trasnport-docno = lw_api_data-vbeln.
        lw_eway_trasnport-gjahr  = lw_api_data-doc_year.
        REPLACE 'https://' IN lw_output-MESSAGE-ewaybillpdf WITH space.
        CONDENSE lw_output-MESSAGE-ewaybillpdf.
        lw_eway_trasnport-eway_print = lw_output-MESSAGE-ewaybillpdf.
        APPEND lw_eway_trasnport TO ex_eway_transport.
        CLEAR lw_eway_trasnport.

        lwa_message-TYPE = 'S'.
        lwa_message-message_v1 = 'E-Way Bill is successfully generated'.
        lwa_message-message_v2 = 'for document'.
        lwa_message-message_v3 = lv_doc.
* Find print URL
        lv_print_url =  lw_output-MESSAGE-ewaybillpdf.
        SPLIT lv_print_url AT '/' INTO lv_ew1 lv_ew2 lv_ew3 lv_ew4.
        REPLACE ALL OCCURRENCES OF '"}' IN lv_ew4 WITH space.
        lv_print_url = lv_ew4.
        CONCATENATE '/' lv_print_url INTO lv_print_url.
* Find print URL
        lwa_message-message_v4 = lv_print_url.

        APPEND lwa_message TO ex_messages.
        CLEAR lwa_message.
        ex_return = 'S'.

      ENDIF.

    ENDIF.
  ENDIF.
*  ENDIF.



ENDFUNCTION.
