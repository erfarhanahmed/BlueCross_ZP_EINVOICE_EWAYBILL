FUNCTION ZFM_EWAY_BILL_UPD_TRANSID_API.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_TOKEN) TYPE  STRING
*"     REFERENCE(IM_API_DATA) TYPE  ZTT_EWAY_BILL_UPDAT_TRN_ID_STR
*"  EXPORTING
*"     REFERENCE(EX_RETURN) TYPE  STRING
*"     REFERENCE(EX_MESSAGES) TYPE  BAPIRET2_T
*"     REFERENCE(EX_API_DATA) TYPE  ZTT_EWAY_BILL_EXTEND_VALIDITY
*"----------------------------------------------------------------------

  DATA:it_api_data TYPE TABLE OF zst_eway_bill_updat_trn_id_str,
        wa_api_data TYPE zst_eway_bill_updat_trn_id_str.
  DATA:wa_ex_api_data TYPE zst_eway_bill_extend_validity.
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
        lv_token        TYPE string,
        http_status     TYPE string.

  DATA: lv_url TYPE string.
  DATA: ls_config TYPE zteinv_api.

  DATA :lr_rest_exception TYPE REF TO cx_rest_client_exception,
        lc_string         TYPE string.
  it_api_data = im_api_data.
  lv_token = im_token.


  CLEAR lv_url.
  SELECT SINGLE * FROM zteinv_api INTO ls_config WHERE apiid = 'EWAY_UPT'.
  IF sy-subrc IS INITIAL.
    lv_url = ls_config-apiuri.
    LOOP AT it_api_data INTO wa_api_data.
      CONCATENATE: lv_post '{'
      '"access_token":' lv_token ','
      '"userGstin":' wa_api_data-user_gstin ','
      '"eway_bill_number":' wa_api_data-ebillno ','
      '"transporter_id":' wa_api_data-transporter_id ','
      '"transporter_name":' wa_api_data-transporter_name
      '}' INTO lv_post SEPARATED BY ' '.
      cl_http_client=>create_by_url(
      EXPORTING
        url = lv_url "'https://clientbasic.mastersindia.co/transporterIdUpdate'
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
        ENDTRY.

        lo_rest_entity = lo_rest_client->get_response_entity( ).
        lv_string = lo_rest_entity->get_string_data( ).
        http_status   = lo_rest_entity->get_header_field( '~status_code' ).
        IF  http_status EQ '200'.
          ex_return = lv_string ."'S'.

          DATA : lv_ew  TYPE string,
                lv_ew1 TYPE string.
          DATA : lv_ew2  TYPE string,
                lv_ew3  TYPE string,
                lv_ew4  TYPE string,
                lv_ew5  TYPE string,
                lv_ew6  TYPE string,
                lv_ew7  TYPE string,
                lv_ew8  TYPE string,
                lv_ew9  TYPE string,
                lv_ew10 TYPE string,
                lv_data TYPE string.
          DATA:lv_date TYPE dats.
          DATA:lv_time TYPE sy-uzeit.
          DATA:it_texttab TYPE TABLE OF string,
                wa_texttab TYPE string.

          DATA lw_message TYPE bapiret2.
          REPLACE ALL OCCURRENCES OF  '"' IN wa_api_data-ebillno WITH ' '.
          SPLIT  lv_string AT ',' INTO :  lv_ew lv_ew1 lv_ew2 lv_ew3 lv_ew4 lv_ew5.
          SPLIT  lv_ew AT ':' INTO : lv_ew lv_ew6 lv_ew8.
          REPLACE ALL OCCURRENCES OF '"' IN lv_ew8 WITH ' '.
          CALL FUNCTION 'SOTR_SERV_STRING_TO_TABLE'
          EXPORTING
            TEXT        = lv_ew8
*             FLAG_NO_LINE_BREAKS       = 'X'
            line_length = '50'
            langu       = sy-langu
          TABLES
            text_tab    = it_texttab.

          IF lv_ew1 CS 'no content'.
            ex_return = 'E'.
            LOOP AT it_texttab INTO wa_texttab.
              IF sy-tabix > 1.
                lw_message-message_v2 = wa_texttab.
            ELSEIF sy-tabix > 2 .
                lw_message-message_v3 = wa_texttab.
              ELSE.
                lw_message-message_v4 = wa_api_data-ebillno.
                lw_message-TYPE = 'E'.
                lw_message-message_v1 = wa_texttab.
              ENDIF.

            ENDLOOP.
            APPEND lw_message TO ex_messages.
            CLEAR lw_message.
            CONTINUE.
        ELSEIF lv_ew2 CS 'No content'.
            ex_return = 'E'.
            LOOP AT it_texttab INTO wa_texttab.
              IF sy-tabix > 1.
                lw_message-message_v2 = wa_texttab.
            ELSEIF sy-tabix > 2 .
                lw_message-message_v3 = wa_texttab.
              ELSE.
                lw_message-message_v4 = wa_api_data-ebillno.
                lw_message-TYPE = 'E'.
                lw_message-message_v1 = wa_texttab.
              ENDIF.

            ENDLOOP.
            APPEND lw_message TO ex_messages.
            CLEAR lw_message.
            CONTINUE.
          ENDIF.

          CLEAR:lv_ew,lv_ew6.
          SPLIT lv_string AT '{' INTO lv_ew5 lv_ew9 lv_ew10 lv_data.
          SPLIT lv_data AT '}' INTO lv_data lv_ew5.
          CONCATENATE '{' lv_data '}' INTO lv_data.
          CALL METHOD cl_fdt_json=>json_to_data
          EXPORTING
            iv_json = lv_data
          CHANGING
            ca_data = wa_ex_api_data.

          APPEND wa_ex_api_data TO ex_api_data.
          CLEAR wa_ex_api_data.

          lw_message-TYPE = 'S'.
          lw_message-message_v1 = 'Transported ID successfully Updated'.
          lw_message-message_v2 = 'For the E-Way Bill No'.
          lw_message-message_v3 = wa_api_data-ebillno.
          APPEND lw_message TO ex_messages.
          ex_return = 'S'.
        ELSE.
          ex_return =  'E'.

        ENDIF.


      ENDIF.

      CLEAR:lv_post,lv_string.

    ENDLOOP.
  ENDIF.



ENDFUNCTION.
