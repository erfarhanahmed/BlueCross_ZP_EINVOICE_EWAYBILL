FUNCTION ZFM_EWAY_BILL_DELETE_API.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_TOKEN) TYPE  STRING
*"     REFERENCE(IM_API_DATA) TYPE  ZTT_EWAY_API_STRUCT
*"  EXPORTING
*"     REFERENCE(EX_RETURN) TYPE  STRING
*"     REFERENCE(EX_MESSAGES) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------

  DATA:it_api_data TYPE TABLE OF zst_eway_api_struct,
        wa_api_data TYPE zst_eway_api_struct.
  DATA:lv_string  TYPE string,
        lc_string  TYPE string,
        lv_url     TYPE string,
        lv_tok     TYPE string,
        lv_action  TYPE string,
        lv_gstin   TYPE string,
        lv_hsncode TYPE string,
        lv_token   TYPE string,
        lv_delete  TYPE string,
        ls_config  TYPE zteinv_api,
        http_status TYPE string.

  DATA: lo_http_client    TYPE REF TO if_http_client,
        lo_rest_client1   TYPE REF TO cl_rest_http_client,
        lo_rest_client    TYPE REF TO if_rest_client,
        lo_rest_entity    TYPE REF TO if_rest_entity,
        lr_rest_exception TYPE REF TO cx_rest_client_exception,
        lo_response       TYPE REF TO     if_rest_entity.
  DATA:it_texttab TYPE TABLE OF string,
        wa_texttab TYPE string.

  DATA lw_message TYPE bapiret2.
  it_api_data = im_api_data.
  lv_token = im_token.


  CLEAR lv_url.
  SELECT SINGLE * FROM zteinv_api INTO ls_config WHERE apiid = 'EWAY_DEL'.
  IF sy-subrc IS INITIAL.
    lv_url = ls_config-apiuri.


    LOOP AT it_api_data INTO wa_api_data.
      CONCATENATE lv_delete '{'
      '"access_token":' lv_token ','
      '"gstin":' wa_api_data-user_gstin ','
      '"document_number":' wa_api_data-document_number
      '}' INTO lv_delete SEPARATED BY ' '.

      CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = lv_url
      IMPORTING
        CLIENT             = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.




      IF sy-subrc <> 0.
        "error handling
      ENDIF.
      CREATE OBJECT lo_rest_client TYPE cl_rest_http_client
      EXPORTING
        io_http_client = lo_http_client.

      lo_rest_entity = lo_rest_client->create_request_entity( ).

      lo_rest_entity->set_content_type( if_rest_media_type=>gc_appl_json ).
      CALL METHOD lo_rest_entity->set_string_data
      EXPORTING
        iv_data = lv_delete.
      TRY.

        lo_rest_client->delete( ).
      CATCH cx_rest_client_exception INTO lr_rest_exception.
        MESSAGE 'Error occured' TYPE 'E' RAISING cx_rest_client_exception.
      ENDTRY.

      lo_rest_entity = lo_rest_client->get_response_entity( ).

      lv_string = lo_rest_entity->get_string_data( ).
      http_status   = lo_rest_entity->get_header_field( '~status_code' ).

      IF  http_status EQ '200'.
        DATA : lv_ew  TYPE string,
              lv_ew1 TYPE string.
        DATA : lv_ew2    TYPE string,
              lv_ew3    TYPE string,
              lv_ew4    TYPE string,
              lv_ew5    TYPE string,
              lv_ew6    TYPE string,
              lv_ew7    TYPE string,
              lv_ew8    TYPE string,
              lv_ew9    TYPE string,
              lv_item   TYPE string,
              lv_item1  TYPE string,
              lv_header TYPE string,
              lv_items  TYPE string,
              alert     TYPE string.
        SPLIT lv_string AT ',' INTO lv_ew lv_ew1 lv_ew2 lv_ew3.
        SPLIT  lv_ew AT ':' INTO : lv_ew lv_ew6 lv_ew8.
        REPLACE ALL OCCURRENCES OF '"' IN lv_ew8 WITH ' '.
        REPLACE ALL OCCURRENCES OF '"' IN wa_api_data-document_number WITH ' '.
        CALL FUNCTION 'SOTR_SERV_STRING_TO_TABLE'
        EXPORTING
          TEXT        = lv_ew8
*           FLAG_NO_LINE_BREAKS       = 'X'
          line_length = '40'
          langu       = sy-langu
        TABLES
          text_tab    = it_texttab.
        IF lv_ew1 CS 'no content'.
          ex_return = 'E'.
          LOOP AT it_texttab INTO wa_texttab.
            IF sy-tabix = 2.
              lw_message-message_v2 = wa_texttab.
          ELSEIF sy-tabix = 3 .
              lw_message-message_v3 = wa_texttab.
            ELSE.
              lw_message-message_v4 = wa_api_data-document_number.
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
            IF sy-tabix = 2.
              lw_message-message_v2 = wa_texttab.
          ELSEIF sy-tabix = 3 .
              lw_message-message_v3 = wa_texttab.
            ELSE.
              lw_message-message_v4 = wa_api_data-document_number.
              lw_message-TYPE = 'E'.
              lw_message-message_v1 = wa_texttab.
            ENDIF.

          ENDLOOP.
          APPEND lw_message TO ex_messages.
          CLEAR lw_message.
          CONTINUE.
        ELSE.
          ex_return = 'S'.
          lw_message-TYPE = 'S'.
          lw_message-message_v1 = 'E-Way Bill for document'."Deleted successfully.'.
          lw_message-message_v2 = wa_api_data-document_number."'For the E-Way Bill No'.
          lw_message-message_v3 = 'is deleted successfully.'.
          APPEND lw_message TO ex_messages.
        ENDIF.

        ex_return = 'E'.
      ENDIF.
    ENDLOOP.

  ENDIF.



ENDFUNCTION.
