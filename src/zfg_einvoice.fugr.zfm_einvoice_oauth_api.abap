FUNCTION ZFM_EINVOICE_OAUTH_API.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(EX_TOKEN) TYPE  STRING
*"     VALUE(EX_RETURN) TYPE  STRING
*"     VALUE(ET_MESSAGES) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------

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
        lo_rest_entity TYPE REF TO if_rest_entity,
        http_status TYPE string.

  DATA: ls_match        TYPE match_result,
        lv_offset_start TYPE sy-tabix,
        lv_offset_end   TYPE sy-tabix,
        lv_string       TYPE string,
        lv_model_code   TYPE char12,
        lv_length       TYPE sy-tabix,
        lv_post         TYPE  string,
        lc_string       TYPE string,
        lv_cnt_id       TYPE string,
        lv_cnt_secret   TYPE string.
  DATA : lv_token TYPE string,
        lv_tok   TYPE  string.
  DATA lr_rest_exception TYPE REF TO cx_rest_client_exception.
  DATA:ls_config        TYPE zteinv_api,
        ls_client_id     TYPE zteinv_api,
        ls_client_secret TYPE zteinv_api,
        lv_url           TYPE string,
        lw_message       TYPE bapiret2,
        ls_token         TYPE zteinv_api,
        lv_time          TYPE sy-uzeit.


  SELECT SINGLE * FROM zteinv_api INTO ls_token WHERE apiid = 'ACC_TOKEN'.
  IF sy-subrc IS INITIAL.
    IF ls_token-apiuri IS NOT INITIAL.
      IF ls_token-aedat = sy-datum.
        lv_time = sy-uzeit - ls_token-aetim.
        IF lv_time < '050000'.
          ex_token = ls_token-apiuri.
          ex_return = 'S'.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.



  SELECT SINGLE * FROM zteinv_api INTO ls_config WHERE apiid = 'AUTH'.
  IF sy-subrc IS INITIAL.

    lv_url = ls_config-apiuri.

    SELECT SINGLE * FROM zteinv_api INTO ls_client_id WHERE apiid = 'CNT_ID'.
    IF sy-subrc IS  INITIAL.
      lv_cnt_id = ls_client_id-apiuri.
    ENDIF.

    SELECT SINGLE * FROM zteinv_api INTO ls_client_secret WHERE apiid = 'CNT_SECRET'.
    IF sy-subrc IS  INITIAL.
      lv_cnt_secret = ls_client_secret-apiuri.
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


* Create REST Client
      CREATE OBJECT lo_rest_client TYPE cl_rest_http_client
      EXPORTING
        io_http_client = lo_http_client.

* Create REST entity Object
      lo_rest_entity = lo_rest_client->create_request_entity( ).

* Create Request JSON data in a string form


*      CONCATENATE '{' '"username":"' ls_config-uname '",'
*      '"password" :"' ls_config-password '"' ','
*      '"client_id":"fIXefFyxGNfDWOcCWn"' ','
*      '"client_secret":"QFd6dZvCGqckabKxTapfZgJc"' ','
*      '"grant_type":"password"'
*      '}'
*       INTO lc_string.


      CONCATENATE '{' '"username":"' ls_config-uname '",'
      '"password" :"' ls_config-password '",'
      '"client_id":"' lv_cnt_id '",'
      '"client_secret":"' lv_cnt_secret '",'
      '"grant_type":"password"'
      '}'
      INTO lc_string.



      lo_rest_entity->set_content_type( if_rest_media_type=>gc_appl_json ).

      CALL METHOD lo_rest_entity->set_string_data
      EXPORTING
        iv_data = lc_string.

      .
      TRY.
* POST request ( or call the web service )
        lo_rest_client->post( lo_rest_entity ).
      CATCH cx_rest_client_exception INTO lr_rest_exception.

      ENDTRY.

* Get Response entiry
      lo_rest_entity = lo_rest_client->get_response_entity( ).

*Get Respose XML as string
      lv_string = lo_rest_entity->get_string_data( ).

      CLEAR :  lv_token.
      SPLIT  lv_string AT ':' INTO :  lv_token   lv_tok  .
      SPLIT  lv_tok AT  ',' INTO : lv_token lv_string.

      http_status   = lo_rest_entity->get_header_field( '~status_code' ).

      IF http_status EQ '200'.
        ex_return = 'S'.
        ex_token = lv_token.


        UPDATE zteinv_api SET apiuri = lv_token
        aenam  = sy-uname
        aedat  = sy-datum
        aetim  = sy-uzeit
        WHERE apiid = 'ACC_TOKEN'.



      ELSE.
        CLEAR:ex_token ,
        lv_token.
        ex_return = 'E'.

* Set message

        lw_message-TYPE = 'E'.
        lw_message-ID = '00'.
        lw_message-NUMBER = 001.
        lw_message-message_v1 = 'Access Error'.
        APPEND lw_message TO et_messages.

      ENDIF.

      CLEAR :  lv_token , lv_string.



    ENDIF.
  ENDIF.



ENDFUNCTION.
