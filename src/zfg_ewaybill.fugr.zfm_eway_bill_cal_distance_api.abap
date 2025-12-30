FUNCTION ZFM_EWAY_BILL_CAL_DISTANCE_API.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_TOKEN) TYPE  STRING
*"     VALUE(IM_ACTION) TYPE  CHAR100
*"     VALUE(IM_USER_GSTIN) TYPE  CHAR25
*"     VALUE(IM_HSNCODE) TYPE  CHAR16
*"     VALUE(IM_FROM_PINCODE) TYPE  CHAR10
*"     VALUE(IM_TO_PINCODE) TYPE  CHAR10
*"  EXPORTING
*"     VALUE(EX_RETURN) TYPE  STRING
*"     VALUE(EX_MESSAGES) TYPE  BAPIRET2_T
*"     VALUE(EX_DISTANCE) TYPE  STRING
*"----------------------------------------------------------------------

  DATA:lv_token TYPE string.
*  DATA:it_data TYPE TABLE OF zeway_cal_distance_stru,
*       wa_data TYPE zeway_cal_distance_stru.
  DATA:lv_string  TYPE string,
        lc_string  TYPE string,
        lv_tok     TYPE string,
        lv_action  TYPE string,
        lv_gstin   TYPE string,
        lv_hsncode TYPE string.
*  it_data = im_api_data.
  DATA: lo_http_client    TYPE REF TO if_http_client,
        lo_rest_client1   TYPE REF TO cl_rest_http_client,
        lo_rest_client    TYPE REF TO if_rest_client,
        lo_rest_entity    TYPE REF TO if_rest_entity,
        lr_rest_exception TYPE REF TO cx_rest_client_exception,
        lo_response       TYPE REF TO     if_rest_entity.
  DATA:it_texttab TYPE TABLE OF string,
        wa_texttab TYPE string.

  DATA:ls_config TYPE zteinv_api,
        lv_url    TYPE string.


  DATA lw_message TYPE bapiret2.
  CLEAR lc_string.
*  ENDIF.
  lv_token = im_token.
  lv_hsncode = im_hsncode.
  REPLACE ALL OCCURRENCES OF '"' IN lv_token WITH ''.
  CONDENSE lv_token.

  CLEAR lv_url.
  SELECT SINGLE * FROM zteinv_api INTO ls_config WHERE apiid = 'GET_DIST'.
  IF sy-subrc IS INITIAL.
    lv_url = ls_config-apiuri.



    CONCATENATE  lv_url
    '?access_token=' lv_token '&fromPincode=' im_from_pincode '&toPincode=' im_to_pincode  INTO lv_url.
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
      MESSAGE 'The URL can not be defined' TYPE 'E'.
    ENDIF.


    "setting request method
    lo_http_client->request->set_method('GET').

    lv_action = im_action.
    lv_gstin = im_user_gstin.
    lv_hsncode = im_hsncode.

    lo_http_client->request->set_header_field( name = 'access_token' VALUE = lv_token ).
    lo_http_client->request->set_header_field( name = 'action' VALUE = lv_action ).
    "API Key for API Sandbox
    lo_http_client->request->set_header_field( name = 'gstin' VALUE = lv_gstin ).
    lo_http_client->request->set_header_field( name = 'hsncode' VALUE = lv_hsncode ).


    CALL METHOD lo_http_client->send
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      http_invalid_timeout       = 4
      OTHERS                     = 5.


    IF sy-subrc = 0.
      CALL METHOD lo_http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 5.
    ENDIF.


    IF sy-subrc <> 0.
      "error handling
    ENDIF.


    lc_string = lo_http_client->response->get_cdata( ).
    DATA : lv_ew  TYPE string,
          lv_ew1 TYPE string.
    DATA : lv_ew2 TYPE string,
          lv_ew3 TYPE string,
          lv_ew4 TYPE string,
          lv_ew5 TYPE string,
          lv_ew6 TYPE string,
          lv_ew7 TYPE string.

    SPLIT  lc_string AT ',' INTO :  lv_ew lv_ew1 lv_ew2."lv_ew3." lv_ew4 lv_ew5 lv_ew6
    SPLIT lv_ew AT ':' INTO lv_ew3 lv_ew4 lv_ew5.
    REPLACE ALL OCCURRENCES OF '"' IN lv_ew5 WITH ' '.
    IF  lv_ew1 CS 'no content'.

      ex_return = 'E'.
      lw_message-TYPE = 'E'.
      lw_message-message_v1 = lv_ew5.
*        lw_message-message_v2 = 'For the E-Way Bill No'.
*        lw_message-message_v3 = wa_api_data-ebillno.
      APPEND lw_message TO ex_messages.

    ELSE.
      ex_return = 'S'.
      ex_distance = lv_ew5.
      lw_message-TYPE = 'S'.
      lw_message-message_v1 = 'Distance is calculated successfully.'.
*        lw_message-message_v2 = 'For the E-Way Bill No'.
*        lw_message-message_v3 = wa_api_data-ebillno.
      APPEND lw_message TO ex_messages.
    ENDIF.


  ENDIF.



ENDFUNCTION.
