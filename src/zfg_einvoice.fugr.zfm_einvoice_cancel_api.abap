FUNCTION ZFM_EINVOICE_CANCEL_API.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_TOKEN) TYPE  STRING
*"     VALUE(IM_INVREF) TYPE  ZTT_J_1IG_INVREFNUM
*"     VALUE(IM_GSTIN) TYPE  KNA1-STCD3
*"     VALUE(IM_REASON) TYPE  CHAR20
*"     VALUE(IM_REMARKS) TYPE  CHAR255
*"  EXPORTING
*"     VALUE(EX_RETURN) TYPE  STRING
*"     VALUE(EX_CANCEL_DATE) TYPE  J_1IG_CANC_DATE
*"     VALUE(EX_INVREF) TYPE  ZTT_J_1IG_INVREFNUM
*"     VALUE(EX_MESSAGES) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------
  TYPES: BEGIN OF ty_cancel1,
    results TYPE string,
  END OF ty_cancel1,
  BEGIN OF ty_cancel3,
    irn        TYPE string,
    canceldate TYPE string,
  END OF ty_cancel3,
  BEGIN OF ty_cancel2,
    MESSAGE      TYPE ty_cancel3,
    errormessage TYPE string,
    infodtls     TYPE string,
    status       TYPE string,
    CODE         TYPE string,
  END OF ty_cancel2.


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
        lv_length1       TYPE I,
        lv_post         TYPE  string,
        lc_string       TYPE string,
        lv_token        TYPE string,
        ls_cancel1      TYPE ty_cancel1,
        ls_cancel2      TYPE ty_cancel2,
        ls_cancel3      TYPE ty_cancel3.

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
        alert   TYPE string.

  DATA: "wa_einv  TYPE zeinv_gen,
        lv_date    TYPE dats,
        lv_gstin   TYPE  kna1-stcd3,
        lv_reason  TYPE char20,
        lv_remarks TYPE char255,
        lv_tabix   TYPE sy-tabix,
        lv_irn     TYPE string.
  DATA lr_rest_exception TYPE REF TO cx_rest_client_exception.
  DATA lw_message TYPE bapiret2.

  DATA:lw_invref    TYPE  j_1ig_invrefnum,
        lw_config    TYPE zteinv_api,
        lv_url       TYPE string,
        http_status  TYPE string,
        http_status1 TYPE string.



  lv_token   = im_token.
  lv_gstin   = im_gstin.
  lv_reason  = im_reason.
  lv_remarks = im_remarks.
  ex_invref  = im_invref.

  CLEAR ex_messages.

  SELECT SINGLE * FROM zteinv_api INTO lw_config WHERE apiid = 'EINVCAN'.
  IF sy-subrc IS INITIAL.
    lv_url = lw_config-apiuri.
  ENDIF.


  LOOP AT im_invref INTO lw_invref.
    lv_tabix   =  sy-tabix.
    CLEAR: lv_irn.
    CONCATENATE '"' lw_invref-irn '"' INTO lv_irn.

    CONCATENATE '{'
    '"access_token":' lv_token ','
    '"user_gstin":' lv_gstin ','
    '"irn":' lv_irn ','
    '"cancel_reason":' lv_reason ','      "'1'
    '"cancel_remarks":' lv_remarks    "'Wrong Entry'
    '}'    INTO lv_post  SEPARATED BY space.


    cl_http_client=>create_by_url(
    EXPORTING
      url = lv_url "'https://clientbasic.mastersindia.co/cancelEinvoice'
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

      http_status   = lo_rest_entity->get_header_field( '~status_code' ).
      http_status1   = lo_rest_entity->get_header_field( 'status' ).

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
          ca_data = ls_cancel2.


        IF ls_cancel2-status = 'Failed'.

          ex_return = 'E'.

          lw_message-TYPE = 'E'.
          lw_message-message_v1 = ls_cancel2-errormessage.
          lw_message-message_v2 = 'for document'.
          lw_message-message_v3 = lw_invref-docno.

          APPEND lw_message TO ex_messages.

          CONTINUE.
        ENDIF.

        SPLIT ls_cancel2-MESSAGE-canceldate AT ' ' INTO lv_ew1 lv_ew3.
        CONCATENATE lv_ew1+0(4) lv_ew1+5(2) lv_ew1+8(2) INTO lv_date.

        lw_invref-cancel_date = lv_date.
        lw_invref-irn_status = 'CNL'.
        lw_invref-ernam = sy-uname.
        lw_invref-erdat = sy-datum.
        lw_invref-erzet = sy-uzeit.
        MODIFY ex_invref FROM lw_invref INDEX lv_tabix TRANSPORTING ernam erdat erzet cancel_date irn_status.


        lw_message-TYPE = 'S'.
        lw_message-message_v1 = 'IRN successfully cancelled for document'.
        lw_message-message_v2 = lw_invref-docno.
        APPEND lw_message TO ex_messages.


        ex_return = 'S'.

      ELSE.
        ex_return =  'E'.
      ENDIF.

    ENDIF.

    CLEAR: lv_post,lw_message.
  ENDLOOP.




ENDFUNCTION.
