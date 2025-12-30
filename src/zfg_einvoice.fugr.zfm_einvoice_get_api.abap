FUNCTION ZFM_EINVOICE_GET_API.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------


  DATA: lo_http_client    TYPE REF TO if_http_client,
        lo_rest_client    TYPE REF TO cl_rest_http_client,
        lv_url            TYPE        string,
        lv_body           TYPE        string,
        token             TYPE        string,
        oauth_http_client TYPE REF TO if_http_client,
        lo_response       TYPE REF TO     if_rest_entity,
        http_status       TYPE string,
        json_response     TYPE string.
  DATA l_username TYPE string.
  DATA l_password TYPE string.

  DATA: service_url TYPE string .


* Create HTTP intance using RFC restination created.
* You can directly use the REST service URL as well
  cl_http_client=>create_by_destination(
  EXPORTING
    destination              = 'E_Invoice'    " Logical destination (specified in function call)
  IMPORTING
    CLIENT                   = oauth_http_client    " HTTP Client Abstraction
  EXCEPTIONS
    argument_not_found       = 1
    destination_not_found    = 2
    destination_no_authority = 3
    plugin_not_active        = 4
    internal_error           = 5
    OTHERS                   = 6
    ).

  oauth_http_client->propertytype_logon_popup = oauth_http_client->co_disabled.


  CALL METHOD oauth_http_client->authenticate
  EXPORTING
    username = l_username
    password = l_password.

* Create REST client instance
  CREATE OBJECT lo_rest_client
  EXPORTING
    io_http_client = oauth_http_client.
* Set HTTP version
  oauth_http_client->request->set_version( if_http_request=>co_protocol_version_1_0 ).



  IF oauth_http_client IS BOUND AND lo_rest_client IS BOUND.

    cl_http_utility=>set_request_uri(
    EXPORTING
      request = oauth_http_client->request    " HTTP Framework (iHTTP) HTTP Request
      uri     = service_url                    " URI String (in the Form of /path?query-string)
      ).

    lo_rest_client->if_rest_client~GET( ).

* HTTP response
    lo_response = lo_rest_client->if_rest_client~get_response_entity( ).

* HTTP return status
    http_status   = lo_response->get_header_field( '~status_code' ).

* HTTP JSON return string
    json_response = lo_response->get_string_data( ).

*  token = json_response+182(25).

  ENDIF.


ENDFUNCTION.
