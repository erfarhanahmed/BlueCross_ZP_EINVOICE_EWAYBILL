FUNCTION ZFM_EDOC_SEND_MAIL.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_EWAYBILL) TYPE  XFELD OPTIONAL
*"     REFERENCE(IM_EINVOICE) TYPE  XFELD OPTIONAL
*"     REFERENCE(IM_MESSAGES) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------

* This funcion module can be modified according toclient specific emailcontent and recepients
  DATA:lv_html_string  TYPE string,
        lv_table_string TYPE string,
        lw_message      TYPE bapiret2,
        lv_count        TYPE I.

  DATA: lv_xhtml_string  TYPE xstring,
        lt_hex           TYPE solix_tab,
        lo_send_request  TYPE REF TO cl_bcs,
        lo_sender        TYPE REF TO if_sender_bcs,
        lo_document      TYPE REF TO cl_document_bcs,
        lo_bcs_exception TYPE REF TO cx_bcs,
        lo_recipient     TYPE REF TO if_recipient_bcs,
        ls_rec_mail      TYPE ad_smtpadr,
        lt_rec_mail      TYPE TABLE OF ad_smtpadr,
        lv_name          TYPE string,
        lv_ext           TYPE string,
        lv_pernr         TYPE pernr_d.


  IF im_einvoice IS NOT INITIAL.
    CONCATENATE '<html><strong></strong>'
    '<p>Dear Sir/Madam, </p>'
    '<p>Error occured during E-Invoice creation.</p>'  INTO lv_html_string.

  ELSE.
    CONCATENATE '<html><strong></strong>'
    '<p>Dear Sir/Madam, </p>'
    '<p>Error occured during E-Invoice creation.</p>'  INTO lv_html_string.

  ENDIF.

  CLEAR lv_table_string.
  CONCATENATE '<body style="background-color: #D7ECF3;">'
  '<TABLE BORDER=4>'
  '<TR style="background-color: #96A5A0;"> <TH> Vendor </TH> '
  '<TH> Message </TH> '
  INTO
  lv_table_string.

  LOOP AT im_messages INTO lw_message.
    CONCATENATE lv_table_string '<TD>' lw_message-MESSAGE '</TD> </TR>'   INTO lv_table_string.

  ENDLOOP.

  CONCATENATE lv_table_string '</TABLE>' INTO lv_table_string.
  CONCATENATE lv_html_string lv_table_string INTO lv_html_string.

  CONCATENATE  lv_html_string '<br> </br>' INTO lv_html_string.
  CONCATENATE lv_html_string
  '<p>Thanks & Regards</p>'
  '<p></p>'
  INTO lv_html_string .

  CONCATENATE  lv_html_string '<br> </br>' INTO lv_html_string.
  CONCATENATE  lv_html_string '<br> </br>' INTO lv_html_string.


  CONCATENATE lv_html_string  '<p style="color:red;">This is an auto generated email, Please do not reply.</p>' INTO lv_html_string.



  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
  EXPORTING
    TEXT   = lv_html_string
  IMPORTING
    BUFFER = lv_xhtml_string
  EXCEPTIONS
    failed = 1
    OTHERS = 2.

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
  EXPORTING
    BUFFER     = lv_xhtml_string
  TABLES
    binary_tab = lt_hex.



  TRY.

    lo_send_request = cl_bcs=>create_persistent( ).

    lo_document = cl_document_bcs=>create_document(
    i_type    = 'HTM'
    i_hex    = lt_hex
    i_subject = 'E-Doc creation Error' ).

    CALL METHOD lo_send_request->set_document( lo_document ).


    lo_sender = cl_sapuser_bcs=>create( sy-uname ).
    CALL METHOD lo_send_request->set_sender
    EXPORTING
      i_sender = lo_sender.


* Add logic for recipts.

    SELECT SINGLE pernr FROM pa0105 INTO lv_pernr WHERE subty = '0001'
    AND usrty = '0001'
    AND usrid = sy-uname
    AND endda >= sy-datum
    AND begda <= sy-datum.

    IF sy-subrc IS INITIAL .
      SELECT SINGLE usrid_long INTO ls_rec_mail FROM pa0105 WHERE pernr = lv_pernr
      AND  subty = '0010'
      AND endda >= sy-datum
      AND begda <= sy-datum.
      IF sy-subrc IS INITIAL.
        APPEND ls_rec_mail TO lt_rec_mail.
      ENDIF.
    ENDIF.


    LOOP AT lt_rec_mail INTO ls_rec_mail.

      lo_recipient = cl_cam_address_bcs=>create_internet_address( ls_rec_mail ).

      CALL METHOD lo_send_request->add_recipient
      EXPORTING
        i_recipient  = lo_recipient
        i_express    = 'X'
        i_copy       = ' '
        i_blind_copy = ' '
        i_no_forward = ' '.

    ENDLOOP.


    lo_send_request->set_send_immediately( 'X' ).

    lo_send_request->send_request->set_link_to_outbox( 'X' ).

    CALL METHOD lo_send_request->send( ).

    COMMIT WORK.


  CATCH cx_bcs INTO lo_bcs_exception.
    MESSAGE i865(so) WITH lo_bcs_exception->error_type.
  ENDTRY.



ENDFUNCTION.
