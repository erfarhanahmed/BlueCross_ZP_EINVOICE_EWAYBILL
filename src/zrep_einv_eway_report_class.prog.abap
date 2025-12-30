*&---------------------------------------------------------------------*
*&  Include           ZREP_EINV_EWAY_REPORT_CLASS
*&---------------------------------------------------------------------*

* Data declartion for class

DATA:lw_tool TYPE stb_button,
      lw_menu TYPE stb_btnmnu.
DATA:dg_dyndoc_id   TYPE REF TO cl_dd_document,
      dg_splitter    TYPE REF TO cl_gui_splitter_container,
      dg_parent_grid TYPE REF TO cl_gui_container,
      dg_html_cntrl  TYPE REF TO cl_gui_html_viewer,
      dg_parent_html TYPE REF TO cl_gui_container.


* Class defenition
CLASS lcl_event DEFINITION .

  PUBLIC SECTION.
  METHODS :toolbar FOR EVENT toolbar OF  cl_gui_alv_grid
  IMPORTING e_object,
    user_command FOR EVENT user_command OF cl_gui_alv_grid
  IMPORTING e_ucomm,
    menu FOR EVENT menu_button  OF cl_gui_alv_grid
  IMPORTING e_object
    e_ucomm ,
    handle_print_top_of_page
    FOR EVENT print_top_of_page OF
    cl_gui_alv_grid,
    handle_top_of_page FOR EVENT top_of_page OF
  cl_gui_alv_grid IMPORTING e_dyndoc_id,

    handle_hotspot_click FOR EVENT hotspot_click OF
  cl_gui_alv_grid IMPORTING e_row_id e_column_id.
ENDCLASS.



DATA: go_obj TYPE REF TO lcl_event.



* Class Implementation
CLASS lcl_event IMPLEMENTATION.

* Toolbar
  METHOD toolbar.

    READ TABLE gt_doctyp TRANSPORTING NO FIELDS WITH KEY einv = abap_true.
    IF sy-subrc IS INITIAL.
      lw_tool-FUNCTION = gc_einv.
      lw_tool-TEXT     = 'E-Invoice Generation'(040).
      lw_tool-butn_type = gc_btn_type_2.
      lw_tool-ICON     = gc_icon_einv.
      APPEND lw_tool TO e_object->mt_toolbar.

    ENDIF.

    READ TABLE gt_doctyp TRANSPORTING NO FIELDS WITH KEY eway = abap_true.
    IF sy-subrc IS INITIAL.

      lw_tool-FUNCTION = gc_eway.
      lw_tool-TEXT     = 'E-Way Bill Generation'(041).
      lw_tool-butn_type = gc_btn_type_2.
      lw_tool-ICON     = gc_icon_eway.
      APPEND lw_tool TO e_object->mt_toolbar.

      lw_tool-FUNCTION = gc_edit.
      lw_tool-TEXT     = 'Edit Transporter (SAP Only)'(042).
      lw_tool-ICON     = gc_icon_edit.
      lw_tool-butn_type = gc_btn_type_0.
      APPEND lw_tool TO e_object->mt_toolbar.

    ENDIF.
*          READ TABLE gt_doctyp TRANSPORTING NO FIELDS WITH KEY einv = abap_true.
*          IF sy-subrc IS INITIAL.

    lw_tool-FUNCTION = gc_json.
    lw_tool-TEXT     = 'Download JSON'(120).
    lw_tool-butn_type = gc_btn_type_2.
    lw_tool-ICON     = gc_icon_json.
    APPEND lw_tool TO e_object->mt_toolbar.

*          ENDIF.


    lw_tool-FUNCTION = 'EDOCD'.
    lw_tool-TEXT     = 'Download'.
    lw_tool-ICON     = '@30@'.
    lw_tool-butn_type = '0'.
    APPEND lw_tool TO e_object->mt_toolbar.

    lw_tool-FUNCTION = 'EDOCU'.
    lw_tool-TEXT     = 'Upload'.
    lw_tool-ICON     = '@2Z@'.
    lw_tool-butn_type = '0'.
    APPEND lw_tool TO e_object->mt_toolbar.


  ENDMETHOD.             "DISPLAY

* Menu

  METHOD menu .

    IF e_ucomm = gc_ucomm_einv.

      CALL METHOD e_object->add_function
      EXPORTING
        fcode = gc_fcode_einvg
        TEXT  = 'Generate E-Invoice'(043).
*                icon  = gc_icon_39.
      SELECT SINGLE * FROM zteinv_canc_user INTO @DATA(wa_user1) WHERE username = @sy-uname.
      IF sy-subrc EQ 0.
        CALL METHOD e_object->add_function
        EXPORTING
          fcode = gc_fcode_einvc
          TEXT  = 'Cancel E-Invoice'(044).
      ENDIF.
      CALL METHOD e_object->add_function
      EXPORTING
        fcode = gc_fcode_einvp
        TEXT  = 'Download E-Invoice'.

      .
    ENDIF.

    IF e_ucomm = gc_ucomm_json.

      CALL METHOD e_object->add_function
      EXPORTING
        fcode = gc_fcode_einvj
        TEXT  = 'JSON For E-invoice'.
      CALL METHOD e_object->add_function
      EXPORTING
        fcode = gc_fcode_ewayj
        TEXT  = 'JSON For E-waybill'.

    ENDIF.

    IF e_ucomm = gc_ucomm_eway.

      CALL METHOD e_object->add_function
      EXPORTING
        fcode = gc_fcode_eeayg
        TEXT  = 'Generate E-Way Bill(Part A & B)'.


      CALL METHOD e_object->add_function
      EXPORTING
        fcode = gc_fcode_eeayb
        TEXT  = 'Generate E-Way Bill(Part A)'.


      CALL METHOD e_object->add_function
      EXPORTING
        fcode = gc_fcode_ewayp
        TEXT  = 'Download E-Way Bill'.

      CALL METHOD e_object->add_function
      EXPORTING
        fcode = gc_fcode_ewayu
        TEXT  = 'Update E-Way Bill Transporter'.

      CALL METHOD e_object->add_function
      EXPORTING
        fcode = gc_fcode_ewayv
        TEXT  = 'Update E-Way Bill Vehicle No.'.

      CALL METHOD e_object->add_function
      EXPORTING
        fcode = gc_fcode_ewaye
        TEXT  = 'Extend E-Way Bill Validity'.
      SELECT SINGLE * FROM zteinv_canc_user INTO @DATA(wa_user) WHERE username = @sy-uname.
      IF sy-subrc EQ 0.
        CALL METHOD e_object->add_function
        EXPORTING
          fcode = gc_fcode_ewayc
          TEXT  = 'Cancel E-Way Bill'.
      ENDIF.



    ENDIF.


  ENDMETHOD.


* User Command
  METHOD user_command.

    DATA: lw_index TYPE lvc_s_row,
          lv_val   TYPE xfeld.


    gv_e_comm = e_ucomm.

    CLEAR gv_download.
    IF gv_e_comm = gc_ucomm_einvj OR gv_e_comm  = gc_ucomm_ewayj.
      gv_download = abap_true.
    ENDIF.


    CASE e_ucomm.
    WHEN gc_ucomm_einvg OR gc_ucomm_einvj.
      PERFORM einvoice_generate.

    WHEN gc_ucomm_einvc.
      PERFORM einvoice_cancel_old.

    WHEN gc_ucomm_einvp.
      PERFORM einvoice_print.

    WHEN gc_ucomm_eeayg  OR gc_ucomm_eeayb OR gc_ucomm_ewayj.
      PERFORM ewaybill_generate.

    WHEN gc_ucomm_ewayu.
      PERFORM ewaybill_update_transporter.

    WHEN gc_ucomm_ewayv.
      PERFORM ewaybill_update_vehicle.

    WHEN gc_ucomm_ewaye.
      PERFORM ewaybill_extend.

    WHEN gc_ucomm_ewayc.
      PERFORM ewaybill_cancel_old.

    WHEN gc_ucomm_ewayp.
      PERFORM ewaybill_print.

    WHEN gc_ucomm_ewayt.
      PERFORM check_selected_data CHANGING lv_val.
      IF lv_val IS INITIAL.
        CALL SCREEN 9001 STARTING AT 20 20 ENDING AT 120 75.
      ELSE.
        MESSAGE 'Please select at least one document to edit Transporter details'(007) TYPE 'I'.
        EXIT.
      ENDIF.


    WHEN 'EDOCD'.
      PERFORM download_edoc.
    WHEN 'EDOCU'.
      PERFORM upload_edoc_file.


    ENDCASE.

  ENDMETHOD.                    "USER_COMMAND
* Top-of-Page

  METHOD handle_print_top_of_page.
    IF sy-pagno = 1.
    ENDIF.
  ENDMETHOD.

* Handle Top-of-page
  METHOD handle_top_of_page.
    PERFORM top_of_page USING dg_dyndoc_id.
  ENDMETHOD.


* Hotsspot click

  METHOD  handle_hotspot_click.
    IF e_column_id = 'VBELN'.
      PERFORM handle_hotspot_on_vbeln USING e_row_id.   " Subroutine to handle hotspot on customer number
    ENDIF.
  ENDMETHOD.


ENDCLASS.                    "LCL_EVENT IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM top_of_page USING   dg_dyndoc_id TYPE REF TO cl_dd_document.
  DATA : dl_text(255) TYPE C,
        lv_line      TYPE I,
        lv_lines     TYPE char10.
* Populating header to top-of-page
  READ TABLE gt_final INTO gw_final INDEX 1.
  CALL METHOD dg_dyndoc_id->add_text
  EXPORTING
    TEXT      = 'E-Invoice/E-Way Bill Register'(052)
    sap_style = cl_dd_area=>heading.

  CALL METHOD dg_dyndoc_id->new_line.
  CLEAR : dl_text.
  CONCATENATE 'Company Code:'(053) gw_final-bukrs
  INTO dl_text SEPARATED BY space.
  PERFORM add_text USING dl_text.
* Add new-line
  CALL METHOD dg_dyndoc_id->new_line.


  CLEAR : dl_text.
  CONCATENATE 'Generator/Approver:'(054) sy-uname
  INTO dl_text SEPARATED BY space.
  PERFORM add_text USING dl_text.

  CALL METHOD dg_dyndoc_id->new_line.


  CLEAR : dl_text.
  DESCRIBE TABLE  gt_final LINES lv_line.
  lv_lines = lv_line.

  CONCATENATE 'No of Record(s):'(055) lv_lines
  INTO dl_text SEPARATED BY space.
  PERFORM add_text USING dl_text.

  CALL METHOD dg_dyndoc_id->new_line.


* Populating data to html control
  PERFORM html.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DL_TEXT  text
*----------------------------------------------------------------------*
FORM add_text USING p_text TYPE sdydo_text_element.
* Adding text
  CALL METHOD dg_dyndoc_id->add_text
  EXPORTING
    TEXT         = p_text
    sap_emphasis = cl_dd_area=>heading.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HTML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM html .
  DATA : dl_length        TYPE I,                           " Length
        dl_background_id TYPE sdydo_key VALUE space. " Background_id
* Creating html control
  IF dg_html_cntrl IS INITIAL.
    CREATE OBJECT dg_html_cntrl
    EXPORTING
      parent = dg_parent_html.
  ENDIF.
* Reuse_alv_grid_commentary_set
  CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'
  EXPORTING
    document = dg_dyndoc_id
    bottom   = space
  IMPORTING
    length   = dl_length.
* Get TOP->HTML_TABLE ready
  CALL METHOD dg_dyndoc_id->merge_document.
* Set wallpaper
  CALL METHOD dg_dyndoc_id->set_document_background
  EXPORTING
    picture_id = dl_background_id.
* Connect TOP document to HTML-Control
  dg_dyndoc_id->html_control = dg_html_cntrl.
* Display TOP document
  CALL METHOD dg_dyndoc_id->display_document
  EXPORTING
    reuse_control      = 'X'
    parent             = dg_parent_html
  EXCEPTIONS
    html_display_error = 1.
  IF sy-subrc NE 0.
    MESSAGE  'Error in displaying top-of-page'(036) TYPE 'I'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_ON_VBELN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID  text
*----------------------------------------------------------------------*
FORM handle_hotspot_on_vbeln  USING    p_e_row_id.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_ON_VBELN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID  text
*----------------------------------------------------------------------*

  DATA: lw_final TYPE ty_final.

  READ TABLE gt_final INTO lw_final INDEX p_e_row_id.
  IF sy-subrc IS INITIAL.
    IF  lw_final-MOD = gc_sd.

      CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        tcode  = 'VF03'
      EXCEPTIONS
        ok     = 1
        not_ok = 2
        OTHERS = 3.
      IF sy-subrc <> 1.
        MESSAGE 'You are not a authorised User for VF03'(056) TYPE 'I'.
        EXIT.
    ELSEIF sy-subrc = 1.
        SET PARAMETER ID 'VF' FIELD lw_final-vbeln.
        CALL TRANSACTION  'VF03' AND SKIP FIRST SCREEN.
      ENDIF.

    ENDIF.

  ENDIF.
ENDFORM.
