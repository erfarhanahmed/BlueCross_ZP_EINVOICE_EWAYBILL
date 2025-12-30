*----------------------------------------------------------------------*
***INCLUDE ZREP_EINV_EWAY_REPORT_STATUO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS '9000'.
  SET TITLEBAR  '9000'.

  DATA: lt_variant TYPE  disvariant,
        lw_layout  TYPE lvc_s_layo.
  DATA:o_eventreceiver  TYPE REF TO lcl_event.
  CLEAR lw_layout.

  lw_layout-zebra = space.
  lw_layout-cwidth_opt = abap_true.
  lw_layout-sel_mode = gc_lay_mode_a."'A'.

* Create Controls
  IF gref_alv_container IS NOT BOUND.
    CREATE OBJECT gref_alv_container
    EXPORTING
      container_name = 'CUST_ALV'.

    CREATE OBJECT dg_dyndoc_id
    EXPORTING
      style = 'ALV_GRID'.
* Create Splitter for custom_container
    CREATE OBJECT dg_splitter
    EXPORTING
      parent  = gref_alv_container
      ROWS    = 2
      columns = 1.
* Split the custom_container to two containers and move the reference
* to receiving containers g_parent_html and g_parent_grid
    "i am allocating the space for grid and top of page
    CALL METHOD dg_splitter->get_container
    EXPORTING
      row       = 1
      column    = 1
      RECEIVING
      container = dg_parent_html.

    CALL METHOD dg_splitter->get_container
    EXPORTING
      row       = 2
      column    = 1
      RECEIVING
      container = dg_parent_grid.

    "you can set the height of it
* Set height for g_parent_html
    CALL METHOD dg_splitter->set_row_height
    EXPORTING
      ID     = 1
      height = 15.
      "from here as usual..you need to specify parent as splitter part
      "which we alloted for grid
      CREATE OBJECT gref_alv_grid
      EXPORTING
        i_parent = dg_parent_grid.

      IF gref_alv_container IS BOUND.

*  Populate Field Catalog
        PERFORM get_fieldcatalog.

        CREATE OBJECT go_obj.
        IF go_obj IS BOUND.
          SET HANDLER : go_obj->toolbar FOR gref_alv_grid,
          go_obj->user_command FOR gref_alv_grid,
          go_obj->menu FOR gref_alv_grid,
          go_obj->handle_top_of_page FOR gref_alv_grid,
          go_obj->handle_hotspot_click FOR   gref_alv_grid.


          lt_variant-REPORT = sy-repid.
          lt_variant-username = sy-uname.
          lt_variant-handle = 'GRID'.


          CALL METHOD gref_alv_grid->set_table_for_first_display
          EXPORTING
            is_layout       = lw_layout
            is_variant      = lt_variant
            i_save          = gc_alv_save_a
          CHANGING
            it_outtab       = gt_final[]
            it_fieldcatalog = gt_fieldcat[].
        ENDIF.
      ENDIF.

    ELSE.
      CALL METHOD gref_alv_grid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
      IF sy-subrc <> 0.
        MESSAGE 'Table refresh error'(012) TYPE 'I'.
      ENDIF.

    ENDIF.
    CALL METHOD gref_alv_grid->list_processing_events
    EXPORTING
      i_event_name = 'TOP_OF_PAGE'
      i_dyndoc_id  = dg_dyndoc_id.

  ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  GET_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM get_fieldcatalog .

    REFRESH: gt_fieldcat.
* Document
    PERFORM fill_fieldcatalog USING 'E-Invoice Status'(086) 'ICON' 'GT_FINAL' '7' 'X'.
    PERFORM fill_fieldcatalog USING 'E-Way Status'(102)  'EWAY_ICON' 'GT_FINAL' '7' 'X'.
    PERFORM fill_fieldcatalog USING 'Supplier GSTIN'(063) 'SUP_GSTIN' 'GT_FINAL' '20' 'X'.
    PERFORM fill_fieldcatalog USING 'Plant'(064) 'WERKS' 'GT_FINAL' '6' 'X'.
    PERFORM fill_fieldcatalog USING 'Document Number'(065) 'VBELN' 'GT_FINAL' '15' 'X'.
    PERFORM fill_fieldcatalog USING 'Document Date'(066) 'FKDAT' 'GT_FINAL' '15' 'X'.
    PERFORM fill_fieldcatalog USING 'Doc Type'(067) 'FKART' 'GT_FINAL' '10' ''.
    PERFORM fill_fieldcatalog USING 'Created By'(068) 'DOC_ERNAM' 'GT_FINAL' '12' ''.
    PERFORM fill_fieldcatalog USING 'GST Invoice No'(069) 'ODNNO' 'GT_FINAL' '16' ''.
    IF p_mod = gc_sd.
      PERFORM fill_fieldcatalog USING 'Customer'(070) 'KUNNR' 'GT_FINAL' '12' ''.
      PERFORM fill_fieldcatalog USING 'Customer Name'(071) 'NAME1' 'GT_FINAL' '20' ''.
  ELSEIF p_mod = gc_mm.
      PERFORM fill_fieldcatalog USING  'Vendor'(072) 'KUNNR' 'GT_FINAL' '12' ''.
      PERFORM fill_fieldcatalog USING  'Vendor Name'(073) 'NAME1' 'GT_FINAL' '20' ''.
    ENDIF.
    PERFORM fill_fieldcatalog USING 'Customer GSTIN'(117) 'SHIP_GSTIN' 'GT_FINAL' '20' ''.

    PERFORM fill_fieldcatalog USING 'Ship To'(074) 'SHIP_TO' 'GT_FINAL' '12' ''.
    PERFORM fill_fieldcatalog USING 'Ship To Name'(075) 'SHIP_TO_NAME' 'GT_FINAL' '20' ''.
* Values
    PERFORM fill_fieldcatalog USING 'Invoice Value'(076) 'INVVAL' 'GT_FINAL' '15' ''.
    PERFORM fill_fieldcatalog USING 'Taxable Value'(077) 'TAXAMT' 'GT_FINAL' '15' ''.
    PERFORM fill_fieldcatalog USING 'CGST'(078) 'CGST_AMT' 'GT_FINAL' '15' ''.
    PERFORM fill_fieldcatalog USING 'SGST'(079) 'SGST_AMT' 'GT_FINAL' '15' ''.
    PERFORM fill_fieldcatalog USING 'IGST'(080) 'IGST_AMT' 'GT_FINAL' '15' ''.
    PERFORM fill_fieldcatalog USING 'CESS'(081) 'CESS_AMT' 'GT_FINAL' '15' ''.
* E-Invoice
    READ TABLE gt_doctyp TRANSPORTING NO FIELDS WITH KEY einv = abap_true.
    IF sy-subrc IS INITIAL.
      PERFORM fill_fieldcatalog USING 'Ack No'(082) 'ACK_NO' 'GT_FINAL' '15' ''.
      PERFORM fill_fieldcatalog USING 'Ack Date'(083) 'ACK_DT' 'GT_FINAL' '15' ''.
      PERFORM fill_fieldcatalog USING 'IRN'(084) 'IRN' 'GT_FINAL' '25' ''.
      PERFORM fill_fieldcatalog USING 'Cancel Date'(085) 'CANC_DT' 'GT_FINAL' '12' ''.
      PERFORM fill_fieldcatalog USING 'Error description'(087) 'EINV_ERROR' 'GT_FINAL' '50' ''.
      PERFORM fill_fieldcatalog USING 'Created By'(088) 'ERNAM' 'GT_FINAL' '12' ''.
      PERFORM fill_fieldcatalog USING 'Created Date'(089) 'ERDAT' 'GT_FINAL' '10' ''.
      PERFORM fill_fieldcatalog USING 'Created Time'(090) 'ERZET' 'GT_FINAL' '8' ''.
    ENDIF.

* E-Way Bill
    READ TABLE gt_doctyp TRANSPORTING NO FIELDS WITH KEY eway = abap_true.
    IF sy-subrc IS INITIAL.

      PERFORM fill_fieldcatalog USING 'Transporter ID'(091) 'T_ID' 'GT_FINAL' '15' ''.
      PERFORM fill_fieldcatalog USING 'Transporter Name'(092) 'T_NAME' 'GT_FINAL' '20' ''.
      PERFORM fill_fieldcatalog USING 'Mode'(093)          'T_MODE' 'GT_FINAL' '5' ''.
      PERFORM fill_fieldcatalog USING 'Document No'(094)   'T_DOC_NO' 'GT_FINAL' '15' ''.
      PERFORM fill_fieldcatalog USING 'Document Date'(095) 'T_DATE' 'GT_FINAL' '12' ''.
*    PERFORM fill_fieldcatalog USING 'Distance in KM' 'T_DISTANCE' 'GT_FINAL' '5' ''.
      PERFORM fill_fieldcatalog USING 'Vehicle No'(096)   'V_NUMBER' 'GT_FINAL' '20' ''.
      PERFORM fill_fieldcatalog USING 'Vehicle Type'(097) 'V_TYPE' 'GT_FINAL' '20' ''.
      PERFORM fill_fieldcatalog USING 'E-Waybill No'(098) 'EWAY_NUM' 'GT_FINAL' '12' ''.
      PERFORM fill_fieldcatalog USING 'E-Way date'(099)   'EWAY_DATE' 'GT_FINAL' '15' ''.
      PERFORM fill_fieldcatalog USING 'Valid Upto'(100)      'EWAY_V_TO' 'GT_FINAL' '10' ''.
      PERFORM fill_fieldcatalog USING 'Cancel Date'(101)  'EWAY_CANC_DT' 'GT_FINAL' '10' ''.
      PERFORM fill_fieldcatalog USING 'Error description'(103) 'EWAY_ERROR' 'GT_FINAL' '50' ''.
      PERFORM fill_fieldcatalog USING 'Created By'(104)   'EWAY_ERNAM' 'GT_FINAL' '12' ''.
      PERFORM fill_fieldcatalog USING 'Created Date'(105) 'EWAY_ERDAT' 'GT_FINAL' '10' ''.
      PERFORM fill_fieldcatalog USING 'Created Time'(106) 'EWAY_ERZET' 'GT_FINAL' '8' ''.

    ENDIF.

  ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0707   text
*      -->P_0708   text
*      -->P_0709   text
*      -->P_0710   text
*----------------------------------------------------------------------*
  FORM fill_fieldcatalog  USING    VALUE(p_0707)
        VALUE(p_0708)
        VALUE(p_0709)
        VALUE(p_0710)
        VALUE(p_0711).


    DATA: lw_fcat TYPE lvc_s_fcat.

    CLEAR: lw_fcat.
    lw_fcat-reptext    = p_0707.
    lw_fcat-fieldname  = p_0708.
    lw_fcat-ref_table  = p_0709.
    lw_fcat-outputlen  = p_0710.
    lw_fcat-KEY        = p_0711.
    IF  lw_fcat-fieldname  = 'ICON' OR
    lw_fcat-fieldname  = 'EWAY_ICON'.
      lw_fcat-ICON = 'X'.
    ENDIF.
    IF lw_fcat-fieldname EQ 'INVVAL' OR
    lw_fcat-fieldname EQ 'TAXAMT' OR
    lw_fcat-fieldname EQ 'CGST_AMT' OR
    lw_fcat-fieldname EQ 'SGST_AMT' OR
    lw_fcat-fieldname EQ 'IGST_AMT' OR
    lw_fcat-fieldname EQ 'CESS_AMT'.

      lw_fcat-do_sum = 'X'.
    ENDIF.

    IF  lw_fcat-fieldname  = 'VBELN'.
      lw_fcat-HOTSPOT = 'X'.
    ENDIF.

    IF lw_fcat-fieldname  = 'ERZET' OR
    lw_fcat-fieldname  = 'EWAY_ERZET'.
      lw_fcat-edit_mask = '__:__:__'.

    ENDIF.


    APPEND lw_fcat TO gt_fieldcat.

  ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE status_9001 OUTPUT.
    SET PF-STATUS '9001_STATUS'.
    SET TITLEBAR '9001_TITLE'.

    PERFORM set_distance.
    PERFORM set_port_det.


  ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  SET_DISTANCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM set_distance .
    DATA:lw_index LIKE LINE OF gt_index,
          lv_val   TYPE xfeld.

    FIELD-SYMBOLS <gw_final>  TYPE ty_final.

    PERFORM check_selected_data CHANGING lv_val.
    IF lv_val IS INITIAL.
      LOOP AT  gt_index INTO lw_index.
        READ TABLE gt_final ASSIGNING <gw_final> INDEX lw_index-INDEX.
        IF sy-subrc IS INITIAL AND <gw_final>-t_distance IS INITIAL.
          IF <gw_final>-t_distance IS NOT INITIAL.
            zst_einv_api_struct-transportation_distance = <gw_final>-t_distance.
          ENDIF.
        ENDIF.
        zst_einv_api_struct-transporter_id              = <gw_final>-t_id.
        zst_einv_api_struct-transporter_name            = <gw_final>-t_name.

        zst_einv_api_struct-transportation_mode         = <gw_final>-t_mode.
        zst_einv_api_struct-transportation_distance     = <gw_final>-t_distance.
        zst_einv_api_struct-transporter_document_number = <gw_final>-t_doc_no.

        CONCATENATE <gw_final>-t_date+6(4)
        <gw_final>-t_date+3(2)
        <gw_final>-t_date+0(2)
        INTO zst_einv_api_struct-transporter_document_date.

        zst_einv_api_struct-vehicle_number              = <gw_final>-v_number.


        zst_einv_api_struct-vehicle_type              = <gw_final>-v_type.
        IF zst_einv_api_struct-vehicle_type IS INITIAL.
          zst_einv_api_struct-vehicle_type = gc_v_type_1.
        ENDIF.
        zst_einv_api_struct-reason_code_for_vehicle_updati             = <gw_final>-v_reason_code.
        zst_einv_api_struct-reason_for_vehicle_updation            = <gw_final>-v_reason.
        zst_einv_api_struct-remaining_distance        = <gw_final>-t_r_distance.
        zst_einv_api_struct-extend_validity_reason    = <gw_final>-t_ext_valid_reason.
        zst_einv_api_struct-extend_remarks            = <gw_final>-t_ext_valid_remarks.
        zst_einv_api_struct-from_pincode              = <gw_final>-t_from_pin.
        zst_einv_api_struct-consignment_status        = <gw_final>-t_consignment_status.
        zst_einv_api_struct-transit_type              = <gw_final>-t_transit_type.
        zst_einv_api_struct-address_line1             = <gw_final>-t_address1.
        zst_einv_api_struct-address_line2             = <gw_final>-t_address2.
        zst_einv_api_struct-address_line3             = <gw_final>-t_address3.
        zst_einv_api_struct-zport_code                     = <gw_final>-zport_code.
        zst_einv_api_struct-consignor_place                     = <gw_final>-consignor_place.
        zst_einv_api_struct-consignor_state                = <gw_final>-consignor_state.

      ENDLOOP.

    ENDIF.
  ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SET_PORT_DET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM set_port_det .

    DATA:lt_list TYPE vrm_values,
          lw_list TYPE vrm_value,
          lt_port TYPE TABLE OF ty_export,
          ls_port TYPE ty_export.

    CLEAR:lt_list,lt_port.

    SELECT zport_code  zport land1 zport_state
    zport_address1 zport_address2
    zport_place zport_pincode
    FROM ztedoc_export INTO TABLE lt_port WHERE land1 = gc_land1_in..

    IF sy-subrc IS INITIAL.
      LOOP AT lt_port INTO ls_port.
        lw_list-KEY = ls_port-zport_code.
        lw_list-TEXT = ls_port-zport.
        APPEND lw_list TO lt_list.
      ENDLOOP.

      CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        ID              = 'ZST_EINV_API_STRUCT-ZPORT_CODE'
        values          = lt_list
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.

    ENDIF.

  ENDFORM.
