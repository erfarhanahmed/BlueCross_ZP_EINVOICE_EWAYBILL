FUNCTION ZFM_EWAY_BILL_VALIDATION.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_UCOMM) TYPE  SY-UCOMM
*"     REFERENCE(IM_API_HEADER) TYPE  ZTT_EWAY_API_STRUCT OPTIONAL
*"     REFERENCE(IM_API_ITEM) TYPE  ZTT_EWAY_API_STRUCT_ITM OPTIONAL
*"     REFERENCE(IM_API_TRANSPORT) TYPE  ZTT_EWAY_BILL_UPDAT_TRN_ID_STR
*"       OPTIONAL
*"     REFERENCE(IM_API_VEHICLE) TYPE  ZTT_EWAY_BILL_UPDAT_VEHNO_STR
*"       OPTIONAL
*"     REFERENCE(IM_API_EXTEND) TYPE  ZTT_EWAY_BILL_EXT_VALID_API
*"       OPTIONAL
*"  EXPORTING
*"     REFERENCE(EX_ERROR) TYPE  XFELD
*"----------------------------------------------------------------------
DATA:lt_show_message TYPE esp1_message_tab_type,
      lw_show_message LIKE LINE OF lt_show_message.
DATA:wa_header    TYPE zst_eway_api_struct,
      wa_transport TYPE zst_eway_bill_updat_trn_id_str,
      wa_vehicle   TYPE zst_eway_bill_updat_vehno_str,
      wa_extend    TYPE zst_eway_bill_ext_valid_api.

CLEAR: lt_show_message[].
CASE im_ucomm.

************** E-Way Bill generation Part (A and B) & Part A
WHEN 'EEAYG' OR 'EEAYB'.
  LOOP AT im_api_header INTO wa_header.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_header-transporter_id WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_header-document_number WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_header-transporter_name WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_header-transporter_document_number WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_header-transporter_document_date WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_header-transportation_mode WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_header-vehicle_number WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_header-vehicle_type WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_header-pincode_of_consignor WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_header-state_of_consignor WITH ' '.
    IF im_ucomm = 'EEAYB'.
      IF wa_header-transporter_id IS INITIAL.
        lw_show_message-msgid =  gc_msgid_01.
        lw_show_message-msgty =  gc_mtype_e.
        lw_show_message-msgno =  gc_msgno_319.
        lw_show_message-msgv1 =  'Transporter ID  can not be null'(001).
        lw_show_message-msgv2 =  'for document No'(003).
        lw_show_message-msgv3 =  wa_header-document_number.
        APPEND lw_show_message TO lt_show_message.
      ENDIF.

      IF wa_header-transporter_name IS INITIAL.
        lw_show_message-msgid =  gc_msgid_01.
        lw_show_message-msgty =  gc_mtype_e.
        lw_show_message-msgno =  gc_msgno_319.
        lw_show_message-msgv1 =  'Transporter Name  can not be null'(002).
        lw_show_message-msgv2 =  'for document No'(003).
        lw_show_message-msgv3 =  wa_header-document_number.
        APPEND lw_show_message TO lt_show_message.
      ENDIF.


    ENDIF.



********************   Generating E-way bill Part A
    IF im_ucomm = 'EEAYG'.

      IF wa_header-transportation_mode IS INITIAL.
        lw_show_message-msgid =  gc_msgid_01.
        lw_show_message-msgty =  gc_mtype_e.
        lw_show_message-msgno =  gc_msgno_319.
        lw_show_message-msgv1 =  'Transporter Mode can not be null'(004).
        lw_show_message-msgv2 =  'for document No'(003).
        lw_show_message-msgv3 =  wa_header-document_number.
        APPEND lw_show_message TO lt_show_message.
      ELSE.
        IF wa_header-transportation_mode = 'ROAD'.

          IF wa_header-vehicle_number IS INITIAL.
            lw_show_message-msgid =  gc_msgid_01.
            lw_show_message-msgty =  gc_mtype_e.
            lw_show_message-msgno =  gc_msgno_319.
            lw_show_message-msgv1 =  'Vehicle Number can not be null'(005).
            lw_show_message-msgv2 =  'for document No'(003).
            lw_show_message-msgv3 =  wa_header-document_number.
            APPEND lw_show_message TO lt_show_message.
          ENDIF.
          IF wa_header-vehicle_type IS INITIAL.
            lw_show_message-msgid =  gc_msgid_01.
            lw_show_message-msgty =  gc_mtype_e.
            lw_show_message-msgno =  gc_msgno_319.
            lw_show_message-msgv1 =  'Vehicle type Date can not be null'(006).
            lw_show_message-msgv2 =  'for document No'(003).
            lw_show_message-msgv3 =  wa_header-document_number.
            APPEND lw_show_message TO lt_show_message.
          ENDIF.

        ELSE.

          IF wa_header-transporter_document_number IS INITIAL.
            lw_show_message-msgid =  gc_msgid_01.
            lw_show_message-msgty =  gc_mtype_e.
            lw_show_message-msgno =  gc_msgno_319.
            lw_show_message-msgv1 =  'Transporter Document Number can not be null'(007).
            lw_show_message-msgv2 =  'for document No'(003).
            lw_show_message-msgv3 =  wa_header-document_number.
            APPEND lw_show_message TO lt_show_message.
          ENDIF.
          IF wa_header-transporter_document_date IS INITIAL.
            lw_show_message-msgid =  gc_msgid_01.
            lw_show_message-msgty =  gc_mtype_e.
            lw_show_message-msgno =  gc_msgno_319.
            lw_show_message-msgv1 =  'Transporter Document Date can not be null'(008).
            lw_show_message-msgv2 =  'for document No'(003).
            lw_show_message-msgv3 =  wa_header-document_number.
            APPEND lw_show_message TO lt_show_message.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.
  ENDLOOP.

**************  Update E-Way Bill Transporter
WHEN 'EWAYU'.
  LOOP AT im_api_transport INTO wa_transport.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_transport-transporter_id WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_transport-transporter_name WITH ' '.
    IF wa_transport-transporter_id IS INITIAL.
      lw_show_message-msgid =  gc_msgid_01.
      lw_show_message-msgty =  gc_mtype_e.
      lw_show_message-msgno =  gc_msgno_319.
      lw_show_message-msgv1 =  'Transporter ID  can not be null'(001).
      lw_show_message-msgv2 =  'for E-Way Bill No'(009).
      lw_show_message-msgv3 =  wa_transport-ebillno.
      APPEND lw_show_message TO lt_show_message.
    ENDIF.
    IF wa_transport-transporter_name IS INITIAL.
      lw_show_message-msgid =  gc_msgid_01.
      lw_show_message-msgty =  gc_mtype_e.
      lw_show_message-msgno =  gc_msgno_319.
      lw_show_message-msgv1 =  'Transporter Name  can not be null'(002).
      lw_show_message-msgv2 =  'for E-Way Bill No'(009).
      lw_show_message-msgv3 =   wa_transport-ebillno.
      APPEND lw_show_message TO lt_show_message.
    ENDIF.

  ENDLOOP.

***************** E-Way Bill Vehicle Number

WHEN 'EWAYV'.

  LOOP AT im_api_vehicle INTO wa_vehicle.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_vehicle-vehicle_number WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_vehicle-vehicle_type WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_vehicle-reason_code_for_vehicle_updati WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_vehicle-reason_for_vehicle_updation WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_vehicle-transporter_document_number WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_vehicle-transporter_document_date WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_vehicle-transportation_mode WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_vehicle-reason_code_for_vehicle_updati WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_vehicle-reason_for_vehicle_updation WITH ' '.

    IF wa_vehicle-vehicle_number IS INITIAL.
      lw_show_message-msgid =  gc_msgid_01.
      lw_show_message-msgty =  gc_mtype_e.
      lw_show_message-msgno =  gc_msgno_319.
      lw_show_message-msgv1 =  'Vehicle Number can not be null'(005).
      lw_show_message-msgv2 =  'for E-Way Bill No'(009).
      lw_show_message-msgv3 =  wa_vehicle-ebillno.
      APPEND lw_show_message TO lt_show_message.
    ENDIF.
    IF wa_vehicle-vehicle_type IS INITIAL.
      lw_show_message-msgid =  gc_msgid_01.
      lw_show_message-msgty =  gc_mtype_e.
      lw_show_message-msgno =  '319'.
      lw_show_message-msgv1 =  'Vehicle Type can not be null'(006).
      lw_show_message-msgv2 =  'for E-Way Bill No'(009).
      lw_show_message-msgv3 =  wa_vehicle-ebillno.
      APPEND lw_show_message TO lt_show_message.
    ENDIF.
    IF wa_vehicle-reason_code_for_vehicle_updati IS INITIAL.
      lw_show_message-msgid =  gc_msgid_01.
      lw_show_message-msgty =  gc_mtype_e.
      lw_show_message-msgno =  gc_msgno_319.
      lw_show_message-msgv1 =  'Vehicle Change Reason can not be null'(010).
      lw_show_message-msgv2 =  'for E-Way Bill No'(009).
      lw_show_message-msgv3 =  wa_vehicle-ebillno.
      APPEND lw_show_message TO lt_show_message.
    ENDIF.

    IF wa_vehicle-reason_code_for_vehicle_updati = '3'.
      IF wa_vehicle-reason_for_vehicle_updation IS INITIAL.
        lw_show_message-msgid =  gc_msgid_01.
        lw_show_message-msgty =  gc_mtype_e.
        lw_show_message-msgno =  gc_msgno_319.
        lw_show_message-msgv1 =  'Remarks can not be null'(011).
        lw_show_message-msgv2 =  'for E-Way Bill No'(009).
        lw_show_message-msgv3 =  wa_vehicle-ebillno.
        APPEND lw_show_message TO lt_show_message.
      ENDIF.
    ENDIF.

    IF wa_vehicle-transportation_mode IS INITIAL.
      lw_show_message-msgid =  gc_msgid_01.
      lw_show_message-msgty =  gc_mtype_e.
      lw_show_message-msgno =  gc_msgno_319.
      lw_show_message-msgv1 =  'Transportation Mode can not be null'(004).
      lw_show_message-msgv2 =  'for E-Way Bill No'(009).
      lw_show_message-msgv3 =   wa_vehicle-ebillno.
      APPEND lw_show_message TO lt_show_message.
    ENDIF.

    IF wa_vehicle-transportation_mode NE 'ROAD'.
      IF wa_vehicle-transporter_document_number IS INITIAL.
        lw_show_message-msgid =  gc_msgid_01.
        lw_show_message-msgty =  gc_mtype_e.
        lw_show_message-msgno =  gc_msgno_319.
        lw_show_message-msgv1 =  'Transporter Document Number can not be null'(007).
        lw_show_message-msgv2 =  'for E-Way Bill No'(009).
        lw_show_message-msgv3 =  wa_vehicle-ebillno.
        APPEND lw_show_message TO lt_show_message.
      ENDIF.
      IF wa_vehicle-transporter_document_date IS INITIAL.
        lw_show_message-msgid =  gc_msgid_01.
        lw_show_message-msgty =  gc_mtype_e.
        lw_show_message-msgno =  gc_msgno_319.
        lw_show_message-msgv1 =  'Transporter document Date can not be null'(008).
        lw_show_message-msgv2 =  'for E-Way Bill No'(009).
        lw_show_message-msgv3 =  wa_vehicle-ebillno.
        APPEND lw_show_message TO lt_show_message.
      ENDIF.
    ENDIF.

  ENDLOOP.

WHEN 'EWAYE'.
**************Extend E-Way Bill Validity
  LOOP AT im_api_extend INTO wa_extend.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_extend-ebillno WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_extend-vehicle_number WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_extend-transporter_document_number WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_extend-transporter_document_date WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_extend-transportation_mode WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_extend-extend_validity_reason WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_extend-extend_remarks WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_extend-from_pincode WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_extend-consignment_status WITH ' '.
    REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN wa_extend-remaining_distance WITH ' '.


    IF wa_extend-vehicle_number IS INITIAL.
      lw_show_message-msgid =  gc_msgid_01.
      lw_show_message-msgty =  gc_mtype_e.
      lw_show_message-msgno =  gc_msgno_319.
      lw_show_message-msgv1 =  'Vehicle Number can not be null'(005).
      lw_show_message-msgv2 =  'for E-Way Bill No'(009).
      lw_show_message-msgv3 =  wa_extend-ebillno.
      APPEND lw_show_message TO lt_show_message.
    ENDIF.

    IF wa_extend-transportation_mode IS INITIAL.
      lw_show_message-msgid =  gc_msgid_01.
      lw_show_message-msgty =  gc_mtype_e.
      lw_show_message-msgno =  gc_msgno_319.
      lw_show_message-msgv1 =  'Transportation Mode can not be null'(004).
      lw_show_message-msgv2 =  'for E-Way Bill No'(009).
      lw_show_message-msgv3 =  wa_extend-ebillno.
      APPEND lw_show_message TO lt_show_message.
    ENDIF.
    IF wa_extend-transportation_mode NE 'ROAD'.
      IF wa_extend-transporter_document_number IS INITIAL.
        lw_show_message-msgid =  gc_msgid_01.
        lw_show_message-msgty =  gc_mtype_e.
        lw_show_message-msgno =  gc_msgno_319.
        lw_show_message-msgv1 =  'Transporter Document Number can not be null'(007).
        lw_show_message-msgv2 =  'for E-Way Bill No'(009).
        lw_show_message-msgv3 =  wa_extend-ebillno.
        APPEND lw_show_message TO lt_show_message.
      ENDIF.
      IF wa_extend-transporter_document_date IS INITIAL.
        lw_show_message-msgid =  gc_msgid_01.
        lw_show_message-msgty =  gc_mtype_e.
        lw_show_message-msgno =  gc_msgno_319.
        lw_show_message-msgv1 =  'Transporter Document date can not be null'(008).
        lw_show_message-msgv2 =  'for E-Way Bill No'(009).
        lw_show_message-msgv3 =  wa_extend-ebillno.
        APPEND lw_show_message TO lt_show_message.
      ENDIF.
    ENDIF.

    IF  wa_extend-extend_validity_reason IS INITIAL.
      lw_show_message-msgid =  gc_msgid_01.
      lw_show_message-msgty =  gc_mtype_e.
      lw_show_message-msgno =  gc_msgno_319.
      lw_show_message-msgv1 =  'Extension Reason can not be null'(012).
      lw_show_message-msgv2 =  'for E-Way Bill No'(009).
      lw_show_message-msgv3 =  wa_extend-ebillno.
      APPEND lw_show_message TO lt_show_message.
    ENDIF.
    IF wa_extend-extend_remarks IS INITIAL.
      lw_show_message-msgid =  gc_msgid_01.
      lw_show_message-msgty =  gc_mtype_e.
      lw_show_message-msgno =  gc_msgno_319.
      lw_show_message-msgv1 =  'Remarks can not be null'(011).
      lw_show_message-msgv2 =  'for E-Way Bill No'(009).
      lw_show_message-msgv3 =  wa_extend-ebillno.
      APPEND lw_show_message TO lt_show_message.
    ENDIF.
    IF wa_extend-remaining_distance IS INITIAL.
      lw_show_message-msgid =  gc_msgid_01.
      lw_show_message-msgty =  gc_mtype_e.
      lw_show_message-msgno =  gc_msgno_319.
      lw_show_message-msgv1 =  'Remaining Distance can not be null'(013).
      lw_show_message-msgv2 =  'for E-Way Bill No'(009).
      lw_show_message-msgv3 =  wa_extend-ebillno.
      APPEND lw_show_message TO lt_show_message.
    ENDIF.

    IF wa_extend-from_pincode IS INITIAL.
      lw_show_message-msgid =  gc_msgid_01.
      lw_show_message-msgty =  gc_mtype_e.
      lw_show_message-msgno =  gc_msgno_319.
      lw_show_message-msgv1 =  'From Pincode can not be null'(014).
      lw_show_message-msgv2 =  'for E-Way Bill No'(009).
      lw_show_message-msgv3 =  wa_extend-ebillno.
      APPEND lw_show_message TO lt_show_message.
    ENDIF.
    IF wa_extend-consignment_status IS INITIAL.
      lw_show_message-msgid =  gc_msgid_01.
      lw_show_message-msgty =  gc_mtype_e.
      lw_show_message-msgno =  gc_msgno_319.
      lw_show_message-msgv1 =  'Consignment Status can not be null'(015).
      lw_show_message-msgv2 =  'for E-Way Bill No'(009).
      lw_show_message-msgv3 =  wa_extend-ebillno.
      APPEND lw_show_message TO lt_show_message.
    ENDIF.
    IF wa_extend-transportation_mode = 'INTRANSIT'.
      IF wa_extend-transit_type IS INITIAL.
        lw_show_message-msgid =  gc_msgid_01.
        lw_show_message-msgty =  gc_mtype_e.
        lw_show_message-msgno =  gc_msgno_319.
        lw_show_message-msgv1 =  'Transit Type can not be null'(016).
        lw_show_message-msgv2 =  'for E-Way Bill No'(009).
        lw_show_message-msgv3 =  wa_extend-ebillno.
        APPEND lw_show_message TO lt_show_message.
      ENDIF.

    ENDIF.

  ENDLOOP.
ENDCASE.

IF lt_show_message IS NOT INITIAL.
  CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
  TABLES
    i_message_tab = lt_show_message.
  ex_error = abap_true.
ELSE.
  ex_error = ' '.
ENDIF.


ENDFUNCTION.
