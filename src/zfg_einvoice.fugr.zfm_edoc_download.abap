FUNCTION ZFM_EDOC_DOWNLOAD.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_DATA) TYPE  STRING
*"----------------------------------------------------------------------

  DATA : lv_file TYPE string.
  DATA : lt_file TYPE rlgrap-filename.
  DATA: file_name TYPE string.

  DATA: lv_filename TYPE string.
  DATA: lv_path     TYPE string,
        lv_result   TYPE I,
        lv_fullpath TYPE string.

  DATA : lv_table TYPE TABLE OF string WITH HEADER LINE INITIAL SIZE 0.

  APPEND im_data TO lv_table.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
  EXPORTING
    window_title      = 'File Directory'
    default_extension = ' '
  CHANGING
    filename          = lv_filename
    path              = lv_path
    fullpath          = lv_fullpath
    user_action       = lv_result.

  lv_file = lv_fullpath.          "STRING CONVERSION

  lv_filename = lv_fullpath.
  file_name = lv_filename.

*Download  file
  CALL METHOD cl_gui_frontend_services=>gui_download
  EXPORTING
    filename                = file_name
    filetype                = 'ASC'
  CHANGING
    data_tab                = lv_table[]
*     fieldnames              = lt_fieldname
  EXCEPTIONS
    file_write_error        = 1
    no_batch                = 2
    gui_refuse_filetransfer = 3
    invalid_type            = 4
    no_authority            = 5
    unknown_error           = 6
    header_not_allowed      = 7
    separator_not_allowed   = 8
    filesize_not_allowed    = 9
    header_too_long         = 10
    dp_error_create         = 11
    dp_error_send           = 12
    dp_error_write          = 13
    unknown_dp_error        = 14
    access_denied           = 15
    dp_out_of_memory        = 16
    disk_full               = 17
    dp_timeout              = 18
    file_not_found          = 19
    dataprovider_exception  = 20
    control_flush_error     = 21
    not_supported_by_gui    = 22
    error_no_gui            = 23
    OTHERS                  = 24.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.



ENDFUNCTION.
