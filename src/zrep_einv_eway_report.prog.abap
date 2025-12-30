*&---------------------------------------------------------------------*
*& Report ZREP_EINV_EWAY_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZREP_EINV_EWAY_REPORT.
* Type Pool declartion
TYPE-POOLS: ICON.


* Includes
INCLUDE zrep_einv_eway_report_top.
INCLUDE zrep_einv_eway_report_class.
INCLUDE zrep_einv_eway_report_sel.
INCLUDE zrep_einv_eway_report_forms.

* Initialization
INITIALIZATION.

* Fetch User configuration based on selection
PERFORM fetch_user_config.

* Fetch all domain Values
PERFORM fetch_domain_values.


AT SELECTION-SCREEN OUTPUT.

* Selection screen comments
WRITE icon_message_information AS ICON TO ICON.
WRITE icon_message_information AS ICON TO icon1.
WRITE icon_message_information AS ICON TO icon2.
WRITE icon_message_information AS ICON TO icon3.
PERFORM set_module_value.


* Start of selection
START-OF-SELECTION.

* Get all required data
PERFORM get_data.

* If data available to dosplay call screen
IF gt_final IS NOT INITIAL.
  SORT gt_final BY vbeln DESCENDING.
  CALL SCREEN 9000.
ELSE.

* Message
  MESSAGE 'No data for given selection'(m01) TYPE 'I'.
  LEAVE LIST-PROCESSING.

ENDIF.


* Includes
INCLUDE zrep_einv_eway_report_statuo01.

INCLUDE zrep_einv_eway_report_user_i01.

*INCLUDE zrep_einv_eway_report_statuo01.
*
*INCLUDE zrep_einv_eway_report_user_i01.
