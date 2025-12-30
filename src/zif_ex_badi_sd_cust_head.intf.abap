interface ZIF_EX_BADI_SD_CUST_HEAD
  public .


  interfaces IF_BADI_INTERFACE .

  methods ACTIVATE_TAB_PAGE
    importing
      !FVBRK type VBRK
      !FVBUK type VBUK
      !FXVBPA type VA_VBPAVB_T
    exporting
      !FCAPTION type CHAR40
      !FPROGRAM type SYREPID
      !FDYNPRO type SYDYNNR
      !FTAB type CHAR40 .
  methods CHECK_BADI_ACTIVATE
    returning
      value(BADI_ACTIVATE) type SAP_BOOL .
  methods TRANSFER_DATA_TO_SUBSCREEN
    importing
      !F_VBRK type VBRK optional
      !FT180 type T180
    changing
      !FXVBPA type VA_VBPAVB_T
      !FXYVBRK type VBRKVB_T
      !FXVBRK type VBRKVB_T
      !FRV60A type RV60A
      !FXYVBADR type SHP_SADRVB_T
      !FXVBADR type SHP_SADRVB_T
      !FXYVBPA type VA_VBPAVB_T
      !FXVBRP type VBRPVB_T
      !FXYVBRP type VBRPVB_T
      !FVBRK type VBRK .
  methods TRANSFER_DATA_FROM_SUBSCREEN
    importing
      !FT180 type T180
    changing
      !FVBRK type VBRK optional
      !FXVBPA type VA_VBPAVB_T
      !FXYVBRK type VBRKVB_T
      !FXVBRK type VBRKVB_T
      !FRV60A type RV60A
      !FXYVBADR type SHP_SADRVB_T
      !FXVBADR type SHP_SADRVB_T
      !FXYVBPA type VA_VBPAVB_T
      !FXVBRP type VBRPVB_T
      !FXYVBRP type VBRPVB_T
      !FFCODE type FCODE optional .
  methods PASS_FCODE_TO_SUBSCREEN
    importing
      !FFCODE_SAME_PAGE type FCODE default 'ENT1'
      !FFCODE type FCODE .
endinterface.
