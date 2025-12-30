"Name: \PR:SAPLV60A\FO:USEREXIT_NUMBER_RANGE\SE:END\EI
ENHANCEMENT 0 ZENH_EINV_VF11_VALIDATION.
IF sy-tcode EQ 'VF11'.
  READ TABLE xvbrk INDEX 1 INTO DATA(wa_vbrk).
  IF sy-subrc EQ 0.
    DATA : wa_inv TYPE j_1ig_invrefnum,
          wa_eway TYPE J_1IG_EWAYBILL.
    SELECT SINGLE * FROM j_1ig_invrefnum
    INTO wa_inv WHERE bukrs = wa_vbrk-bukrs
    AND docno = wa_vbrk-vbeln
    AND ODN = wa_vbrk-xblnr
    AND irn_status = 'ACT'.
    IF sy-subrc EQ 0.
      MESSAGE 'Please Cancel related E-invoice first.' TYPE 'E'.
    ENDIF.

    SELECT SINGLE * FROM J_1IG_EWAYBILL
    INTO wa_eway WHERE bukrs = wa_vbrk-bukrs
    AND docno = wa_vbrk-vbeln
    AND status = 'A'.
    IF sy-subrc EQ 0.
      MESSAGE 'Please Cancel related E-waY Bill As well.' TYPE 'E'.
    ENDIF.
  ENDIF.
ENDIF.
ENDENHANCEMENT.
