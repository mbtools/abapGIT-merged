************************************************************************
* ZABAPGIT_INSTALL_ENCODE
************************************************************************

REPORT ZABAPGIT_INSTALL_ENCODE.

CONSTANTS:
  c_source   TYPE program VALUE 'ZABAPGIT_FULL',
  c_target   TYPE program VALUE 'ZABAPGIT_INSTALLER',
  c_linesize TYPE i VALUE 253.

TYPES:
  BEGIN OF ty_code,
    code TYPE c LENGTH 255,
  END OF ty_code,
  BEGIN OF ty_code_tmp,
    code TYPE c LENGTH 253,
  END OF ty_code_tmp.

DATA:
  gt_code_in  TYPE TABLE OF ty_code,
  gt_code_tmp TYPE TABLE OF ty_code_tmp,
  gt_code_out TYPE TABLE OF ty_code,
  gr_zip      TYPE REF TO cl_abap_zip,
  gv_data     TYPE xstring,
  gv_zip      TYPE xstring,
  gv_more     TYPE i,
  gv_line     TYPE ty_code,
  gv_code     TYPE string.

START-OF-SELECTION.

  READ REPORT c_source INTO gt_code_in.
  IF sy-subrc <> 0.
    MESSAGE e666(01) WITH 'Program not found' c_source.
    RETURN.
  ENDIF.

  CALL FUNCTION 'SCMS_TEXT_TO_XSTRING'
    IMPORTING
      buffer   = gv_data
    TABLES
      text_tab = gt_code_in
    EXCEPTIONS
      failed   = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
    MESSAGE e666(01) WITH 'Error converting data'.
    RETURN.
  ENDIF.

  FREE gt_code_in.

* Add to ZIP file
  CREATE OBJECT gr_zip.

  gr_zip->add( name = 'N' content = gv_data ).

  FREE gv_data.

  gv_zip = gr_zip->save( ).

  gv_more = xstrlen( gv_zip ) MOD 4.
  WHILE gv_more > 0.
    gv_zip = gv_zip && '0x00'.
    gv_more = gv_more - 1.
  ENDWHILE.

  PERFORM z85_encode USING gv_zip CHANGING gv_code.

  CALL FUNCTION 'SCMS_STRING_TO_FTEXT'
    EXPORTING
      text      = gv_code
    TABLES
      ftext_tab = gt_code_tmp.

  READ REPORT c_target INTO gt_code_out.
  IF sy-subrc <> 0.
    MESSAGE e666(01) WITH 'Program not found' c_target.
    RETURN.
  ENDIF.

  LOOP AT gt_code_tmp INTO gv_line-code.
    gv_line-code = '*' && gv_line-code.
    APPEND gv_line TO gt_code_out.
  ENDLOOP.

  FREE gt_code_tmp.

*  BREAK-POINT.

  INSERT REPORT c_target FROM gt_code_out.

FORM z85_encode USING data TYPE xstring CHANGING encoded TYPE string.

  CONSTANTS:
    " Maps base 256 to base 85
    encoder TYPE c LENGTH 85 VALUE '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.-:+=^!/*?&<>()[]{}@%$#'.

  DATA:
    data_size    TYPE i,
    encoded_size TYPE i,
    char_nbr     TYPE i,
    byte_nbr     TYPE i,
    value        TYPE int8,
    divisor      TYPE i,
    pos          TYPE i.

  " Accepts only byte arrays bounded to 4 bytes
  data_size = xstrlen( data ).
  IF ( data_size MOD 4 ) <> 0.
    RETURN.
  ENDIF.

  CLEAR encoded.
  encoded_size = data_size * 5 / 4.
  char_nbr = 0.
  byte_nbr = 0.
  value = 0.
  WHILE ( byte_nbr < data_size ).
    " Accumulate value in base 256 (binary)
    value = value * 256 + data+byte_nbr(1).
    byte_nbr = byte_nbr + 1.
    IF ( byte_nbr MOD 4 = 0 ).
      " Output value in base 85
      divisor = 85 * 85 * 85 * 85.
      WHILE ( divisor > 0 ).
        pos =  value DIV divisor MOD 85.
        encoded = encoded && encoder+pos(1).
        char_nbr = char_nbr + 1.
        divisor = divisor DIV 85.
      ENDWHILE.
      value = 0.
    ENDIF.
  ENDWHILE.
  ASSERT char_nbr =  encoded_size.

ENDFORM.
