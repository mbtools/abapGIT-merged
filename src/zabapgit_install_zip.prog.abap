************************************************************************
* ZABAPGIT_INSTALL_ZIP
* abapGIT Installer for ZIP of merged program
************************************************************************

REPORT zabapgit_install_zip.

* If we were to use $ABAPGIT, the uninstalling the abapGIT project
* would also delete the stand-alone program ZABAPGIT_FULL (but for that
* we have the installer )
DATA:
  gr_zip         TYPE REF TO cl_abap_zip,
  g_zip_file     TYPE string   VALUE 'C:\Temp\zabapgit_latest.zip',
  g_zip_url      TYPE string   VALUE 'https://github.com/larshp/abapGit/archive/master.zip',
  g_abapgit_file TYPE string   VALUE 'zabapgit.abap',
  g_abapgit_devc TYPE devclass VALUE '$ABAPGIT_MERGED',
  g_abapgit_prog TYPE progname VALUE 'ZABAPGIT_FULL',
  g_abapgit_tran TYPE tcode    VALUE 'ZAG',
  g_abapgit_name TYPE repti    VALUE 'abapGIT (merged)',
  g_korrnum      TYPE korrnum.

PERFORM install_zip
  USING g_zip_file.

PERFORM install_program
  USING g_abapgit_devc g_abapgit_prog g_abapgit_name g_abapgit_file.

PERFORM install_transaction
  USING g_abapgit_devc g_abapgit_prog g_abapgit_name g_abapgit_tran.

*----------------------------------------------------------------------*

FORM install_zip USING i_filename.

  DATA:
    l_subrc      TYPE sy-subrc,
    l_filename   TYPE string,
    l_filelength TYPE i,
    l_zip        TYPE xstring,
    lt_zip       TYPE STANDARD TABLE OF ssfbin.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filetype                = 'BIN'
      filename                = i_filename
    IMPORTING
      filelength              = l_filelength
    TABLES
      data_tab                = lt_zip
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.
  IF sy-subrc = 0.
    WRITE: / 'Package successfully uploaded'.
  ELSE.
    WRITE: / 'Error uploading package', l_filename.
    RETURN.
  ENDIF.

  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      input_length = l_filelength
    IMPORTING
      buffer       = l_zip
    TABLES
      binary_tab   = lt_zip
    EXCEPTIONS
      failed       = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    WRITE: / 'Error reading package', l_filename.
    RETURN.
  ENDIF.

  TRY.
      CREATE OBJECT gr_zip.
      gr_zip->load( l_zip ).
    CATCH cx_root.
      WRITE: / 'Error unzipping package', l_filename.
      RETURN.
  ENDTRY.

ENDFORM.                    "install_zip

*----------------------------------------------------------------------*

FORM install_program
  USING i_devclass i_program i_title i_filename.

  DATA:
    l_exist    TYPE abap_bool,
    l_program  TYPE progname,
    l_devclass TYPE devclass,
    ls_trdir   TYPE trdir,
    l_msg      TYPE string,
    l_word     TYPE string,
    l_line     TYPE i,
    l_text_1   TYPE sy-msgv1,
    l_text_2   TYPE sy-msgv2,
    l_function TYPE funcname,
    l_title    TYPE repti,
    l_save_ina TYPE abap_bool,
    l_r_conv   TYPE REF TO cl_abap_conv_in_ce,
    l_data     TYPE xstring,
    l_abap     TYPE string,
    lt_abap    TYPE TABLE OF abaptxt255.

  l_program = i_program.

* TRDIR entry
  SELECT SINGLE * FROM trdir INTO ls_trdir WHERE name = l_program.
  IF sy-subrc = 0.
    l_exist = abap_true.
    WRITE: / l_program, 'already exists and will be updated'.

    SELECT SINGLE devclass FROM tadir INTO l_devclass
       WHERE pgmid = 'R3TR' AND object = 'PROG'
         AND obj_name = l_program.
    IF sy-subrc <> 0.
      l_devclass = i_devclass.
    ENDIF.
  ELSE.
    l_devclass = i_devclass.
    l_exist = abap_false.
    WRITE: / l_program, 'not found and will be installed'.
  ENDIF.

  CALL FUNCTION 'RS_CORR_INSERT'
    EXPORTING
      object              = l_program
      object_class        = 'ABAP'
      mode                = 'I'
      devclass            = l_devclass
      korrnum             = g_korrnum
      author              = sy-uname
      master_language     = sy-langu
    IMPORTING
      devclass            = l_devclass
      korrnum             = g_korrnum
    EXCEPTIONS
      cancelled           = 1
      permission_failure  = 2
      unknown_objectclass = 3
      OTHERS              = 4.
  IF sy-subrc = 0.
    IF g_abapgit_devc CP '$*'.
      WRITE: / l_program, 'created as local object'.
    ELSE.
      WRITE: / l_program, 'added to transport', g_korrnum.
    ENDIF.
  ELSE.
    WRITE: / l_program, 'error creating object directory entry' COLOR COL_NEGATIVE.
    RETURN.
  ENDIF.

  CALL METHOD gr_zip->get
    EXPORTING
      name            = i_filename
    IMPORTING
      content         = l_data
    EXCEPTIONS
      zip_index_error = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    WRITE: / l_program, 'program not found in ZIP file', i_filename.
    RETURN.
  ENDIF.

  TRY.
      CALL METHOD cl_abap_conv_in_ce=>create
        EXPORTING
          input       = l_data
          encoding    = 'DEFAULT'
          replacement = '#'
          ignore_cerr = abap_false
        RECEIVING
          conv        = l_r_conv.

      CALL METHOD l_r_conv->read
        IMPORTING
          data = l_abap.

*     First try PC format instead (CRLF)
      SPLIT l_abap AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_abap.
      l_line = lines( lt_abap ).
      IF l_line < 70000.
*       Then try UNIX format (LF)
        SPLIT l_abap AT cl_abap_char_utilities=>newline INTO TABLE lt_abap.
      ENDIF.
      l_line = lines( lt_abap ).
      IF l_line < 70000.
        WRITE: / l_program, 'error during linefeed conversion' COLOR COL_NEGATIVE.
        RETURN.
      ENDIF.
    CATCH cx_root.                                       "#EC CATCH_ALL
      WRITE: / l_program, 'error during data conversion' COLOR COL_NEGATIVE.
      RETURN.
  ENDTRY.

  SYNTAX-CHECK FOR lt_abap PROGRAM l_program
    MESSAGE l_msg LINE l_line WORD l_word.
  IF sy-subrc <> 0.
    WRITE l_line TO l_text_2 LEFT-JUSTIFIED.
    CONCATENATE 'syntax error:' l_msg INTO l_text_1
      SEPARATED BY space.
    CONCATENATE 'Line:' l_text_2 'Token:' l_word INTO l_text_2
      SEPARATED BY space.
    WRITE: / l_program, l_text_1, l_text_2.

    l_save_ina = abap_true.
  ENDIF.

  l_title = i_title.

  IF l_exist = abap_true.
    l_function = 'RPY_PROGRAM_UPDATE'.

    CALL FUNCTION l_function
      EXPORTING
        program_name      = l_program
        program_type      = '1'
        title_string      = l_title
        development_class = l_devclass
        temporary         = abap_true
        save_inactive     = l_save_ina
      TABLES
        source_extended   = lt_abap
      EXCEPTIONS
        cancelled         = 1
        permission_error  = 2
        not_found         = 3
        OTHERS            = 4.
    IF sy-subrc = 0.
      WRITE: / l_program, 'updated successfully'.
    ELSE.
      WRITE: / l_program, 'error during update' COLOR COL_NEGATIVE.
    ENDIF.
  ELSE.
    l_function = 'RPY_PROGRAM_INSERT'.

    IF sy-saprl < '730'.
      CALL FUNCTION l_function
        EXPORTING
          program_name      = l_program
          program_type      = '1'
          title_string      = l_title
          development_class = l_devclass
          temporary         = abap_true
          save_inactive     = l_save_ina
        TABLES
          source_extended   = lt_abap
        EXCEPTIONS
          already_exists    = 1
          cancelled         = 2
          name_not_allowed  = 3
          permission_error  = 4
          OTHERS            = 5.
    ELSE.
      CALL FUNCTION l_function
        EXPORTING
          program_name      = l_program
          program_type      = '1'
          title_string      = l_title
          development_class = l_devclass
          temporary         = abap_true
          save_inactive     = l_save_ina
          suppress_dialog   = abap_true
        TABLES
          source_extended   = lt_abap
        EXCEPTIONS
          already_exists    = 1
          cancelled         = 2
          name_not_allowed  = 3
          permission_error  = 4
          OTHERS            = 5.
    ENDIF.
    IF sy-subrc = 0.
      WRITE: / l_program, 'successfully installed'.
    ELSE.
      WRITE: / l_program, 'error during installation:' COLOR COL_NEGATIVE.
    ENDIF.
  ENDIF.

ENDFORM.                    "install_program

FORM install_transaction
  USING i_devclass i_program i_title i_tcode.

  DATA:
    l_exist    TYPE abap_bool,
    l_tcode    TYPE tstct-tcode,
    l_title    TYPE tstct-ttext,
    l_devclass TYPE devclass,
    ls_tstcp   TYPE tstcp.

  l_tcode = i_tcode.

* TCTCP entry
  SELECT SINGLE * FROM tstcp INTO ls_tstcp WHERE tcode = l_tcode.
  IF sy-subrc = 0.
    l_exist = abap_true.
    WRITE: / l_tcode, 'already exists and will be updated'.

    SELECT SINGLE devclass FROM tadir INTO l_devclass
       WHERE pgmid = 'R3TR' AND object = 'TRAN'
         AND obj_name = l_tcode.
    IF sy-subrc <> 0.
      l_devclass = i_devclass.
    ENDIF.
  ELSE.
    l_devclass = i_devclass.
    l_exist = abap_false.
    WRITE: / l_tcode, 'not found and will be installed'.
  ENDIF.

  CALL FUNCTION 'RS_CORR_INSERT'
    EXPORTING
      object              = l_tcode
      object_class        = 'ABAP'
      mode                = 'I'
      devclass            = l_devclass
      korrnum             = g_korrnum
      author              = sy-uname
      master_language     = sy-langu
    IMPORTING
      devclass            = l_devclass
      korrnum             = g_korrnum
    EXCEPTIONS
      cancelled           = 1
      permission_failure  = 2
      unknown_objectclass = 3
      OTHERS              = 4.
  IF sy-subrc = 0.
    IF g_abapgit_devc CP '$*'.
      WRITE: / l_tcode, 'created as local object'.
    ELSE.
      WRITE: / l_tcode, 'added to transport', g_korrnum.
    ENDIF.
  ELSE.
    WRITE: / l_tcode, 'error creating object directory entry' COLOR COL_NEGATIVE.
    RETURN.
  ENDIF.

  l_title = i_title.

  IF l_exist = abap_true.
    CALL FUNCTION 'RPY_TRANSACTION_DELETE'
      EXPORTING
        transaction      = l_tcode
        transport_number = g_korrnum
      EXCEPTIONS
        not_excecuted    = 1
        object_not_found = 2
        OTHERS           = 3.
    IF sy-subrc = 0.
      WRITE: / l_tcode, 'successfully deleted'.
    ELSE.
      WRITE: / l_tcode, 'error during deletion' COLOR COL_NEGATIVE.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'RPY_TRANSACTION_INSERT'
    EXPORTING
      transaction         = l_tcode
      program             = i_program
      development_class   = l_devclass
      transport_number    = g_korrnum
      transaction_type    = 'R'
      shorttext           = l_title
    EXCEPTIONS
      cancelled           = 1
      already_exist       = 2
      permission_error    = 3
      name_not_allowed    = 4
      name_conflict       = 5
      illegal_type        = 6
      object_inconsistent = 7
      db_access_error     = 8
      OTHERS              = 9.
  IF sy-subrc = 0.
    WRITE: / l_tcode, 'successfully installed'.
  ELSE.
    WRITE: / l_tcode, 'error during installation' COLOR COL_NEGATIVE.
  ENDIF.

ENDFORM.                    "install_transaction
