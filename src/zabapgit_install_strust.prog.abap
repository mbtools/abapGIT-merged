************************************************************************
* ZABAPGIT_INSTALL_STRUST
* abapGIT Installer for STRUST certificates
************************************************************************

REPORT zabapgit_install_strust.

TYPES:
  BEGIN OF ys_certattr,
    subject     TYPE string,
    issuer      TYPE string,
    serialno    TYPE string,
    validfrom   TYPE string,
    validto     TYPE string,
    datefrom    TYPE d,
    dateto      TYPE d,
    certificate TYPE xstring,
  END OF ys_certattr.

DATA:
  g_param_value TYPE pfevalue,
  g_servssflib  TYPE i,
  g_subject     TYPE certsubjct,
  g_psepath     TYPE trfile,
  g_certfile    TYPE string,
  g_cert_own    TYPE xstring,
  g_cert_list   TYPE xstring,
  gs_cert_new   TYPE ys_certattr,
  gt_cert_new   TYPE TABLE OF ys_certattr,
  gs_cert_old   TYPE ys_certattr,
  g_psename     TYPE ssfpsename,
*  g_pseprof     TYPE localfile,
  g_profile     TYPE ssfpab,
  g_profilepw   TYPE ssfpabpw,
  g_psetext     TYPE strustappltxt,
  g_distrib     TYPE ssfflag,
  g_id          TYPE ssfid,
*  g_pselen      TYPE i,
*  gt_pse        TYPE TABLE OF ssfbin,
  g_credname    TYPE icm_credname,
  g_tempfile    TYPE localfile,
  gt_certlist   TYPE ssfbintab.

START-OF-SELECTION.

* Check some SSL parameters
  CALL FUNCTION 'SXPG_PROFILE_PARAMETER_GET'
    EXPORTING
      parameter_name  = 'ssl/pse_provider'
    IMPORTING
      parameter_value = g_param_value.

  IF g_param_value IS INITIAL.
    MESSAGE w095(trust).
  ENDIF.

  CALL FUNCTION 'SXPG_PROFILE_PARAMETER_GET'
    EXPORTING
      parameter_name  = 'ssl/ciphersuites'
    IMPORTING
      parameter_value = g_param_value.

  IF g_param_value <> '135:PFS:HIGH::EC_P256:EC_HIGH'.

  ENDIF.

  CALL FUNCTION 'SXPG_PROFILE_PARAMETER_GET'
    EXPORTING
      parameter_name  = 'ssl/client_ciphersuites'
    IMPORTING
      parameter_value = g_param_value.

  IF g_param_value <> '150:PFS:HIGH::EC_P256:EC_HIGH'.

  ENDIF.

* Check if sapcryptolib is installed
  PERFORM check_cryptolib_installed IN PROGRAM s_trustmanager
    CHANGING g_servssflib.
  IF g_servssflib = 2.     "no seculib found
    MESSAGE e029(trust).
  ELSEIF g_servssflib = 3. "no licence ticket
    MESSAGE i080(trust).
  ENDIF.

* Import certificate file (PEM base64 format)
  DO 4 TIMES.
    CASE sy-index.
      WHEN 1.
        g_certfile = 'C:\Users\Hp\OneDrive\Data MBT\! Tools\Certificates\github.com\github.pem'.
      WHEN 2.
        g_certfile = 'C:\Users\Hp\OneDrive\Data MBT\! Tools\Certificates\github.com\github_api.pem'.
      WHEN 3.
        g_certfile = 'C:\Users\Hp\OneDrive\Data MBT\! Tools\Certificates\github.com\digitrust_ca.pem'.
      WHEN 4.
        g_certfile = 'C:\Users\Hp\OneDrive\Data MBT\! Tools\Certificates\github.com\digitrust_ica.pem'.
    ENDCASE.

    CALL FUNCTION 'SSFC_CERTIFICATE_IMPORT'
      EXPORTING
        filename                   = g_certfile
        encoding                   = 'B'
      IMPORTING
        certificate                = gs_cert_new-certificate
        subject                    = gs_cert_new-subject
        issuer                     = gs_cert_new-issuer
        serialno                   = gs_cert_new-serialno
        validfrom                  = gs_cert_new-validfrom
        validto                    = gs_cert_new-validto
      EXCEPTIONS
        sapgui_required            = 1
        no_upload_authorization    = 2
        data_transmission_error    = 3
        file_open_error            = 4
        file_access_denied         = 5
        file_read_error            = 6
        unknown_error              = 7
        import_parameter_missing   = 8
        encoding_not_supported     = 9
        invalid_certificate_format = 10
        OTHERS                     = 11.
    IF sy-subrc <> 0.
      BREAK-POINT ##NO_BREAK.
      PERFORM exit.
      RETURN.
    ENDIF.
    gs_cert_new-datefrom = gs_cert_new-validfrom(8).
    gs_cert_new-dateto   = gs_cert_new-validto(8).
    APPEND gs_cert_new TO gt_cert_new.
  ENDDO.

* Get filename of Anonymous-SSL Client PSE
  CALL FUNCTION 'SSFPSE_FILENAME'
    EXPORTING
      context       = 'SSLC'
      applic        = 'ANONYM'
    IMPORTING
      psename       = g_psename
      psetext       = g_psetext
      distrib       = g_distrib
    EXCEPTIONS
      pse_not_found = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    BREAK-POINT ##NO_BREAK.
    PERFORM exit.
    RETURN.
  ENDIF.

* Anonymous-SSL is always distributed to all servers
* Just checking...
  ASSERT g_distrib = abap_true AND g_psename = 'SAPSSLA.pse'.

  WRITE: / 'Updating PSE:', g_psetext.
  ULINE.
  SKIP.

* Check if PSE exists
  CALL FUNCTION 'SSFPSE_LOAD'
    EXPORTING
      psename           = g_psename
    EXCEPTIONS
      authority_missing = 1
      database_failed   = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
*   Create new PSE (using RSA-SHA256 2048 which is the default in STRUST in recent releases)
    g_subject = g_id = 'CN=anonymous'.

    CALL FUNCTION 'SSFPSE_CREATE'
      EXPORTING
        dn                = g_subject
        alg               = 'S'
        keylen            = 2048
      IMPORTING
        psepath           = g_psepath
      EXCEPTIONS
        ssf_unknown_error = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      MESSAGE e040(trust).
    ENDIF.

    WRITE: / 'Created PSE' COLOR COL_POSITIVE.
    SKIP.

    g_tempfile = g_psepath.

    PERFORM save.
  ENDIF.

* Lock PSE
  CALL FUNCTION 'SSFPSE_ENQUEUE'
    EXPORTING
      psename         = g_psename
    EXCEPTIONS
      database_failed = 1
      foreign_lock    = 2
      internal_error  = 3
      OTHERS          = 4.
  IF sy-subrc <> 0.
    BREAK-POINT ##NO_BREAK.
    PERFORM exit.
    RETURN.
  ENDIF.

* Load PSE (generates a temporary file for it)
  CALL FUNCTION 'SSFPSE_LOAD'
    EXPORTING
      psename           = g_psename
    IMPORTING
      id                = g_id
      fname             = g_tempfile
    EXCEPTIONS
      authority_missing = 1
      database_failed   = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    BREAK-POINT ##NO_BREAK.
    PERFORM exit.
    RETURN.
  ENDIF.

  WRITE: / 'Loaded PSE' COLOR COL_POSITIVE.
  SKIP.

  g_profile = g_tempfile.

  CALL FUNCTION 'SSFC_GET_OWNCERTIFICATE'
    EXPORTING
      profile               = g_profile
      profilepw             = g_profilepw
    IMPORTING
      certificate           = g_cert_own
    EXCEPTIONS
      ssf_krn_error         = 1
      ssf_krn_nomemory      = 2
      ssf_krn_nossflib      = 3
      ssf_krn_invalid_par   = 4
      ssf_krn_nocertificate = 5
      OTHERS                = 6.
  IF sy-subrc <> 0.
    BREAK-POINT ##NO_BREAK.
    PERFORM exit.
    RETURN.
  ENDIF.

  CALL FUNCTION 'SSFC_PARSE_CERTIFICATE'
    EXPORTING
      certificate         = g_cert_own
    IMPORTING
      subject             = gs_cert_old-subject
      issuer              = gs_cert_old-issuer
      serialno            = gs_cert_old-serialno
      validfrom           = gs_cert_old-validfrom
      validto             = gs_cert_old-validto
    EXCEPTIONS
      ssf_krn_error       = 1
      ssf_krn_nomemory    = 2
      ssf_krn_nossflib    = 3
      ssf_krn_invalid_par = 4
      OTHERS              = 5.
  IF sy-subrc <> 0.
    BREAK-POINT ##NO_BREAK.
    PERFORM exit.
    RETURN.
  ENDIF.
  gs_cert_old-datefrom = gs_cert_old-validfrom(8).
  gs_cert_old-dateto = gs_cert_old-validto(8).

  WRITE: / 'Own certificate:', AT 30 gs_cert_old-subject,
    AT /30 gs_cert_old-issuer,
    AT /30 gs_cert_old-datefrom, gs_cert_old-dateto.
  SKIP.

* Check if certificate already exists and if yes, remove it
  CALL FUNCTION 'SSFC_GET_CERTIFICATELIST'
    EXPORTING
      profile               = g_profile
      profilepw             = g_profilepw
    IMPORTING
      certificatelist       = gt_certlist
    EXCEPTIONS
      ssf_krn_error         = 1
      ssf_krn_nomemory      = 2
      ssf_krn_nossflib      = 3
      ssf_krn_invalid_par   = 4
      ssf_krn_nocertificate = 5
      OTHERS                = 6.
  IF sy-subrc <> 0.
    BREAK-POINT ##NO_BREAK.
    PERFORM exit.
    RETURN.
  ENDIF.

  WRITE: / 'List of existing certificates' COLOR COL_POSITIVE.
  SKIP.

  LOOP AT gt_certlist INTO g_cert_list.
    CALL FUNCTION 'SSFC_PARSE_CERTIFICATE'
      EXPORTING
        certificate         = g_cert_list
      IMPORTING
        subject             = gs_cert_old-subject
        issuer              = gs_cert_old-issuer
        serialno            = gs_cert_old-serialno
        validfrom           = gs_cert_old-validfrom
        validto             = gs_cert_old-validto
      EXCEPTIONS
        ssf_krn_error       = 1
        ssf_krn_nomemory    = 2
        ssf_krn_nossflib    = 3
        ssf_krn_invalid_par = 4
        OTHERS              = 5.
    IF sy-subrc <> 0.
      BREAK-POINT ##NO_BREAK.
      PERFORM exit.
      RETURN.
    ENDIF.
    gs_cert_old-datefrom = gs_cert_old-validfrom(8).
    gs_cert_old-dateto = gs_cert_old-validto(8).

    WRITE: / 'Certificate list:', AT 30 gs_cert_old-subject,
      AT /30 gs_cert_old-issuer,
      AT /30 gs_cert_old-datefrom, gs_cert_old-dateto.
    SKIP.

    LOOP AT gt_cert_new INTO gs_cert_new
      WHERE subject = gs_cert_old-subject AND dateto >= gs_cert_old-dateto.
      CALL FUNCTION 'SSFC_REMOVECERTIFICATE'
        EXPORTING
          profile               = g_profile
          profilepw             = g_profilepw
          subject               = gs_cert_old-subject
          issuer                = gs_cert_old-issuer
          serialno              = gs_cert_old-serialno
        EXCEPTIONS
          ssf_krn_error         = 1
          ssf_krn_nomemory      = 2
          ssf_krn_nossflib      = 3
          ssf_krn_invalid_par   = 4
          ssf_krn_nocertificate = 5
          OTHERS                = 6.
      IF sy-subrc <> 0.
        BREAK-POINT ##NO_BREAK.
        PERFORM exit.
        RETURN.
      ENDIF.

      WRITE: AT /30 'Removed certificate' COLOR COL_TOTAL.
      SKIP.
    ENDLOOP.
  ENDLOOP.
  IF sy-subrc <> 0.
    WRITE: AT /30 'No certificates'.
    SKIP.
  ENDIF.

  WRITE: / 'List of certificates to be upserted' COLOR COL_POSITIVE.
  SKIP.

  LOOP AT gt_cert_new INTO gs_cert_new.

    WRITE: / 'New certificate:', AT 30 gs_cert_new-subject,
      AT /30 gs_cert_new-issuer,
      AT /30 gs_cert_new-datefrom, gs_cert_new-dateto.
    SKIP.

*   Add certificate to PSE
    CALL FUNCTION 'SSFC_PUT_CERTIFICATE'
      EXPORTING
        profile             = g_profile
        profilepw           = g_profilepw
        certificate         = gs_cert_new-certificate
      EXCEPTIONS
        ssf_krn_error       = 1
        ssf_krn_nomemory    = 2
        ssf_krn_nossflib    = 3
        ssf_krn_invalid_par = 4
        ssf_krn_certexists  = 5
        OTHERS              = 6.
    IF sy-subrc <> 0.
      BREAK-POINT ##NO_BREAK.
      PERFORM exit.
      RETURN.
    ENDIF.

    WRITE: AT /30 'Added certificate' COLOR COL_POSITIVE.
    SKIP.
  ENDLOOP.

  PERFORM save.

  PERFORM exit.


*----------------------------------------------------------------------*

FORM save.

* Store PSE
  CALL FUNCTION 'SSFPSE_STORE'
    EXPORTING
      fname             = g_tempfile
      psepin            = g_profilepw
      psename           = g_psename
      id                = g_id
      b_newdn           = abap_false
      b_distribute      = g_distrib
    EXCEPTIONS
      file_load_failed  = 1
      storing_failed    = 2
      authority_missing = 3
      OTHERS            = 4.
  IF sy-subrc <> 0.
    BREAK-POINT ##NO_BREAK.
    PERFORM exit.
    RETURN.
  ENDIF.

  WRITE: / 'Saved PSE' COLOR COL_POSITIVE.
  SKIP.

  g_credname = g_psename.

  CALL FUNCTION 'ICM_SSL_PSE_CHANGED'
    EXPORTING
      global              = 1
      cred_name           = g_credname
    EXCEPTIONS
      icm_op_failed       = 1
      icm_get_serv_failed = 2
      icm_auth_failed     = 3
      OTHERS              = 4.
  IF sy-subrc = 0.
    MESSAGE s086(trust).
  ELSE.
    MESSAGE s085(trust).
  ENDIF.

ENDFORM.                    "save

*----------------------------------------------------------------------*

FORM exit.

  IF sy-subrc <> 0.
    WRITE: / 'RC =', sy-subrc COLOR COL_NEGATIVE.
  ENDIF.

* Drop temporary file
  TRY.
      DELETE DATASET g_tempfile.
    CATCH cx_sy_file_open .
      BREAK-POINT ##NO_BREAK.
    CATCH cx_sy_file_authority .
      BREAK-POINT ##NO_BREAK.
  ENDTRY.

* Unlock PSE
  CALL FUNCTION 'SSFPSE_DEQUEUE'
    EXPORTING
      psename         = g_psename
    EXCEPTIONS
      database_failed = 1
      foreign_lock    = 2
      internal_error  = 3
      OTHERS          = 4.
  IF sy-subrc <> 0.
* Ignore
  ENDIF.

ENDFORM.                    "exit
