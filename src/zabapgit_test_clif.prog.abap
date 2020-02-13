************************************************************************
* ZABAPGIT_TEST_CLIF
* abapGIT test of CLIF user exit
************************************************************************

REPORT zabapgit_test_clif.

TABLES: seoclass.

INCLUDE zabapgit_user_exit.

SELECT-OPTIONS so_class FOR seoclass-clsname OBLIGATORY.

DATA:
  gr_exit      TYPE REF TO zcl_abapgit_user_exit,
  gt_classes   TYPE TABLE OF seoclsname,
  gs_class_key TYPE seoclskey,
  gv_source    TYPE string,
  gt_source    TYPE zif_abapgit_definitions=>ty_string_tt,
  g_msg        TYPE string,
  g_word       TYPE string,
  g_line       TYPE i,
  g_text_1     TYPE sy-msgv1,
  g_text_2     TYPE sy-msgv2.

START-OF-SELECTION.

  CREATE OBJECT gr_exit.

  SELECT clsname FROM seoclass INTO TABLE gt_classes
    WHERE clsname IN so_class
    ORDER BY PRIMARY KEY.

  LOOP AT gt_classes INTO gs_class_key-clsname.

    WRITE: / gs_class_key-clsname COLOR COL_HEADING.
    SKIP.

    TRY.
        CALL METHOD gr_exit->zif_abapgit_exit~custom_serialize_abap_clif
          EXPORTING
            is_class_key = gs_class_key
          RECEIVING
            rt_source    = gt_source.
      CATCH cx_root.
        WRITE: / 'Error' COLOR COL_NEGATIVE, sy-msgv1, sy-msgv2, sy-msgv3, sy-msgv4.
        SKIP.
        CONTINUE.
    ENDTRY.

    WRITE: / 'Success' COLOR COL_POSITIVE.
    SKIP.

*    SYNTAX-CHECK FOR gt_source "PROGRAM gs_class_key-clsname
*      MESSAGE g_msg LINE g_line WORD g_word.
*    IF sy-subrc = 0.
*      WRITE: / 'Success' COLOR COL_POSITIVE.
*      SKIP.
*    ELSE.
*      WRITE g_line TO g_text_2 LEFT-JUSTIFIED.
*      CONCATENATE 'syntax error:' g_msg INTO g_text_1
*        SEPARATED BY space.
*      CONCATENATE 'Line:' g_text_2 'Token:' g_word INTO g_text_2
*        SEPARATED BY space.
*      WRITE: / g_text_1 COLOR COL_NEGATIVE, g_text_2 COLOR COL_NEGATIVE.
*    ENDIF.

*    LOOP AT gt_source INTO gv_source.
*      IF gv_source IS INITIAL.
*        SKIP.
*      ELSE.
*        WRITE / gv_source.
*      ENDIF.
*    ENDLOOP.

  ENDLOOP.
