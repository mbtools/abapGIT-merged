************************************************************************
* ZABAPGIT_TEST_ANALYZE_CLIF
* abapGIT - analyze consistency of CLIF serialization
************************************************************************

REPORT zabapgit_test_analyze_clif.

TABLES: seoclassdf.

SELECT-OPTIONS: so_class FOR seoclassdf-clsname.

CLASS lcl_abapgit_oo_serializer DEFINITION
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS serialize_abap_old
      IMPORTING
        !is_clskey       TYPE seoclskey
      RETURNING
        VALUE(rt_source) TYPE zif_abapgit_definitions=>ty_string_tt
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_abap_new
      IMPORTING
        !is_clskey       TYPE seoclskey
      RETURNING
        VALUE(rt_source) TYPE zif_abapgit_definitions=>ty_string_tt
      RAISING
        zcx_abapgit_exception
        cx_sy_dyn_call_error .
    METHODS remove_signatures
      CHANGING
        !ct_source TYPE zif_abapgit_definitions=>ty_string_tt .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.
CLASS lcl_abapgit_oo_serializer IMPLEMENTATION.
  METHOD remove_signatures.

* signatures messes up in CL_OO_SOURCE when deserializing and serializing
* within same session

    DATA: lv_begin  TYPE string,
          lv_end    TYPE string,
          lv_remove TYPE abap_bool,
          lv_source LIKE LINE OF ct_source.

    "@TODO: Put under test
    CONCATENATE '* <SIGNATURE>------------------------------------'
      '---------------------------------------------------+'
      INTO lv_begin.

    CONCATENATE '* +------------------------------------------------'
      '--------------------------------------</SIGNATURE>'
      INTO lv_end.

    lv_remove = abap_false.
    LOOP AT ct_source INTO lv_source.
      IF lv_source = lv_begin.
        lv_remove = abap_true.
      ENDIF.
      IF lv_remove = abap_true.
        DELETE ct_source INDEX sy-tabix.
      ENDIF.
      IF lv_source = lv_end.
        lv_remove = abap_false.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD serialize_abap_new.

    DATA: lo_source   TYPE REF TO object,
          lo_instance TYPE REF TO object.

* do not call the class/methods statically, as it will
* give syntax errors on old versions
    CALL METHOD ('CL_OO_FACTORY')=>('CREATE_INSTANCE')
      RECEIVING
        result = lo_instance.

    CALL METHOD lo_instance->('CREATE_CLIF_SOURCE')
      EXPORTING
        clif_name = is_clskey-clsname
        version   = 'A'
      RECEIVING
        result    = lo_source.

    CALL METHOD lo_source->('PRETTY_PRINT').

    CALL METHOD lo_source->('GET_SOURCE')
      IMPORTING
        source = rt_source.

  ENDMETHOD.
  METHOD serialize_abap_old.
* for old ABAP AS versions
    DATA: lo_source TYPE REF TO cl_oo_source.

    CREATE OBJECT lo_source
      EXPORTING
        clskey             = is_clskey
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from CL_OO_SOURCE. Subrc = { sy-subrc }| ).
    ENDIF.

    lo_source->read( 'A' ).
    rt_source = lo_source->get_old_source( ).
    remove_signatures( CHANGING ct_source = rt_source ).

  ENDMETHOD.
ENDCLASS.

DATA:
  gs_tadir         TYPE tadir,
  gt_tadir         TYPE TABLE OF tadir,
  gv_source        TYPE string,
  gv_match         TYPE string,
  gv_upper         TYPE string,
  gs_clskey        TYPE seoclskey,
  gr_serializer    TYPE REF TO lcl_abapgit_oo_serializer,
  gt_source        TYPE string_table,
  gt_source_old    TYPE string_table,
  gt_source_pretty TYPE string_table,
  g_subrc          TYPE i,
  g_count          TYPE i.

START-OF-SELECTION.

  SELECT * FROM tadir INTO TABLE gt_tadir
    WHERE devclass LIKE '$ABAPGIT%' AND ( object = 'CLAS' OR object = 'INTF' )
    AND obj_name <> 'ZCL_ABAPGIT_OO_CLASS_NEW' AND obj_name <> 'ZCL_ABAPGIT_USER_EXIT' AND obj_name IN so_class
    ORDER BY PRIMARY KEY.

  CREATE OBJECT gr_serializer.

  LOOP AT gt_tadir INTO gs_tadir.

    WRITE: / gs_tadir-object, gs_tadir-obj_name.

    gs_clskey-clsname = gs_tadir-obj_name.

*   get source in new version
    CALL METHOD gr_serializer->serialize_abap_new
      EXPORTING
        is_clskey = gs_clskey
      RECEIVING
        rt_source = gt_source.

    WRITE: 'Lines:', lines( gt_source ) COLOR COL_NORMAL.

*   get source in old version
    CALL METHOD gr_serializer->serialize_abap_old
      EXPORTING
        is_clskey = gs_clskey
      RECEIVING
        rt_source = gt_source_old.

    IF gt_source = gt_source_old.
      WRITE 'New =  Old' COLOR COL_POSITIVE.
    ELSE.
      WRITE 'New <> Old' COLOR COL_TOTAL.
    ENDIF.

*    IF gs_tadir-obj_name CS 'ZCX'.
*      BREAK-POINT.
*    ENDIF.

*   Pretty print
    CALL FUNCTION 'PRETTY_PRINTER'
      EXPORTING
        inctoo                  = ''
      IMPORTING
        indentation_maybe_wrong = g_subrc
      TABLES
        ntext                   = gt_source_pretty
        otext                   = gt_source
      EXCEPTIONS
        enqueue_table_full      = 1
        include_enqueued        = 2
        include_readerror       = 3
        include_writeerror      = 4
        OTHERS                  = 5.
    IF sy-subrc = 0.
      IF gt_source = gt_source_pretty.
        WRITE: 'Pretty' COLOR COL_POSITIVE.
      ELSE.
        WRITE: '  Ugly' COLOR COL_TOTAL.
      ENDIF.
    ELSE.
      WRITE: ' Error' COLOR COL_NEGATIVE.
    ENDIF.

*   Identifier
    DO 3 TIMES.
      CASE sy-index.
        WHEN 1.
          IF gs_tadir-object = 'INTF'. CONTINUE. ENDIF.
          gv_source = 'CLASS (.*) DEFINITION'.
        WHEN 2.
          IF gs_tadir-object = 'INTF'. CONTINUE. ENDIF.
          gv_source = 'CLASS (.*) IMPLEMENTATION'.
        WHEN 3.
          IF gs_tadir-object = 'CLAS'. CONTINUE. ENDIF.
          gv_source = 'INTERFACE (.*)'.
      ENDCASE.

      WRITE: gv_source && ':'.

      FIND REGEX gv_source IN TABLE gt_source
        SUBMATCHES gv_match IGNORING CASE.
      IF sy-subrc = 0.
        gv_upper = gv_match.
        TRANSLATE gv_upper TO UPPER CASE.
        IF gv_match = gv_upper.
          WRITE: 'Upper' COLOR COL_TOTAL.
        ELSE.
          WRITE: 'Lower' COLOR COL_POSITIVE.
        ENDIF.
      ELSE.
        WRITE: 'Error' COLOR COL_NEGATIVE.
      ENDIF.
    ENDDO.

    IF gs_tadir-object = 'INTF'. CONTINUE. ENDIF.

*   Missing class sections
    DO 3 TIMES.
      CASE sy-index.
        WHEN 1.
          gv_source = 'PUBLIC SECTION.'.
        WHEN 2.
          gv_source = 'PROTECTED SECTION.'.
        WHEN 3.
          gv_source = 'PRIVATE SECTION.'.
      ENDCASE.

      WRITE: gv_source && ':'.

      FIND gv_source IN TABLE gt_source IGNORING CASE MATCH COUNT g_count.
      IF sy-subrc = 0.
        IF g_count > 0.
          WRITE: 'Yes' COLOR COL_POSITIVE.
        ELSE.
          WRITE: ' No' COLOR COL_TOTAL.
        ENDIF.
      ELSE.
        WRITE: ' No' COLOR COL_TOTAL.
      ENDIF.
    ENDDO.

*   Signatures
    gv_source = '<SIGNATURE>'.

    WRITE: gv_source && ':'.

    FIND gv_source  IN TABLE gt_source MATCH COUNT g_count.
    IF g_count > 0.
      WRITE: 'Yes' COLOR COL_TOTAL.
    ELSE.
      WRITE: ' No' COLOR COL_POSITIVE.
    ENDIF.

  ENDLOOP.
