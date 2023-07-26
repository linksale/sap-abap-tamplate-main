*&---------------------------------------------------------------------*
*& Report ZLS_ALVMAINTANENCE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zls_alvmaintanence.
************************************************************************
* PROGRAM ID           : ZLS_MANUFACTURING
* PROGRAM TITLE        : planned and Actual Order Report            *
* DATE                 : 13/06/2023                                    *
* DEVELOPMENT ID       : S4USER050                                     *
* CHANGE REQUEST NUMBER: S4HK902002                                    *
* DESCRIPTION          : This ALV report can be used for any tables    *
*             The selection screen is generated based on the key fields*
*      A custom BADI with table name as filter is used for any specific*
*                              functionalities like update fieldcatalog*
*           add new toolbar buttons and code for custom toolbar options*
*      A fallback BADI implementation will serve most of the use cases *
*======================================================================*
*
*                                                                      *
*----------------------------------------------------------------------*




PARAMETERS p_tab TYPE tabname16 MEMORY ID dtb OBLIGATORY.

DATA ok_code TYPE sy-ucomm.


*----------------------------------------------------------------------*
*       CLASS lcl_table DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_table DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_alv_style,
           alv_action TYPE char1,
           celltab TYPE lvc_t_styl,
    END OF ty_alv_style.

    DATA: mo_alv TYPE REF TO cl_gui_alv_grid,
          mo_container TYPE REF TO cl_gui_custom_container.

    METHODS: constructor IMPORTING iv_tabname TYPE tabname16
                         RAISING   zcx_generic_exception,
             display_table RAISING zcx_generic_exception,
             start RAISING zcx_generic_exception,
             execute_user_command IMPORTING iv_ucomm TYPE sy-ucomm
                                 RAISING zcx_generic_exception,
             free_objects,
             set_mode IMPORTING iv_mode TYPE c
                      RAISING zcx_generic_exception.



  PRIVATE SECTION.
    DATA: mv_tabname TYPE tabname16,
          mt_fieldcatalog TYPE lvc_t_fcat,
          mt_where TYPE rsds_where_tab,
          mv_first_display TYPE abap_bool,
          mv_title TYPE sy-title,
          mv_mode TYPE c,
          mo_data TYPE REF TO data,
          mo_deleted_data TYPE REF TO data,
          mv_style_name TYPE lvc_fname,
          mo_badi TYPE REF TO zsm30.

    METHODS: build_fieldcatalog RAISING zcx_generic_exception,
             init_alv,
             set_style,
             generate_selection RETURNING VALUE(rv_exit) TYPE abap_bool
                                RAISING zcx_generic_exception,
             handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed,
             handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
             handle_user_command  FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
             get_exclude RETURNING VALUE(rt_exclude) TYPE ui_functions,
             get_data RAISING zcx_generic_exception,
             display_messages IMPORTING it_messages TYPE bapiret2_t,
             create_output_table RAISING zcx_generic_exception,
             save_data RAISING zcx_generic_exception,
             refresh_alv_data RAISING zcx_generic_exception,
             delete_rows,
             copy_rows,
             check_transportable RETURNING VALUE(rv_transportable) TYPE abap_bool,
             transport_entries IMPORTING it_data TYPE STANDARD TABLE
                               RETURNING VALUE(rv_tr) TYPE trkorr
                               RAISING zcx_generic_exception.



ENDCLASS.                    "lcl_table DEFINITION



*----------------------------------------------------------------------*
*       CLASS lcl_table IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_table IMPLEMENTATION.

  METHOD constructor.
    DATA: ls_ddtext TYPE dd02t,
          ls_dd02 TYPE dd02l,
          lv_message TYPE string,
          lv_tabname TYPE string.

    lv_tabname = mv_tabname = iv_tabname.

    SELECT SINGLE * FROM dd02l INTO ls_dd02 WHERE tabname = mv_tabname.
    IF sy-subrc IS NOT INITIAL.
      CONCATENATE 'Table' mv_tabname 'not found' INTO lv_message SEPARATED BY space.
      zcx_generic_exception=>raise( lv_message ).
    ENDIF.

    SELECT SINGLE * FROM dd02t INTO ls_ddtext WHERE tabname = mv_tabname AND ddlanguage = sy-langu.
    mv_title = ls_ddtext-ddtext.

     GET BADI mo_badi
      FILTERS
        tabname = lv_tabname.
    TRY.
        set_mode( 'C' ).
      CATCH zcx_generic_exception.
        set_mode( 'D' ).
    ENDTRY.
    create_output_table( ).
  ENDMETHOD.                    "constructor

  METHOD check_transportable.

    DATA: lv_protocol TYPE ddprotocol,
          lv_tabname TYPE vim_name,
          lv_maintflag TYPE maintflag.

    SELECT SINGLE tabname INTO lv_tabname FROM tvdir WHERE tabname = mv_tabname.
    IF sy-subrc IS INITIAL.
      SELECT SINGLE protokoll INTO lv_protocol FROM dd09l WHERE tabname = mv_tabname.
      IF lv_protocol IS INITIAL.
        rv_transportable = abap_true.
      ENDIF.
    ELSE.
      SELECT SINGLE mainflag INTO lv_maintflag FROM dd02l WHERE tabname = mv_tabname.
      IF lv_maintflag IS INITIAL.
        rv_transportable = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "check_transportable

  METHOD transport_entries.

    DATA: lt_e071k TYPE TABLE OF e071k,
          ls_e071k LIKE LINE OF lt_e071k,
          lt_fieldcatalog TYPE lvc_t_fcat,
          lt_e071 TYPE TABLE OF e071,
          ls_e071 LIKE LINE OF lt_e071,
          lv_typ1 TYPE c LENGTH 1,
          lv_start    LIKE sy-tabix,
          lv_len      LIKE sy-tabix,
          lv_max_len LIKE sy-tabix,
          lv_type_error(1),
          lv_key_error(1),
          ls_message TYPE bapiret2,
          lv_contflag TYPE dd02l-contflag,
          lv_category TYPE e070-korrdev,
          lv_task  TYPE e070-trkorr,
          ls_fieldcatalog LIKE LINE OF lt_fieldcatalog.


    FIELD-SYMBOLS: <ls_data> TYPE any,
                   <lv_value> TYPE any.

    AUTHORITY-CHECK OBJECT 'S_TRANSPRT'
      ID 'TTYPE' FIELD 'TASK'
      ID 'ACTVT' FIELD '02'.
    IF sy-subrc <> 0.
      zcx_generic_exception=>raise( 'Not authorized to create transport' ).
    ENDIF.

    ls_e071-pgmid = 'R3TR'.
    ls_e071-object = 'TABU'.
    ls_e071-obj_name = mv_tabname.
    ls_e071-objfunc = 'K'.
    APPEND ls_e071 TO lt_e071.

    lt_fieldcatalog = mt_fieldcatalog.
    DELETE lt_fieldcatalog WHERE key IS INITIAL.

    LOOP AT it_data ASSIGNING <ls_data>.
      ls_e071k-pgmid = 'R3TR'.
      ls_e071k-mastertype =  ls_e071k-object = 'TABU'.
      ls_e071k-mastername = ls_e071k-objname = mv_tabname.
      ls_e071k-sortflag = '2'.

      LOOP AT lt_fieldcatalog INTO ls_fieldcatalog.
        ASSIGN COMPONENT ls_fieldcatalog-fieldname OF STRUCTURE <ls_data> TO <lv_value>.
        CONCATENATE ls_e071k-tabkey <lv_value> INTO ls_e071k-tabkey RESPECTING BLANKS.
*       check if field is character type -> otherwise only *-transport
        DESCRIBE FIELD  <lv_value> TYPE lv_typ1.
        IF lv_typ1 NA 'CDNT'.
          ls_e071k-tabkey+lv_start(1) = '*'.
          lv_type_error = 'X'.
          EXIT.
        ENDIF.

        DESCRIBE FIELD <lv_value> LENGTH lv_len IN CHARACTER MODE.
*       check if key is longer than C120 --> error
        lv_max_len = lv_len + lv_start.
        IF lv_max_len > 120.
          ls_e071k-tabkey+lv_start(1) = '*'.
          lv_key_error = 'X'.
          EXIT.
        ENDIF.
        ls_e071k-tabkey+lv_start(lv_len) = <lv_value>.
        ADD lv_len TO lv_start.
      ENDLOOP.
      APPEND ls_e071k TO lt_e071k.
    ENDLOOP.

    IF lv_type_error = 'X' OR lv_key_error = 'X'.
      ls_message-type = 'E'.
      ls_message-id = 'TK'.
      ls_message-number = 320.
      ls_message-message_v1 = mv_tabname.
      zcx_generic_exception=>raise_message( ls_message ).
    ENDIF.

*..check category of table
    SELECT SINGLE contflag FROM dd02l INTO lv_contflag
             WHERE tabname  = mv_tabname
               AND as4local = 'A'.
    IF sy-subrc = 0 AND
       ( lv_contflag = 'C' OR lv_contflag = 'G').
      lv_category = 'CUST'.
    ELSE.
      lv_category = 'SYST'.
    ENDIF.

    CALL FUNCTION 'TR_ORDER_CHOICE_CORRECTION'
      EXPORTING
        iv_category = lv_category
      IMPORTING
        ev_order    = rv_tr
        ev_task     = lv_task
      EXCEPTIONS
        OTHERS      = 3.

    IF sy-subrc <> 0.
      ls_message-type = 'E'.
      ls_message-id = sy-msgid.
      ls_message-number = sy-msgno.
      ls_message-message_v1 = sy-msgv1.
      ls_message-message_v2 = sy-msgv2.
      ls_message-message_v3 = sy-msgv3.
      ls_message-message_v4 = sy-msgv4.
      zcx_generic_exception=>raise_message( ls_message ).
    ENDIF.

    CALL FUNCTION 'TR_APPEND_TO_COMM_OBJS_KEYS'
      EXPORTING
        wi_simulation         = ' '
        wi_suppress_key_check = ' '
        wi_trkorr             = lv_task
      TABLES
        wt_e071               = lt_e071
        wt_e071k              = lt_e071k
      EXCEPTIONS
        OTHERS                = 68.

    IF sy-subrc <> 0.
      ls_message-type = 'E'.
      ls_message-id = sy-msgid.
      ls_message-number = sy-msgno.
      ls_message-message_v1 = sy-msgv1.
      ls_message-message_v2 = sy-msgv2.
      ls_message-message_v3 = sy-msgv3.
      ls_message-message_v4 = sy-msgv4.
      zcx_generic_exception=>raise_message( ls_message ).
    ENDIF.

  ENDMETHOD.                    "transport_entries

  METHOD refresh_alv_data.

    get_data( ).
    mo_alv->refresh_table_display( i_soft_refresh = abap_true ).

  ENDMETHOD.                    "refresh_alv_data

  METHOD free_objects.
    mo_alv->free( ).
    mo_container->free( ).
  ENDMETHOD.                    "free_objects

  METHOD create_output_table.

    FIELD-SYMBOLS <lt_data> TYPE STANDARD TABLE.

    build_fieldcatalog( ).

*   Create table based on the fieldcatalog
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        i_style_table   = abap_true
        it_fieldcatalog = mt_fieldcatalog
      IMPORTING
        e_style_fname   = mv_style_name
        ep_table        = mo_data.

    ASSIGN mo_data->* TO <lt_data>.

    CREATE DATA mo_deleted_data LIKE <lt_data>.

  ENDMETHOD.                    "create_output_table

  METHOD save_data.

    DATA: lo_table TYPE REF TO data,
          lv_where TYPE string,
          lv_tr TYPE trkorr.

    FIELD-SYMBOLS: <lt_changed_data> TYPE STANDARD TABLE,
                   <lt_inserted_data> TYPE STANDARD TABLE,
                   <lt_deleted_data> TYPE STANDARD TABLE,
                   <lt_table_data> TYPE STANDARD TABLE,
                   <ls_table_data> TYPE any,
                   <lt_data> TYPE STANDARD TABLE,
                   <ls_data> TYPE any.

*   Get changed Data
    ASSIGN mo_data->* TO <lt_data>.
    CREATE DATA lo_table LIKE <lt_data>.
    ASSIGN lo_table->* TO <lt_changed_data>.
    <lt_changed_data> = <lt_data>.
    CONCATENATE 'ALV_ACTION NE' '''C''' INTO lv_where SEPARATED BY space.
    DELETE <lt_changed_data> WHERE (lv_where).

*   Get inserted Data
    CREATE DATA lo_table LIKE <lt_data>."<gt_data>.
    ASSIGN lo_table->* TO <lt_inserted_data>.
    <lt_inserted_data> = <lt_data>.
    lv_where = 'ALV_ACTION IS NOT INITIAL'.
    DELETE <lt_inserted_data> WHERE (lv_where).

    ASSIGN mo_deleted_data->* TO <lt_deleted_data>.
    IF <lt_inserted_data> IS INITIAL AND <lt_changed_data> IS INITIAL AND <lt_deleted_data> IS INITIAL.
      zcx_generic_exception=>raise( 'No changes made.' ).
    ENDIF.

    CREATE DATA lo_table TYPE TABLE OF (mv_tabname).
    ASSIGN lo_table->* TO <lt_table_data>.
    LOOP AT <lt_deleted_data> ASSIGNING <ls_data>.
      APPEND INITIAL LINE TO <lt_table_data> ASSIGNING <ls_table_data>.
      MOVE-CORRESPONDING <ls_data> TO <ls_table_data>.
    ENDLOOP.
    DELETE (mv_tabname) FROM TABLE <lt_table_data>.

    CLEAR <lt_table_data>.
    LOOP AT <lt_inserted_data> ASSIGNING <ls_data>.
      APPEND INITIAL LINE TO <lt_table_data> ASSIGNING <ls_table_data>.
      MOVE-CORRESPONDING <ls_data> TO <ls_table_data>.
    ENDLOOP.
    INSERT (mv_tabname) FROM TABLE <lt_table_data>.

    CLEAR <lt_table_data>.
    LOOP AT <lt_changed_data> ASSIGNING <ls_data>.
      APPEND INITIAL LINE TO <lt_table_data> ASSIGNING <ls_table_data>.
      MOVE-CORRESPONDING <ls_data> TO <ls_table_data>.
    ENDLOOP.
    UPDATE (mv_tabname) FROM TABLE <lt_table_data>.

    refresh_alv_data( ).

    IF check_transportable( ) EQ abap_true.
      APPEND LINES OF <lt_inserted_data> TO <lt_changed_data>.
      APPEND LINES OF <lt_deleted_data> TO <lt_changed_data>.
      CLEAR: <lt_inserted_data>, <lt_deleted_data>.
      lv_tr = transport_entries( <lt_changed_data> ).
    ENDIF.
    CLEAR: <lt_inserted_data>, <lt_deleted_data>.

  ENDMETHOD.                    "save_data

  METHOD delete_rows.

    DATA: lt_index_rows TYPE lvc_t_row,
          ls_index_row LIKE LINE OF lt_index_rows,
          lv_where TYPE string.

    FIELD-SYMBOLS: <ls_data> TYPE any,
                   <ls_changed_data> TYPE any,
                   <lt_deleted_data> TYPE STANDARD TABLE,
                   <lt_data> TYPE STANDARD TABLE,
                   <lv_action> TYPE any.

    CALL METHOD mo_alv->get_selected_rows
      IMPORTING
        et_index_rows = lt_index_rows.

    ASSIGN mo_data->* TO <lt_data>.
    ASSIGN mo_deleted_data->* TO <lt_deleted_data>.
    LOOP AT lt_index_rows INTO ls_index_row.
      READ TABLE <lt_data> ASSIGNING <ls_data> INDEX ls_index_row-index.
      ASSIGN COMPONENT 'ALV_ACTION' OF STRUCTURE <ls_data> TO <lv_action>.

      IF <lv_action> IS NOT INITIAL. " Means it is an existing row, add to deleted data
        APPEND INITIAL LINE TO <lt_deleted_data> ASSIGNING <ls_changed_data>.
        MOVE-CORRESPONDING <ls_data> TO <ls_changed_data>.
        ASSIGN COMPONENT 'ALV_ACTION' OF STRUCTURE <ls_changed_data> TO <lv_action>.
        <lv_action> = 'D'.
      ENDIF.
      CLEAR <ls_data>.
    ENDLOOP.
    lv_where = 'TABLE_LINE IS INITIAL'.

    DELETE <lt_data> WHERE (lv_where).
    mo_alv->refresh_table_display( i_soft_refresh = abap_true ).

  ENDMETHOD.                    "delete_rows

  METHOD copy_rows.

    DATA: lt_index_rows TYPE lvc_t_row,
          ls_index_row LIKE LINE OF lt_index_rows,
          lv_index TYPE i,
          lv_counter TYPE i.

    FIELD-SYMBOLS: <ls_data> TYPE any,
                   <ls_copied_data> TYPE any,
                   <lt_data> TYPE STANDARD TABLE,
                   <lv_action> TYPE any,
                   <lv_style> TYPE any.

    CALL METHOD mo_alv->get_selected_rows
      IMPORTING
        et_index_rows = lt_index_rows.

    SORT lt_index_rows BY index ASCENDING.

    ASSIGN mo_data->* TO <lt_data>.
    LOOP AT lt_index_rows INTO ls_index_row.
      lv_index = ls_index_row-index + lv_counter.
      READ TABLE <lt_data> ASSIGNING <ls_data> INDEX lv_index.
      ASSIGN COMPONENT 'ALV_ACTION' OF STRUCTURE <ls_data> TO <lv_action>.
      INSERT INITIAL LINE INTO <lt_data> ASSIGNING <ls_copied_data> INDEX lv_index.
      lv_counter = lv_counter + 1.
      MOVE-CORRESPONDING <ls_data> TO <ls_copied_data>.
      ASSIGN COMPONENT 'ALV_ACTION' OF STRUCTURE <ls_copied_data> TO <lv_action>.
      CLEAR <lv_action>.
      ASSIGN COMPONENT mv_style_name OF STRUCTURE <ls_copied_data> TO <lv_style>.
      CLEAR <lv_style>.
    ENDLOOP.
    mo_alv->refresh_table_display( i_soft_refresh = abap_true ).
  ENDMETHOD.                    "copy_rows
  METHOD set_mode.
    DATA: ls_message TYPE bapiret2,
          lv_tabname TYPE rstable-tabname,
          lv_rc TYPE subrc.


    IF iv_mode = 'C'.
      CALL BADI mo_badi->authority_check
        EXPORTING
          iv_mode  = iv_mode
        CHANGING
          cv_subrc = lv_rc.

      IF lv_rc IS INITIAL.
        lv_tabname = mv_tabname.
        CALL FUNCTION 'ENQUEUE_E_TABLEE'
          EXPORTING
            mode_rstable   = 'E'
            tabname        = lv_tabname
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.
        IF sy-subrc <> 0.
          ls_message-type = 'E'.
          ls_message-id = sy-msgid.
          ls_message-number = sy-msgno.
          ls_message-message_v1 = sy-msgv1.
          ls_message-message_v2 = sy-msgv2.
          ls_message-message_v3 = sy-msgv3.
          ls_message-message_v4 = sy-msgv4.
          zcx_generic_exception=>raise_message( ls_message ).
        ENDIF.
      ELSE.
        zcx_generic_exception=>raise( 'No authorization for change' ).
      ENDIF.
    ENDIF.
    mv_mode = iv_mode.
  ENDMETHOD.                    "set_mode

  METHOD display_messages.

    DATA ls_message LIKE LINE OF it_messages.
    CHECK it_messages IS NOT INITIAL.

    CALL FUNCTION 'MESSAGES_INITIALIZE'
      EXPORTING
        reset  = abap_true
      EXCEPTIONS
        OTHERS = 3.

    LOOP AT it_messages INTO ls_message.

      CALL FUNCTION 'MESSAGE_STORE'
        EXPORTING
          arbgb  = ls_message-id
          msgty  = ls_message-type
          txtnr  = ls_message-number
          msgv1  = ls_message-message_v1
          msgv2  = ls_message-message_v2
          msgv3  = ls_message-message_v3
          msgv4  = ls_message-message_v4
        EXCEPTIONS
          OTHERS = 4.

    ENDLOOP.

*   Stop and display the collected messages
    CALL FUNCTION 'MESSAGES_STOP'
      EXPORTING
        i_reset_identification = abap_true
      EXCEPTIONS
        OTHERS                 = 6.

    CALL FUNCTION 'MESSAGES_SHOW'
      EXCEPTIONS
        inconsistent_range = 1
        no_messages        = 2
        OTHERS             = 3.


  ENDMETHOD.                    "display_messages

  METHOD start.

    DATA lt_return TYPE bapiret2_t.

    FIELD-SYMBOLS <lt_data> TYPE STANDARD TABLE.

    IF generate_selection( ) = abap_true.
      RETURN.
    ENDIF.

    get_data( ).

    ASSIGN mo_data->* TO <lt_data>.
    CALL BADI mo_badi->validations_before_load
      EXPORTING
        it_data   = <lt_data>
      CHANGING
        ct_return = lt_return.

    IF lt_return IS NOT INITIAL.
      display_messages( lt_return ).
    ENDIF.


    CALL SCREEN 100.
  ENDMETHOD.                    "start

  METHOD handle_toolbar.

    FIELD-SYMBOLS: <s_toolbar> TYPE stb_button.
    CALL BADI mo_badi->update_toolbar
      CHANGING
        ct_toolbar = e_object->mt_toolbar.

    READ TABLE e_object->mt_toolbar ASSIGNING <s_toolbar> WITH KEY function = mo_alv->mc_fc_loc_delete_row.
    IF sy-subrc IS INITIAL.
      <s_toolbar>-function  = 'DELETE'.
    ENDIF.

    READ TABLE e_object->mt_toolbar ASSIGNING <s_toolbar> WITH KEY function = mo_alv->mc_fc_loc_copy_row.
    IF sy-subrc IS INITIAL.
      <s_toolbar>-function  = 'COPY'.
    ENDIF.

    READ TABLE e_object->mt_toolbar ASSIGNING <s_toolbar> WITH KEY function = mo_alv->mc_fc_loc_paste.
    IF sy-subrc IS INITIAL.
      <s_toolbar>-function  = 'INSRC'.
    ENDIF.


  ENDMETHOD.                    "handle_toolbar

  METHOD get_data.

    DATA: ls_celltab TYPE lvc_s_styl,
          lt_fieldcat TYPE lvc_t_fcat,
          ls_fieldcat LIKE LINE OF lt_fieldcat,
          lo_data TYPE REF TO data.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE,
                   <lt_output_data> TYPE STANDARD TABLE,
                   <ls_data> TYPE any,
                   <ls_output_data> TYPE any,
                   <lv_action> TYPE any,
                   <lt_celltab> TYPE lvc_t_styl.


    lt_fieldcat = mt_fieldcatalog.
    DELETE lt_fieldcat WHERE key IS INITIAL.

    TRY.
        CREATE DATA lo_data TYPE TABLE OF (mv_tabname).
      CATCH cx_sy_create_data_error.
        zcx_generic_exception=>raise( 'Table not found' ).
    ENDTRY.

    ASSIGN lo_data->* TO <lt_data>.


    SELECT * FROM (mv_tabname) INTO  TABLE <lt_data> WHERE (mt_where).

    ASSIGN mo_data->* TO <lt_output_data>.

    CLEAR <lt_output_data>.

    LOOP AT <lt_data> ASSIGNING <ls_data>.
      APPEND INITIAL LINE TO <lt_output_data> ASSIGNING <ls_output_data>.
      MOVE-CORRESPONDING <ls_data> TO <ls_output_data>.
      ASSIGN COMPONENT 'ALV_ACTION' OF STRUCTURE <ls_output_data> TO <lv_action>.
      <lv_action> = 'E'.
      ASSIGN COMPONENT mv_style_name OF STRUCTURE <ls_output_data> TO <lt_celltab>.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

      LOOP AT lt_fieldcat INTO ls_fieldcat.
        ls_celltab-fieldname = ls_fieldcat-fieldname.
        INSERT ls_celltab INTO TABLE <lt_celltab>.
      ENDLOOP.
    ENDLOOP.



  ENDMETHOD.                    "get_data
  METHOD generate_selection.

    DATA : lt_where TYPE rsds_twhere,
           ls_where LIKE LINE OF lt_where,
           lt_texpr TYPE rsds_texpr,
           lv_selid TYPE rsdynsel-selid,
           lv_actnum TYPE sy-tfill,
           ls_fieldcatalog LIKE LINE OF mt_fieldcatalog,
           lt_tabs TYPE TABLE OF rsdstabs,
           ls_tabs LIKE LINE OF lt_tabs,
           lt_flds TYPE TABLE OF rsdsfields,
           ls_flds LIKE LINE OF lt_flds.



    LOOP AT mt_fieldcatalog INTO ls_fieldcatalog WHERE key = abap_true.
      CHECK ls_fieldcatalog-fieldname NE 'MANDT'.
      ls_flds-tablename = mv_tabname.
      ls_flds-type = 'S'.
      ls_flds-fieldname = ls_fieldcatalog-fieldname.
      APPEND ls_flds TO lt_flds.

    ENDLOOP.

    ls_where-tablename = mv_tabname.
    APPEND ls_where TO lt_where.

    ls_tabs-prim_tab = mv_tabname.
    APPEND ls_tabs TO lt_tabs.

    CALL FUNCTION 'FREE_SELECTIONS_WHERE_2_EX'
      EXPORTING
        where_clauses        = lt_where
      IMPORTING
        expressions          = lt_texpr
      EXCEPTIONS
        incorrect_expression = 1
        OTHERS               = 2.

    IF sy-subrc IS NOT INITIAL.
      zcx_generic_exception=>raise( 'Error generating selection screen' ).
    ENDIF.

    CALL FUNCTION 'FREE_SELECTIONS_INIT'
      EXPORTING
        kind                     = 'T'
        expressions              = lt_texpr
      IMPORTING
        selection_id             = lv_selid
        number_of_active_fields  = lv_actnum
      TABLES
        tables_tab               = lt_tabs
        fields_tab               = lt_flds
      EXCEPTIONS
        fields_incomplete        = 01
        fields_no_join           = 02
        field_not_found          = 03
        no_tables                = 04
        table_not_found          = 05
        expression_not_supported = 06
        incorrect_expression     = 07
        illegal_kind             = 08
        area_not_found           = 09
        inconsistent_area        = 10
        kind_f_no_fields_left    = 11
        kind_f_no_fields         = 12
        too_many_fields          = 13.

    IF sy-subrc IS NOT INITIAL.
      zcx_generic_exception=>raise( 'Error generating selection screen' ).
    ENDIF.


    CALL FUNCTION 'FREE_SELECTIONS_DIALOG'
      EXPORTING
        selection_id            = lv_selid
        title                   = mv_title
        tree_visible            = ' '
        status                  = 1 "Execute button
      IMPORTING
        where_clauses           = lt_where
        expressions             = lt_texpr
        number_of_active_fields = lv_actnum
      TABLES
        fields_tab              = lt_flds
      EXCEPTIONS
        internal_error          = 01
        no_action               = 02
        no_fields_selected      = 03
        no_tables_selected      = 04
        selid_not_found         = 05.

    IF sy-subrc = 02.
      rv_exit = abap_true.
      RETURN.
    ENDIF.

    IF sy-subrc IS NOT INITIAL AND sy-subrc NE 02.
      zcx_generic_exception=>raise( 'Error generating selection screen' ).
    ENDIF.

    READ TABLE lt_where WITH KEY tablename = mv_tabname INTO ls_where.
    mt_where = ls_where-where_tab.


  ENDMETHOD.                    "generate_selection
  METHOD build_fieldcatalog.

    DATA: lv_structure TYPE tabname,
          lt_alv_style TYPE TABLE OF ty_alv_style,
          lt_fieldcatalog TYPE lvc_t_fcat.

    FIELD-SYMBOLS <ls_fieldcatalog> LIKE LINE OF mt_fieldcatalog.

    lv_structure = mv_tabname.

    mt_fieldcatalog = zcl_alv_util=>get_field_catalog( iv_structure = lv_structure ).

    lt_fieldcatalog = zcl_alv_util=>get_field_catalog( it_data = lt_alv_style ).

    APPEND LINES OF lt_fieldcatalog TO mt_fieldcatalog.

    LOOP AT mt_fieldcatalog ASSIGNING <ls_fieldcatalog>.
      IF <ls_fieldcatalog>-fieldname = 'MANDT' OR <ls_fieldcatalog>-fieldname = 'ALV_ACTION'.
        <ls_fieldcatalog>-no_out = abap_true.
      ENDIF.
      <ls_fieldcatalog>-edit = abap_true.
    ENDLOOP.

    CALL BADI mo_badi->change_fieldcatalog
      CHANGING
        ct_fieldcatalog = mt_fieldcatalog.

  ENDMETHOD.                    "build_fieldcatalog

  METHOD display_table.
    DATA: lt_fieldcatalog TYPE lvc_t_fcat,
          lt_exclude TYPE ui_functions,
          ls_variant TYPE disvariant,
          ls_layout TYPE lvc_s_layo.

    FIELD-SYMBOLS <lt_output_data> TYPE STANDARD TABLE.

    ASSIGN mo_data->* TO <lt_output_data>.
    IF mo_alv IS NOT BOUND.
      init_alv( ).
    ENDIF.

    IF mv_mode = 'C'.
      mo_alv->set_ready_for_input( 1 ).
    ELSE.
      mo_alv->set_ready_for_input( 2 ).
    ENDIF.

    IF mv_first_display IS INITIAL.

      lt_exclude = get_exclude( ).

      ls_variant-report   = sy-repid.
      ls_layout-grid_title = mv_tabname.
      ls_layout-stylefname = mv_style_name.

      mo_alv->set_table_for_first_display(
       EXPORTING
          is_variant             = ls_variant
          i_save                 = 'A'
          is_layout              = ls_layout
          it_toolbar_excluding   = lt_exclude
       CHANGING
          it_fieldcatalog        = mt_fieldcatalog
          it_outtab              = <lt_output_data>
       EXCEPTIONS
          OTHERS                 = 1 ).

      mv_first_display = abap_true.

    ELSE.
      mo_alv->refresh_table_display( ).
    ENDIF.

  ENDMETHOD.                    "display_table


  METHOD init_alv.
    CHECK mo_alv IS NOT BOUND.
    CREATE OBJECT mo_container
      EXPORTING
        container_name = 'TABLE'.

    CREATE OBJECT mo_alv
      EXPORTING
        i_parent = mo_container
      EXCEPTIONS
        OTHERS   = 1.

    CALL METHOD mo_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    SET HANDLER handle_data_changed FOR mo_alv.
    SET HANDLER handle_toolbar FOR mo_alv.
    SET HANDLER handle_user_command FOR mo_alv.

    mo_alv->set_ready_for_input( i_ready_for_input = 1 ).

  ENDMETHOD.                    "init_alv

  METHOD handle_data_changed.
    DATA: ls_good TYPE lvc_s_modi.
    FIELD-SYMBOLS: <lv_action> TYPE any,
                   <ls_data> TYPE any,
                   <lt_data> TYPE STANDARD TABLE.

    ASSIGN mo_data->* TO <lt_data>.

    LOOP AT er_data_changed->mt_mod_cells INTO ls_good.
      READ TABLE <lt_data> ASSIGNING <ls_data> INDEX ls_good-row_id.
      CHECK sy-subrc IS INITIAL.
      ASSIGN COMPONENT 'ALV_ACTION' OF STRUCTURE <ls_data> TO <lv_action>.
      IF <lv_action> = 'E'.
        <lv_action> = 'C'.
      ENDIF.
    ENDLOOP.

    CALL BADI mo_badi->handle_data_changed
      EXPORTING
        io_data_changed = er_data_changed
      CHANGING
        ct_data         = <lt_data>.
  ENDMETHOD.                    "handle_data_changed

  METHOD set_style.
    DATA: ls_celltab TYPE lvc_s_styl,
          lt_fieldcat TYPE lvc_t_fcat,
          ls_fieldcat LIKE LINE OF lt_fieldcat.

    FIELD-SYMBOLS: <ls_data> TYPE any,
                   <lv_action> TYPE any,
                   <lt_data> TYPE STANDARD TABLE,
                   <lt_celltab> TYPE lvc_t_styl.

    lt_fieldcat = mt_fieldcatalog.

    DELETE lt_fieldcat WHERE key IS INITIAL.
    ASSIGN mo_data->* TO <lt_data>.

    LOOP AT <lt_data> ASSIGNING <ls_data>.
      ASSIGN COMPONENT 'ALV_ACTION' OF STRUCTURE <ls_data> TO <lv_action>.
      ASSIGN COMPONENT mv_style_name OF STRUCTURE <ls_data> TO <lt_celltab>.
      IF <lv_action> IS INITIAL.
        ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
      ELSE.
        ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
      ENDIF.

      LOOP AT lt_fieldcat INTO ls_fieldcat.
        ls_celltab-fieldname = ls_fieldcat-fieldname.
        INSERT ls_celltab INTO TABLE <lt_celltab>.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.                    "set_style


  METHOD get_exclude.

    CALL BADI mo_badi->exclude_functions
      CHANGING
        ct_exclude = rt_exclude.

  ENDMETHOD.                    "get_exclude

  METHOD execute_user_command.

    CASE iv_ucomm.
      WHEN 'CHNG'.
        set_mode( 'C' ).
      WHEN 'DISP'.
        set_mode( 'D' ).
      WHEN 'SAVE'.
        save_data( ).
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                    "handle_user_command
  METHOD handle_user_command.
    DATA: lt_selected_rows TYPE lvc_t_row,
          ls_selected_rows LIKE LINE OF lt_selected_rows,
          lv_refresh TYPE c VALUE 'X',
          lo_exc TYPE REF TO zcx_generic_exception,
          lv_tr TYPE trkorr,
          lv_msg TYPE string,
          lo_transport TYPE REF TO data.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE,
                   <ls_data> TYPE any,
                   <ls_transport_data> TYPE any,
                   <lt_transport_data> TYPE STANDARD TABLE.


    TRY.
        CALL METHOD mo_alv->check_changed_data
          CHANGING
            c_refresh = lv_refresh.

        CALL METHOD mo_alv->get_selected_rows
          IMPORTING
            et_index_rows = lt_selected_rows.

        CASE e_ucomm.

          WHEN 'TRNS'.
            ASSIGN mo_data->* TO <lt_data>.
            CREATE DATA lo_transport LIKE <lt_data>.
            ASSIGN lo_transport->* TO <lt_transport_data>.
            LOOP AT lt_selected_rows INTO ls_selected_rows.
              READ TABLE <lt_data> ASSIGNING <ls_data> INDEX ls_selected_rows-index.
              APPEND INITIAL LINE TO <lt_transport_data> ASSIGNING <ls_transport_data>.
              MOVE-CORRESPONDING <ls_data> TO <ls_transport_data>.
            ENDLOOP.


            lv_tr = transport_entries( <lt_transport_data> ).

          WHEN 'DELETE'.
            delete_rows( ).

          WHEN 'COPY' OR 'INSRC'.
            copy_rows( ).

          WHEN OTHERS.
            ASSIGN mo_data->* TO <lt_data>.
            CALL BADI mo_badi->process_user_command
              EXPORTING
                iv_ucomm         = e_ucomm
                it_data          = <lt_data>
                it_selected_rows = lt_selected_rows.
        ENDCASE.

      CATCH zcx_generic_exception INTO lo_exc.
        lv_msg = lo_exc->get_message( ).
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.                    "handle_user_command
ENDCLASS.                    "lcl_table

DATA: lo_table TYPE REF TO lcl_table.


START-OF-SELECTION.

  DATA: lo_exception TYPE REF TO zcx_generic_exception,
        lv_message TYPE string.

  TRY.
      CREATE OBJECT lo_table
        EXPORTING
          iv_tabname = p_tab.

      lo_table->start( ).

    CATCH zcx_generic_exception INTO lo_exception.
      lv_message = lo_exception->get_message( ).
      MESSAGE lv_message TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.


END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display OUTPUT.
  lo_table->display_table( ).
ENDMODULE.                 " DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: lo_exc TYPE REF TO zcx_generic_exception,
        lv_msg TYPE string.

  TRY.
      lo_table->execute_user_command( ok_code ).
    CATCH zcx_generic_exception INTO lo_exc.
      lv_msg = lo_exc->get_message( ).
      MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_0100 INPUT.
  lo_table->free_objects( ).
  CLEAR lo_table.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " EXIT_0100  INPUT

Create a custom BADI ZSM30 with interface ZIF_EX_MAINTENANCE_VIEW.

interface ZIF_EX_MAINTENANCE_VIEW
  public .


  interfaces IF_BADI_INTERFACE .

  methods CHANGE_FIELDCATALOG
    changing
      !CT_FIELDCATALOG type LVC_T_FCAT .
  methods PROCESS_USER_COMMAND
    importing
      !IV_UCOMM type SY-UCOMM
      !IT_DATA type STANDARD TABLE
      !IT_SELECTED_ROWS type LVC_T_ROW
    raising
      ZCX_GENERIC_EXCEPTION .
  methods UPDATE_TOOLBAR
    changing
      !CT_TOOLBAR type TTB_BUTTON .
  methods BEFORE_SAVE
    changing
      !CT_DATA type STANDARD TABLE
    raising
      ZCX_GENERIC_EXCEPTION .
  methods VALIDATIONS_BEFORE_LOAD
    importing
      !IT_DATA type STANDARD TABLE
    changing
      value(CT_RETURN) type BAPIRET2_T
    raising
      ZCX_GENERIC_EXCEPTION .
  methods HANDLE_DATA_CHANGED
    importing
      !IO_DATA_CHANGED type ref to CL_ALV_CHANGED_DATA_PROTOCOL
    changing
      !CT_DATA type STANDARD TABLE .
  methods EXCLUDE_FUNCTIONS
    changing
      !CT_EXCLUDE type UI_FUNCTIONS .
  methods AUTHORITY_CHECK
    importing
      !IV_MODE type CHAR1
    changing
      value(CV_SUBRC) type SUBRC .
endinterface.



A fallback implementation class ZCL_EX_MAINTENANCE_VIEW.

class ZCL_EX_MAINTENANCE_VIEW definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces ZIF_EX_MAINTENANCE_VIEW .
protected section.
private section.
ENDCLASS.



CLASS ZCL_EX_MAINTENANCE_VIEW IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EX_MAINTENANCE_VIEW->ZIF_EX_MAINTENANCE_VIEW~BEFORE_SAVE
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CT_DATA                        TYPE        STANDARD TABLE
* | [!CX!] ZCX_GENERIC_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD zif_ex_maintenance_view~before_save.


ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EX_MAINTENANCE_VIEW->ZIF_EX_MAINTENANCE_VIEW~CHANGE_FIELDCATALOG
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CT_FIELDCATALOG                TYPE        LVC_T_FCAT
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD zif_ex_maintenance_view~change_fieldcatalog.

  FIELD-SYMBOLS <ls_fieldcatalog> LIKE LINE OF ct_fieldcatalog.

  LOOP AT ct_fieldcatalog ASSIGNING <ls_fieldcatalog>.
    CASE <ls_fieldcatalog>-fieldname.
      WHEN 'MANDT' OR 'AEDAT' OR 'AEZEIT' OR 'AENAM'.
        <ls_fieldcatalog>-no_out = abap_true.
    ENDCASE.
  ENDLOOP.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EX_MAINTENANCE_VIEW->ZIF_EX_MAINTENANCE_VIEW~HANDLE_DATA_CHANGED
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_DATA_CHANGED                TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
* | [<-->] CT_DATA                        TYPE        STANDARD TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD zif_ex_maintenance_view~handle_data_changed.

  DATA ls_good TYPE lvc_s_modi.

  FIELD-SYMBOLS: <ls_data> TYPE any,
                 <lv_field_value> TYPE any.

  LOOP AT io_data_changed->mt_good_cells INTO ls_good.
    READ TABLE ct_data ASSIGNING <ls_data> INDEX ls_good-row_id.
    ASSIGN COMPONENT ls_good-fieldname OF STRUCTURE  <ls_data> TO <lv_field_value>.
    CHECK <lv_field_value> IS ASSIGNED.
    <lv_field_value> = ls_good-value.
  ENDLOOP.


ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EX_MAINTENANCE_VIEW->ZIF_EX_MAINTENANCE_VIEW~PROCESS_USER_COMMAND
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_UCOMM                       TYPE        SY-UCOMM
* | [--->] IT_DATA                        TYPE        STANDARD TABLE
* | [--->] IT_SELECTED_ROWS               TYPE        LVC_T_ROW
* | [!CX!] ZCX_GENERIC_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
method ZIF_EX_MAINTENANCE_VIEW~PROCESS_USER_COMMAND.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EX_MAINTENANCE_VIEW->ZIF_EX_MAINTENANCE_VIEW~UPDATE_TOOLBAR
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CT_TOOLBAR                     TYPE        TTB_BUTTON
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD zif_ex_maintenance_view~update_toolbar.

  DATA ls_toolbar LIKE LINE OF ct_toolbar.

  ls_toolbar-function = 'TRNS'.
  ls_toolbar-quickinfo = 'Transport'.
  ls_toolbar-icon = icon_transport.
  APPEND ls_toolbar TO ct_toolbar.


ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EX_MAINTENANCE_VIEW->ZIF_EX_MAINTENANCE_VIEW~VALIDATIONS_BEFORE_LOAD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_DATA                        TYPE        STANDARD TABLE
* | [<-->] CT_RETURN                      TYPE        BAPIRET2_T
* | [!CX!] ZCX_GENERIC_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
method ZIF_EX_MAINTENANCE_VIEW~VALIDATIONS_BEFORE_LOAD.
endmethod.
ENDCLASS.
