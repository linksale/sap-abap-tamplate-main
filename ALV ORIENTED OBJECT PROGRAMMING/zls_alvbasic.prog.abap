*&---------------------------------------------------------------------*
*& Report ZLS_ALVBASIC
*&---------------------------------------------------------------------*
*&Report using OOPs with Basic functionalities.
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZLS_ALVBASIC.



TABLES : vbak.
TYPE-POOLS : abap, rsanm.

TYPES : BEGIN OF t_vbak.
INCLUDE TYPE vbak.
TYPES : t_color TYPE lvc_t_scol,
        END OF t_vbak.

DATA : lt_vbak TYPE STANDARD TABLE OF t_vbak,
       ls_vbak TYPE t_vbak.

FIELD-SYMBOLS <>  TYPE t_vbak.

DATA : l_rec(5)  TYPE n.

** Declaration for ALV Grid **
DATA : gr_table TYPE REF TO cl_salv_table.

** Declarations for ALV Functions
DATA : gr_functions TYPE REF TO cl_salv_functions_list.

** declaration for ALV Columns
DATA : gr_columns TYPE REF TO cl_salv_columns_table,
       gr_column  TYPE REF TO cl_salv_column_table,
       lt_column_ref TYPE salv_t_column_ref,
       ls_column_ref TYPE salv_s_column_ref.

** declaration for Layout Settings
DATA : gr_layout TYPE REF TO cl_salv_layout,
       gr_layout_key TYPE salv_s_layout_key,
       ls_layout TYPE salv_s_layout,
       lt_layout_info TYPE salv_t_layout_info.

** Declaration for Global Display Settings
DATA : gr_display TYPE REF TO cl_salv_display_settings,
       lv_title   TYPE lvc_title.

** Declaration for Aggregate Function Settings
DATA : gr_aggr    TYPE REF TO cl_salv_aggregations.

** Declaration for Sort Function Settings
DATA : gr_sort    TYPE REF TO cl_salv_sorts.

** Declaration for Table Selection settings
DATA : gr_select  TYPE REF TO cl_salv_selections.

** Declaration for Top of List settings
DATA : gr_content TYPE REF TO cl_salv_form_element.

** Class for handling Events
CLASS : lcl_handle_events DEFINITION DEFERRED.
DATA  : gr_events TYPE REF TO lcl_handle_events,
        lr_events TYPE REF TO cl_salv_events_table.

** Coloring of Date columns **
DATA : lt_colo TYPE STANDARD TABLE OF lvc_s_colo,
       ls_colo TYPE lvc_s_colo.

** Color Structure of columns **
DATA : lt_color TYPE lvc_t_scol,
       ls_color TYPE lvc_s_scol.
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS : on_double_click FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column.
    METHODS : on_link_click FOR EVENT link_click OF cl_salv_events_table
      IMPORTING row column.
ENDCLASS.                    "lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_double_click.
    PERFORM get_ord_info USING row column.
  ENDMETHOD.                    "on_double_click
  METHOD on_link_click.
    PERFORM get_ord_info USING row column.
  ENDMETHOD.                    "on_link_click
ENDCLASS.                    "lcl_handle_events IMPLEMENTATION
SELECTION-SCREEN BEGIN OF BLOCK b1.
SELECT-OPTIONS : p_erdat FOR vbak-erdat.
PARAMETER : p_var TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var.
  CLEAR : ls_layout.
  TRY.
      CALL METHOD cl_salv_table=>factory
        EXPORTING
          list_display = if_salv_c_bool_sap=>false
        IMPORTING
          r_salv_table = gr_table
        CHANGING
          t_table      = lt_vbak.
    CATCH cx_salv_msg .
  ENDTRY.

  IF gr_table IS NOT INITIAL.
    MOVE sy-repid TO gr_layout_key-report.     "Set Layout Key as Report ID"
    gr_layout = gr_table->get_layout( ).       "Get Layout of the Table"
    gr_layout->set_key( gr_layout_key ).       "Set Layout key to Layout"
    lt_layout_info = gr_layout->get_layouts( )."Get the Layouts of report"
    IF lt_layout_info[] IS NOT INITIAL.
      ls_layout = gr_layout->f4_layouts( ).    "Activate F4 Help for Layouts"
      IF ls_layout IS NOT INITIAL.
        MOVE ls_layout-layout TO p_var.
      ENDIF.
    ENDIF.
  ENDIF.

START-OF-SELECTION.
  REFRESH : lt_vbak.
  SELECT * FROM vbak
    INTO CORRESPONDING FIELDS OF TABLE lt_vbak
    WHERE erdat IN p_erdat.


  IF sy-subrc EQ 0.
    DESCRIBE TABLE lt_vbak LINES l_rec.
    PERFORM set_color.
    PERFORM alv_grid_display.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  alv_grid_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_grid_display.
  CLEAR : gr_table.
  TRY.
      CALL METHOD cl_salv_table=>factory
        EXPORTING
          list_display = if_salv_c_bool_sap=>false
        IMPORTING
          r_salv_table = gr_table
        CHANGING
          t_table      = lt_vbak.
    CATCH cx_salv_msg .
  ENDTRY.
  IF gr_table IS INITIAL.
    MESSAGE 'Error Creating ALV Grid ' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
** Get functions details
  gr_functions = gr_table->get_functions( ).

** Activate All Buttons in Tool Bar
  gr_functions->set_all( if_salv_c_bool_sap=>true ).

******* Layout Settings  *******
  CLEAR : gr_layout, gr_layout_key.
  MOVE sy-repid TO gr_layout_key-report.                        "Set Report ID as Layout Key"

  gr_layout = gr_table->get_layout( ).                          "Get Layout of Table"
  gr_layout->set_key( gr_layout_key ).                          "Set Report Id to Layout"
  gr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ). "No Restriction to Save Layout"
  IF p_var IS INITIAL.
    gr_layout->set_default( if_salv_c_bool_sap=>true ).         "Set Default Variant"
  ELSE.
    gr_layout->set_initial_layout( p_var ).                     "Set the Selected Variant as Initial"
  ENDIF.

******* Global Display Settings  *******
  CLEAR : gr_display.
  MOVE 'Sales Order Details' TO lv_title.
  gr_display = gr_table->get_display_settings( ).               " Global Display settings"
  gr_display->set_striped_pattern( if_salv_c_bool_sap=>true ).  "Activate Strip Pattern"
  gr_display->set_list_header( lv_title ).                      "Report Header"

******* Aggregate Function Settings *******
  gr_aggr = gr_table->get_aggregations( ).                      "Get Aggregate Functions"

******* Sort Functions *******
  gr_sort = gr_table->get_sorts( ).
  IF gr_sort IS NOT INITIAL.
    TRY.
        gr_sort->add_sort( columnname = 'ERNAM'
                           position = 1
                           sequence   = if_salv_c_sort=>sort_up
                           subtotal   = if_salv_c_bool_sap=>true
                           group      = if_salv_c_sort=>group_none
                           obligatory = if_salv_c_bool_sap=>false ).
      CATCH cx_salv_not_found .
      CATCH cx_salv_existing .
      CATCH cx_salv_data_error .
    ENDTRY.

    TRY.
        gr_sort->add_sort( columnname = 'ERDAT'
                           position = 2
                           sequence   = if_salv_c_sort=>sort_down
                           subtotal   = if_salv_c_bool_sap=>false
                           group      = if_salv_c_sort=>group_none
                           obligatory = if_salv_c_bool_sap=>false ).
      CATCH cx_salv_not_found .
      CATCH cx_salv_existing .
      CATCH cx_salv_data_error .
    ENDTRY.
  ENDIF.
******* Table Selection Settings *******
  gr_select = gr_table->get_selections( ).
  IF gr_select IS NOT INITIAL.
    gr_select->set_selection_mode( if_salv_c_selection_mode=>row_column ). "Allow single row Selection"
  ENDIF.

******* Top of List settings *******
  PERFORM top_of_page CHANGING gr_content.
  gr_table->set_top_of_list( gr_content ).

******* Event Register settings *******
  lr_events = gr_table->get_event( ).
  CREATE OBJECT gr_events.
  SET HANDLER gr_events->on_double_click FOR lr_events.
  SET HANDLER gr_events->on_link_click FOR lr_events.

** Get the columns from ALV Table
  gr_columns = gr_table->get_columns( ).
  IF gr_columns IS NOT INITIAL.
    REFRESH : lt_column_ref.
    CLEAR   : ls_column_ref.
    lt_column_ref = gr_columns->get( ).

** Get columns properties
    gr_columns->set_optimize( if_salv_c_bool_sap=>true ).
    gr_columns->set_key_fixation( if_salv_c_bool_sap=>true ).
    TRY.
        gr_columns->set_color_column( 'T_COLOR' ).
      CATCH cx_salv_data_error .
    ENDTRY.
** Individual Column Properties.
    PERFORM column_settings.
  ENDIF.
  CALL METHOD gr_table->display.
ENDFORM.                    "alv_grid_display

*&---------------------------------------------------------------------*
*&      Form  column_settings
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM column_settings.
  LOOP AT lt_column_ref INTO ls_column_ref.
    TRY.
        gr_column ?= gr_columns->get_column( ls_column_ref-columnname ).
      CATCH cx_salv_not_found.
    ENDTRY.
    IF gr_column IS NOT INITIAL.
** Make Mandt column invisible **
      IF gr_column->get_ddic_datatype( ) = 'CLNT'.
        gr_column->set_technical( if_salv_c_bool_sap=>true ).
      ENDIF.
** Create Aggregate function total for All Numeric/Currency Fields **
      IF gr_column->get_ddic_inttype( ) EQ 'P' OR
         gr_column->get_ddic_datatype( ) EQ 'CURR'.
        IF gr_aggr IS NOT INITIAL.
          TRY.
              gr_aggr->add_aggregation( columnname = ls_column_ref-columnname
                                        aggregation = if_salv_c_aggregation=>total ).
            CATCH cx_salv_data_error .
            CATCH cx_salv_not_found .
            CATCH cx_salv_existing .
          ENDTRY.
        ENDIF.
      ENDIF.
** Create Check box for fields with domain "XFELD"
      IF gr_column->get_ddic_domain( ) EQ 'XFELD'.
        gr_column->set_cell_type( if_salv_c_cell_type=>checkbox ).
      ENDIF.

** Set color to Date Columns **
      IF gr_column->get_ddic_datatype( ) EQ 'DATS'.
        CLEAR : ls_colo.
        MOVE 2 TO ls_colo-col.
        MOVE 1 TO ls_colo-int.
        MOVE 1 TO ls_colo-inv.
        gr_column->set_color( ls_colo ).
      ENDIF.

** Add Hotspot&Hyper Link to the column vbeln
      IF ls_column_ref-columnname EQ 'VBELN'.
        gr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
        gr_column->set_key( if_salv_c_bool_sap=>true ).
      ENDIF.

    ENDIF.
  ENDLOOP.
ENDFORM.                    "column_settings

*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
FORM top_of_page CHANGING lr_content TYPE REF TO cl_salv_form_element.
  DATA : lr_grid TYPE REF TO cl_salv_form_layout_grid,
         lr_text  TYPE REF TO cl_salv_form_text,
         lr_label TYPE REF TO cl_salv_form_label,
         lr_head  TYPE string.

  MOVE 'Sales Order List' TO lr_head.
  CREATE OBJECT lr_grid.
** Header of Top of Page **
  lr_grid->create_header_information( row     = 1
                                      column  = 1
                                      text    = lr_head
                                      tooltip = lr_head ).
** Add Row **
  lr_grid->add_row( ).

** Add Label in Grid **
  lr_label = lr_grid->create_label( row = 2
                                    column = 1
                                    text = 'No of Records'
                                    tooltip = 'No of Records' ).

** Add Text in The Grid **
  lr_text = lr_grid->create_text( row = 2
                                  column = 2
                                  text = l_rec
                                  tooltip = l_rec ).
** Set Label and Text Link **
  lr_label->set_label_for( lr_text ).

** Move lr_grid to lr_content **
  lr_content = lr_grid.
ENDFORM.                    "top_of_page

*&---------------------------------------------------------------------*
*&      Form
*&---------------------------------------------------------------------*
FORM get_ord_info USING row TYPE salv_de_row
                        column TYPE salv_de_column.
  IF column EQ 'VBELN'.
    CLEAR : ls_vbak.
    READ TABLE lt_vbak INTO ls_vbak INDEX row.
    IF sy-subrc EQ 0.
      SET PARAMETER ID 'AUN' FIELD ls_vbak-vbeln.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDIF.
ENDFORM.                    "get_ord_info

*&---------------------------------------------------------------------*
*&      Form  set_color
*&---------------------------------------------------------------------*
* Set color to the column VBELN & NETWR for Order Value > 1000
*----------------------------------------------------------------------*
FORM set_color.
  LOOP AT  lt_vbak ASSIGNING.
    IF netwr GT 1000.
      REFRESH : lt_color.

      CLEAR : ls_color.
      MOVE 'VBELN' TO ls_color-fname.
      MOVE 3       TO ls_color-color-col.
      MOVE 0       TO ls_color-color-int.
      MOVE 0       TO ls_color-color-inv.
      APPEND ls_color TO lt_color.

      CLEAR : ls_color.
      MOVE 'NETWR' TO ls_color-fname.
      MOVE 3       TO ls_color-color-col.
      MOVE 0       TO ls_color-color-int.
      MOVE 0       TO ls_color-color-inv.
      APPEND ls_color TO lt_color.

      MOVE lt_color TO -t_color.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "set_color
