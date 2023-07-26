*&---------------------------------------------------------------------*
*&  Include           ZLSMANF_PLANCSACT_SEL
*&---------------------------------------------------------------------*
************************************************************************
* PROGRAM ID           : ZLS_MANUFACTURING
* PROGRAM TITLE        : planned and Actual Order Report            *
* DEVELOPMENT ID       : S4USER050                                     *
* CHANGE REQUEST NUMBER: S4HK902002                                     *
* DESCRIPTION          : This is to check the progress of       *
*                        orders on that resource and also planned      *
*                        orders on that resource of sel-screen.                      *
*                        SELECTION SCREEN                              *                   *
*======================================================================*
*
*                                                                      *
*----------------------------------------------------------------------*

************************************************************************
* SELECTION SCREEN
************************************************************************
*-----------------Selection Criteria.--------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE text-001.
PARAMETERS:     p_werks TYPE werks_d MEMORY ID wrk OBLIGATORY. "Plant
SELECT-OPTIONS: s_wctr FOR crhd-arbpl MEMORY ID agr
                           MATCHCODE OBJECT cram,      "Resource
                s_date FOR sy-datum OBLIGATORY NO-EXTENSION .
PARAMETERS:     p_now(4)  TYPE n .
SELECT-OPTIONS: s_mtype FOR t134-mtart MEMORY ID mta,  "Material Type
                s_matno FOR mara-matnr MEMORY ID mat.  "Material Number
SELECT-OPTIONS: s_orstat FOR v_dummy
                           MATCHCODE OBJECT /EUR/RGBPTE_ORDSTAT.
                                                       "Order Status
SELECTION-SCREEN END OF BLOCK a.

*----------------- Display Options.--------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE text-002.
PARAMETERS: cb_color AS CHECKBOX DEFAULT c_x.  "Colours
PARAMETERS: cb_plan AS CHECKBOX.               "Planned Orders

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rb_quan RADIOBUTTON GROUP gr2.     "Quantities
SELECTION-SCREEN POSITION 15.
SELECTION-SCREEN COMMENT 15(20) text-023 FOR FIELD rb_quan.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE .
SELECTION-SCREEN POSITION 5.
PARAMETERS: rb_ord RADIOBUTTON GROUP grp1.     "Order Quantities
SELECTION-SCREEN COMMENT 10(14) text-017 FOR FIELD rb_ord.
PARAMETERS: rb_kgm RADIOBUTTON GROUP grp1.     "KG Quantities
SELECTION-SCREEN POSITION 30.
SELECTION-SCREEN COMMENT 30(20) text-018 FOR FIELD rb_kgm.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rb_fval RADIOBUTTON GROUP gr2.     "Fiscal Values
SELECTION-SCREEN POSITION 15.
SELECTION-SCREEN COMMENT 15(20) text-024 FOR FIELD rb_fval.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b.

*----------Report Type----------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK c WITH FRAME TITLE text-003.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rb_week RADIOBUTTON GROUP gr3.     "Weekly Report
SELECTION-SCREEN POSITION 15.
SELECTION-SCREEN COMMENT 15(20) text-021 FOR FIELD rb_week.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE .
SELECTION-SCREEN POSITION 5.
PARAMETERS: rb_det RADIOBUTTON GROUP gr4.      "Summary Report
SELECTION-SCREEN COMMENT 10(10) text-019 FOR FIELD rb_det.
PARAMETERS: rb_sum RADIOBUTTON GROUP gr4.
SELECTION-SCREEN POSITION 30.
SELECTION-SCREEN COMMENT 35(20) text-020 FOR FIELD rb_sum.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rb_daily RADIOBUTTON GROUP gr3.    "Daily Report
SELECTION-SCREEN POSITION 15.
SELECTION-SCREEN COMMENT 15(20) text-022 FOR FIELD rb_daily.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK c.
