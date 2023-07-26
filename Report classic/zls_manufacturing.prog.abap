*&---------------------------------------------------------------------*
*& Report ZLS_MANUFACTURING
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

************************************************************************
* PROGRAM ID           : ZLS_MANUFACTURING
* PROGRAM TITLE        : planned and Actual Order Report               *
* DEVELOPMENT ID       : S4USER050                                     *
* CHANGE REQUEST NUMBER: S4HK902002                                     *
* DESCRIPTION          :  This Report is to check the progress of       *
*                        orders on that resource and also planned      *
*                        orders on that resource.                      *
*                        SUBROUTINES                                   *                  *
*======================================================================*
*

REPORT ZLS_MANUFACTURING.

** Top inlcude for Global Data Declarations
INCLUDE ZLSMANF_PLANVSACT_TOP.

**Top include for Selection Screen
INCLUDE ZLSMANF_PLANCSACT_SEL.

** Include for Performs
INCLUDE ZLSMANFSUB_PLANVSACT_F01.




*------------ INITIALIZATION -------------------------------*
INITIALIZATION.
  PERFORM fill_dates.

*------------ AT SELECTION-SCREEN --------------------------*
AT SELECTION-SCREEN ON p_werks.
  PERFORM validate_plant.

AT SELECTION-SCREEN ON s_wctr.
  IF NOT s_wctr[] IS INITIAL.
    PERFORM  validate_workcenter.
  ENDIF.

AT SELECTION-SCREEN ON p_now.
  PERFORM validate_weeks.

AT SELECTION-SCREEN ON s_mtype.
  IF NOT s_mtype[] IS INITIAL.
    PERFORM validate_material_type.
  ENDIF.

AT SELECTION-SCREEN ON s_matno.
  IF NOT s_matno[] IS INITIAL.
    PERFORM validate_matno.
  ENDIF.

AT SELECTION-SCREEN ON s_orstat.
  IF NOT s_orstat[] IS INITIAL.
    PERFORM validate_order_status.
  ENDIF.

AT SELECTION-SCREEN .
  PERFORM date_validations.
  IF NOT s_mtype[] IS INITIAL AND NOT s_matno[] IS INITIAL.
    PERFORM validate_matnr_mtart.
  ENDIF.

*------------ START OF SELECTION -----------------------------------*
START-OF-SELECTION.

*---------------Set the GUI status ----------------------------------*
  SET PF-STATUS c_main.
  PERFORM select_materials.

*-----------ACTUAL ORDERS  DATA RETRIEVAL----------------------------*
  PERFORM select_actual_orders.
  PERFORM select_status.
  PERFORM select_workcenter_details.
  PERFORM select_document_segment_info.    "From MSEG TABLE
  PERFORM select_finalactual_orders.

*---------PLANNED ORDERS DATA RETRIEVAL------------------------------*
  IF cb_plan = c_x.
    PERFORM select_planned_orders.
    PERFORM select_production_versions.  "Data from MKAL.
    PERFORM select_finalplanned_orders.
  ENDIF.

*------------------END OF SELECTION----------------------------------*
  IF i_finalord[] IS INITIAL.
    MESSAGE s040. "No Orders found for Selection Criteria
    LEAVE LIST-PROCESSING.
  ENDIF.

END-OF-SELECTION.
* Report Header displayed only once based on client feedback
*  PERFORM display_report_header.
  IF rb_week = c_x.
    PERFORM merge_data.
    PERFORM display_weekly_report.
*   PERFORM display_report_footer.
  ELSEIF rb_daily = c_x.
    PERFORM display_daily_report.
*   PERFORM display_report_footer.
  ENDIF.

*------------------TOP OF PAGE-------------------------------------*
* TOP-OF-PAGE event commented out based on client feedback.
* TOP-OF-PAGE
*  PERFORM display_report_header.

*------AT LINE SELECTION--------------------------------------------*
AT LINE-SELECTION.
  IF rb_week = c_x.
    PERFORM display_flow USING rec_weekly-aufnr.
  ELSE.
    PERFORM display_flow USING rec_finalord-aufnr.
  ENDIF.

*-----------AT USER COMMAND-----------------------------------------*
AT USER-COMMAND.

  CASE sy-ucomm.
    WHEN c_exit OR c_canc.                    "Exit or Cancel
      LEAVE LIST-PROCESSING.
    WHEN c_ord.                               "Order UoM
      CLEAR: rb_kgm, rb_fval.
      rb_ord = c_x. rb_quan = c_x.
      IF rb_week = c_x.
*       PERFORM display_report_header.
        PERFORM merge_data.
        PERFORM display_weekly_report.
*       PERFORM display_report_footer.
      ELSE.
*       PERFORM display_report_header.
        PERFORM display_daily_report.
*       PERFORM display_report_footer.
      ENDIF.
    WHEN c_kg.                                "KG UoM
      CLEAR: rb_ord, rb_fval.
      rb_kgm = c_x. rb_quan = c_x.
      IF rb_week = c_x.
*       PERFORM display_report_header.
        PERFORM merge_data.
        PERFORM display_weekly_report.
*       PERFORM display_report_footer.
      ELSE.
*       PERFORM display_report_header.
        PERFORM display_daily_report.
*       PERFORM display_report_footer.
      ENDIF.
    WHEN c_fisc.                               "Fiscal Values
      CLEAR: rb_quan.
      rb_fval = c_x.
      IF rb_week = c_x.
*       PERFORM display_report_header.
        PERFORM merge_data.
        PERFORM display_weekly_report.
*       PERFORM display_report_footer.
      ELSE.
*       PERFORM display_report_header.
        PERFORM display_daily_report.
*       PERFORM display_report_footer.
      ENDIF.
    WHEN c_det.                               "Details
      CLEAR : rb_sum.
      if rb_week = c_x.
       rb_det = c_x.
*      PERFORM display_report_header.
       PERFORM display_weekly_report.
*      PERFORM display_report_footer.
      else.
       message i045. "This Button works only for weekly report
      endif.
    WHEN c_summ.                              "Summary
      CLEAR: rb_det.
      if rb_week = c_x.
       rb_sum = c_x.
*      PERFORM display_report_header.
       PERFORM display_weekly_report.
*      PERFORM display_report_footer.
      else.
       message i045. "This Button works only for weekly report
      endif.
  ENDCASE.
