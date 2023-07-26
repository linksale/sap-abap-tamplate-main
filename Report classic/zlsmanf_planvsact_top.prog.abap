*&---------------------------------------------------------------------*
*&  Include           ZLSMANF_PLANVSACT_TOP
*&---------------------------------------------------------------------*
************************************************************************


***************************************************************
* DATA DECLARATIONS
***************************************************************
*------------ Tables ----------------------------------------*

TABLES: crhd, mara, t134, t001w, t001k.

*-------------------constants-------------------------------*
CONSTANTS: c_x         TYPE c         VALUE 'X',        "CheckValue
           c_a         TYPE c         VALUE 'A',        "WrkCenterType
           c_101       TYPE bwart     VALUE '101',      "Mvmt Type 101
           c_102       TYPE bwart     VALUE '102',      "Mvmt Type 102
           c_kgm       TYPE meins     VALUE 'KG',       "KG
           c_na(4)     TYPE c         VALUE 'N/A',      "NotApplicable
           c_date      TYPE sy-datum  VALUE '20031103', "Default Date
           c_null(3)   TYPE c         VALUE '***',      "NullValues
           c_i         TYPE c         VALUE 'I' ,       "Include
           c_bt(2)     TYPE c         VALUE 'BT',       "Between
           c_br1(3)    TYPE c         VALUE 'BR1',      "Memory-ID
           c_tcode(4)  TYPE c         VALUE 'COR3',     "TranasCode
           c_1         TYPE c         VALUE '1',            "wkdy 1-Mon
           c_2         TYPE c         VALUE '2',        "wkday 2-Tue
           c_3         TYPE c         VALUE '3',        "wkday 3-Wed
           c_4         TYPE c         VALUE '4',        "wkday 4-Thu
           c_5         TYPE c         VALUE '5',        "wkday 5-Fri
           c_6         TYPE c         VALUE '6',        "wkday 6-Sat
           c_7         TYPE c         VALUE '7',        "wkday 7-Sun
           c_main(4)   TYPE c         VALUE 'MAIN',     "PF Status
           c_ord(3)    TYPE c         VALUE 'ORD',      "Order Unit
           c_kg(6)     TYPE c         VALUE 'KGUNIT',   "KG unit
           c_fisc(4)   TYPE c         VALUE 'FISC',     "Fiscal values
           c_det(3)    TYPE c         VALUE 'DET',      "Detail
           c_summ(4)   TYPE c         VALUE 'SUMM',     "Summary
           c_exit(4)   TYPE c         VALUE 'EXIT',     "Exit
           c_canc(4)   TYPE c         VALUE 'CANC'.     "Cancel

*------------ flag variables--------------------------------*
DATA: flg_matsel(1) TYPE c,  "flag for Material Selection
      flg_ordsel(1) TYPE c,  "flag for order selection
      flg_stat(1)   TYPE c.  "flag for status

*------------  variables--------------------------------*
DATA: v_dummy(4)  TYPE c,         "Dummy variable for Ord. Status
      v_waers     TYPE waers,     "Currency Key
      v_text1     TYPE name1,     "Name Of the Palnt
      v_text3(60) TYPE c,         "Report Header String
      v_monday    TYPE sy-datum,  "Default Mon Date
      v_sunday    TYPE sy-datum,  "Default Sun Date
      v_fmday     TYPE sy-datum,  "Variable To Hold Date
      v_rtext     TYPE cr_ktext.  "Resource Description

*----------------Type Definitions--------------------------*
TYPES: difftype TYPE p DECIMALS 3,
       disptype TYPE p DECIMALS 0.

*------------------- Types ---------------------------------*
* Type for Materials Selection.
TYPES: BEGIN OF ty_mara,
        matnr TYPE matnr,    "Material Number
        mtart TYPE mtart,    "Material Type
        meins TYPE meins,    "Unit of Measure
       END OF ty_mara.

*Types for Material Description
TYPES: BEGIN OF ty_matdesc,
        matnr TYPE matnr,     "Material Number
        maktx TYPE maktx,     "Material Description
       END OF ty_matdesc.

*Type for Orders Selection
TYPES: BEGIN OF ty_caufv,
        aufnr  TYPE caufv-aufnr,  "Order Number
        objnr  TYPE caufv-objnr,  "Object Number
        gltrp  TYPE caufv-gltrp,  "Basic Finish Date
        gstrp  TYPE caufv-gstrp,  "Basic Start Date
        gamng  TYPE caufv-gamng,  "Total Order Quantity
        gmein  TYPE caufv-gmein,  "Unit of Measure
        plnbez TYPE caufv-plnbez, "Material Number
        aufpl  TYPE caufv-aufpl,  "Routing Number
       END OF ty_caufv.

** Types for all phases of orders
TYPES: BEGIN OF ty_afvv_afvc,
        aufpl TYPE co_aufpl,   "Routing Number
        vornr TYPE vornr,      "Operation Number
        arbid TYPE cr_objid,   "Objid Of Resource
       END OF ty_afvv_afvc.

** Types for Work Center Details
TYPES:  BEGIN OF ty_crhd,
         objid TYPE cr_objid,  "Objid Of Resource
         arbpl TYPE arbpl,     "WorkCenter
        END OF ty_crhd.

**Types for material document segment
TYPES: BEGIN OF ty_mseg,
        mblnr TYPE mblnr,    "Document Number
        mjahr TYPE mjahr,    "Document Year
        zeile TYPE mblpo,    "Counter
        bwart TYPE bwart,    "Movement Type
        matnr TYPE matnr,    "Material Number
        aufnr TYPE aufnr,    "Process Order Number
        menge TYPE menge_d,  "Quantity
       END OF ty_mseg.

**Types for system Status
TYPES: BEGIN OF ty_tj02t,
        txt04 TYPE j_txt04,  "Individual Status Of an Object
       END OF ty_tj02t.

**Types for Planned Orders.
TYPES: BEGIN OF ty_plaf,
        matnr TYPE matnr,    "Material Number
        plnum TYPE plnum,    "Planned Order Number
        verid TYPE verid,    "Production Version
        gsmng TYPE gsmng,    "Total Planned Order Quantity
        pedtr TYPE pedtr,    "Order Finish Date
        pertr TYPE pertr,    "Planned Opening date
        meins TYPE meins,   " Unit of measure
       END OF ty_plaf.

**Types for Material Valuation
TYPES: BEGIN OF ty_price,
        matnr TYPE matnr,     "MaterialNumber
        stprs TYPE stprs,     "Standard Price
        peinh TYPE peinh,     "Price Unit.
       END OF ty_price.

**Types For Production Versions
TYPES: BEGIN OF ty_mkal,
        matnr  TYPE matnr,    "MaterialNumber
        mdv01 TYPE mdv01,     "Production Line.
       END OF ty_mkal.

** Types for Final Orders
TYPES: BEGIN OF ty_finalord,
        wdate   TYPE sydatum,     "Week date
        arbpl   TYPE arbpl,       "Resource /Work Center
        matnr   TYPE matnr,       "material Number
        aufnr   TYPE aufnr,       "process order Number
        spwoc   TYPE scal-week,   "week day/period to analyze
        mtart   TYPE mtart,       "Material Type
        maktx   TYPE maktx,       "Material Description
        gstrp   TYPE dats,        "Basic start date
        gltrp   TYPE dats,        "Basic End Date
        gamng   TYPE disptype,    "Planned quantity
        menge   TYPE disptype,    "Actual Quantity
        diff    TYPE disptype,    "Difference
        meins   TYPE meins,       "Unit Of Mesaure
        zplan   TYPE disptype,    "KG planned qty
        zact    TYPE disptype,    "KG actual qty
        zdiff   TYPE disptype,    "KG diff
        zmeins  TYPE meins,       "KG uom
        fplan   TYPE disptype,    "Fiscal Planned Qty
        fact    TYPE disptype,    "Fiscal Actual Qty
        fdiff   TYPE disptype,    "Fiscal Difference
        fcurr   TYPE waers,       "Fiscal Currency
        wkday   TYPE week_day,    "Week day
       END OF ty_finalord.

** Types For Getting the week names.
TYPES: BEGIN OF ty_wkday,
        wkday TYPE week_day,     "WeekDay
       END OF ty_wkday.

** Types for week day material uom details,
TYPES: BEGIN OF ty_week1,
        spwoc TYPE spwoc,      "Week
        wkday TYPE week_day,   "Week Day
        arbpl TYPE arbpl,      "Resource
        matnr TYPE matnr,      "Material No
        meins TYPE meins,      "UOM
       END OF ty_week1.
* Types for week day resource uom details
TYPES: BEGIN OF ty_week2,
        spwoc TYPE spwoc,      "Week
        wkday TYPE week_day,   "Week Day
        arbpl TYPE arbpl,      "Resource
        meins TYPE meins,      "UOM
       END OF ty_week2.
* Types for week day uom details
TYPES: BEGIN OF ty_week3,
       spwoc TYPE spwoc,     "Week
       wkday TYPE week_day,  "Week Day
       meins TYPE meins,     "UOM
       END OF ty_week3.

* Types for Weekly Report Details
TYPES: BEGIN OF ty_weekly,
        spwoc      TYPE scal-week,  "Week
        arbpl      TYPE arbpl,      "Resource
        matnr      TYPE matnr,      "Material Number
        aufnr      TYPE aufnr,      "Order Number
        mtart      TYPE mtart,      "Material Type
        maktx      TYPE maktx,      "Material Description
        mon_plan   TYPE disptype,   "Mon Planned Qty
        mon_act    TYPE disptype,   "Mon Actual Qty
        mon_diff   TYPE disptype,   "Mon Diff Qty
        mon_perc   TYPE disptype,   "Mon Perc
        tue_plan   TYPE disptype,   "Tue Planned
        tue_act    TYPE disptype,   "Tue Actual
        tue_diff   TYPE disptype,   "Tue Diff
        tue_perc   TYPE disptype,   "Tue Percentage
        wed_plan   TYPE disptype,   "Wed Planned
        wed_act    TYPE disptype,   "Wed actual
        wed_diff   TYPE disptype,   "Wed Diff
        wed_perc   TYPE disptype,   "Wed Perc
        thu_plan   TYPE disptype,   "Thu Planned
        thu_act    TYPE disptype,   "Thu actual
        thu_diff   TYPE disptype,   "Thu Diff
        thu_perc   TYPE disptype,   "Thu Perc
        fri_plan   TYPE disptype,   "Fri Planned
        fri_act    TYPE disptype,   "Fri Actual
        fri_diff   TYPE disptype,   "Fri diff
        fri_perc   TYPE disptype,   "Fri Perc
        sat_plan   TYPE disptype,   "Sat Planned
        sat_act    TYPE disptype,   "Sat Actual
        sat_diff   TYPE disptype,   "Sat Diff
        sat_perc   TYPE disptype,   "Sat Perc
        sun_plan   TYPE disptype,   "Sun Plan
        sun_act    TYPE disptype,   "Sun Actual
        sun_diff   TYPE disptype,   "Sun Diff
        sun_perc   TYPE disptype,   "Sun perc
        tot_plan   TYPE disptype,   "Tot planned
        tot_act    TYPE disptype,   "Total Actual
        tot_diff   TYPE disptype,   "Total Diff
        tot_perc   TYPE disptype,   "Total Perc
        meins      TYPE meins,      "UOM
        waers      TYPE waers,      "Currency Key
       END OF ty_weekly.

*------------------- Internal Tables -----------------------*
**Internal table for material Selection.
DATA:  i_mara TYPE STANDARD TABLE OF ty_mara
       INITIAL SIZE 0 WITH HEADER LINE.
**Internal table for Order Selection.
DATA:  i_caufv TYPE  STANDARD TABLE OF  ty_caufv
       INITIAL SIZE 0  WITH HEADER LINE.
**Internal table for all phases of data.
DATA:  i_afvv_afvc TYPE STANDARD TABLE OF ty_afvv_afvc
       INITIAL SIZE 0  WITH HEADER LINE.
**Internal table for order header data.
DATA:  i_tj02t TYPE STANDARD TABLE OF ty_tj02t
       INITIAL SIZE 0 WITH HEADER LINE.
**Internal table for planned Orders.
DATA:  i_plaf TYPE STANDARD TABLE OF ty_plaf
       INITIAL SIZE 0 WITH HEADER LINE.
**Internal Table Declaration for Production Versions of Material
DATA:  i_mkal TYPE STANDARD TABLE OF ty_mkal
       INITIAL SIZE 0  WITH HEADER LINE.
**Internal Table Declaation for final orders.
DATA:  i_finalord TYPE STANDARD TABLE OF ty_finalord
       INITIAL SIZE 0 WITH HEADER LINE.
**Internal Table Declartion for Work Center Details
DATA:  i_crhd TYPE STANDARD TABLE OF ty_crhd
       INITIAL SIZE 0 WITH HEADER LINE.
**Internal Table Declartion for Material document segemnt
DATA:  i_mseg TYPE STANDARD TABLE OF ty_mseg
       INITIAL SIZE 0 WITH HEADER LINE.
**Internal Table Declartion for WeekNames
DATA:  i_wkday TYPE STANDARD TABLE OF ty_wkday
       INITIAL SIZE 0 WITH HEADER LINE.
** Internal table to populate Week day resource material UOM details.
DATA:  i_week1 TYPE STANDARD TABLE OF ty_week1
       INITIAL SIZE 0 WITH HEADER LINE.
** internal table to populate week day resource uom details
DATA:  i_week2 TYPE STANDARD TABLE OF ty_week2
       INITIAL SIZE 0 WITH HEADER LINE.
**internal table to populate wekday uom Details.
DATA:  i_week3 TYPE STANDARD TABLE OF ty_week3
       INITIAL SIZE 0 WITH HEADER LINE.
**Internal Table Declaration for weekly report .
DATA:  i_weekly TYPE STANDARD TABLE OF ty_weekly
       INITIAL SIZE 0 WITH HEADER LINE.

**Work Area Declarations
DATA: rec_finalord TYPE ty_finalord,
      rec_weekly TYPE ty_weekly.
