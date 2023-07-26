*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&  Include           ZLSMANFSUB_PLANVSACT_F01                      *
*&---------------------------------------------------------------------*


************************************************************************

* PROGRAM TITLE        : planned and Actual Order Report            *
* DEVELOPMENT ID       : S4USER050                                     *
* CHANGE REQUEST NUMBER: S4HK902002                                     *
* DESCRIPTION          :  This Report is to check the progress of       *
*                        orders on that resource and also planned      *
*                        orders on that resource.                      *
*                        SUBROUTINES                                   *                  *
*======================================================================*
*

*                                                                      *



*Text elements
*----------------------------------------------------------
* 017 Order Quantities
* 018 KGM Quantities
* 019 Detail Report
* 020 Summary Report
* 021 Weekly Report
* 022 Daily Report
* 023 Quantities
* 024 Fiscal Values
* 030 Weekly Report
* 032 Order Quantities

*Selection texts
*----------------------------------------------------------
* CB_COLOR         Display Color Legend
* CB_PLAN         Planned Orders
* P_NOW         Number of weeks
* P_WERKS         Plant
* RB_DAILY         Daily Report
* RB_DET         Detail
* RB_FVAL         Fiscal Values
* RB_KGM         KGM Conversion
* RB_ORD         Orders Unit
* RB_QUAN         Quantities
* RB_SUM         Summary
* RB_WEEK         Weekly Report
* S_DATE         Date to define first week
* S_MATNO         Material
* S_MTYPE         Material Type
* S_ORSTAT         Order Status Contains
* S_WCTR         Work Center

*Messages
*----------------------------------------------------------
*
* Message class: RGBPT_DEV
*001
*009
*010
*011
*015
*033
*035
*038
*039
*040
*043
*044
*045


*&---------------------------------------------------------------------*
*&   FORM  FILL_DATES
*&---------------------------------------------------------------------*
*    Subroutine to fill default dates.
*----------------------------------------------------------------------*


FORM   fill_dates .
  DATA:   l_dayofweek(1) TYPE  c,   "aux. day of week
          l_auxdate      TYPE dats. "var to hold auxilary date.

  CALL FUNCTION 'DATE_COMPUTE_DAY'
    EXPORTING
      date = sy-datum
    IMPORTING
      day  = l_dayofweek.

  l_auxdate = sy-datum + 1 - l_dayofweek.
  s_date-low = l_auxdate.
  s_date-high = s_date-low + 6.
  s_date-option = c_bt.
  s_date-sign = c_i.
  APPEND s_date.

ENDFORM.                    " fill_dates

*&---------------------------------------------------------------------*
*&   FORM VALIDATE_PLANT
*&---------------------------------------------------------------------*
*   subroutine to validate_plant
*----------------------------------------------------------------------*
FORM  validate_plant.

  SELECT SINGLE name1 bwkey
           FROM t001w
           INTO (v_text1 , t001w-bwkey)
           WHERE werks = p_werks.
  IF sy-subrc NE 0.
    MESSAGE e001 WITH p_werks. "Enter a valid Plant
  ELSE.
* Do Authority Check
    AUTHORITY-CHECK OBJECT 'C_AFKO_AWK'
             ID 'WERKS' FIELD p_werks
             ID 'AUFART' FIELD 'DUMMY'.
    IF sy-subrc <> 0.
      MESSAGE e035 WITH p_werks.
      "Authorization failed for displaying Process Orders in Plant
    ENDIF.

    SELECT SINGLE bukrs
           FROM t001k
           INTO t001k-bukrs
           WHERE bwkey = t001w-bwkey.
    IF sy-subrc <> 0 OR t001k-bukrs IS INITIAL.
      MESSAGE e009 WITH t001w-bwkey.
      "No Company Code found for Valuation Area
    ENDIF.

    SELECT SINGLE waers
           FROM t001
           INTO v_waers
           WHERE bukrs = t001k-bukrs.
    IF sy-subrc <> 0 OR v_waers IS INITIAL.
      MESSAGE e010 WITH t001k-bukrs. "No Currency found for Company Code
    ENDIF.
  ENDIF.
ENDFORM.                    " Validate_plant

*&---------------------------------------------------------------------*
*&     FORM VALIDATE_WORKCENTER
*&---------------------------------------------------------------------*
*      subroutine to validate work center
*----------------------------------------------------------------------*
FORM validate_workcenter.
  SELECT arbpl
    UP TO 1 ROWS
    FROM crhd
    INTO crhd-arbpl
    WHERE arbpl IN s_wctr.
  ENDSELECT.

  IF sy-subrc <> 0.
    MESSAGE e038. "Entered Resource is Invalid
  ENDIF.
ENDFORM.                    " validate_workcenter

*&---------------------------------------------------------------------*
*&      FORM  VALIDATE_WEEKS
*&---------------------------------------------------------------------*
*      subroutine to validate Number of weeks.
*----------------------------------------------------------------------*
FORM validate_weeks .
  IF NOT s_date-low IS INITIAL
  AND NOT s_date-high IS INITIAL
  AND NOT p_now IS INITIAL.

    MESSAGE e043. "No Of Weeks Cannot be Entered
                  "As Date Range is Filled
  ENDIF.
ENDFORM.                    " validate_weeks

&---------------------------------------------------------------------*
*&     FORM  VALIDATE_MATERIAL_TYPE
*&---------------------------------------------------------------------*
*    subroutine to validate material type
*----------------------------------------------------------------------*
FORM validate_material_type .
  SELECT  mtart
    UP TO 1 ROWS
    FROM t134
    INTO t134-mtart
    WHERE mtart  IN s_mtype.
  ENDSELECT.
  IF sy-subrc <> 0.
    MESSAGE e011. "Entered Material Type(s) is invalid
  ENDIF.
ENDFORM.                    " validate_material_type

&---------------------------------------------------------------------*
*&      FORM  VALIDATE_MATNO
*&---------------------------------------------------------------------*
*     subroutine to validate material Number
*----------------------------------------------------------------------*
FORM  validate_matno .
  SELECT matnr
   UP TO 1 ROWS
    FROM mara
    INTO mara-matnr
    WHERE matnr IN s_matno.
  ENDSELECT.
  IF sy-subrc <> 0.
    MESSAGE e015. "Entered Material(s) is invalid
  ENDIF.
ENDFORM.                    " validate_matno

*&---------------------------------------------------------------------*
*&      FORM  VALIDATE_ORDER_STATUS
*&---------------------------------------------------------------------*
*      subroutine to validate order status
*----------------------------------------------------------------------*
FORM validate_order_status .
  data: l_txt04 type j_txt04.

  SELECT txt04
   UP TO 1 ROWS
   FROM tj02t
   INTO l_txt04
   WHERE spras = sy-langu
   AND   txt04 IN s_orstat.
  ENDSELECT.
  IF sy-subrc NE 0.
    MESSAGE e039. "Entered Order Status is Invalid
  ENDIF.
ENDFORM.                    " validate_order_status

*&---------------------------------------------------------------------*
*&      FORM  DATE_VALIDATIONS
*&---------------------------------------------------------------------*
*       subroutine for date validations
*----------------------------------------------------------------------*
FORM date_validations .

  IF  s_date-high IS INITIAL AND
      p_now IS INITIAL.

    REFRESH  s_date.
    s_date-high = s_date-low + 6.
    s_date-sign = c_i.
    s_date-option = c_bt.
    APPEND s_date.
    MESSAGE w044. "Date Range is Defaulted to 1 week
  ELSE.
    IF s_date-high IS INITIAL   AND
       NOT p_now IS INITIAL.

      REFRESH s_date.
      s_date-high = s_date-low + ( 7 * p_now ) - 1.
      s_date-sign = c_i.
      s_date-option = c_bt.
      APPEND s_date.
    ENDIF.
  ENDIF.

ENDFORM.                    " DATE_VALIDATIONS

*&---------------------------------------------------------------------*
*&      FORM  VALIDTAE_MATNR_MTART
*&---------------------------------------------------------------------*
*   subroutine to validate material number and material type
*----------------------------------------------------------------------*
FORM validate_matnr_mtart .
  SELECT mtart UP TO 1 ROWS
         FROM mara
         INTO mara-mtart
         WHERE matnr IN s_matno
          AND mtart IN s_mtype.
  ENDSELECT.
  IF sy-subrc <> 0.
    MESSAGE e033. "Invalid Material for the Material type
  ENDIF.
ENDFORM.                    " validate_matnr_mtart

*&---------------------------------------------------------------------*
*&     FORM  SELECT_MATERIALS
*&---------------------------------------------------------------------*
*    subroutine to select materials
*----------------------------------------------------------------------*
FORM select_materials .

  CLEAR flg_matsel.
  SELECT matnr
         mtart
         meins
  FROM mara
  INTO TABLE i_mara
  WHERE matnr IN s_matno
  AND mtart IN s_mtype.

* Sy-subrc will not fail here. Validations have been done before
  IF sy-subrc = 0.
    flg_matsel = c_x.
    SORT i_mara BY matnr.
  ENDIF.

ENDFORM.                    " select_materials

*&---------------------------------------------------------------------*
*&      FORM SELECT_ACTUAL_ORDERS
*&---------------------------------------------------------------------*
*       subroutine to select actual orders
*----------------------------------------------------------------------*
FORM select_actual_orders .

  REFRESH: i_caufv, i_afvv_afvc.
  CLEAR:   i_caufv, i_afvv_afvc.

  IF flg_matsel IS INITIAL.
    SELECT aufnr
           objnr
           gltrp
           gstrp
           gamng
           gmein
           plnbez
           aufpl
     FROM caufv
     INTO TABLE i_caufv
     WHERE werks = p_werks
     AND   gstrp IN s_date.
  ELSE.
    SELECT aufnr
           objnr
           gltrp
           gstrp
           gamng
           gmein
           plnbez
           aufpl
     INTO TABLE i_caufv
     FROM caufv
     FOR ALL ENTRIES IN i_mara
     WHERE plnbez = i_mara-matnr
     AND  werks = p_werks
     AND gstrp IN s_date.
  ENDIF.
  IF sy-subrc = 0.
    SORT i_caufv BY aufnr.
  ENDIF.

  IF NOT i_caufv[] IS INITIAL.
    SELECT afvc~aufpl
           afvc~vornr
           afvc~arbid
    INTO TABLE i_afvv_afvc
    FROM  afvc JOIN afvv
    ON  afvc~aufpl = afvv~aufpl
    FOR ALL entries IN i_caufv
    WHERE afvc~aufpl = i_caufv-aufpl
    AND afvc~phflg = c_x.
    IF sy-subrc = 0.
      SORT i_afvv_afvc BY aufpl ASCENDING vornr DESCENDING.
    ENDIF.
  ENDIF.

ENDFORM.                    " select_actual_orders

*&---------------------------------------------------------------------*
*&          FORM SELECT_STATUS
*&---------------------------------------------------------------------*
*      subroutine to retrieve System status texts from table TJ02T
*----------------------------------------------------------------------*
FORM select_status .
  REFRESH i_tj02t.
  CLEAR   i_tj02t.

  IF NOT  s_orstat[] IS INITIAL.
    SELECT txt04
    FROM tj02t INTO TABLE i_tj02t
    WHERE spras =  sy-langu
    AND   txt04 IN s_orstat.

    IF sy-subrc = 0.
      SORT i_tj02t BY txt04.
      DELETE ADJACENT DUPLICATES FROM i_tj02t COMPARING txt04.
    ENDIF.
  ENDIF.
ENDFORM.                    " select_status

*&---------------------------------------------------------------------*
*&     FORM SELECT_WORKCENTER_DETAILS
*&---------------------------------------------------------------------*
*      subroutine to get work center details
*----------------------------------------------------------------------*
FORM select_workcenter_details .

  SELECT objid
         arbpl
  FROM crhd
  INTO TABLE i_crhd
  WHERE objty = c_a
  AND   werks = p_werks
  AND   arbpl IN s_wctr.

  IF sy-subrc = 0.
    SORT i_crhd BY objid.
  ENDIF.

ENDFORM.                    " select_workcenter_details

*&---------------------------------------------------------------------*
*&      FORM SELECT_DOCUMENT_SEGMENT_INFO                              *
*&---------------------------------------------------------------------*
*  Subroutine to get the document segment information for a material   *
*----------------------------------------------------------------------*
FORM select_document_segment_info .

  CHECK NOT i_caufv[] IS INITIAL.

  SELECT mblnr
         mjahr
         zeile
         bwart
         matnr
         aufnr
         menge
   FROM mseg
   INTO TABLE i_mseg
   FOR ALL ENTRIES IN i_caufv
   WHERE bwart IN (c_101 , c_102)
   AND matnr = i_caufv-plnbez
   AND aufnr = i_caufv-aufnr.

  IF sy-subrc = 0.
    SORT i_mseg BY matnr aufnr.
  ENDIF.
ENDFORM.                    " select_document_segment_info

*&---------------------------------------------------------------------*
*&     FORM SELECT_FINALACTUAL_ORDERS
*&---------------------------------------------------------------------*
*       Subroutine to get the final actual orders
*----------------------------------------------------------------------*
FORM select_finalactual_orders .
  DATA: l_stext(100)  TYPE c,
        l_baseqty     TYPE gamng,
        l_price       TYPE stprs.

  LOOP AT i_caufv.

    IF NOT i_tj02t[] IS INITIAL.
      CLEAR flg_stat.
      PERFORM get_status_info USING i_caufv-objnr l_stext.
      LOOP AT i_tj02t.
        IF l_stext NS i_tj02t-txt04.
          flg_stat = c_x.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF  flg_stat = c_x.
        CONTINUE.
      ENDIF.
    ENDIF.

    CLEAR flg_ordsel.
    LOOP AT i_afvv_afvc WHERE aufpl  =  i_caufv-aufpl.
      READ TABLE i_crhd WITH KEY objid = i_afvv_afvc-arbid
                                 BINARY SEARCH.
      IF sy-subrc = 0.
        flg_ordsel = c_x.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF flg_ordsel IS INITIAL.
      CONTINUE.
    ENDIF.

*---Move the required data into internal table i_finalord
    CLEAR i_finalord.
    MOVE:  i_crhd-arbpl      TO i_finalord-arbpl, "RESOURCE
           i_caufv-plnbez    TO i_finalord-matnr, "Material Number
           i_caufv-gstrp     TO i_finalord-gstrp, "Basic start date
           i_caufv-gltrp     TO i_finalord-gltrp, "Basic End date
           i_caufv-gltrp     TO i_finalord-wdate, " Activity Date
           i_caufv-aufnr     TO i_finalord-aufnr, "Process Order No
           i_caufv-gamng     TO i_finalord-gamng, "Planned Quantity
           i_caufv-gmein     TO i_finalord-meins, "Unit of measure
           c_kgm             TO i_finalord-zmeins,"KG Conversion
           v_waers           TO i_finalord-fcurr. "Currency

*----Get the Material Description
    PERFORM get_matdesc USING       i_finalord-matnr
                        CHANGING    i_finalord-maktx.
*----Get the Dayname
    PERFORM get_dayname USING       i_finalord-gltrp
                        CHANGING    i_finalord-wkday."Week Day
*----Get the Week
    PERFORM get_week    USING       i_finalord-gltrp
                        CHANGING    i_finalord-spwoc."Week
*---Get the ACTUAL Quantity(menge) from mseg.
    PERFORM get_actual_qauntites.
*---Quantitity Conversions: Convert Actual quantity.
    PERFORM convert_quantity USING i_finalord-matnr
                                   i_finalord-meins
                                   i_finalord-menge
                                   i_finalord-zmeins
                          CHANGING i_finalord-zact.
*---Qunatitiy conversions: Convert Planned Quantity.
    PERFORM convert_quantity USING i_finalord-matnr
                                   i_finalord-meins
                                   i_finalord-gamng
                                   i_finalord-zmeins
                          CHANGING i_finalord-zplan.

*---Calculate the costs--------------------------*
    READ TABLE i_mara WITH KEY matnr = i_finalord-matnr BINARY SEARCH.
    IF sy-subrc = 0.
*--Get the Material Description------------------*
      MOVE i_mara-mtart TO i_finalord-mtart.

*----Get the Price
      CLEAR l_price.
      PERFORM get_price USING i_finalord-matnr
                              p_werks
                     CHANGING l_price.

*---Qunatitiy conversions: Convert Planned Quantity.
      CLEAR l_baseqty.
      PERFORM convert_quantity USING  i_finalord-matnr
                                      i_finalord-meins
                                      i_finalord-gamng
                                      i_mara-meins
                             CHANGING l_baseqty.

      i_finalord-fplan = l_baseqty * l_price.
      CLEAR l_baseqty.

*---Qunatitiy conversions: Convert Actual Quantity.
      PERFORM convert_quantity USING  i_finalord-matnr
                                      i_finalord-meins
                                      i_finalord-menge
                                      i_mara-meins
                             CHANGING l_baseqty.
      i_finalord-fact = l_baseqty * l_price.
    ENDIF.

*---Calculate the differences
    PERFORM calculate_differences.
    APPEND i_finalord.
  ENDLOOP.
*--Sort the Internal table i_finalord By wdate reosource matno and aufnr
  SORT i_finalord BY wdate arbpl matnr aufnr.
ENDFORM.                    " select_finalactual_orders

*&---------------------------------------------------------------------
*&      Form  GET_STATUS_INFO
*&---------------------------------------------------------------------*
*      Subroutine to retrieve the status information
*----------------------------------------------------------------------*
*      -->P_I_CAUFV_OBJID  text
*      -->P_L_STEXT  text
*----------------------------------------------------------------------*
FORM get_status_info  USING    p_i_caufv_objid
                               p_l_stext.

  DATA:   l_text(4) TYPE c,
          l_stsma TYPE j_stsma,
          l_i_status TYPE STANDARD TABLE OF jstat WITH HEADER LINE.

  REFRESH l_i_status.
  CLEAR: p_l_stext.

*--Read Object Status
  CALL FUNCTION 'STATUS_READ'
    EXPORTING
      objnr       = p_i_caufv_objid
      only_active = c_x
    IMPORTING
      stsma       = l_stsma
    TABLES
      status      = l_i_status.

  LOOP AT l_i_status.
*--Status number conversion
    CALL FUNCTION 'STATUS_NUMBER_CONVERSION'
      EXPORTING
        language      = sy-langu
        status_number = l_i_status-stat
        stsma         = l_stsma
      IMPORTING
        txt04         = l_text.

    CONCATENATE p_l_stext l_text INTO p_l_stext.
  ENDLOOP.

ENDFORM.                    " GET_STATUS_INFO

*&---------------------------------------------------------------------*
*&      FORM SELECT_PLANNED_ORDERS.
*&---------------------------------------------------------------------*
*  Subroutine for selecting planned orders
*----------------------------------------------------------------------*
FORM select_planned_orders.
** Check for the matsel flag
** If it is initial retrieve the records
** From plaf based on plant and start date
** Else Retrieve the records from plaf based
** Material Number and plant and stat date
  IF flg_matsel IS INITIAL.
    SELECT matnr
           aufnr
           verid
           gsmng
           pedtr
           pertr
           meins
     FROM plaf
     INTO TABLE i_plaf
     WHERE plwrk = p_werks
     AND  pertr IN s_date.
  ELSE.
    SELECT matnr
           aufnr
           verid
           gsmng
           pedtr
           pertr
           meins
     INTO TABLE i_plaf
     FROM plaf
     FOR ALL ENTRIES IN  i_mara
     WHERE matnr  =   i_mara-matnr
     AND   plwrk  =   p_werks
     AND   pertr  IN  s_date.
  ENDIF.
  IF sy-subrc = 0.
    SORT i_plaf BY matnr verid.
  ENDIF.
ENDFORM.                    " select_planned_orders

*&---------------------------------------------------------------------*
*&      FORM SELECT_PRODUCTION_VERSIONS
*&---------------------------------------------------------------------*
*       subroutine to select production versions
*----------------------------------------------------------------------*
FORM select_production_versions .
**Check the internal table i_plaf
  CHECK NOT i_plaf[] IS INITIAL.
**If it is not initial Select the records from
**mkal(Production Vesrions)
  SELECT   matnr
           mdv01
  INTO TABLE i_mkal
  FROM mkal
  FOR ALL ENTRIES IN i_plaf
  WHERE  matnr = i_plaf-matnr
  AND    werks = p_werks
  AND    verid = i_plaf-verid
  AND    mdv01 IN s_wctr[].
  IF sy-subrc = 0.
    SORT i_mkal BY matnr.
  ENDIF.

ENDFORM.                    " select_production_versions

*&---------------------------------------------------------------------*
*&      Form  SELECT_FINALPLANNED_ORDERS
*&---------------------------------------------------------------------*
*       subroutine to select final planned orders
*----------------------------------------------------------------------*
FORM select_finalplanned_orders .

  DATA:   l_baseqty     TYPE gamng,
          l_price       TYPE stprs.

  LOOP AT i_plaf.
    READ TABLE i_mkal WITH  KEY matnr = i_plaf-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      CLEAR i_finalord.
**Move the corresponding records from  i_plaf to i_finalord
      MOVE: i_plaf-matnr TO i_finalord-matnr,
            i_plaf-plnum TO i_finalord-aufnr,
            i_plaf-pedtr TO i_finalord-gltrp,
            i_plaf-pertr TO i_finalord-gstrp,
            i_plaf-pedtr TO i_finalord-wdate,
            i_plaf-gsmng TO i_finalord-gamng,
            i_plaf-meins TO i_finalord-meins,
            c_kgm        TO i_finalord-zmeins,
            v_waers      TO i_finalord-fcurr.

      IF i_mkal-mdv01 = space.
        MOVE: text-092 TO i_finalord-arbpl.
      ELSE.
        MOVE: i_mkal-mdv01 TO i_finalord-arbpl.
      ENDIF.
*----Get the Material Description
      PERFORM get_matdesc   USING        i_finalord-matnr
                            CHANGING     i_finalord-maktx."material desc
*----Get the Day Name
      PERFORM get_dayname   USING        i_finalord-gltrp
                            CHANGING     i_finalord-wkday."Week Day
*----Get the Week
      PERFORM get_week      USING        i_finalord-gltrp
                            CHANGING     i_finalord-spwoc."Week

*---Qunatitiy conversions: Convert Planned Quantity
      PERFORM convert_quantity USING  i_finalord-matnr
                                      i_finalord-meins
                                      i_finalord-gamng
                                      i_finalord-zmeins
                             CHANGING i_finalord-zplan.
*----Calculate the costs
      READ TABLE i_mara WITH KEY matnr = i_finalord-matnr BINARY SEARCH.
      IF sy-subrc = 0.
        MOVE i_mara-mtart TO i_finalord-mtart.
        CLEAR l_price.
        PERFORM get_price USING i_finalord-matnr
                                 p_werks
                        CHANGING l_price.

        CLEAR l_baseqty.
        PERFORM convert_quantity USING  i_finalord-matnr
                                        i_finalord-meins
                                        i_finalord-gamng
                                        i_mara-meins
                               CHANGING l_baseqty.
        i_finalord-fplan = l_baseqty * l_price.
        CLEAR l_baseqty.
      ENDIF.

*----Calculate the differences
      PERFORM calculate_differences.

      APPEND i_finalord.
    ENDIF.
  ENDLOOP.

  SORT i_finalord BY wdate arbpl matnr aufnr.
ENDFORM.                    " SELECT_FINALPLANNED_ORDERS

*&---------------------------------------------------------------------*
*&      Form  get_week
*&---------------------------------------------------------------------*
*       Subroutine to get week from date
*----------------------------------------------------------------------*
*      -->P_I_FINALORD_GLTRP  text
*      <--P_I_FINALORD_SPWOC  text
*----------------------------------------------------------------------*
FORM get_week  USING    p_i_finalord_gltrp
               CHANGING p_i_finalord_spwoc.

*---Function Module to Get the Week based on date.
  CALL FUNCTION 'DATE_GET_WEEK'
    EXPORTING
      date = p_i_finalord_gltrp
    IMPORTING
      week = p_i_finalord_spwoc.

ENDFORM.                    " get_week

*&---------------------------------------------------------------------*
*&    FORM GET_ACTUAL_QUANTITES
*&---------------------------------------------------------------------*
*    Subroutine to get the actual quantities from mseg.
*----------------------------------------------------------------------*
FORM get_actual_qauntites .

  LOOP AT i_mseg WHERE aufnr = i_caufv-aufnr
                 AND   matnr = i_caufv-plnbez.
    IF i_mseg-bwart = c_101.
      i_finalord-menge = i_finalord-menge + i_mseg-menge.
    ELSE.
      i_finalord-menge = i_finalord-menge - i_mseg-menge.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " get_actual_qauntites

&---------------------------------------------------------------------*
*&     FORM  CALCULATE_DIFFERENCES
*&---------------------------------------------------------------------*
*      Subroutine for calculating the differences
*----------------------------------------------------------------------*
FORM calculate_differences .

  i_finalord-diff =   abs( i_finalord-gamng - i_finalord-menge ).
  i_finalord-zdiff =  abs( i_finalord-zplan - i_finalord-zact  ).
  i_finalord-fdiff =  abs( i_finalord-fplan - i_finalord-fact  ).

ENDFORM.                    " CALCULATE_DIFFERENCES


*&---------------------------------------------------------------------*
*&      FORM  CONVERT_QUANTITY
*&---------------------------------------------------------------------*
*       Subroutine for quantity conversions
*----------------------------------------------------------------------*
*      -->P_I_FINALORD_MATNR  text
*      -->P_I_FINALORD_MEINS  text
*      -->P_I_FINALORD_MENGE  text
*      <--P_I_FINALORD_ZACT  text
*----------------------------------------------------------------------*
FORM convert_quantity  USING    p_i_matnr
                                p_i_sr_meins
                                p_i_sr_menge
                                p_i_tg_meins
                       CHANGING p_i_tg_menge.

  DATA:  l_in_qty  TYPE bstmg,
         l_out_qty TYPE bstmg.

  MOVE: p_i_sr_menge TO l_in_qty.
**Function Module to Convert Material Unit from One Unit to another
  CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
    EXPORTING
      i_matnr              = p_i_matnr
      i_in_me              = p_i_sr_meins
      i_out_me             = p_i_tg_meins
      i_menge              = l_in_qty
    IMPORTING
      e_menge              = l_out_qty
    EXCEPTIONS
      error_in_application = 1
      error                = 2
      OTHERS               = 3.

  IF sy-subrc = 0.
    MOVE l_out_qty TO p_i_tg_menge.
  ELSE.
    CLEAR p_i_tg_menge.
  ENDIF.

ENDFORM.                    " convert_quantity

*&---------------------------------------------------------------------*
*&      FORM  GET_PRICE
*&---------------------------------------------------------------------*
*       Subroutine to get the price
*----------------------------------------------------------------------*
*      -->P_V_MATNR  text
*      -->P_V_WERKS  text
*      <--P_V_PRICE  text
*----------------------------------------------------------------------*
FORM get_price  USING    p_v_matnr
                         p_v_werks
                CHANGING p_v_price.

  STATICS i_price  TYPE STANDARD TABLE OF ty_price
                    INITIAL SIZE 0 WITH HEADER LINE.

  CLEAR: i_price, p_v_price.
  READ TABLE  i_price WITH KEY matnr = p_v_matnr.

  IF sy-subrc <> 0.
    SELECT  matnr
            stprs
            peinh
            UP TO  1 ROWS
            FROM mbew
            INTO i_price
            WHERE matnr = p_v_matnr
            AND   bwkey = p_v_werks.
    ENDSELECT.

    IF sy-subrc = 0.
      APPEND i_price.
    ENDIF.
  ENDIF.

  IF i_price-peinh <> 0.
    p_v_price = i_price-stprs / i_price-peinh.
  ENDIF.

ENDFORM.                    " get_price

&---------------------------------------------------------------------*
*&      FORM  MERGE_DATA                                               *
*&---------------------------------------------------------------------*
*  Subroutine to merge  weekly data into final internal table i_week   *
*----------------------------------------------------------------------*
FORM merge_data .

  DATA: l_wkday_num TYPE cind,
        l_planned TYPE disptype,
        l_actual  TYPE disptype,
        l_diff    TYPE disptype,
        l_perc    TYPE p DECIMALS 0 .
**For weekly report Consolidate data from i_finalord into internal
**tables i_weekly
  REFRESH: i_weekly.

  LOOP AT i_finalord.

    CLEAR i_weekly.
    MOVE: i_finalord-spwoc   TO  i_weekly-spwoc,
          i_finalord-arbpl   TO  i_weekly-arbpl,
          i_finalord-matnr   TO  i_weekly-matnr,
          i_finalord-aufnr   TO  i_weekly-aufnr,
          i_finalord-mtart   TO  i_weekly-mtart,
          i_finalord-maktx   TO  i_weekly-maktx.

    IF rb_quan = c_x.
      IF rb_ord = c_x .
        MOVE: i_finalord-gamng TO l_planned,
              i_finalord-menge TO l_actual,
              i_finalord-diff  TO l_diff,
              i_finalord-meins TO i_weekly-meins.

        PERFORM fill_iweek1.
        PERFORM fill_iweek2.
        PERFORM fill_iweek3.
      ELSE.
        MOVE: i_finalord-zplan TO l_planned,
              i_finalord-zact  TO l_actual,
              i_finalord-zdiff TO l_diff,
              i_finalord-zmeins TO i_weekly-meins.
      ENDIF.
    ELSE.
      MOVE: i_finalord-fplan TO l_planned,
            i_finalord-fact  TO l_actual,
            i_finalord-fdiff TO l_diff,
            i_finalord-fcurr TO i_weekly-waers.
    ENDIF.

    PERFORM calculate_percentage USING l_planned
                                       l_diff
                                       CHANGING l_perc.

    CLEAR l_wkday_num.
    CALL FUNCTION 'DATE_COMPUTE_DAY'
      EXPORTING
        date = i_finalord-wdate
      IMPORTING
        day  = l_wkday_num.

    CASE l_wkday_num.
      WHEN c_1.
        MOVE : l_planned  TO i_weekly-mon_plan,
               l_actual   TO i_weekly-mon_act,
               l_diff     TO i_weekly-mon_diff,
               l_perc     TO i_weekly-mon_perc.

      WHEN c_2.
        MOVE:  l_planned  TO i_weekly-tue_plan,
               l_actual   TO i_weekly-tue_act,
               l_diff     TO i_weekly-tue_diff,
               l_perc     TO i_weekly-tue_perc.

      WHEN c_3.
        MOVE:  l_planned  TO i_weekly-wed_plan,
               l_actual   TO i_weekly-wed_act,
               l_diff     TO i_weekly-wed_diff,
               l_perc     TO i_weekly-wed_perc.

      WHEN c_4.
        MOVE:  l_planned  TO i_weekly-thu_plan,
               l_actual   TO i_weekly-thu_act,
               l_diff     TO i_weekly-thu_diff,
               l_perc     TO i_weekly-thu_perc.

      WHEN c_5.
        MOVE:  l_planned  TO i_weekly-fri_plan,
               l_actual   TO i_weekly-fri_act,
               l_diff     TO i_weekly-fri_diff,
               l_perc     TO i_weekly-fri_perc.

      WHEN c_6.
        MOVE:  l_planned  TO i_weekly-sat_plan,
               l_actual   TO i_weekly-sat_act,
               l_diff     TO i_weekly-sat_diff,
               l_perc     TO i_weekly-sat_perc.

      WHEN c_7.
        MOVE:  l_planned  TO i_weekly-sun_plan,
               l_actual   TO i_weekly-sun_act,
               l_diff     TO i_weekly-sun_diff,
               l_perc     TO i_weekly-sun_perc.
    ENDCASE.

    MOVE:  l_planned  TO i_weekly-tot_plan,
           l_actual   TO i_weekly-tot_act,
           l_diff     TO i_weekly-tot_diff,
           l_perc     TO i_weekly-tot_perc.

    APPEND i_weekly.
  ENDLOOP.
ENDFORM.                    " merge_data

*&---------------------------------------------------------------------*
*&     FORM FILL_IWEEK1                                              *
*&---------------------------------------------------------------------*
*  Subroutine to fill the i_week1 internal table
*----------------------------------------------------------------------*
FORM fill_iweek1.
**Consolidate Data into Internal table i_week1 based on
**week,weekday,resource and material Number, consolidating at material
**level
  READ TABLE i_week1 WITH KEY  spwoc = i_finalord-spwoc
                               wkday = i_finalord-wkday
                               arbpl = i_finalord-arbpl
                               matnr = i_finalord-matnr.

  IF sy-subrc = 0.
    IF i_week1-meins <> c_null.
      IF i_week1-meins <> i_finalord-meins.
        i_week1-meins = c_null.
        MODIFY i_week1 INDEX sy-tabix TRANSPORTING meins.
      ENDIF.
    ENDIF.
  ELSE.
    CLEAR i_week1.
    MOVE: i_finalord-spwoc TO i_week1-spwoc,
          i_finalord-wkday TO i_week1-wkday,
          i_finalord-arbpl TO i_week1-arbpl,
          i_finalord-matnr TO i_week1-matnr,
          i_finalord-meins TO i_week1-meins.

    APPEND i_week1.
  ENDIF.

ENDFORM.                    " fill_iweek1

*&---------------------------------------------------------------------*
*&      FORM FILL_IWEEK2
*&---------------------------------------------------------------------*
* Subroutine to fill the internal table i_week2 based on week,day,res
*----------------------------------------------------------------------*
FORM fill_iweek2.
**Consolidate Data into Internal table i_week2 based on
**week,weekday,resource consolidating at resource level

  READ TABLE i_week2 WITH KEY spwoc = i_finalord-spwoc
                              wkday = i_finalord-wkday
                              arbpl = i_finalord-arbpl.
  IF sy-subrc = 0.
    IF i_week2-meins <> c_null.
      IF i_week2-meins <> i_finalord-meins.
        i_week2-meins = c_null.
        MODIFY i_week2 INDEX sy-tabix TRANSPORTING meins.
      ENDIF.
    ENDIF.
  ELSE.
    CLEAR i_week2.
    MOVE: i_finalord-spwoc TO i_week2-spwoc,
          i_finalord-wkday TO i_week2-wkday,
          i_finalord-arbpl TO i_week2-arbpl,
          i_finalord-meins TO i_week2-meins.

    APPEND i_week2.
  ENDIF.

ENDFORM.                    " fill_iweek2

*&---------------------------------------------------------------------*
*&   FORM FILL_IWEEK3
*&---------------------------------------------------------------------*
* Subroutine to fill the internal table i_week3 based on week,day,res
*--------------------------------------------------------------------*
FORM fill_iweek3.
**Consolidate Data into Internal table i_week3 based on
**week,weekday consolidating at weeklevel
  READ TABLE i_week3 WITH KEY spwoc = i_finalord-spwoc
                              wkday = i_finalord-wkday.

  IF sy-subrc = 0.
    IF i_week3-meins <> c_null.
      IF i_week3-meins <> i_finalord-meins.
        i_week3-meins = c_null.
        MODIFY i_week3 INDEX sy-tabix TRANSPORTING meins.
      ENDIF.
    ENDIF.
  ELSE.
    CLEAR : i_week3.
    MOVE: i_finalord-spwoc TO i_week3-spwoc,
          i_finalord-wkday TO i_week3-wkday,
          i_finalord-meins TO i_week3-meins.
    APPEND i_week3.
  ENDIF.

ENDFORM.                    " fill_iweek3

*&---------------------------------------------------------------------*
*&      Form  display_weekly_report
*&---------------------------------------------------------------------*
*      Subrotuine to display weekly  report
*----------------------------------------------------------------------*
FORM display_weekly_report.

  DATA:  l_date TYPE sydatum.
  l_date = c_date.
  DO 7 TIMES.
    PERFORM get_week_names USING    l_date
                           CHANGING i_wkday-wkday .
    l_date = l_date + 1.
  ENDDO.

  LOOP AT i_weekly.
    CLEAR rec_weekly.
    MOVE i_weekly TO rec_weekly.
    AT NEW spwoc.
      PERFORM display_week_header.
    ENDAT.
** At new workcenter display the workcenter header
    AT NEW arbpl.
      PERFORM wrkcenter_header.
    ENDAT.
** At new Material display the material Header
    AT NEW matnr.
      PERFORM material_header.
    ENDAT.
**Check for the Detail level Report or Summary Level Report
**If it is a Detail Level Report display the materials and
**Corresponding orders
    IF rb_det = c_x.
      IF cb_color = c_x.
        FORMAT COLOR col_normal INTENSIFIED OFF.
      ENDIF.
      PERFORM show_weekly_orders.
    ENDIF.
**if it is summary level report Show only the material numbers
    AT END OF  matnr.
      SUM.
** check whether to display the quantities and order units
      IF rb_quan = c_x AND rb_ord = c_x.
        PERFORM check_uom_week_res_mat.
      ENDIF.
      PERFORM calculate_wk_perc.
      IF cb_color = c_x.
        FORMAT COLOR col_total INTENSIFIED On.
      ENDIF.
      PERFORM display_wk_tot USING text-088.
    ENDAT.

    AT END OF arbpl.
      SUM.
** check whether to display the quantities and order units
      IF rb_quan = c_x AND rb_ord = c_x.
        PERFORM check_uom_week_res.
      ENDIF.
      PERFORM calculate_wk_perc.
      IF cb_color = c_x.
        FORMAT COLOR col_key INTENSIFIED Off.
      ENDIF.
      PERFORM display_wk_tot USING text-089.
    ENDAT.

    AT END OF spwoc.
      SUM.
** check whether to display the quantities and order units
      IF rb_quan = c_x AND rb_ord = c_x.
        PERFORM check_uom_week.
      ENDIF.
      PERFORM calculate_wk_perc.
      IF cb_color = c_x.
        FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
      ENDIF.
      PERFORM display_wk_tot USING text-091.
      SKIP 1.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " display_report

*&---------------------------------------------------------------------*
*&      Form  display_report_header
*&---------------------------------------------------------------------*
*   Sub routine to display Report Header
*----------------------------------------------------------------------*
FORM display_report_header.
  DATA: l_text1(20) TYPE c,
        l_text2(20) TYPE c,
        l_text3(40) TYPE c.
**Report Header Details Depending on daily report or weekly Report
  FORMAT COLOR COL_BACKGROUND.
*  CALL FUNCTION '/NESGLB/X_PRINT_REPORT_HEADER'
*    EXPORTING
*      prog_id      = sy-cprog
*      prog_title   = sy-title
*      sysid        = sy-sysid
*      first_call   = ' '
*      print_header = c_x.
**If weekly report.
  IF rb_week = c_x.
    WRITE: text-030,
           15 s_date-low,
           28(10) text-031,
           35 s_date-high,
           50 p_werks,
           60 v_text1.
  ELSE.
    WRITE : text-071,
             50 p_werks,
             60 v_text1.
  ENDIF.
  SKIP.
  IF rb_week = c_x.
    CLEAR: l_text1,
         l_text2,
         l_text3.
    IF rb_ord = c_x.
      l_text1 = text-032.
    ELSE.
      l_text1 = text-033.
    ENDIF.
**if the quantities is checked
    IF rb_quan = c_x.
      l_text2 = text-034.
    ELSE.
      l_text2 = text-035.
    ENDIF.

    IF rb_det = c_x.
      l_text3 = text-036.
    ELSE.
      l_text3 = text-037.
    ENDIF.

    CONCATENATE l_text1 text-038
                l_text2 text-038
                l_text3 INTO v_text3
                SEPARATED BY  space.
    WRITE : v_text3.

    IF cb_color = c_x.
      WRITE: 45 text-039,
          (16)  text-040     COLOR  COL_POSITIVE INTENSIFIED OFF,
          (16)  text-041     COLOR  col_key  INTENSIFIED Off,
          (16)  text-042     COLOR  COL_group    INTENSIFIED OFF,
          (16)  text-043     COLOR  col_total    INTENSIFIED on,
          (16)  text-044     COLOR  col_normal   INTENSIFIED OFF.
    ENDIF.
    ULINE (245).
**If the Report To be displayed is daily report
  ELSEIF rb_daily = c_x.
    CLEAR: l_text1,
           l_text2,
           l_text3.
    IF rb_ord = c_x.
      l_text1 = text-032.
    ELSE.
      l_text1 = text-033.
    ENDIF.

    IF rb_quan = c_x.
      l_text2 = text-034.
    ELSE.
      l_text2 = text-035.
    ENDIF.
    CONCATENATE l_text1 text-038
                l_text2  INTO v_text3
                SEPARATED BY space.
    WRITE : v_text3.
    IF cb_color = c_x.
      WRITE: 45 text-039,
      (16) text-045   COLOR  COL_POSITIVE INTENSIFIED OFF,
      (16) text-041   COLOR  col_key  INTENSIFIED Off,
      (16) text-042   COLOR  COL_group    INTENSIFIED OFF,
      (16) text-043   COLOR  col_total    INTENSIFIED On,
      (16) text-044   COLOR  col_normal   INTENSIFIED OFF.
    ENDIF.
    ULINE /(150).
  ENDIF.

ENDFORM.                    " display_report_header

*&---------------------------------------------------------------------*
*&      FORM DISPLAY_WEEK_HEADER
*&---------------------------------------------------------------------*
*    Subroutine To display the weekly header
*----------------------------------------------------------------------*
FORM display_week_header .
**Make the firstday and lastday as Monday and sunday respectively
  CALL FUNCTION 'WEEK_GET_FIRST_DAY'
    EXPORTING
      week         = rec_weekly-spwoc
    IMPORTING
      date         = v_monday
    EXCEPTIONS
      week_invalid = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    CLEAR v_monday.
  ENDIF.

  v_sunday = v_monday + 6.

  IF  v_monday < s_date-low.
    v_monday = s_date-low.
  ENDIF.

  IF v_sunday > s_date-high.
    v_sunday = s_date-high.
  ENDIF.

  IF cb_color = c_x.
    FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
  ENDIF.

  ULINE /(245).

  WRITE: / text-046,
          25 rec_weekly-spwoc,
          40 text-047,
          46 v_monday,
          60 text-048,
          65 v_sunday,
          245 sy-vline.
  ULINE (245).

ENDFORM.                    " display_week_header

*&---------------------------------------------------------------------*
*&      FORM  WRKCENTER_HEADER.
*&---------------------------------------------------------------------*
*     Subroutine to display work center Header
*----------------------------------------------------------------------*
FORM wrkcenter_header.
**Make the Default day/ as monday of the week.
  CLEAR: v_rtext.
  CALL FUNCTION 'WEEK_GET_FIRST_DAY'
    EXPORTING
      week         = i_weekly-spwoc
    IMPORTING
      date         = v_monday
    EXCEPTIONS
      week_invalid = 1
      OTHERS       = 2.

  IF sy-subrc <> 0.
    CLEAR v_monday.
  ENDIF.

  v_fmday = v_monday.

  IF cb_color = c_x.
    FORMAT COLOR col_key INTENSIFIED Off.
  ENDIF.

  WRITE: / text-049,
           12 rec_weekly-arbpl.
**Get the Work Center Description .
  PERFORM get_resdesc USING    rec_weekly-arbpl
                               p_werks
                      CHANGING v_rtext.
  WRITE:   24 v_rtext,
           245 sy-vline.
  IF cb_color = c_x.
    FORMAT COLOR col_heading INTENSIFIED ON.
  ENDIF.

  WRITE : /(14) ' ' , sy-vline.
  WRITE:(8)  text-050,(13) v_fmday,44  sy-vline. v_fmday =  v_fmday + 1.
  WRITE:(10) text-051,(13) v_fmday,72  sy-vline. v_fmday =  v_fmday + 1.
  WRITE:(10) text-052,(13) v_fmday,100 sy-vline. v_fmday =  v_fmday + 1.
  WRITE:(10) text-053,(13) v_fmday,128 sy-vline. v_fmday =  v_fmday + 1.
  WRITE:(10) text-054,(13) v_fmday,156 sy-vline. v_fmday =  v_fmday + 1.
  WRITE:(10) text-055,(13) v_fmday,184 sy-vline. v_fmday =  v_fmday + 1.
  WRITE:(10) text-056,(13) v_fmday,212 sy-vline.
  WRITE:(26) text-057,   240 sy-vline, (3) '', 245 sy-vline.
  WRITE : / text-058.

  DO 8 TIMES.
    WRITE: sy-vline, text-068.
  ENDDO.
  IF rb_quan = c_x.
    WRITE: sy-vline NO-GAP, (4) text-070.
  ELSE.
    WRITE : sy-vline NO-GAP ,(4) text-087.
  ENDIF.
  WRITE: 245 sy-vline.
  ULINE (245).
ENDFORM.                    " Wrkcenter_Header

*&---------------------------------------------------------------------*
*&      FORM  MATERIAL_HEADER
*&---------------------------------------------------------------------*
*      Subroutine To display Material Header
*----------------------------------------------------------------------*
FORM material_header .
  DATA: l_maktx TYPE maktx.
  PERFORM get_matdesc USING rec_weekly-matnr CHANGING l_maktx.
  IF cb_color = c_x.
    FORMAT COLOR  COL_GROUP INTENSIFIED OFF.
  ENDIF.
  WRITE :/1 rec_weekly-matnr, 40 rec_weekly-mtart, 47 l_maktx,
          245 sy-vline.
  ULINE (245).

ENDFORM.                    " material_Header

*&---------------------------------------------------------------------*
*&      FORM DISPLAY_DAILY_REPORT
*&---------------------------------------------------------------------*
*   subroutine to disply daily report
*----------------------------------------------------------------------*
FORM display_daily_report.
  DATA: l_uom_day TYPE meins,
        l_uom_res TYPE meins,
        l_uom_mat TYPE meins,
        l_perc TYPE disptype.

  LOOP AT i_finalord.
    CLEAR: rec_finalord, l_perc.
** Move the Records into WorkArea
    MOVE i_finalord TO rec_finalord.
** At start of New Date, Display day header
** and Get the UOM into l_uom_day
    AT NEW wdate.
      PERFORM display_day_header.
      l_uom_day = rec_finalord-meins.
    ENDAT.
** At start of New Resource Display the Resource Header
** and get the UOM into l_uom_res
    AT NEW arbpl.
      PERFORM display_resource_header.
      l_uom_res = rec_finalord-meins.
    ENDAT.
    AT NEW matnr.
**At start of New Material Display the material Header
**and get the UOM into l_uom_mat.
      PERFORM  display_material_header.
      l_uom_mat = rec_finalord-meins.
      PERFORM display_order_header.
    ENDAT.

    if cb_color = c_x.
     FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    endif.
    PERFORM write_vlines_daily.
**Check for the OrderUnit,Quantities and compare whether all the UOMs
** are matching on particular day and calculate the Corresponding
**percentages and Display the Orders for the planned ,actual and fiscal
**respectively
    IF rb_quan = c_x.
      IF rb_ord =  c_x.
        IF rec_finalord-meins <> l_uom_mat.
          CLEAR l_uom_mat.
        ENDIF.
        IF rec_finalord-meins <> l_uom_res.
          CLEAR l_uom_res.
        ENDIF.
        IF rec_finalord-meins <> l_uom_day.
          CLEAR l_uom_day.
        ENDIF.
        PERFORM calculate_percentage USING rec_finalord-gamng
                                           rec_finalord-diff
                                  CHANGING l_perc.
        PERFORM write_orders_daily USING rec_finalord-aufnr
                                         rec_finalord-gamng
                                         rec_finalord-menge
                                         rec_finalord-diff
                                         l_perc
                                         rec_finalord-meins.
      ELSE.
        PERFORM calculate_percentage USING rec_finalord-zplan
                                           rec_finalord-zdiff
                                  CHANGING l_perc.
        PERFORM write_orders_daily USING rec_finalord-aufnr
                                         rec_finalord-zplan
                                         rec_finalord-zact
                                         rec_finalord-zdiff
                                         l_perc
                                         rec_finalord-zmeins.
      ENDIF.
    ELSE.
      PERFORM calculate_percentage USING rec_finalord-fplan
                                         rec_finalord-fdiff
                                CHANGING l_perc.
      PERFORM write_orders_daily USING rec_finalord-aufnr
                                       rec_finalord-fplan
                                       rec_finalord-fact
                                       rec_finalord-fdiff
                                       l_perc
                                       rec_finalord-fcurr.
    ENDIF.
** AT the End of matnr arbpl and wdate calculate the Totals for the day
*if all the UOM s are matching else display the Totals with **
    AT  END OF matnr.
      SUM.
      ULINE /1(109).
      IF cb_color = c_x.
        FORMAT COLOR col_total intensified on.
      ENDIF.
      PERFORM display_totals USING l_uom_mat text-088 .
    ENDAT.

    AT END OF arbpl.
      SUM.
      ULINE /1(109).
      IF cb_color = c_x.
        FORMAT COLOR col_key intensified off.
      ENDIF.
      PERFORM display_totals USING l_uom_res text-089.
    ENDAT.

    AT END OF wdate.
      SUM.
      ULINE /1(109).
      IF cb_color = c_x.
        FORMAT COLOR COL_POSITIVE intensified off.
      ENDIF.
      PERFORM display_totals USING l_uom_day text-090.
      ULINE /1(109).
    ENDAT.

  ENDLOOP.

ENDFORM.                    " display_daily_report

*&---------------------------------------------------------------------*
*&     FORM DISPLAY_DAY_HEADER
*&---------------------------------------------------------------------*
*    Subroutine to Display Day Header
*----------------------------------------------------------------------*
FORM display_day_header.
  IF cb_color = c_x.
    FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
  ENDIF.
  ULINE  /1(109).
  WRITE: /1 sy-vline,
          2 text-072,
          8 rec_finalord-wdate,
          19 rec_finalord-wkday,
          109 sy-vline.

ENDFORM.                    " display_day_header

&---------------------------------------------------------------------*
*&      FORM  DISPLAY_RESOURCE_HEADER
*&---------------------------------------------------------------------*
*  Subroutine to display Resource Header
*----------------------------------------------------------------------*
FORM display_resource_header .
  CLEAR: v_rtext.
  IF cb_color = c_x.
    FORMAT COLOR col_key INTENSIFIED Off .
  ENDIF.
  ULINE  /1(109).
  WRITE: /1 sy-vline,
          2 text-073,
          14 rec_finalord-arbpl.

  PERFORM get_resdesc USING  rec_finalord-arbpl
                              p_werks
                     CHANGING v_rtext.
  WRITE:    30 v_rtext,
            109 sy-vline.

ENDFORM.                    " display_resource_header

*&---------------------------------------------------------------------*
*&      FORM  DISPLAY_MATERIAL_HEADER
*&---------------------------------------------------------------------*
*   Subroutine to Display Material Header
*----------------------------------------------------------------------*
FORM display_material_header.

  IF cb_color = c_x.
    FORMAT COLOR col_group INTENSIFIED OFF.
  ENDIF.
  ULINE  /1(109).
  WRITE: /1 sy-vline,
          2 text-074,
          14 rec_finalord-matnr,
          35 rec_finalord-maktx,
          77 rec_finalord-mtart,
          109 sy-vline.
  ULINE /1(109).

ENDFORM.                    " display_material_header

*&---------------------------------------------------------------------*
*&      FORM  GET_DAYNAME
*&---------------------------------------------------------------------*
*    Subroutine to get the day names
*----------------------------------------------------------------------*
*      -->P_I_FINALORD_WDATE  text
*      <--P_I_FINALORD_WKDAY  text
*----------------------------------------------------------------------*
FORM get_dayname  USING     p_i_finalord_wdate
                  CHANGING  p_i_finalord_wkday.
  CALL FUNCTION 'DATE_TO_DAY'
    EXPORTING
      date    = p_i_finalord_wdate
    IMPORTING
      weekday = p_i_finalord_wkday.

ENDFORM.                    " get_dayname

*&---------------------------------------------------------------------*
*&      FORM  DISPLAY_ORDER_HEADER
*&---------------------------------------------------------------------*
*      Subroutine to display Order Header
*----------------------------------------------------------------------*
FORM display_order_header .
  DATA: l_text1(25)  TYPE c,
        l_text2(25)  TYPE c,
        l_text3(25)  TYPE c.

  IF cb_color = c_x.
    FORMAT COLOR col_heading INTENSIFIED On.
  ENDIF.
  IF rb_quan = c_x.
    WRITE: /1 sy-vline,
           text-076,
           19 sy-vline,
           text-077,
           38 sy-vline,
           text-078,
           59 sy-vline,
           text-079,
           82 sy-vline,
           text-081,
           103 sy-vline,
           text-082,
           109 sy-vline.
  ELSE.
    CONCATENATE  text-083  v_waers INTO  l_text1 SEPARATED BY space.
    CONCATENATE  text-084  v_waers INTO  l_text2 SEPARATED BY space.
    CONCATENATE  text-085  v_waers INTO  l_text3 SEPARATED BY space.
    WRITE: / sy-vline,
             text-076,
         19  sy-vline,
             l_text1,
         38  sy-vline,
             l_text2,
         59  sy-vline,
             l_text3,
         82  sy-vline,
             text-081,
         103 sy-vline,
             text-087,
         109  sy-vline.
  ENDIF.
  ULINE /1(109).

ENDFORM.                    " display_order_header

*&---------------------------------------------------------------------*
*&      FORM  DISPLAY_TOTALS
*&---------------------------------------------------------------------*
*   Subroutine to Display Daily Totals
*----------------------------------------------------------------------*
*      -->P_i_UOM  text
*      -->P_i_MAT  text
*----------------------------------------------------------------------*
FORM display_totals USING    p_i_uom
                             p_i_mat.
  DATA: l_perc TYPE disptype.

  PERFORM write_vlines_daily.
  IF rb_quan = c_x.
    IF rb_ord = c_x.
      IF NOT p_i_uom IS INITIAL.
        CLEAR l_perc.
        PERFORM calculate_percentage USING i_finalord-gamng
                                           i_finalord-diff
                                CHANGING   l_perc.
        PERFORM write_orders_daily USING   p_i_mat
                                           i_finalord-gamng
                                           i_finalord-menge
                                           i_finalord-diff
                                           l_perc
                                           p_i_uom.
      ELSE.
        PERFORM write_orders_daily USING   p_i_mat
                                           c_null
                                           c_null
                                           c_null
                                           c_na
                                           c_null.
      ENDIF.
    ELSE.
      CLEAR l_perc.
      PERFORM calculate_percentage USING i_finalord-zplan
                                         i_finalord-zdiff
                             CHANGING    l_perc.
      PERFORM write_orders_daily USING   p_i_mat
                                         i_finalord-zplan
                                         i_finalord-zact
                                         i_finalord-zdiff
                                         l_perc
                                         rec_finalord-zmeins.
    ENDIF.
  ELSE.
    CLEAR l_perc.
    PERFORM calculate_percentage USING  i_finalord-fplan
                                        i_finalord-fdiff
                           CHANGING     l_perc.
    PERFORM write_orders_daily USING    p_i_mat
                                        i_finalord-fplan
                                        i_finalord-fact
                                        i_finalord-fdiff
                                        l_perc
                                        rec_finalord-fcurr.
  ENDIF.

ENDFORM.                    " display_totals

*&---------------------------------------------------------------------*
*&      FORM  CALCULATE_PERCENTAGE
*&---------------------------------------------------------------------*
*  Subroutine To calculate Percentages
*----------------------------------------------------------------------*
*      -->P_L_PLANNED  text
*      -->P_L_DIFF  text
*      <--P_L_PERC  text
*----------------------------------------------------------------------*
FORM calculate_percentage  USING    p_l_planned
                                    p_l_diff
                           CHANGING p_l_perc.

  IF p_l_planned IS INITIAL.
    CLEAR p_l_perc.
  ELSE.
    p_l_perc =  ( 1 - ( p_l_diff / p_l_planned ) ) * 100.
  ENDIF.

ENDFORM.                    " calculate_percentage

*&---------------------------------------------------------------------*
*&      FORM  WRITE_VLINES_DAILY
*&---------------------------------------------------------------------*
*       Subroutine to write vlines
*----------------------------------------------------------------------*
FORM write_vlines_daily .
  WRITE: /1 sy-vline,
          19 sy-vline,
          38 sy-vline,
          59 sy-vline,
          82 sy-vline,
          103 sy-vline,
          109 sy-vline.
ENDFORM.                    " write_vlines_daily

*&---------------------------------------------------------------------*
*&      FORM  WRITE_ORDERS_DAILY
*&---------------------------------------------------------------------*
*       Subroutine to write daily orders
*----------------------------------------------------------------------*
*      -->P_I_AUFNR  text
*      -->P_I_PLANNED  text
*      -->P_I_ACTUAL  text
*      -->P_I_DIFF  text
*      -->P_I_PERC  text
*      -->P_I_UOM  text
*----------------------------------------------------------------------*
FORM write_orders_daily  USING    p_i_aufnr
                                  p_i_planned
                                  p_i_actual
                                  p_i_diff
                                  p_i_perc
                                  p_i_uom.
  WRITE:   2  p_i_aufnr.
  HIDE:       p_i_aufnr.
  WRITE:   21 p_i_planned,
           39 p_i_actual,
           62 p_i_diff,
           85 p_i_perc,
           104 p_i_uom.

ENDFORM.                    " write_orders_daily

*&---------------------------------------------------------------------*
*&      FORM  GET_MATDESC
*&---------------------------------------------------------------------*
*     Subroutine to get the material Descriptions
*----------------------------------------------------------------------*
*      -->P_I_MATNR  text
*      <--P_I_MAKTX  text
*----------------------------------------------------------------------*
FORM get_matdesc  USING    p_i_matnr
                  CHANGING p_i_maktx.

  STATICS: st_i_matdesc TYPE STANDARD TABLE OF ty_matdesc
            INITIAL SIZE 0 WITH HEADER LINE.

  CLEAR st_i_matdesc.
  READ TABLE st_i_matdesc WITH KEY matnr = p_i_matnr.
  IF sy-subrc <> 0.
    SELECT SINGLE matnr maktx
    INTO st_i_matdesc
    FROM makt
    WHERE matnr = p_i_matnr
    AND spras = sy-langu.
    IF sy-subrc = 0.
      APPEND st_i_matdesc.
    ENDIF.
  ENDIF.
  p_i_maktx = st_i_matdesc-maktx.

ENDFORM.                    " get_matdesc

&---------------------------------------------------------------------*
*&      FORM  SHOW_WEEKLY_ORDERS
*&---------------------------------------------------------------------*
*       Subroutine for Displaying weekly orders
*----------------------------------------------------------------------*
FORM show_weekly_orders .

  WRITE:  /(14) rec_weekly-aufnr.
  HIDE rec_weekly-aufnr.
  WRITE:        sy-vline,
           (6)  rec_weekly-mon_plan NO-SIGN,
           (6)  rec_weekly-mon_act  NO-SIGN,
           (6)  rec_weekly-mon_diff NO-SIGN,
           (4)  rec_weekly-mon_perc NO-SIGN,
                sy-vline,
           (6)  rec_weekly-tue_plan NO-SIGN,
           (6)  rec_weekly-tue_act NO-SIGN,
           (6)  rec_weekly-tue_diff NO-SIGN,
           (4)  rec_weekly-tue_perc NO-SIGN,
                sy-vline,
           (6)  rec_weekly-wed_plan NO-SIGN,
           (6)  rec_weekly-wed_act NO-SIGN,
           (6)  rec_weekly-wed_diff NO-SIGN,
           (4)  rec_weekly-wed_perc NO-SIGN,
                sy-vline,
           (6)  rec_weekly-thu_plan NO-SIGN,
           (6)  rec_weekly-thu_act NO-SIGN,
           (6)  rec_weekly-thu_diff NO-SIGN,
           (4)  rec_weekly-thu_perc NO-SIGN,
                sy-vline,
           (6)  rec_weekly-fri_plan NO-SIGN,
           (6)  rec_weekly-fri_act NO-SIGN,
           (6)  rec_weekly-fri_diff NO-SIGN,
           (4)  rec_weekly-fri_perc NO-SIGN,
                sy-vline,
           (6)  rec_weekly-sat_plan NO-SIGN,
           (6)  rec_weekly-sat_act NO-SIGN,
           (6)  rec_weekly-sat_diff NO-SIGN,
           (4)  rec_weekly-sat_perc NO-SIGN,
                sy-vline,
           (6)  rec_weekly-sun_plan NO-SIGN,
           (6)  rec_weekly-sun_act NO-SIGN,
           (6)  rec_weekly-sun_diff NO-SIGN,
           (4)  rec_weekly-sun_perc NO-SIGN,
                sy-vline,
           (6)  rec_weekly-tot_plan NO-SIGN,
           (6)  rec_weekly-tot_act NO-SIGN,
           (6)  rec_weekly-tot_diff NO-SIGN,
           (4)  rec_weekly-tot_perc NO-SIGN,
                sy-vline.
  IF rb_quan = c_x.
    WRITE: (3) rec_weekly-meins, 245 sy-vline.
  ELSE.
    WRITE: (3) rec_weekly-waers, 245 sy-vline.
  ENDIF.
  ULINE (245).

ENDFORM.                    " show_weekly_orders

*&---------------------------------------------------------------------*
*&      FORM  GET_WEEK_NAMES
*&---------------------------------------------------------------------*
*    Subroutine to get the week names
*----------------------------------------------------------------------*
FORM  get_week_names USING     p_i_date
                     CHANGING  p_i_wkday_wkday.
  CALL FUNCTION 'DATE_TO_DAY'
    EXPORTING
      date    = p_i_date
    IMPORTING
      weekday = p_i_wkday_wkday.

  APPEND i_wkday.

ENDFORM.                    " get_week_names

*&---------------------------------------------------------------------*
*&      Form  check_uom_week_res_mat
*&---------------------------------------------------------------------*
*      Subroutine To check UOM for summing at material Level
*----------------------------------------------------------------------*
FORM check_uom_week_res_mat.

  DATA l_meins TYPE meins.

  READ TABLE i_wkday INDEX 1.
  READ TABLE i_week1 WITH KEY spwoc = i_weekly-spwoc
                              wkday = i_wkday-wkday
                              arbpl = i_weekly-arbpl
                              matnr = i_weekly-matnr.

  IF sy-subrc <> 0 OR i_week1-meins = c_null.
    CLEAR: i_weekly-mon_plan,
           i_weekly-mon_act,
           i_weekly-mon_diff,
           i_weekly-mon_perc.
  ENDIF.
  READ TABLE i_wkday INDEX 2.
  READ TABLE i_week1 WITH KEY spwoc = i_weekly-spwoc
                              wkday = i_wkday-wkday
                              arbpl = i_weekly-arbpl
                              matnr = i_weekly-matnr.

  IF sy-subrc <> 0 OR i_week1-meins = c_null.
    CLEAR: i_weekly-tue_plan,
           i_weekly-tue_act,
           i_weekly-tue_diff,
           i_weekly-tue_perc.
  ENDIF.

  READ TABLE i_wkday INDEX 3.
  READ TABLE i_week1 WITH KEY spwoc = i_weekly-spwoc
                              wkday = i_wkday-wkday
                              arbpl = i_weekly-arbpl
                              matnr = i_weekly-matnr.

  IF sy-subrc <> 0 OR i_week1-meins = c_null.
    CLEAR: i_weekly-wed_plan,
           i_weekly-wed_act,
           i_weekly-wed_diff,
           i_weekly-wed_perc.
  ENDIF.

  READ TABLE i_wkday INDEX 4.
  READ TABLE i_week1 WITH KEY spwoc = i_weekly-spwoc
                              wkday = i_wkday-wkday
                              arbpl = i_weekly-arbpl
                              matnr = i_weekly-matnr.

  IF sy-subrc <> 0 OR i_week1-meins = c_null.
    CLEAR: i_weekly-thu_plan,
           i_weekly-thu_act,
           i_weekly-thu_diff,
           i_weekly-thu_perc.
  ENDIF.

  READ TABLE i_wkday INDEX 5.
  READ TABLE i_week1 WITH KEY spwoc = i_weekly-spwoc
                              wkday = i_wkday-wkday
                              arbpl = i_weekly-arbpl
                              matnr = i_weekly-matnr.

  IF sy-subrc <> 0 OR i_week1-meins = c_null.
    CLEAR: i_weekly-fri_plan,
           i_weekly-fri_act,
           i_weekly-fri_diff,
           i_weekly-fri_perc.
  ENDIF.

  READ TABLE i_wkday INDEX 6.
  READ TABLE i_week1 WITH KEY spwoc = i_weekly-spwoc
                              wkday = i_wkday-wkday
                              arbpl = i_weekly-arbpl
                              matnr = i_weekly-matnr.

  IF sy-subrc <> 0 OR i_week1-meins = c_null.
    CLEAR: i_weekly-sat_plan,
           i_weekly-sat_act,
           i_weekly-sat_diff,
           i_weekly-sat_perc.
  ENDIF.

  READ TABLE i_wkday INDEX 7.
  READ TABLE i_week1 WITH KEY spwoc = i_weekly-spwoc
                              wkday = i_wkday-wkday
                              arbpl = i_weekly-arbpl
                              matnr = i_weekly-matnr.

  IF sy-subrc <> 0 OR i_week1-meins = c_null.
    CLEAR: i_weekly-sun_plan,
           i_weekly-sun_act,
           i_weekly-sun_diff,
           i_weekly-sun_perc.
  ENDIF.

  LOOP AT i_week1 WHERE spwoc = i_weekly-spwoc AND
                        arbpl = i_weekly-arbpl AND
                        matnr = i_weekly-matnr.
    IF i_week1-meins = c_null.
      CLEAR: i_weekly-tot_plan,
             i_weekly-tot_act,
             i_weekly-tot_diff,
             i_weekly-tot_perc.
      EXIT.
    ELSE.
      IF sy-tabix = 1.
        l_meins = i_week1-meins.
      ELSE.
        IF i_week1-meins <> l_meins.
          CLEAR: i_weekly-tot_plan,
                 i_weekly-tot_act,
                 i_weekly-tot_diff,
                 i_weekly-tot_perc.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " check_uom_week_res_mat

*&---------------------------------------------------------------------*
*&      Form  check_uom_week_res
*&---------------------------------------------------------------------*
*       Checking the UOM for summation at resource Level.
*----------------------------------------------------------------------*
FORM check_uom_week_res .

  DATA l_meins TYPE meins.

  READ TABLE i_wkday  INDEX 1.
  READ TABLE i_week2 WITH KEY spwoc = i_weekly-spwoc
                              wkday = i_wkday-wkday
                              arbpl = i_weekly-arbpl.
  IF sy-subrc <> 0 OR i_week2-meins = c_null.
    CLEAR: i_weekly-mon_plan,
           i_weekly-mon_act,
           i_weekly-mon_diff,
           i_weekly-mon_perc.
  ENDIF.

  READ TABLE i_wkday INDEX 2.
  READ TABLE i_week2 WITH KEY spwoc = i_weekly-spwoc
                              wkday = i_wkday-wkday
                              arbpl = i_weekly-arbpl.
  IF sy-subrc <> 0 OR i_week2-meins = c_null.
    CLEAR: i_weekly-tue_plan,
           i_weekly-tue_act,
           i_weekly-tue_diff,
           i_weekly-tue_perc.
  ENDIF.

  READ TABLE i_wkday  INDEX 3.
  READ TABLE i_week2 WITH KEY spwoc = i_weekly-spwoc
                              wkday = i_wkday-wkday
                              arbpl = i_weekly-arbpl.
  IF sy-subrc <> 0 OR i_week2-meins = c_null.
    CLEAR: i_weekly-wed_plan,
           i_weekly-wed_act,
           i_weekly-wed_diff,
           i_weekly-wed_perc.
  ENDIF.

  READ TABLE i_wkday  INDEX 4.
  READ TABLE i_week2 WITH KEY spwoc = i_weekly-spwoc
                              wkday = i_wkday-wkday
                              arbpl = i_weekly-arbpl.
  IF sy-subrc <> 0 OR i_week2-meins = c_null.
    CLEAR: i_weekly-thu_plan,
           i_weekly-thu_act,
           i_weekly-thu_diff,
           i_weekly-thu_perc.
  ENDIF.

  READ TABLE i_wkday  INDEX 5.
  READ TABLE i_week2 WITH KEY spwoc = i_weekly-spwoc
                              wkday = i_wkday-wkday
                              arbpl = i_weekly-arbpl.
  IF sy-subrc <> 0 OR i_week2-meins = c_null.
    CLEAR: i_weekly-fri_plan,
           i_weekly-fri_act,
           i_weekly-fri_diff,
           i_weekly-fri_perc.
  ENDIF.

  READ TABLE i_wkday  INDEX 6.
  READ TABLE i_week2 WITH KEY spwoc = i_weekly-spwoc
                              wkday = i_wkday-wkday
                              arbpl = i_weekly-arbpl.
  IF sy-subrc <> 0 OR i_week2-meins = c_null.
    CLEAR: i_weekly-sat_plan,
           i_weekly-sat_act,
           i_weekly-sat_diff,
           i_weekly-sat_perc.
  ENDIF.

  READ TABLE i_wkday  INDEX 7.
  READ TABLE i_week2 WITH KEY spwoc = i_weekly-spwoc
                              wkday = i_wkday-wkday
                              arbpl = i_weekly-arbpl.
  IF sy-subrc <> 0 OR i_week2-meins = c_null.
    CLEAR: i_weekly-sun_plan,
           i_weekly-sun_act,
           i_weekly-sun_diff,
           i_weekly-sun_perc.
  ENDIF.

  LOOP AT i_week2 WHERE spwoc = i_weekly-spwoc AND
                         arbpl = i_weekly-arbpl.
    IF i_week2-meins = c_null.
      CLEAR: i_weekly-tot_plan,
             i_weekly-tot_act,
             i_weekly-tot_diff,
             i_weekly-tot_perc.
      EXIT.
    ELSE.
      IF sy-tabix = 1.
        l_meins = i_week2-meins.
      ELSE.
        IF i_week2-meins <> l_meins.
          CLEAR: i_weekly-tot_plan,
                 i_weekly-tot_act,
                 i_weekly-tot_diff,
                 i_weekly-tot_perc.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " check_uom_week_res

*&---------------------------------------------------------------------*
*&      Form  check_uom_week
*&---------------------------------------------------------------------*
*       Checking the UOM for summation at week Level.
*----------------------------------------------------------------------*
FORM check_uom_week .

  DATA l_meins TYPE meins.
  READ TABLE i_wkday  INDEX 1.
  READ TABLE i_week3 WITH KEY spwoc = i_weekly-spwoc
                              wkday = i_wkday-wkday.

  IF sy-subrc <> 0 OR i_week3-meins = c_null.
    CLEAR: i_weekly-mon_plan,
           i_weekly-mon_act,
           i_weekly-mon_diff,
           i_weekly-mon_perc.
  ENDIF.

  READ TABLE i_wkday INDEX 2.
  READ TABLE i_week3 WITH KEY spwoc = i_weekly-spwoc
                              wkday = i_wkday-wkday.

  IF sy-subrc <> 0 OR i_week3-meins = c_null.
    CLEAR: i_weekly-tue_plan,
           i_weekly-tue_act,
           i_weekly-tue_diff,
           i_weekly-tue_perc.
  ENDIF.

  READ TABLE i_wkday  INDEX 3.
  READ TABLE i_week3 WITH KEY spwoc = i_weekly-spwoc
                              wkday = i_wkday-wkday.

  IF sy-subrc <> 0 OR i_week3-meins = c_null.
    CLEAR: i_weekly-wed_plan,
           i_weekly-wed_act,
           i_weekly-wed_diff,
           i_weekly-wed_perc.
  ENDIF.

  READ TABLE i_wkday  INDEX 4.
  READ TABLE i_week3 WITH KEY spwoc = i_weekly-spwoc
                              wkday = i_wkday-wkday.

  IF sy-subrc <> 0 OR i_week3-meins = c_null.
    CLEAR: i_weekly-thu_plan,
           i_weekly-thu_act,
           i_weekly-thu_diff,
           i_weekly-thu_perc.
  ENDIF.

  READ TABLE i_wkday  INDEX 5.
  READ TABLE i_week3 WITH KEY spwoc = i_weekly-spwoc
                              wkday = i_wkday-wkday.

  IF sy-subrc <> 0 OR i_week3-meins = c_null.
    CLEAR: i_weekly-fri_plan,
           i_weekly-fri_act,
           i_weekly-fri_diff,
           i_weekly-fri_perc.
  ENDIF.

  READ TABLE i_wkday  INDEX 6.
  READ TABLE i_week3 WITH KEY spwoc = i_weekly-spwoc
                              wkday = i_wkday-wkday.

  IF sy-subrc <> 0 OR i_week3-meins = c_null.
    CLEAR: i_weekly-sat_plan,
           i_weekly-sat_act,
           i_weekly-sat_diff,
           i_weekly-sat_perc.
  ENDIF.

  READ TABLE i_wkday  INDEX 7.
  READ TABLE i_week3 WITH KEY spwoc = i_weekly-spwoc
                              wkday = i_wkday-wkday.

  IF sy-subrc <> 0 OR i_week3-meins = c_null.
    CLEAR: i_weekly-sun_plan,
           i_weekly-sun_act,
           i_weekly-sun_diff,
           i_weekly-sun_perc.
  ENDIF.

  LOOP AT i_week3 WHERE  spwoc = i_weekly-spwoc.
    IF i_week3-meins = c_null.
      CLEAR: i_weekly-tot_plan,
             i_weekly-tot_act,
             i_weekly-tot_diff,
             i_weekly-tot_perc.
      EXIT.
    ELSE.
      IF sy-tabix = 1.
        l_meins = i_week3-meins.
      ELSE.
        IF i_week3-meins <> l_meins.
          CLEAR: i_weekly-tot_plan,
                 i_weekly-tot_act,
                 i_weekly-tot_diff,
                 i_weekly-tot_perc.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " check_uom_week

*&---------------------------------------------------------------------*
*&      FORM  DISPLAY_WK_TOT
*&---------------------------------------------------------------------*
*       Subroutine to display the week day Totals
*----------------------------------------------------------------------*
FORM display_wk_tot USING p_i_text.
  WRITE:  /(14) p_i_text,
                 sy-vline,
            (6)  i_weekly-mon_plan NO-SIGN,
            (6)  i_weekly-mon_act  NO-SIGN,
            (6)  i_weekly-mon_diff NO-SIGN,
            (4)  i_weekly-mon_perc NO-SIGN,
                 sy-vline,
            (6)  i_weekly-tue_plan NO-SIGN,
            (6)  i_weekly-tue_act NO-SIGN,
            (6)  i_weekly-tue_diff NO-SIGN,
            (4)  i_weekly-tue_perc NO-SIGN,
                 sy-vline,
            (6)  i_weekly-wed_plan NO-SIGN,
            (6)  i_weekly-wed_act NO-SIGN,
            (6)  i_weekly-wed_diff NO-SIGN,
            (4)  i_weekly-wed_perc NO-SIGN,
                 sy-vline,
            (6)  i_weekly-thu_plan NO-SIGN,
            (6)  i_weekly-thu_act NO-SIGN,
            (6)  i_weekly-thu_diff NO-SIGN,
            (4)  i_weekly-thu_perc NO-SIGN,
                 sy-vline,
            (6)  i_weekly-fri_plan NO-SIGN,
            (6)  i_weekly-fri_act NO-SIGN,
            (6)  i_weekly-fri_diff NO-SIGN,
            (4)  i_weekly-fri_perc NO-SIGN,
                 sy-vline,
            (6)  i_weekly-sat_plan NO-SIGN,
            (6)  i_weekly-sat_act NO-SIGN,
            (6)  i_weekly-sat_diff NO-SIGN,
            (4)  i_weekly-sat_perc NO-SIGN,
                 sy-vline,
            (6)  i_weekly-sun_plan NO-SIGN,
            (6)  i_weekly-sun_act NO-SIGN,
            (6)  i_weekly-sun_diff NO-SIGN,
            (4)  i_weekly-sun_perc NO-SIGN,
                 sy-vline,
            (6)  i_weekly-tot_plan NO-SIGN,
            (6)  i_weekly-tot_act  NO-SIGN,
            (6)  i_weekly-tot_diff NO-SIGN,
            (4)  i_weekly-tot_perc NO-SIGN,
                 sy-vline.
  IF rb_fval = c_x.
    WRITE: (3) rec_weekly-waers.
  ENDIF.
  WRITE: 245 sy-vline.
  ULINE (245).

ENDFORM.                    " display_wk_tot.

*&---------------------------------------------------------------------*
*&          FORM  CALCULATE_WK_PERC
*&---------------------------------------------------------------------*
*           Subroutine to Calculate Week Percentages
*----------------------------------------------------------------------*
FORM calculate_wk_perc.
  PERFORM calculate_percentage  USING    i_weekly-mon_plan
                                         i_weekly-mon_diff
                                CHANGING i_weekly-mon_perc.

  PERFORM calculate_percentage  USING    i_weekly-tue_plan
                                         i_weekly-tue_diff
                                CHANGING i_weekly-tue_perc.

  PERFORM calculate_percentage  USING    i_weekly-wed_plan
                                         i_weekly-wed_diff
                                CHANGING i_weekly-wed_perc.

  PERFORM calculate_percentage  USING    i_weekly-thu_plan
                                         i_weekly-thu_diff
                                CHANGING i_weekly-thu_perc.

  PERFORM calculate_percentage  USING    i_weekly-fri_plan
                                         i_weekly-fri_diff
                                CHANGING i_weekly-fri_perc.

  PERFORM calculate_percentage  USING    i_weekly-sat_plan
                                         i_weekly-sat_diff
                                CHANGING i_weekly-sat_perc.

  PERFORM calculate_percentage  USING    i_weekly-sun_plan
                                         i_weekly-sun_diff
                                CHANGING i_weekly-sun_perc.

  PERFORM calculate_percentage  USING    i_weekly-tot_plan
                                         i_weekly-tot_diff
                                CHANGING i_weekly-tot_perc.

ENDFORM.                    "  calculate_wk_prec

*&---------------------------------------------------------------------*
*&          FORM  CALCULATE_WK_PERC
*&---------------------------------------------------------------------*
*           Subroutine to Calculate Week Percentages
*----------------------------------------------------------------------*
FORM calculate_wk_perc.
  PERFORM calculate_percentage  USING    i_weekly-mon_plan
                                         i_weekly-mon_diff
                                CHANGING i_weekly-mon_perc.

  PERFORM calculate_percentage  USING    i_weekly-tue_plan
                                         i_weekly-tue_diff
                                CHANGING i_weekly-tue_perc.

  PERFORM calculate_percentage  USING    i_weekly-wed_plan
                                         i_weekly-wed_diff
                                CHANGING i_weekly-wed_perc.

  PERFORM calculate_percentage  USING    i_weekly-thu_plan
                                         i_weekly-thu_diff
                                CHANGING i_weekly-thu_perc.

  PERFORM calculate_percentage  USING    i_weekly-fri_plan
                                         i_weekly-fri_diff
                                CHANGING i_weekly-fri_perc.

  PERFORM calculate_percentage  USING    i_weekly-sat_plan
                                         i_weekly-sat_diff
                                CHANGING i_weekly-sat_perc.

  PERFORM calculate_percentage  USING    i_weekly-sun_plan
                                         i_weekly-sun_diff
                                CHANGING i_weekly-sun_perc.

  PERFORM calculate_percentage  USING    i_weekly-tot_plan
                                         i_weekly-tot_diff
                                CHANGING i_weekly-tot_perc.

ENDFORM.                    "  calculate_wk_prec

*&---------------------------------------------------------------------*
*&   FORM  DISPLAY_FLOW
*&---------------------------------------------------------------------*
* Subroutine for to displaying the COR3 transaction based on order No
*----------------------------------------------------------------------*
*      -->P_I_WEEKLY_AUFNR  text
*----------------------------------------------------------------------*
FORM display_flow USING    p_i_weekly-aufnr.
  IF NOT p_i_weekly-aufnr IS INITIAL.
    SET PARAMETER ID c_br1 FIELD p_i_weekly-aufnr.
    CALL TRANSACTION c_tcode AND  SKIP FIRST SCREEN.
  ENDIF.
ENDFORM.                    " display_flow

*&---------------------------------------------------------------------*
*&      FORM  DISPLAY_REPORT_FOOTER
*&---------------------------------------------------------------------*
*  Subroutine to display the report footer
*----------------------------------------------------------------------*
FORM display_report_footer .
  FORMAT COLOR COL_BACKGROUND.
  CALL FUNCTION '/NESGLB/X_PRINT_REPORT_FOOTER'
    EXPORTING
      linsz = sy-linsz
      linct = sy-linct
      linno = sy-linno.
ENDFORM.                    " display_report_footer
