*&---------------------------------------------------------------------*
*& Report ZAADT_UTIL_UPL_ZIP
*&---------------------------------------------------------------------*
REPORT zaadt_util_upl_zip.

DATA:git_seltexts TYPE TABLE OF rsseltexts,
     gwa_seltexts TYPE rsseltexts.
CONSTANTS:gc_separator TYPE char1 VALUE '^'.
CONSTANTS:gc_dclas TYPE char40 VALUE 'ZCL_AADT_CIPHER'.
CONSTANTS:gc_method TYPE string VALUE 'DECRYPT_STRING2STRING'.
CONSTANTS:gc_log_table TYPE  e071-obj_name VALUE 'ZATB_OBJECTS'.
CONSTANTS:gc_code_table TYPE  e071-obj_name VALUE 'ZDEPLOYED_CODE'.
DATA:gv_key TYPE string.
DATA :gv_db    TYPE char10 , "VALUE sy-dbsys,
      gv_ehp   TYPE host   , "VALUE sy-host,
      gv_inst  TYPE c  LENGTH 10,
      gv_dclnt TYPE mandt.  "VALUE sy-mandt.

TYPES:BEGIN OF ty_code,
        col1 TYPE string,
        col2 TYPE string,
        col3 TYPE string,
        col4 TYPE string,
        col5 TYPE string,
        col6 TYPE string,
        col7 TYPE string,
        col8 TYPE string,
      END OF ty_code.
TYPES:BEGIN OF gty_range,
        sign   TYPE c LENGTH 1,
        option TYPE c LENGTH 2,
        low    TYPE char10,
        high   TYPE char10,
      END OF gty_range.

DATA:git_code TYPE STANDARD TABLE OF ty_code.
DATA:git_ulist TYPE STANDARD TABLE OF ty_code.
DATA:gwa_curr_obj TYPE ty_code.
DATA:gwa_code TYPE ty_code.
DATA:gwa_code2 TYPE ty_code.
DATA:gwa_ulist TYPE ty_code.
DATA it_code TYPE STANDARD TABLE OF string.

DATA: go_ref TYPE REF TO object.
DATA: ptab      TYPE abap_parmbind_tab,
      ptab_line TYPE abap_parmbind,
      i         TYPE i,
      lv_flag   TYPE char1.

DATA:r_enc_key_id   TYPE TABLE OF gty_range,
     gwa_enc_key_id TYPE gty_range.


SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE aaa.
PARAMETERS:p_zfilep TYPE  string DEFAULT 'C:\' OBLIGATORY LOWER CASE .
SELECTION-SCREEN: END OF BLOCK blk1.

INITIALIZATION.
  PERFORM sub_set_sel_text_comm.

AT SELECTION-SCREEN OUTPUT.
  PERFORM sub_default_data.
  PERFORM sub_set_scr_flds.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_zfilep.
  PERFORM sub_f4_file_path CHANGING p_zfilep.

START-OF-SELECTION.

  PERFORM sub_get_xl_data.
  PERFORM sub_get_uniq_list.
  PERFORM generate_key.


  IF git_ulist IS NOT INITIAL.
    LOOP AT git_ulist INTO gwa_curr_obj.

      PERFORM sub_cipher_check CHANGING lv_flag.
      IF lv_flag = abap_false.
        CLEAR ptab_line.
        REFRESH:ptab.
        ptab_line-name = 'KEY_VAL'.
        ptab_line-kind = cl_abap_objectdescr=>exporting.
        GET REFERENCE OF gv_key INTO  ptab_line-value .
        INSERT ptab_line INTO TABLE ptab.
        CREATE OBJECT go_ref TYPE (gc_dclas) PARAMETER-TABLE ptab.
      ENDIF.

      CASE gwa_curr_obj-col1.
        WHEN 'TEST'.
          DELETE git_ulist INDEX 2.
          PERFORM sub_confirm  USING gwa_curr_obj-col2.
        WHEN 'DOMA'.
          PERFORM sub_cre_doma USING gwa_curr_obj-col2.
        WHEN 'DTEL'.
          PERFORM sub_cre_dtel USING gwa_curr_obj-col2.
        WHEN 'TABL'.
          PERFORM sub_cre_tabl USING gwa_curr_obj-col2.
        WHEN 'TTYP'.
          PERFORM sub_cre_ttyp USING gwa_curr_obj-col2.
        WHEN 'PROG'.
          PERFORM sub_cre_prog USING gwa_curr_obj-col2.
        WHEN 'CLAS'.
          PERFORM sub_cre_clas USING gwa_curr_obj-col2.
        WHEN OTHERS.
*         Log message object type not supported
      ENDCASE.
    ENDLOOP.
  ENDIF.
  READ REPORT 'ZPRAD_WRAP_PROG' INTO it_code.
  IF sy-subrc EQ 0.
    PERFORM hide_code.
  ENDIF.

END-OF-SELECTION.

  PERFORM sub_disply_log.


FORM sub_set_sel_text_comm.
  aaa = 'Please enter path to upload .zip file'.
  CLEAR gwa_seltexts.
  gwa_seltexts-name = 'P_ZFILEP'.
  gwa_seltexts-kind = 'P'.
  gwa_seltexts-text = '.zip File path'.
  APPEND gwa_seltexts TO git_seltexts.
  CLEAR gwa_seltexts.
  gwa_seltexts-name = 'gv_db'.
  gwa_seltexts-kind = 'P'.
  gwa_seltexts-text = 'Current System Database'.
  APPEND gwa_seltexts TO git_seltexts.
  CLEAR gwa_seltexts.
  gwa_seltexts-name = 'gv_ehp'.
  gwa_seltexts-kind = 'P'.
  gwa_seltexts-text = 'Current System Host Name'.
  APPEND gwa_seltexts TO git_seltexts.
  CLEAR gwa_seltexts.
  gwa_seltexts-name = 'gv_inst'.
  gwa_seltexts-kind = 'P'.
  gwa_seltexts-text = 'Current System Installation No.'.
  APPEND gwa_seltexts TO git_seltexts.
  CLEAR gwa_seltexts.
  gwa_seltexts-name = 'gv_dclnt'.
  gwa_seltexts-kind = 'P'.
  gwa_seltexts-text = 'Current System Dev Client'.
  APPEND gwa_seltexts TO git_seltexts.

  CALL FUNCTION 'SELECTION_TEXTS_MODIFY'
    EXPORTING
      program                     = sy-cprog
    TABLES
      seltexts                    = git_seltexts
    EXCEPTIONS
      program_not_found           = 1
      program_cannot_be_generated = 2
      OTHERS                      = 3.

  CLEAR:gwa_enc_key_id.
  REFRESH:r_enc_key_id.

  gwa_enc_key_id-sign    = 'I'.
  gwa_enc_key_id-option  = 'EQ'.
  gwa_enc_key_id-low     = 'CODE'.
  APPEND gwa_enc_key_id TO r_enc_key_id.
  gwa_enc_key_id-low     = 'LIMP'.
  APPEND gwa_enc_key_id TO r_enc_key_id.
  gwa_enc_key_id-low     = 'LDEF'.
  APPEND gwa_enc_key_id TO r_enc_key_id.
  gwa_enc_key_id-low     = 'LMAC'.
  APPEND gwa_enc_key_id TO r_enc_key_id.
  gwa_enc_key_id-low     = 'LTES'.
  APPEND gwa_enc_key_id TO r_enc_key_id.
  gwa_enc_key_id-low     = 'METC'.
  APPEND gwa_enc_key_id TO r_enc_key_id.
ENDFORM.
FORM sub_set_scr_flds.
  LOOP AT SCREEN.
    CHECK screen-group1 = 'ALL'.
    screen-input       = '0'.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.
FORM sub_default_data.
  CALL FUNCTION 'SLIC_GET_LICENCE_NUMBER'
    IMPORTING
      license_number = gv_inst.
ENDFORM.
FORM sub_get_xl_data.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = p_zfilep
      filetype                = 'ASC'
      has_field_separator     = cl_abap_char_utilities=>horizontal_tab
      read_by_line            = 'X'
    TABLES
      data_tab                = git_code
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

ENDFORM.
FORM sub_get_uniq_list.
  git_ulist[] = git_code[].
  DELETE git_ulist WHERE col1 CS 'PROG' AND col3 CS 'PROP'.   " Temporary workaround
  DELETE ADJACENT DUPLICATES FROM git_ulist COMPARING col1 col2.
  SORT git_ulist BY col7 DESCENDING.
ENDFORM.
FORM sub_cre_doma USING p_doma TYPE string.
  CONSTANTS:lc_domain TYPE trobjtype VALUE 'DOMA'.
  DATA:lv_domain TYPE ddobjname .
  DATA:lv_state TYPE char1.
  DATA:dd01v_wa TYPE dd01v.
  DATA:dd07v_wa TYPE dd07v.
  DATA:dd07v_tab TYPE TABLE OF dd07v.
  DATA:lv_rc TYPE sy-subrc.
  DATA:lv_correct TYPE char1.
  DATA:lwa_log TYPE ty_code.
  MOVE p_doma TO lv_domain.
  PERFORM sub_check_exists USING    lc_domain lv_domain
                           CHANGING lv_state.
  IF lv_state IS INITIAL.
    LOOP AT git_code INTO gwa_code WHERE col1 CS lc_domain AND
                                         col2 CS p_doma.
      CASE gwa_code-col3.
        WHEN 'DEFN'.
          MOVE gwa_code-col8 TO dd01v_wa.
        WHEN 'VALU'.
          MOVE gwa_code-col8 TO dd07v_wa.
          APPEND dd07v_wa TO dd07v_tab.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
    DELETE git_code WHERE col1 CS lc_domain AND
                          col2 CS p_doma.
    CALL FUNCTION 'DDIF_DOMA_PUT'
      EXPORTING
        name              = dd01v_wa-domname
        dd01v_wa          = dd01v_wa
      TABLES
        dd07v_tab         = dd07v_tab
      EXCEPTIONS
        doma_not_found    = 1
        name_inconsistent = 2
        doma_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.

    IF sy-subrc = 0.
      PERFORM sub_check USING  lc_domain lv_domain
                        CHANGING lv_correct.

      IF lv_correct EQ abap_true.
        CALL FUNCTION 'DDIF_DOMA_ACTIVATE'
          EXPORTING
            name        = dd01v_wa-domname
          IMPORTING
            rc          = lv_rc
          EXCEPTIONS
            not_found   = 1
            put_failure = 2
            OTHERS      = 3.

        IF sy-subrc = 0.
          CLEAR lwa_log.
          lwa_log-col1 = lc_domain.
          lwa_log-col2 = p_doma.
          lwa_log-col3 = 'Domain successfully created and activated.'.
          lwa_log-col4 = '@08@'.
          MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
        ELSE.
          CLEAR lwa_log.
          lwa_log-col1 = lc_domain.
          lwa_log-col2 = p_doma.
          lwa_log-col3 = 'Domain created but activation failed.'.
          lwa_log-col4 = '@0A@'.
          MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
        ENDIF.
      ELSE.
        CLEAR lwa_log.
        lwa_log-col1 = lc_domain.
        lwa_log-col2 = p_doma.
        lwa_log-col3 = 'Domain created but with errors.'.
        lwa_log-col4 = '@0A@'.
        MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
      ENDIF.
    ELSE.
      CLEAR lwa_log.
      lwa_log-col1 = lc_domain.
      lwa_log-col2 = p_doma.
      lwa_log-col3 = 'Error in domain creation function module.'.
      lwa_log-col4 = '@0A@'.
      MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
    ENDIF.
  ELSE.
    CLEAR lwa_log.
    lwa_log-col1 = lc_domain.
    lwa_log-col2 = p_doma.
    lwa_log-col3 = 'Domain already exists in the system.'.
    lwa_log-col4 = '@09@'.
    MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
  ENDIF.
  PERFORM log_table_check CHANGING lv_state.
  IF lv_state = 'A'.
    PERFORM log_obj_tabl USING lc_domain lv_domain lwa_log-col3.
  ENDIF.

ENDFORM.
FORM sub_cre_dtel USING p_dtel TYPE string.
  CONSTANTS:lc_dataelement TYPE trobjtype VALUE 'DTEL'.
  DATA:lv_dataelement TYPE ddobjname .
  DATA:lv_state TYPE char1.
  DATA:lv_rc TYPE sy-subrc.
  DATA:lv_correct TYPE char1.
  DATA:dd04v_wa TYPE dd04v.
  MOVE p_dtel TO lv_dataelement.
  DATA:lwa_log TYPE ty_code.
  PERFORM sub_check_exists USING    lc_dataelement lv_dataelement
                           CHANGING lv_state.
  IF lv_state IS INITIAL.
    LOOP AT git_code INTO gwa_code WHERE col1 CS lc_dataelement AND
                                          col2 CS p_dtel.
      CASE gwa_code-col3.
        WHEN 'DEFN'.
          MOVE gwa_code-col8 TO dd04v_wa.
        WHEN 'TECH'.
*         This parameter is not imp and cannot be used
        WHEN OTHERS.
      ENDCASE.

    ENDLOOP.
    DELETE git_code WHERE col1 CS lc_dataelement AND
                          col2 CS p_dtel.
    CALL FUNCTION 'DDIF_DTEL_PUT'
      EXPORTING
        name              = dd04v_wa-rollname
        dd04v_wa          = dd04v_wa
      EXCEPTIONS
        dtel_not_found    = 1
        name_inconsistent = 2
        dtel_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc = 0.
      PERFORM sub_check USING  lc_dataelement lv_dataelement
                        CHANGING lv_correct.
      IF lv_correct EQ abap_true.
        CALL FUNCTION 'DDIF_DTEL_ACTIVATE'
          EXPORTING
            name        = dd04v_wa-rollname
          IMPORTING
            rc          = lv_rc
          EXCEPTIONS
            not_found   = 1
            put_failure = 2
            OTHERS      = 3.

        IF sy-subrc = 0.
          CLEAR lwa_log.
          lwa_log-col1 = lc_dataelement.
          lwa_log-col2 = p_dtel.
          lwa_log-col3 = 'Data element successfully created and activated.'.
          lwa_log-col4 = '@08@'.
          MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
        ELSE.
          CLEAR lwa_log.
          lwa_log-col1 = lc_dataelement.
          lwa_log-col2 = p_dtel.
          lwa_log-col3 = 'Data element created but activation failed.'.
          lwa_log-col4 = '@0A@'.
          MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
        ENDIF.
      ELSE.
        CLEAR lwa_log.
        lwa_log-col1 = lc_dataelement.
        lwa_log-col2 = p_dtel.
        lwa_log-col3 = 'Data element created but with errors.'.
        lwa_log-col4 = '@0A@'.
        MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
      ENDIF.
    ELSE.
      CLEAR lwa_log.
      lwa_log-col1 = lc_dataelement.
      lwa_log-col2 = p_dtel.
      lwa_log-col3 = 'Error in Data element creation function module.'.
      lwa_log-col4 = '@0A@'.
      MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
    ENDIF.
  ELSE.
    CLEAR lwa_log.
    lwa_log-col1 = lc_dataelement.
    lwa_log-col2 = p_dtel.
    lwa_log-col3 = 'Data element already exists'.lwa_log-col4 = '@09@'.
    MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
  ENDIF.
  PERFORM log_table_check CHANGING lv_state.
  IF lv_state = 'A'.
    PERFORM log_obj_tabl USING lc_dataelement lv_dataelement lwa_log-col3.
  ENDIF.
ENDFORM.
FORM sub_cre_tabl USING p_tabl TYPE string.
  CONSTANTS:lc_table TYPE trobjtype VALUE 'TABL'.
  DATA:lv_table TYPE ddobjname .
  DATA:lv_state TYPE char1.
  DATA:lv_rc TYPE sy-subrc.
  DATA:lv_correct TYPE char1.

  DATA: dd02v_wa  TYPE dd02v,
        dd09l_wa  TYPE dd09v,
        dd03p_tab TYPE TABLE OF dd03p,
        dd03p_wa  TYPE          dd03p,
        dd05m_tab	TYPE TABLE OF	dd05m,
        dd05m_wa  TYPE          dd05m,
        dd08v_tab TYPE TABLE OF dd08v,
        dd08v_wa  TYPE          dd08v,
        dd35v_tab TYPE TABLE OF dd35v,
        dd35v_wa  TYPE          dd35v,
        dd36m_tab TYPE TABLE OF dd36m,
        dd36m_wa  TYPE          dd36m.

  DATA:lwa_log TYPE ty_code.
  MOVE p_tabl TO lv_table.
  PERFORM sub_check_exists USING    lc_table lv_table
                           CHANGING lv_state.
  IF lv_state IS INITIAL.
    LOOP AT git_code INTO gwa_code WHERE col1 CS lc_table AND
                                         col2 CS p_tabl.
      CASE gwa_code-col3.
        WHEN 'DEFN'.
          MOVE gwa_code-col8 TO dd02v_wa.
        WHEN 'TECH'.
          MOVE gwa_code-col8 TO dd09l_wa.
        WHEN 'FLDS'.
          MOVE gwa_code-col8 TO dd03p_wa.
          APPEND dd03p_wa TO dd03p_tab.
        WHEN 'FKF'.
          MOVE gwa_code-col8 TO dd05m_wa.
          APPEND dd05m_wa TO dd05m_tab.
        WHEN 'FKS'.
          MOVE gwa_code-col8 TO dd08v_wa.
          APPEND dd08v_wa TO dd08v_tab.
        WHEN 'TI'.
          MOVE gwa_code-col8 TO dd09l_wa.
        WHEN 'IF'.
*          This will ne created automatically once TI is put
        WHEN 'HSH'.
          MOVE gwa_code-col8 TO dd35v_wa.
          APPEND dd35v_wa TO dd35v_tab.
        WHEN 'ASH'.
          MOVE gwa_code-col8 TO dd36m_wa.
          APPEND dd36m_wa TO dd36m_tab.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
    DELETE git_code WHERE col1 CS lc_table AND
                          col2 CS p_tabl.
    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name              = dd02v_wa-tabname
        dd02v_wa          = dd02v_wa
        dd09l_wa          = dd09l_wa
      TABLES
        dd03p_tab         = dd03p_tab
        dd05m_tab         = dd05m_tab
        dd08v_tab         = dd08v_tab
        dd35v_tab         = dd35v_tab
        dd36m_tab         = dd36m_tab
      EXCEPTIONS
        tabl_not_found    = 1
        name_inconsistent = 2
        tabl_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc = 0.
      CALL FUNCTION 'DDIF_TABT_PUT'
        EXPORTING
          name              = dd02v_wa-tabname
          dd09l_wa          = dd09l_wa
        EXCEPTIONS
          tabt_not_found    = 1
          name_inconsistent = 2
          tabt_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.

      PERFORM sub_check USING  lc_table lv_table
                        CHANGING lv_correct.
      IF lv_correct EQ abap_true.
        CALL FUNCTION 'DDIF_TABL_ACTIVATE'
          EXPORTING
            name        = dd02v_wa-tabname
          IMPORTING
            rc          = lv_rc
          EXCEPTIONS
            not_found   = 1
            put_failure = 2
            OTHERS      = 3.

        IF sy-subrc = 0.
          CLEAR lwa_log.
          lwa_log-col1 = lc_table.
          lwa_log-col2 = p_tabl.
          lwa_log-col3 = 'Table created and activated successfully.'.
          lwa_log-col4 = '@08@'.
          MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
        ELSE.
          CLEAR lwa_log.
          lwa_log-col1 = lc_table.
          lwa_log-col2 = p_tabl.
          lwa_log-col3 = 'Table created but activation failed.'.
          lwa_log-col4 = '@0A@'.
          MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
        ENDIF.
      ELSE.
        CLEAR lwa_log.
        lwa_log-col1 = lc_table.
        lwa_log-col2 = p_tabl.
        lwa_log-col3 = 'Table created but with errors.'.
        lwa_log-col4 = '@0A@'.
        MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
      ENDIF.
    ELSE.
      CLEAR lwa_log.
      lwa_log-col1 = lc_table.
      lwa_log-col2 = p_tabl.
      lwa_log-col3 = 'Error in table creation function module.'.
      lwa_log-col4 = '@0A@'.
      MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
    ENDIF.
  ELSE.
    CLEAR lwa_log.
    lwa_log-col1 = lc_table.
    lwa_log-col2 = p_tabl.
    lwa_log-col3 = 'Table already exists'.lwa_log-col4 = '@09@'.
    MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
  ENDIF.
  PERFORM log_table_check CHANGING lv_state.
  IF lv_state = 'A'.
    PERFORM log_obj_tabl USING lc_table lv_table lwa_log-col3.
  ENDIF.
ENDFORM.
FORM sub_cre_ttyp USING p_ttyp TYPE string.
  CONSTANTS:lc_tabletype TYPE trobjtype VALUE 'TTYP'.
  DATA:lv_tabletype TYPE ddobjname .
  DATA:lv_state TYPE char1.
  DATA:lv_rc TYPE sy-subrc.
  DATA:lv_correct TYPE char1.

  DATA:dd40v_wa  TYPE  dd40v,
       dd42v_tab TYPE TABLE OF dd42v,
       dd42v_wa  TYPE   dd42v,
       dd43v_tab TYPE TABLE OF dd43v,
       dd43v_wa	 TYPE dd43v.

  DATA:lwa_log TYPE ty_code.
  MOVE p_ttyp TO lv_tabletype.
  PERFORM sub_check_exists USING    lc_tabletype lv_tabletype
                           CHANGING lv_state.
  IF lv_state IS INITIAL.
    LOOP AT git_code INTO gwa_code WHERE col1 CS lc_tabletype AND
                                          col2 CS p_ttyp.
      CASE gwa_code-col3.
        WHEN 'DEFN'.
          MOVE gwa_code-col8 TO dd40v_wa.
        WHEN 'KEYS'.
          MOVE gwa_code-col8 TO  dd42v_wa.
          APPEND  dd42v_wa TO  dd42v_tab.
        WHEN 'STRU'.
          MOVE gwa_code-col8 TO  dd43v_wa.
          APPEND  dd43v_wa TO  dd43v_tab.
        WHEN OTHERS.
      ENDCASE.

    ENDLOOP.
    DELETE git_code WHERE col1 CS lc_tabletype AND
                          col2 CS p_ttyp.
    CALL FUNCTION 'DDIF_TTYP_PUT'
      EXPORTING
        name              = dd40v_wa-typename
        dd40v_wa          = dd40v_wa
      TABLES
        dd42v_tab         = dd42v_tab
        dd43v_tab         = dd42v_tab
      EXCEPTIONS
        ttyp_not_found    = 1
        name_inconsistent = 2
        ttyp_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.

    IF sy-subrc = 0.
      PERFORM sub_check USING  lc_tabletype lv_tabletype
                        CHANGING lv_correct.
      IF lv_correct EQ abap_true.
        CALL FUNCTION 'DDIF_TTYP_ACTIVATE'
          EXPORTING
            name        = dd40v_wa-typename
          IMPORTING
            rc          = lv_rc
          EXCEPTIONS
            not_found   = 1
            put_failure = 2
            OTHERS      = 3.

        IF sy-subrc = 0.
          CLEAR lwa_log.
          lwa_log-col1 = lc_tabletype.
          lwa_log-col2 = p_ttyp.
          lwa_log-col3 = 'Table type created and activated successfully.'.
          lwa_log-col4 = '@08@'.
          MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
        ELSE.
          CLEAR lwa_log.
          lwa_log-col1 = lc_tabletype.
          lwa_log-col2 = p_ttyp.
          lwa_log-col3 = 'Table type created but activation failed.'.
          lwa_log-col4 = '@0A@'.
          MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
        ENDIF.
      ELSE.
        CLEAR lwa_log.
        lwa_log-col1 = lc_tabletype.
        lwa_log-col2 = p_ttyp.
        lwa_log-col3 = 'Table type created but with errors'.
        lwa_log-col4 = '@0A@'.
        MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
      ENDIF.
    ELSE.
      CLEAR lwa_log.
      lwa_log-col1 = lc_tabletype.
      lwa_log-col2 = p_ttyp.
      lwa_log-col3 = 'Error in table type creation function module.'.
      lwa_log-col4 = '@0A@'.
      MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
    ENDIF.
  ELSE.
    CLEAR lwa_log.
    lwa_log-col1 = lc_tabletype.
    lwa_log-col2 = p_ttyp.
    lwa_log-col3 = 'Table type already exists'.lwa_log-col4 = '@09@'.
    MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
  ENDIF.
  PERFORM log_table_check CHANGING lv_state.
  IF lv_state = 'A'.
    PERFORM log_obj_tabl USING lc_tabletype lv_tabletype lwa_log-col3.
  ENDIF.
ENDFORM.
FORM sub_cre_prog USING p_prog TYPE string.
  DATA:lwa_reposrc TYPE reposrc.
  DATA:lwa_trdir   TYPE trdir.
  DATA:lwa_string TYPE string,
       lit_string TYPE TABLE OF string.
  CONSTANTS:lc_prog TYPE trobjtype VALUE 'PROG'.
  DATA:lv_program TYPE e071-obj_name .
  DATA:lwa_log TYPE ty_code.
  MOVE p_prog TO lv_program.
  SELECT SINGLE * FROM reposrc INTO lwa_reposrc WHERE progname = p_prog.
  IF sy-subrc <> 0.
    IF lv_program = 'ZP_AADT_ACC_TOOLS'
      "change Feb 5th
      OR lv_program = 'ZPRAD_MAIN_AADT_PROG'
      OR lv_program = 'ZAADT_VALIDATE_DELETE'.

      DATA:lwa_codelist TYPE REF TO data.
      FIELD-SYMBOLS:<lfs_code_log> .
      FIELD-SYMBOLS:<lfs_field> .

      CREATE DATA lwa_codelist TYPE (gc_code_table).

      DATA:lo_type TYPE REF TO cl_abap_typedescr.
      DATA:lo_stru TYPE REF TO cl_abap_structdescr.
      DATA:lit_comps TYPE abap_component_tab.
      DATA:lwa_comps TYPE abap_componentdescr.


      lo_type = cl_abap_typedescr=>describe_by_data_ref( lwa_codelist ).
      lo_stru ?= lo_type.

      CALL METHOD lo_stru->get_components(
        RECEIVING
          p_result = lit_comps ).
      ASSIGN lwa_codelist->* TO <lfs_code_log>.
      LOOP AT git_code INTO gwa_code WHERE col1 CS lc_prog AND
                                           col2 CS lv_program.
        CASE gwa_code-col3.
          WHEN 'PROP'.
            LOOP AT lit_comps INTO lwa_comps.
              ASSIGN COMPONENT lwa_comps-name OF STRUCTURE <lfs_code_log> TO <lfs_field>.
              CASE sy-tabix.
                WHEN 1.
                  CONCATENATE lv_program 'PROP' INTO <lfs_field>.
                WHEN 2.
                WHEN 3.
                  <lfs_field> = gwa_code-col8.
                WHEN OTHERS.
              ENDCASE.
            ENDLOOP.
            IF lwa_codelist IS NOT INITIAL.
              ASSIGN lwa_codelist->* TO <lfs_code_log>.
              MODIFY (gc_code_table) FROM <lfs_code_log> .
              IF sy-subrc = 0.
                COMMIT WORK.
              ENDIF.
            ENDIF.
          WHEN 'CODE'.
            LOOP AT lit_comps INTO lwa_comps.
              ASSIGN COMPONENT lwa_comps-name OF STRUCTURE <lfs_code_log> TO <lfs_field>.
              CASE sy-tabix.
                WHEN 1.
                  <lfs_field> = gwa_code-col2.
                WHEN 2.
                  <lfs_field> = gwa_code-col5.
                WHEN 3.
                  <lfs_field> = gwa_code-col8.
                WHEN OTHERS.
              ENDCASE.
            ENDLOOP.
            IF lwa_codelist IS NOT INITIAL.
              ASSIGN lwa_codelist->* TO <lfs_code_log>.
              MODIFY (gc_code_table) FROM <lfs_code_log> .
              IF sy-subrc = 0.
                COMMIT WORK.
              ENDIF.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
    ELSE.
      LOOP AT git_code INTO gwa_code WHERE col1 CS lc_prog AND
                                           col2 CS lv_program.
        CASE gwa_code-col3.
          WHEN 'PROP'.
            MOVE gwa_code-col8 TO lwa_trdir.
          WHEN 'CODE'.
            IF gwa_code-col6 EQ abap_true AND gwa_code-col3 IN r_enc_key_id.
              DATA:lv_string TYPE string.
              REFRESH:ptab.
              CLEAR:ptab_line.
              ptab_line-name = 'THE_STRING'.
              ptab_line-kind = cl_abap_objectdescr=>exporting.
              GET REFERENCE OF gwa_code-col8 INTO  ptab_line-value.
              INSERT ptab_line INTO TABLE ptab.
              CLEAR:ptab_line.
              ptab_line-name = 'RESULT'.
              ptab_line-kind = cl_abap_objectdescr=>returning.
              GET REFERENCE OF lv_string INTO  ptab_line-value.
              INSERT ptab_line INTO TABLE ptab.
              CALL METHOD go_ref->(gc_method) PARAMETER-TABLE ptab.
              gwa_code-col8 = lv_string.
            ENDIF.
            MOVE  gwa_code-col8 TO lwa_string.
            APPEND lwa_string TO lit_string.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
    ENDIF.
    DELETE git_code WHERE  col1 CS lc_prog AND
                           col2 CS lv_program.

    IF lit_string IS NOT INITIAL.
      INSERT REPORT lv_program FROM lit_string
      DIRECTORY ENTRY lwa_trdir.
      IF sy-subrc = 0.
        CLEAR lwa_log.
        lwa_log-col1 = lc_prog.
        lwa_log-col2 = p_prog.
        lwa_log-col3 = 'Program created and activated successfully.'.
        lwa_log-col4 = '@08@'.
        MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
      ELSE.
        CLEAR lwa_log.
        lwa_log-col1 = lc_prog.
        lwa_log-col2 = p_prog.
        lwa_log-col3 = 'Error in report insertion for program.'.
        lwa_log-col4 = '@0A@'.
        MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
      ENDIF.
    ENDIF.
*    Syntax check for program if possible
  ELSE.
    CLEAR lwa_log.
    lwa_log-col1 = lc_prog.
    lwa_log-col2 = p_prog.
    lwa_log-col3 = 'Program already exists'.lwa_log-col4 = '@09@'.
    MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
  ENDIF.
  DATA:lv_state TYPE char1.
  DATA:lv_col2(100) TYPE c.
  PERFORM log_table_check CHANGING lv_state.
  lv_col2 = lwa_log-col2.
  IF ( lv_state = 'A' ) AND ( lv_col2 NE 'ZP_AADT_UTIL_DEL_OBJ' ).
    PERFORM log_obj_tabl USING lc_prog p_prog lwa_log-col3.
  ENDIF.
ENDFORM.
FORM sub_cre_clas USING p_clas TYPE string.
  CONSTANTS:lc_clas TYPE trobjtype VALUE 'CLAS'.
  DATA:lv_class TYPE e071-obj_name .

  DATA:class          TYPE vseoclass.
  DATA:inheritance    TYPE vseoextend.
  DATA:types          TYPE seoo_types_r.
  DATA:type           TYPE seoo_type_r.
  DATA:method_sources TYPE seo_method_source_table.
  DATA:method_source TYPE seo_method_source.
  DATA:friendships    TYPE seo_friends.
  DATA:friend         TYPE seofriends.
  DATA:implementings  TYPE seor_implementings_r.
  DATA:implementing   TYPE seor_implementing_r.
  DATA:parameters     TYPE seos_parameters_r.
  DATA:parameter      TYPE seos_parameter_r.
  DATA:locals_def TYPE rswsourcet,
       locals_imp TYPE rswsourcet,
       locals_mac TYPE rswsourcet,
       locals_tes TYPE rswsourcet.
  DATA:attributes TYPE seoo_attributes_r.
  DATA:attribute  TYPE seoo_attribute_r.
  DATA:events     TYPE  seoo_events_r.
  DATA:event      TYPE  seoo_event_r.
  DATA:methods     TYPE  seoo_methods_r.
  DATA:method     TYPE  seoo_method_r.
  DATA:exceptions    TYPE seos_exceptions_r.
  DATA:exception    TYPE seos_exception_r.

  DATA:lv_editorder(10)   TYPE n,
       lv_srcrow1(10)     TYPE n,
       lv_srccolumn1(5)   TYPE n,
       lv_srcrow2(10)     TYPE n,
       lv_srccolumn2(5)   TYPE n,
       lv_typesrc_leng(5) TYPE n,
       lv_dispid(10)      TYPE n.
  MOVE p_clas TO lv_class.
  DATA: clskey TYPE  seoclskey,
        lv_na  TYPE seox_boolean.
  clskey = lv_class.
  DATA:lwa_log TYPE ty_code.
  CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
    EXPORTING
      clskey        = clskey
    IMPORTING
      not_active    = lv_na
    EXCEPTIONS
      not_specified = 1
      not_existing  = 2
      is_interface  = 3
      no_text       = 4
      inconsistent  = 5
      OTHERS        = 6.
  IF sy-subrc = 2.

    LOOP AT git_code INTO gwa_code WHERE col1 CS lc_clas AND
                                         col2 CS lv_class.

      IF gwa_code-col3 = 'DEFN'.
        CLEAR class.
        SPLIT gwa_code-col8 AT gc_separator INTO class-clsname
                                        class-version
                                        class-langu
                                        class-descript
                                        class-category
                                        class-exposure
                                        class-state
                                        class-release
                                        class-author
                                        class-createdon
                                        class-changedby
                                        class-changedon
                                        class-chgdanyby
                                        class-chgdanyon
                                        class-clsembed
                                        class-clsabstrct
                                        class-clsfinal
                                        class-clsccincl
                                        class-remote
                                        class-fixpt
                                        class-varcl
                                        class-unicode
                                        class-rstat
                                        class-clsdefatt
                                        class-clsdefmtd
                                        class-clsdefint
                                        class-clsbcimpl
                                        class-r3release
                                        class-clsbctrans
                                        class-clsbccat
                                        class-clsaddon
                                        class-msg_id
                                        class-clsbcnodel
                                        class-clsproxy
                                        class-clssharedmemory
                                        class-with_unit_tests.
      ENDIF.
      IF gwa_code-col3 = 'INHR'.
        CLEAR inheritance.
        MOVE gwa_code-col8 TO inheritance.
      ENDIF.
      IF gwa_code-col3 = 'TYPE'.
        CLEAR:type.
        SPLIT gwa_code-col8 AT gc_separator INTO
                  type-clsname
                  type-cmpname
                  type-version
                  type-langu
                  type-descript
                  type-alias
                  type-exposure
                  type-state
                  lv_editorder
                  type-author
                  type-createdon
                  type-changedby
                  type-changedon
                  type-typtype
                  type-type
                  type-tableof
                  lv_srcrow1
                  lv_srccolumn1
                  lv_srcrow2
                  lv_srccolumn2
                  type-r3release
                  type-locked
                  type-refclsname
                  type-refintname
                  type-refcmpname
                  lv_typesrc_leng
                  type-typesrc.

        type-editorder    = lv_editorder.
        type-srcrow1      = lv_srcrow1.
        type-srccolumn1   = lv_srccolumn1.
        type-srcrow2      = lv_srcrow2.
        type-srccolumn2   = lv_srccolumn2.
        type-typesrc_leng = lv_typesrc_leng.
        REPLACE ALL OCCURRENCES OF 'XXXX' IN type-typesrc WITH cl_abap_char_utilities=>cr_lf.
        APPEND type TO types.
        CLEAR type.
      ENDIF.
      IF gwa_code-col3 = 'FRND'.
        MOVE gwa_code-col8 TO friend.
        APPEND friend TO friendships.
      ENDIF.
      IF gwa_code-col3 = 'IMPL'.
        CLEAR lv_editorder.
        SPLIT gwa_code-col8 AT gc_separator INTO
         implementing-clsname
         implementing-refclsname
         implementing-version
         implementing-exposure
         implementing-state
         implementing-author
         implementing-createdon
         implementing-changedby
         implementing-changedon
         implementing-reltype
         implementing-relname
         implementing-impfinal
         implementing-impabstrct
         lv_editorder.
        MOVE lv_editorder TO implementing-editorder.
        CLEAR lv_editorder.
        APPEND implementing TO implementings.
      ENDIF.

      IF gwa_code-col3 = 'PARA'.
        CLEAR parameter.
        SPLIT gwa_code-col8 AT gc_separator INTO
            parameter-clsname
            parameter-cmpname
            parameter-sconame
            parameter-version
            parameter-langu
            parameter-descript
            parameter-cmptype
            parameter-mtdtype
            lv_editorder
            lv_dispid
            parameter-author
            parameter-createdon
            parameter-changedby
            parameter-changedon
            parameter-pardecltyp
            parameter-parpasstyp
            parameter-typtype
            parameter-type
            parameter-tableof
            parameter-parvalue
            parameter-paroptionl
            parameter-parpreferd
            parameter-locked.
        MOVE lv_editorder TO parameter-editorder.
        MOVE lv_dispid    TO parameter-dispid.
        CLEAR: lv_editorder, lv_dispid.
        APPEND parameter TO parameters.
      ENDIF.
      IF gwa_code-col3 = 'EVEN'.
        CLEAR: event,lv_editorder.
        SPLIT gwa_code-col8 AT gc_separator INTO
               event-clsname
               event-cmpname
               event-version
               event-langu
               event-descript
               event-alias
               event-redefin
               event-exposure
               event-state
               lv_editorder
               event-author
               event-createdon
               event-changedby
               event-changedon
               event-evtdecltyp
               event-refclsname
               event-refcmpname
               event-locked
               event-r3release
               event-bcevtcat.
        event-editorder    = lv_editorder.
        APPEND event TO events.
      ENDIF.
      IF gwa_code-col3 = 'ATTR'.
        CLEAR: lv_editorder,
               lv_srcrow1,
               lv_srccolumn1,
               lv_srcrow2,
               lv_srccolumn2,
               lv_typesrc_leng.
        SPLIT gwa_code-col8 AT gc_separator INTO
         attribute-clsname
         attribute-cmpname
         attribute-version
         attribute-langu
         attribute-descript
         attribute-alias
         attribute-exposure
         attribute-state
         lv_editorder
         attribute-author
         attribute-createdon
         attribute-changedby
         attribute-changedon
         attribute-attdecltyp
         attribute-attdynamic
         attribute-attgetmtd
         attribute-attsetmtd
         attribute-attrdonly
         attribute-attvalue
         attribute-attpersist
         attribute-attexpvirt
         attribute-typtype
         attribute-type
         attribute-tableof
          lv_srcrow1
          lv_srccolumn1
          lv_srcrow2
          lv_srccolumn2
         attribute-refclsname
         attribute-refcmpname
         attribute-locked
         attribute-attkeyfld
         attribute-attbusobj
         attribute-r3release
         lv_typesrc_leng
        attribute-typesrc.
        attribute-editorder    = lv_editorder.
        attribute-srcrow1      = lv_srcrow1.
        attribute-srccolumn1   = lv_srccolumn1.
        attribute-srcrow2      = lv_srcrow2.
        attribute-srccolumn2   = lv_srccolumn2.
        attribute-typesrc_leng = lv_typesrc_leng.
        REPLACE ALL OCCURRENCES OF 'XXXX' IN attribute-typesrc WITH cl_abap_char_utilities=>cr_lf.
        APPEND attribute TO attributes.
      ENDIF.

      IF gwa_code-col3 = 'METH'.
        CLEAR: lv_editorder,lv_dispid.
        SPLIT gwa_code-col8 AT gc_separator INTO
               method-clsname
               method-cmpname
               method-version
               method-langu
               method-descript
               method-alias
               method-redefin
               method-exposure
               method-state
               lv_editorder
               lv_dispid
               method-author
               method-createdon
               method-changedby
               method-changedon
               method-mtdtype
               method-mtddecltyp
               method-mtdabstrct
               method-mtdfinal
               method-refclsname
               method-refintname
               method-refcmpname
               method-dsrnoteom
               method-mtdnewexc
               method-locked
               method-r3release
               method-bcmtdinst
               method-bcmtdcat
               method-bcmtddia
               method-bcmtdcom
               method-bcmtdsyn
               method-mtdreplace
               method-mtdoptnl.
        MOVE:lv_editorder TO method-editorder,
             lv_dispid TO method-dispid.
        APPEND method TO methods.
      ENDIF.
      IF gwa_code-col3 = 'EXEP'.
        SPLIT gwa_code-col8 AT gc_separator INTO
               exception-clsname
               exception-cmpname
               exception-sconame
               exception-version
               exception-langu
               exception-descript
               exception-mtdtype
               lv_editorder
               exception-author
               exception-createdon
               exception-changedby
               exception-changedon
               exception-locked
               exception-is_resumable.
        MOVE lv_editorder TO exception-editorder.
        APPEND exception TO exceptions.
        CLEAR exception.
      ENDIF.
      IF gwa_code-col3 = 'METS'.
        DATA:lv_string  TYPE string.
        DATA:lv_cpdname  TYPE string.
        CLEAR method_source.
        MOVE gwa_code-col8 TO method_source-cpdname.
        LOOP AT git_code INTO gwa_code2 WHERE col1 CS lc_clas AND
                                              col2 CS lv_class AND
                                              col3 CS 'METC'.
          IF gwa_code2-col6 EQ abap_true AND gwa_code2-col3 IN r_enc_key_id.
            REFRESH:ptab.
            CLEAR:ptab_line.
            CLEAR lv_string.
            ptab_line-name = 'THE_STRING'.
            ptab_line-kind = cl_abap_objectdescr=>exporting.
            GET REFERENCE OF gwa_code2-col8 INTO  ptab_line-value.
            INSERT ptab_line INTO TABLE ptab.
            CLEAR:ptab_line.
            ptab_line-name = 'RESULT'.
            ptab_line-kind = cl_abap_objectdescr=>returning.
            GET REFERENCE OF lv_string INTO  ptab_line-value.
            INSERT ptab_line INTO TABLE ptab.
            CALL METHOD go_ref->(gc_method) PARAMETER-TABLE ptab.
            gwa_code2-col8 = lv_string.
            CLEAR lv_string.
          ENDIF.

          SPLIT gwa_code2-col8 AT gc_separator INTO lv_cpdname lv_string.
          IF lv_cpdname = method_source-cpdname.
            APPEND lv_string TO method_source-source.
          ENDIF.
        ENDLOOP.
        APPEND method_source TO method_sources.
      ENDIF.
    ENDLOOP.
    DELETE git_code WHERE  col1 CS lc_clas AND
                           col2 CS lv_class.


    CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
      EXPORTING
        devclass        = '$TMP'
        version         = seoc_version_active
        genflag         = seox_true
        method_sources  = method_sources
        locals_def      = locals_def
        locals_imp      = locals_imp
        locals_mac      = locals_mac
      CHANGING
        class           = class
        inheritance     = inheritance
        implementings   = implementings
        attributes      = attributes
        methods         = methods
        events          = events
        types           = types
        parameters      = parameters
        exceps          = exceptions
        friendships     = friendships
      EXCEPTIONS
        existing        = 1
        is_interface    = 2
        db_error        = 3
        component_error = 4
        no_access       = 5
        other           = 6
        OTHERS          = 7.

    IF sy-subrc = 0.
      CLEAR lwa_log.
      lwa_log-col1 = lc_clas.
      lwa_log-col2 = p_clas.
      lwa_log-col3 = 'Class created and activated successfully.'.
      lwa_log-col4 = '@08@'.
      MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
    ELSE.
      CLEAR lwa_log.
      lwa_log-col1 = lc_clas.
      lwa_log-col2 = p_clas.
      lwa_log-col3 = 'Error in class creation function module'.
      lwa_log-col4 = '@0A@'.
      MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
    ENDIF.
  ELSE.
    CLEAR lwa_log.
    lwa_log-col1 = lc_clas.
    lwa_log-col2 = p_clas.
    lwa_log-col3 = 'Class already exists'.
    lwa_log-col4 = '@09@'.
    MODIFY git_ulist FROM lwa_log TRANSPORTING col1 col2 col3 col4 col5 col6 col7 col8.
  ENDIF.
  DATA:lv_state TYPE char1.
  PERFORM log_table_check CHANGING lv_state.
  IF lv_state = 'A'.
    PERFORM log_obj_tabl USING lc_clas lv_class lwa_log-col3.
  ENDIF.
ENDFORM.
FORM sub_check_exists USING p_class  TYPE trobjtype
                            p_name   TYPE ddobjname
                   CHANGING c_state  TYPE char1.
  DATA:lv_name TYPE  e071-obj_name.
  MOVE p_name TO lv_name.
  CALL FUNCTION 'DD_OBJECT_EXISTS'
    EXPORTING
      class         = p_class
      name          = lv_name
      state         = 'M'
    IMPORTING
      exists        = c_state
    EXCEPTIONS
      illegal_input = 1.
ENDFORM.
FORM sub_check USING p_class  TYPE trobjtype
                     p_name   TYPE ddobjname
            CHANGING lv_correct TYPE char1.
  DATA:lit_checktab TYPE TABLE OF dcinspchk.
  DATA:lwa_checktab TYPE dcinspchk.
  CALL FUNCTION 'DDIF_DD_CHECK'
    EXPORTING
      objname  = p_name
      objtype  = p_class
    TABLES
      checktab = lit_checktab.

  CASE p_class.
    WHEN 'DOMA' OR 'TTYP'.
      READ TABLE lit_checktab INTO lwa_checktab INDEX 2.
      IF lwa_checktab-text CS 'is correct'.
        lv_correct = abap_true.
      ELSE.
        lv_correct = abap_false.
      ENDIF.
    WHEN 'DTEL'.
      READ TABLE lit_checktab INTO lwa_checktab INDEX 2.
      IF lwa_checktab-text CS 'successful'.
        lv_correct = abap_true.
      ELSE.
        lv_correct = abap_false.
      ENDIF.
    WHEN 'TABL'.

      READ TABLE lit_checktab INTO lwa_checktab INDEX 2.
      IF lwa_checktab-text CS 'is consistent' OR  lwa_checktab-text CS 'was checked with warnings'.
        lv_correct = abap_true.
      ELSE.
        lv_correct = abap_false.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
FORM sub_disply_log.
  DATA : lr_table TYPE REF TO cl_salv_table.
  TRY.
      CALL METHOD cl_salv_table=>factory
        EXPORTING
          list_display = if_salv_c_bool_sap=>false
        IMPORTING
          r_salv_table = lr_table
        CHANGING
          t_table      = git_ulist.
    CATCH cx_salv_msg .
  ENDTRY.
  DATA: lo_column TYPE REF TO cl_salv_column.
  DATA: lo_columns TYPE REF TO cl_salv_columns.
  DATA:lo_columns_t TYPE REF TO cl_salv_columns_table.
  CALL METHOD lr_table->get_columns
    RECEIVING
      value = lo_columns.
  lo_columns_t ?= lo_columns.
  lo_columns_t->set_column_position(
  EXPORTING columnname = 'COL1'  position = 2 ).
  lo_columns_t->set_column_position(
  EXPORTING columnname = 'COL2'  position = 3 ).
  lo_columns_t->set_column_position(
  EXPORTING columnname = 'COL3'  position = 4 ).
  lo_columns->set_optimize( abap_true ).
  lo_columns_t->set_column_position(
 EXPORTING columnname = 'COL4'  position = 1 ).
  lo_columns->set_optimize( abap_true ).

  lo_column = lo_columns->get_column( 'COL1' ).
  lo_column->set_long_text( 'Object Type' ).
  lo_column->set_medium_text( 'Object Type' ).
  lo_column->set_short_text( 'Obj. Typ.' ).
  lo_column = lo_columns->get_column( 'COL2' ).
  lo_column->set_long_text( 'Object Name' ).
  lo_column->set_medium_text('Object Name').
  lo_column->set_short_text('Obj.Nam.').
  lo_column = lo_columns->get_column( 'COL3' ).
  lo_column->set_long_text( 'Message' ).
  lo_column->set_medium_text('Message').
  lo_column->set_short_text('Message').
  lo_column = lo_columns->get_column( 'COL4' ).
  lo_column->set_long_text( 'Status' ).
  lo_column->set_medium_text('Status').
  lo_column->set_short_text('Status').


  lo_column = lo_columns->get_column( 'COL5' ).
  lo_column->set_technical( abap_true ).
  lo_column = lo_columns->get_column( 'COL6' ).
  lo_column->set_technical( abap_true ).
  lo_column = lo_columns->get_column( 'COL7' ).
  lo_column->set_technical( abap_true ).
  lo_column = lo_columns->get_column( 'COL8' ).
  lo_column->set_technical( abap_true ).
  lr_table->display( ).

ENDFORM.
FORM generate_key.
  DATA:lv_index(1) TYPE c.

  MOVE sy-dbsys TO gv_db.
  MOVE sy-host TO  gv_ehp.
  MOVE sy-mandt TO gv_dclnt.
  MOVE sy-dbsys TO gv_db.

  IF gv_key IS INITIAL.

    IF gv_db IS NOT INITIAL.
      DATA(lv_len) = strlen( gv_db ).
      lv_len = 8 - lv_len.
      IF lv_len GT 0.
        DO lv_len TIMES.

          MOVE sy-index TO lv_index.
          CONCATENATE gv_db lv_index INTO gv_db.
        ENDDO.
      ENDIF.
    ENDIF.
    IF gv_ehp IS NOT INITIAL.
      lv_len = strlen( gv_ehp ).
      lv_len = 32 - lv_len.
      IF lv_len GT 0.
        DO lv_len TIMES.
          MOVE sy-index TO lv_index.
          CONCATENATE gv_ehp lv_index INTO gv_ehp.
        ENDDO.
      ENDIF.
    ENDIF.
    IF gv_inst IS NOT INITIAL.
      lv_len = strlen( gv_inst ).
      lv_len = 32 - lv_len.
      IF lv_len GT 0.
        DO lv_len TIMES.
          MOVE sy-index TO lv_index.
          CONCATENATE gv_inst lv_index INTO gv_inst.
        ENDDO.
      ENDIF.
    ENDIF.
    CONCATENATE gv_db gv_ehp gv_inst gv_dclnt INTO gv_key.
  ENDIF.
ENDFORM.
FORM sub_confirm USING p_test TYPE string.
  DATA:lv_answer TYPE string.

  LOOP AT git_code INTO gwa_code WHERE col1 CS 'TEST' AND
                                       col2 CS p_test.

    REFRESH:ptab.
    CLEAR ptab_line.
    ptab_line-name = 'THE_STRING'.
    ptab_line-kind = cl_abap_objectdescr=>exporting.
    GET REFERENCE OF gwa_code-col8 INTO  ptab_line-value.
    INSERT ptab_line INTO TABLE ptab.
    CLEAR:ptab_line.
    ptab_line-name = 'RESULT'.
    ptab_line-kind = cl_abap_objectdescr=>returning.
    GET REFERENCE OF lv_answer INTO  ptab_line-value.
    INSERT ptab_line INTO TABLE ptab.
    CALL METHOD go_ref->(gc_method) PARAMETER-TABLE ptab.

    CLEAR:gwa_code-col8.
    MOVE lv_answer TO gwa_code-col8.

    CONCATENATE 'Can you read the text below ?                        '  gwa_code-col8 INTO gwa_code-col8 RESPECTING BLANKS.


    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Decryption test to be done before objects are loaded.'
        text_question         = gwa_code-col8
        text_button_1         = 'Yes'
        text_button_2         = 'No'
        display_cancel_button = abap_false
        start_column          = 25
        start_row             = 6
      IMPORTING
        answer                = lv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    IF lv_answer = 2.
      MESSAGE e208(00) WITH 'Generated code belongs to different client.'.
      EXIT.
    ELSE.
      MESSAGE i208(00) WITH 'Key validation successful, proceeding.'.
    ENDIF.

  ENDLOOP.
ENDFORM.
FORM log_table_check CHANGING c_exist  TYPE char1.

  CALL FUNCTION 'DD_OBJECT_EXISTS'
    EXPORTING
      class         = 'TABL'
      name          = gc_log_table
      state         = 'M'
    IMPORTING
      exists        = c_exist
    EXCEPTIONS
      illegal_input = 1.

ENDFORM.
FORM log_obj_tabl USING p_class
                        p_name
                        p_message TYPE string.
  DATA:lwa_obj_list TYPE REF TO data.
  FIELD-SYMBOLS:<lfs_obj_log> .
  FIELD-SYMBOLS:<lfs_field> .

  CREATE DATA lwa_obj_list TYPE (gc_log_table).

*  DATA: lwa_test like lwa_obj_list.

  DATA:lo_type TYPE REF TO cl_abap_typedescr.
  DATA:lo_stru TYPE REF TO cl_abap_structdescr.
  DATA:lit_comps TYPE abap_component_tab.
  DATA:lwa_comps TYPE abap_componentdescr.


  lo_type = cl_abap_typedescr=>describe_by_data_ref( lwa_obj_list ).
  lo_stru ?= lo_type.

  CALL METHOD lo_stru->get_components(
    RECEIVING
      p_result = lit_comps ).
  ASSIGN lwa_obj_list->* TO <lfs_obj_log>.
  LOOP AT lit_comps INTO lwa_comps.
    ASSIGN COMPONENT lwa_comps-name OF STRUCTURE <lfs_obj_log> TO <lfs_field>.
    CASE sy-tabix.
      WHEN 1.
        MOVE sy-mandt TO <lfs_field>.
      WHEN 2.
        <lfs_field> =  p_class.
      WHEN 3.
        <lfs_field> =  p_name.
      WHEN 4.
        <lfs_field> =  p_message.
      WHEN 5.
        <lfs_field> = abap_false.
      WHEN 6.
        <lfs_field> = sy-datum.
      WHEN 7.
        <lfs_field> =  sy-uname.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.
  IF lwa_obj_list IS NOT INITIAL.
    ASSIGN lwa_obj_list->* TO <lfs_obj_log>.
    MODIFY (gc_log_table) FROM <lfs_obj_log> .
  ENDIF.
ENDFORM.
FORM sub_cipher_check CHANGING lv_na TYPE char1.
  DATA:  clskey TYPE  seoclskey.

  clskey = gc_dclas.
  CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
    EXPORTING
      clskey        = clskey
    IMPORTING
      not_active    = lv_na
    EXCEPTIONS
      not_specified = 1
      not_existing  = 2
      is_interface  = 3
      no_text       = 4
      inconsistent  = 5
      OTHERS        = 6.
  IF sy-subrc = 0.
    lv_na = abap_false.
  ELSE.
    lv_na = abap_true.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_F4_FILE_PATH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      <--P_P_ZFILEP  text
*&---------------------------------------------------------------------*
FORM sub_f4_file_path  CHANGING p_zpath TYPE string.
  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title         = 'Please select folder to upload file'
      initial_folder       = 'C:\'
    CHANGING
      selected_folder      = p_zpath
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF p_zpath IS NOT INITIAL.
    CONCATENATE p_zpath '\AADT_Tools' INTO p_zpath.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HIDE_CODE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM hide_code .

  TABLES: trdir.
  DATA program(40) TYPE c.
  PROGRAM = 'ZPRAD_WRAP_PROG'.

  SELECT SINGLE * FROM trdir WHERE name = program.
  IF sy-subrc <> 0.
    WRITE: / 'Program does not exists!'.
    EXIT.
  ENDIF.
  DATA: f1 TYPE d, f3 TYPE d.
  DATA: f2 TYPE t, f4 TYPE t.
  EXEC SQL.
    SELECT UDAT, UTIME, SDAT, STIME INTO :F1, :F2, :F3, :F4 FROM D010LINF
                         WHERE PROG = :PROGRAM
  ENDEXEC.
  IF f1 < f3 OR ( f1 = f3 AND f2 < f4 ).
    WRITE: / 'The program has no recent generated version!'.
    EXIT.
  ENDIF.
* Compose a new program name
  DATA: new_name(40), i TYPE i, j TYPE i.
  new_name = program.
  DO 8 TIMES.
    i = sy-index - 1.
    new_name+i(1) = '_'.
* Search for acceptable program name variations
    j = 0.
    SELECT * FROM trdir WHERE name LIKE new_name.
      j = j + 1.
    ENDSELECT.
    IF j = 1.
      EXIT.
    ENDIF.
    new_name = program.
  ENDDO.
* Can not generate appropriate program name
  IF j > 1.
    WRITE: / 'Can not generate appropriate program name'.
    EXIT.
  ENDIF.

* Check if it is already in d010s (already hidden)
  DATA: f5(40).
  EXEC SQL.
    SELECT PROG INTO :F5 FROM D010SINF WHERE PROG = :NEW_NAME
  ENDEXEC.
  IF f5 IS INITIAL.
* There is no such hidden program, hide it
    EXEC SQL.
      UPDATE D010SINF SET PROG = :NEW_NAME WHERE PROG = :PROGRAM
    ENDEXEC.
  ELSE.
* There is already a hidden program there, unhide it
    EXEC SQL.
      UPDATE D010SINF SET PROG = :PROGRAM WHERE PROG = :NEW_NAME
    ENDEXEC.
  ENDIF.


ENDFORM.
