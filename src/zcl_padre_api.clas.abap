CLASS zcl_padre_api DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.



    TYPES:
      tt_create_padre   TYPE TABLE FOR CREATE zi_padre_dm\\padrecito,
      tt_mapped_early   TYPE RESPONSE FOR MAPPED EARLY zi_padre_dm,
      tt_failed_early   TYPE RESPONSE FOR FAILED EARLY zi_padre_dm, "tt_response_early
      tt_reported_early TYPE RESPONSE FOR REPORTED EARLY zi_padre_dm,
      tt_reported_late  TYPE RESPONSE FOR REPORTED LATE zi_padre_dm,

      tt_padre_keys     TYPE TABLE FOR READ IMPORT zi_padre_dm\\padrecito,
      tt_padre_result   TYPE TABLE FOR READ RESULT zi_padre_dm\\padrecito,

      tt_padre_update   TYPE TABLE FOR UPDATE zi_padre_dm\\padrecito,

      tt_cba_hijo       TYPE TABLE FOR CREATE zi_padre_dm\\padrecito\_asopadre,

      tt_padre_delete   TYPE TABLE FOR DELETE zi_padre_dm\\padrecito.

    "Constructor
    CLASS-METHODS: get_Instance RETURNING VALUE(ro_instance) TYPE REF TO zcl_padre_api.  "Se retorna una instancia

    DATA: gt_padre      TYPE STANDARD TABLE OF ztt_padre_dm.


    METHODS:
      get_next_id RETURNING VALUE(rv_id) TYPE sysuuid_x16,
      get_next_student_id RETURNING VALUE(rv_studentid) TYPE zde_student_id,


      earlynumbering_create_padre
        IMPORTING entities TYPE tt_create_padre "table for CREATE zi_padre_dm\\padrecito
        CHANGING  mapped   TYPE tt_mapped_early "response for mapped early zi_padre_dm
                  failed   TYPE tt_failed_early "response for failed early zi_padre_dm
                  reported TYPE tt_reported_early, "response for reported early zi_padre_dm

      create_padre
        IMPORTING entities TYPE tt_create_padre "table for CREATE zi_padre_dm\\padrecito
        CHANGING  mapped   TYPE tt_mapped_early "response for mapped early zi_padre_dm
                  failed   TYPE tt_failed_early  "response for failed early zi_padre_dm
                  reported TYPE tt_reported_early, "response for reported early zi_padre_dm

      savedata "esto es para guardar datos nuevos de la tabla ´padre
        CHANGING reported TYPE tt_reported_late, "response for reported late zi_padre_dm

      read_padre
        IMPORTING keys     TYPE tt_padre_keys "table for read import zi_padre_dm\\padrecito "esta tabla tiene las llaves para actualizar registros
        CHANGING  result   TYPE tt_padre_result "table for read result zi_padre_dm\\padrecito
                  failed   TYPE tt_failed_early "response for failed early zi_padre_dm "como ya esta no se define
                  reported TYPE tt_reported_early, " response for reported early zi_padre_dm


      update_padrecito
        IMPORTING entities TYPE tt_padre_update " table for update zi_padre_dm\\padrecito
        CHANGING  mapped   TYPE tt_mapped_early "response for mapped early zi_padre_dm
                  failed   TYPE  tt_failed_early "response for failed early zi_padre_dm
                  reported TYPE tt_reported_early,  "response for reported early zi_padre_dm


      earlynumbering_cba_asopadre
        IMPORTING entities TYPE tt_cba_hijo "table for create zi_padre_dm\\padrecito\_asopadre
        CHANGING  mapped   TYPE tt_mapped_early "response for mapped early zi_padre_dm
                  failed   TYPE tt_failed_early "response for failed early zi_padre_dm
                  reported TYPE tt_reported_early,"response for reported early zi_padre_dm

      cba_asopadre "esto es para guardar los datos de la hija
        IMPORTING entities_cba TYPE tt_cba_hijo "table for create zi_padre_dm\\padrecito\_asopadre
        CHANGING  mapped       TYPE tt_mapped_early "response for mapped early zi_padre_dm
                  failed       TYPE tt_failed_early "response for failed early zi_padre_dm
                  reported     TYPE tt_reported_early, "response for reported early zi_padre_dm

      delete_padre
        IMPORTING keys     TYPE tt_padre_delete "table for delete zi_padre_dm\\padrecito
        CHANGING  mapped   TYPE tt_mapped_early"response for mapped early zi_padre_dm
                  failed   TYPE tt_failed_early"response for failed early zi_padre_dm
                  reported TYPE tt_reported_early."response for reported early zi_padre_dm


  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-DATA : mo_instance   TYPE REF TO zcl_padre_api,
                 gs_mapped     TYPE tt_mapped_early,
                 gt_hijo       TYPE STANDARD TABLE OF ztt_hijo_dm,
                 gr_padre_d    TYPE RANGE OF ztt_padre_dm-id,
                 lv_timestampl TYPE timestampl.

ENDCLASS.


CLASS zcl_padre_api IMPLEMENTATION.
  METHOD get_instance.

    mo_instance = ro_instance = COND #(     WHEN mo_instance IS BOUND
                                            THEN mo_instance
                                            ELSE NEW #( )  ).

  ENDMETHOD.

  METHOD get_next_id."""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        rv_id = cl_uuid_factory=>create_system_uuid(  )->create_uuid_x16(  ).
      CATCH cx_uuid_error.
    ENDTRY.
  ENDMETHOD.

  METHOD get_next_student_id."""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT MAX( studentid ) FROM ztt_padre_dm INTO @DATA(lv_max_studentid).
    rv_studentid = lv_max_studentid + 1.
  ENDMETHOD.

  METHOD earlynumbering_create_padre."""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(ls_mapped) = gs_mapped.
*    DATA(lv_new_id) = cl_uuid_factory=>create_system_uuid(  )->create_uuid_x16(  ).
    DATA(lv_new_id) = get_next_id(  ).

    "Buffer table update buffer = memoria intermedia

    READ TABLE gt_padre ASSIGNING FIELD-SYMBOL(<lfs_padre>) INDEX 1.

    IF <lfs_padre> IS ASSIGNED.
      <lfs_padre>-id =  lv_new_id.
      UNASSIGN <lfs_padre>.
    ENDIF.

    mapped-padrecito = VALUE #(
        FOR ls_entities IN entities WHERE ( id IS INITIAL )

         (  %cid = ls_entities-%cid
            %is_draft = ls_entities-%is_draft
            id = lv_new_id
             )
         ).

  ENDMETHOD.



  METHOD create_padre."""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    gt_padre = CORRESPONDING #( entities MAPPING FROM ENTITY  ).


    "Este loop es una forma antigua de pasar la informacion de la entidad a la tabla en el back
    LOOP AT entities ASSIGNING FIELD-SYMBOL(<lfs_entities>).

      IF NOT gt_padre[] IS INITIAL.
        gt_padre[ 1 ]-studentid = get_next_student_id(  ).

        "este pedazo es para hacer lo del etag
        GET TIME STAMP FIELD lv_timestampl.
        gt_padre[ 1 ]-locallastchangedat = lv_timestampl.
        gt_padre[ 1 ]-lastchangedat = lv_timestampl.
        "hasta aqui va lo del etag

        mapped-padrecito = VALUE #( (
                                    %cid = <lfs_entities>-%cid
                                    %key = <lfs_entities>-%key
                                    %is_draft =  <lfs_entities>-%is_draft
        ) ).
      ENDIF.

    ENDLOOP.

    "Esta es otra forma de pasar los datos a la tabla, es una mas reciente

*    IF NOT gt_padre[] IS INITIAL.
*      gt_padre[ 1 ]-studentid = get_next_student_id(  ).
*    ENDIF.
*
*    mapped = VALUE #(
*      padrecito =  VALUE #(
*          FOR ls_entity IN entities (
*                       %cid = ls_entity-%cid
*                       %key = ls_entity-%key
*                       %is_draft =  ls_entity-%is_draft
*           )
*       )
*     ).

  ENDMETHOD.

  METHOD savedata."""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    IF NOT gt_padre[] IS INITIAL.
      MODIFY ztt_padre_dm FROM TABLE @gt_padre.
    ENDIF.

    IF NOT gt_hijo[] IS INITIAL .
      MODIFY ztt_hijo_dm FROM TABLE @gt_hijo.
    ENDIF.

    IF NOT gr_padre_d IS INITIAL.
      DELETE FROM ztt_padre_dm WHERE id IN @gr_padre_d.
    ENDIF.

  ENDMETHOD.

  METHOD read_padre.""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    SELECT *
    FROM ztt_padre_dm
    FOR ALL ENTRIES IN @keys
    WHERE id = @keys-id
    INTO TABLE @DATA(lt_padre_data).

    Result = CORRESPONDING #( lt_padre_data MAPPING TO ENTITY ).


  ENDMETHOD.

  METHOD update_padrecito."""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    DATA: lt_student_update   TYPE STANDARD TABLE OF ztt_padre_dm,
          lt_student_update_x TYPE STANDARD TABLE OF zst_padre_aux_dm.

    lt_student_update = CORRESPONDING #( entities MAPPING FROM ENTITY  ).
    lt_student_update_x = CORRESPONDING #( entities MAPPING FROM ENTITY USING CONTROL ).

    "este pedazo es para hacer lo del etag
    GET TIME STAMP FIELD lv_timestampl.
    "hasta aqui va lo del etag

    IF NOT lt_student_update IS INITIAL.

      SELECT *
      FROM ztt_padre_dm
      FOR ALL ENTRIES IN @lt_student_update
      WHERE id = @lt_student_update-id
      INTO TABLE @DATA(lt_student_update_old).

    ENDIF.

    gt_padre = VALUE #(

        FOR x = 1 WHILE x <= lines( lt_student_update )
        LET
            ls_control_flag = VALUE #( lt_student_update_x[ x ] OPTIONAL )
            ls_student_new = VALUE #( lt_student_update[ x ] OPTIONAL )
            ls_student_old = VALUE #( lt_student_update_old[ id = ls_student_new-id ] OPTIONAL )
        IN
        (
            id = ls_student_new-id
            studentid  = COND #( WHEN ls_control_flag-studentid IS NOT INITIAL
                                THEN ls_student_new-studentid ELSE ls_student_old-studentid )
            firstname  = COND #( WHEN ls_control_flag-firstname IS NOT INITIAL
                                 THEN ls_student_new-firstname ELSE ls_student_old-firstname )
            lastname   = COND #( WHEN ls_control_flag-lastname IS NOT INITIAL
                                 THEN ls_student_new-lastname ELSE ls_student_old-lastname )
            studentage = COND #( WHEN ls_control_flag-studentage IS NOT INITIAL
                                 THEN ls_student_new-studentage ELSE ls_student_old-studentage )
            course     = COND #( WHEN ls_control_flag-course IS NOT INITIAL
                                 THEN ls_student_new-course ELSE ls_student_old-course )
            courseduration = COND #( WHEN ls_control_flag-courseduration IS NOT INITIAL
                                     THEN ls_student_new-courseduration ELSE ls_student_old-courseduration )
            studentstatus  = COND #( WHEN ls_control_flag-studentstatus IS NOT INITIAL
                                     THEN ls_student_new-studentstatus ELSE ls_student_old-studentstatus )
            gender = COND #( WHEN ls_control_flag-gender IS NOT INITIAL
                             THEN ls_student_new-gender ELSE ls_student_old-gender )
            dob    = COND #( WHEN ls_control_flag-dob IS NOT INITIAL
                             THEN ls_student_new-dob ELSE ls_student_old-dob )
            "esto es para el etag
            locallastchangedat =  lv_timestampl
            lastchangedat =  lv_timestampl
            "hasta aqui va lo del etag
        )
    ).

  ENDMETHOD.

  METHOD earlynumbering_cba_asopadre. """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    DATA(lv_new_result_id) = get_next_id(  ).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<lfs_entities>).

      LOOP AT <lfs_entities>-%target ASSIGNING FIELD-SYMBOL(<lfs_hija_create>).

        mapped-hijita = VALUE #( (
                                %cid = <lfs_hija_create>-%cid
                                %key = <lfs_hija_create>-%key
                                %is_draft =  <lfs_hija_create>-%is_draft
                                ) ).
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

  METHOD cba_asopadre.

    gt_hijo =  VALUE #(
        FOR ls_entities_cba  IN entities_cba
            FOR ls_hijo_cba IN  ls_entities_cba-%target
                LET
                    ls_rap_hijo = CORRESPONDING ztt_hijo_dm(
                        ls_hijo_cba MAPPING FROM ENTITY
                     )
                   IN
                   (
                    ls_rap_hijo
                    )
     ).

    mapped = VALUE #(
       hijita = VALUE #(
           FOR i = 1 WHILE i <= lines( entities_cba )
               LET
                   lt_results = VALUE #( entities_cba[ i ]-%target OPTIONAL )
               IN
                   FOR j = 1  WHILE j <= lines( lt_results )
                       LET
                           ls_curr_results = VALUE #( lt_results[ j ] OPTIONAL )
                       IN
                           (
                               %cid =  ls_curr_results-%cid
                               %key = ls_curr_results-%key
                               id = ls_curr_results-id
                            )
       )
    ).

  ENDMETHOD.

  METHOD delete_padre.

    DATA: lt_padre TYPE STANDARD TABLE OF ztt_padre_dm.
    lt_padre = CORRESPONDING #( keys MAPPING  FROM ENTITY ).

    gr_padre_d = VALUE #(
        FOR ls_padre_d IN lt_padre
        sign = 'I'
        option = 'EQ'
        ( low =  ls_padre_d-id )
    ).

  ENDMETHOD.

ENDCLASS.
