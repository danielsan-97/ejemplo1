CLASS lhc_padrecito DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR padrecito RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR padrecito RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE padrecito.

    METHODS earlynumbering_create FOR NUMBERING
      IMPORTING entities FOR CREATE padrecito.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE padrecito.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE padrecito.

    METHODS read FOR READ
      IMPORTING keys FOR READ padrecito RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK padrecito.

    METHODS rba_Asopadre FOR READ
      IMPORTING keys_rba FOR READ padrecito\_Asopadre FULL result_requested RESULT result LINK association_links.

    METHODS cba_Asopadre FOR MODIFY
      IMPORTING entities_cba FOR CREATE padrecito\_Asopadre.
    METHODS validate_fields FOR VALIDATE ON SAVE
      IMPORTING keys FOR padrecito~validate_fields.
*    METHODS actualizar_duracion_curso FOR DETERMINE ON SAVE "SE COMENTO POR LO QUE SE HACE LA PRUEBA CON MODIFY
    METHODS actualizar_duracion_curso FOR DETERMINE ON MODIFY
      IMPORTING keys FOR padrecito~actualizar_duracion_curso.
    METHODS actualizar_status FOR MODIFY
      IMPORTING keys FOR ACTION padrecito~actualizar_status RESULT result.

    METHODS earlynumbering_cba_Asopadre FOR NUMBERING
      IMPORTING entities FOR CREATE padrecito\_Asopadre.

ENDCLASS.

CLASS lhc_padrecito IMPLEMENTATION.

  METHOD get_instance_features.
  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD create.

    zcl_padre_api=>get_instance(  )->create_padre(
        EXPORTING
        entities = entities
        CHANGING
        mapped = mapped
        failed = failed
        reported = reported
    ).

  ENDMETHOD.

  METHOD earlynumbering_create.

    zcl_padre_api=>get_instance(  )->earlynumbering_create_padre(
                                                            EXPORTING
                                                                entities = entities
                                                            CHANGING
                                                                mapped = mapped
                                                                failed = failed
                                                                reported = reported
                                                                 ).

  ENDMETHOD.

  METHOD update.
    zcl_padre_api=>get_instance( )->update_padrecito(
    EXPORTING
     entities = entities
     CHANGING
     mapped = mapped
     failed = failed
     reported = reported ).
  ENDMETHOD.

  METHOD delete.
    zcl_padre_api=>get_instance( )->delete_padre(
        EXPORTING
        keys = keys
        CHANGING
         mapped = mapped
         failed = failed
         reported = reported
     ).

  ENDMETHOD.

  METHOD read.
    zcl_padre_api=>get_instance( )->read_padre(
        EXPORTING
        keys = keys
        CHANGING
         result = result
         failed = failed
         reported = reported
     ).

  ENDMETHOD.

  METHOD lock.
    "Hay que crear un objeto para bloquear la entidad principal

    TRY.
        DATA(lock) = cl_abap_lock_object_factory=>get_instance( iv_name = 'EZ_LOCK_PADRE_DM' ).

      CATCH cx_abap_lock_failure INTO DATA(exception).
        RAISE SHORTDUMP exception.
    ENDTRY.
    "El try catch es para instanciar el objeto de bloqueo

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<lfs_padre>).

      TRY.
          lock->enqueue(
              it_parameter = VALUE #( ( name = 'id' value = REF #( <lfs_padre>-id ) ) )
           ).

        CATCH cx_abap_foreign_lock INTO DATA(foreign_lock).
          APPEND VALUE #(
              id = keys[ 1 ]-id
              %msg = new_message_with_text(
                  severity = if_abap_behv_message=>severity-error
                  text = 'El registro esta bloqueado por otro usuario' && foreign_lock->user_name
               )
           ) TO reported-padrecito.

          APPEND VALUE #(
             id = keys[ 1 ]-id
          ) TO failed-padrecito.

        CATCH cx_abap_lock_failure INTO exception.
          RAISE SHORTDUMP exception.

      ENDTRY.

    ENDLOOP.

  ENDMETHOD.

  METHOD rba_Asopadre.
  ENDMETHOD.

  METHOD cba_Asopadre.
    zcl_padre_api=>get_instance(  )->cba_asopadre(
    EXPORTING
    entities_cba = entities_cba
    CHANGING
    mapped = mapped
    failed = failed
    reported = reported
    ).
  ENDMETHOD.

  METHOD earlynumbering_cba_Asopadre.

    zcl_padre_api=>get_instance(  )->earlynumbering_cba_asopadre(
        EXPORTING
        entities = entities
        CHANGING
        mapped = mapped
        failed = failed
        reported = reported
     ).




  ENDMETHOD.

  METHOD validate_fields.
    "hay que leer los datos que vienen del fornt end
    READ ENTITIES OF zi_padre_dm
    IN LOCAL MODE
    ENTITY padrecito
    ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(lt_padre_tmp)
    REPORTED DATA(lt_reported)
    FAILED DATA(lt_failed).


    IF NOT lt_padre_tmp[] IS INITIAL. "verificamos que haya algo y no este vacio

      READ TABLE lt_padre_tmp ASSIGNING FIELD-SYMBOL(<lfs_padre_tmp>) INDEX 1."leemos la tabla

      IF  <lfs_padre_tmp> IS ASSIGNED.

        reported-padrecito = VALUE #(
            ( %tky = <lfs_padre_tmp>-%tky %state_area  = 'AREA_N' )
            ( %tky = <lfs_padre_tmp>-%tky %state_area  = 'AREA_A' )
            ( %tky = <lfs_padre_tmp>-%tky %state_area  = 'AREA_E' )
        ).

        IF <lfs_padre_tmp>-firstname IS INITIAL OR <lfs_padre_tmp>-lastname IS INITIAL OR
           <lfs_padre_tmp>-studentage IS INITIAL.

          failed-padrecito = VALUE #( ( %tky = <lfs_padre_tmp>-%tky ) ).

          IF <lfs_padre_tmp>-firstname IS INITIAL. "if para nombre....................................................

            reported-padrecito = VALUE #( (
                %tky = <lfs_padre_tmp>-%tky
                %state_area = 'AREA_N'
                %element-firstname = if_abap_behv=>mk-on "es booleano y depende se si se modifica el campo o eso creo
                %msg = new_message(
                         id       = 'SY'
                         number   = '002'
                         severity = if_abap_behv_message=>severity-error
                             v1       = 'El nobre de usuario es requerido'
                       )
             ) ).

          ENDIF.

          IF <lfs_padre_tmp>-lastname IS INITIAL."esta if para apellido.............................................

            reported-padrecito = VALUE #( BASE reported-padrecito ( "esto es para poner los mensajes de error en una tabla
                %tky = <lfs_padre_tmp>-%tky
                %state_area = 'AREA_A'
                %element-lastname = if_abap_behv=>mk-on"es booleano y depende se si se modifica el campo o eso creo
                %msg = new_message(
                         id       = 'SY'
                         number   = '002'
                         severity = if_abap_behv_message=>severity-error
                             v1       = 'El apellido de usuario es requerido'
                       )
             ) ).

          ENDIF.

          IF <lfs_padre_tmp>-studentage IS INITIAL."esta if para edad.............................................

            reported-padrecito = VALUE #( BASE reported-padrecito (
                %tky = <lfs_padre_tmp>-%tky
                %state_area = 'AREA_E' "esta linea hace q se vea el campo en rojo, y q aparezaca un mensaje, pero no ese que esta ahi, ese es un codigo
                %element-studentage = if_abap_behv=>mk-on"es booleano y depende se si se modifica el campo o eso creo
                %msg = new_message(
                         id       = 'SY'
                         number   = '002'
                         severity = if_abap_behv_message=>severity-error
                             v1       = 'La edad de usuario es requerido'
                       )
             ) ).

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD actualizar_duracion_curso.

    "leer la entidad
    READ ENTITIES OF zi_padre_dm
    IN LOCAL MODE
    ENTITY padrecito
    FIELDS ( course ) WITH CORRESPONDING #( keys )
    RESULT DATA(lt_padre_course).

    LOOP AT lt_padre_course ASSIGNING FIELD-SYMBOL(<lfs_student_course>).

      IF <lfs_student_course>-course EQ 'BIOMEDICA'.

        MODIFY ENTITIES OF zi_padre_dm IN LOCAL MODE
        ENTITY padrecito
        UPDATE FIELDS ( courseduration )
        WITH VALUE #( (
            %tky = <lfs_student_course>-%tky
            courseduration = 5
         ) ).

      ENDIF.

      IF <lfs_student_course>-course EQ 'FISIOTERAPIA'.

        MODIFY ENTITIES OF zi_padre_dm IN LOCAL MODE
        ENTITY padrecito
        UPDATE FIELDS ( courseduration )
        WITH VALUE #( (
            %tky = <lfs_student_course>-%tky
            courseduration = 4
         ) ).

      ENDIF.


    ENDLOOP.




  ENDMETHOD.

  METHOD actualizar_status. "lo del pop uo custom entity, se hace promero con status y despues con curso y duracion

    DATA(lt_keys) = keys.

    READ ENTITIES OF zi_padre_dm IN LOCAL MODE
    ENTITY padrecito
    FIELDS ( studentstatus course courseduration ) WITH CORRESPONDING #( keys ) "se agrgan los dos campos de mas
    RESULT DATA(lt_new_status).

    DATA(lv_new_status) = lt_keys[ 1 ]-%param-studentstatus.
    DATA(lv_new_curso) = lt_keys[ 1 ]-%param-course. "se agregan dos variables, cada una por un campo
    DATA(lv_new_duracion) = lt_keys[ 1 ]-%param-courseduration.

    MODIFY ENTITIES OF zi_padre_dm IN LOCAL MODE
    ENTITY padrecito
    UPDATE FIELDS ( studentstatus course courseduration )  "se actualizan los otros dos cursos
    WITH VALUE  #( (
        %tky = lt_new_status[ 1 ]-%tky
        studentstatus = lv_new_status
        course = lv_new_curso
        courseduration = lv_new_duracion
     ) ).

    READ ENTITIES OF zi_padre_dm IN LOCAL MODE
    ENTITY padrecito
    ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(lt_padreci).

    result = VALUE #( FOR <lfs_student> IN lt_padreci (
                       %tky = <lfs_student>-%tky
                       %param = <lfs_student>
     ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lhc_hijita DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE hijita.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE hijita.

    METHODS read FOR READ
      IMPORTING keys FOR READ hijita RESULT result.

    METHODS rba_Asohija FOR READ
      IMPORTING keys_rba FOR READ hijita\_Asohija FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lhc_hijita IMPLEMENTATION.

  METHOD update.
  ENDMETHOD.

  METHOD delete.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD rba_Asohija.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZI_PADRE_DM DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION. "ETE METODO SIRVE PARA LAS VALIDACIONES

    METHODS save REDEFINITION. "commit datos in database

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZI_PADRE_DM IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save. "este metodo sirve para las validaciones

    DATA: gt_padre_tmp      TYPE STANDARD TABLE OF ztt_padre_dm.
    gt_padre_tmp = zcl_padre_api=>get_instance( )->gt_padre.

    IF NOT gt_padre_tmp[] IS INITIAL.

      READ TABLE gt_padre_tmp ASSIGNING FIELD-SYMBOL(<lfs_padre_tmp>) INDEX 1.

      IF <lfs_padre_tmp> IS ASSIGNED.

        IF <lfs_padre_tmp>-studentage < 18.

          APPEND VALUE #( id = <lfs_padre_tmp>-id ) TO failed-padrecito.

          APPEND VALUE #( id = <lfs_padre_tmp>-id
                          %msg = new_message_with_text(
                            severity = if_abap_behv_message=>severity-error
                            text = 'Edad minima de 18'
                           )
                        ) TO reported-padrecito.

        ENDIF.

        "para validar mas campos y hacer mas mensajes copiamo y pegamos y camnbiamos la condicion
        "este es para hacer q el status siempre se chulee, anton lo quitamos para probar otra cosa pero sirve
*        IF <lfs_padre_tmp>-studentstatus EQ abap_false.
*
*          APPEND VALUE #( id = <lfs_padre_tmp>-id ) TO failed-padrecito.
*
*          APPEND VALUE #( id = <lfs_padre_tmp>-id
*                          %msg = new_message_with_text(
*                            severity = if_abap_behv_message=>severity-error
*                            text = 'Chulee el status'
*                           )
*                        ) TO reported-padrecito.
*
*        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD save.

    zcl_padre_api=>get_instance( )->savedata(
        CHANGING
            reported = reported
    ).

  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize. "este metodo solo se utiliza cuando da un error en el metodo check_before_save
  ENDMETHOD.

ENDCLASS.
