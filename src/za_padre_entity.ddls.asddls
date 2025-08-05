@EndUserText.label: 'Entidad abstracta'
@Metadata.allowExtensions: true
define abstract entity ZA_PADRE_ENTITY
  //  with parameters parameter_name : parameter_type
{
//  @UI.defaultValue:'X' //Esto deja por defecto un valor, como es valor booleano se pone la X
  @UI.defaultValue: #( 'ELEMENT_OF_REFERENCED_ENTITY: studentstatus' )
  studentstatus  : abap_boolean;
  
  @UI.defaultValue: #( 'ELEMENT_OF_REFERENCED_ENTITY: course' )
  course         : abap.char(50);
  
  @UI.defaultValue: #( 'ELEMENT_OF_REFERENCED_ENTITY: courseduration' )
  courseduration : abap.numc(4);
}
