@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumo padre'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZC_PADRE_DM
  provider contract transactional_query as projection on ZI_PADRE_DM
{
    key id,
    studentid,
    firstname,
    lastname,
    studentage,
    course,
    courseduration,
    studentstatus,
    gender,
    dob,
    lastchangedat,
    locallastchangedat, //este campo es el marcado como etag
    /* Associations */
    _asopadre: redirected to composition child ZC_HIJA_DM
}
