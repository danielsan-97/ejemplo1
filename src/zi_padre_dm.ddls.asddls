@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface tabla 1'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZI_PADRE_DM as select from ztt_padre_dm
composition [0..*] of ZI_HIJA_DM as _asopadre
{
    key id ,
    studentid ,
    firstname ,
    lastname ,
    studentage ,
    course ,
    courseduration ,
    studentstatus ,
    gender ,
    dob ,
    lastchangedat ,
    @Semantics.systemDateTime.localInstanceLastChangedAt: true //el sistema entendera esto como el campo etag
    locallastchangedat ,
    _asopadre // Make association public
}
