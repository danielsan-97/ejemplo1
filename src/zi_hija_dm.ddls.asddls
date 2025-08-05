@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface tabla hija'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_HIJA_DM as select from ZTT_HIJO_DM
association to parent ZI_PADRE_DM as _asohija
    on $projection.id = _asohija.id
{
    key id ,
    course ,
    semester ,
    semresult ,
    _asohija // Make association public
}
