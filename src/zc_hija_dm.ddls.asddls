@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumo hija'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity ZC_HIJA_DM as projection on ZI_HIJA_DM
{
    key id,
    course,
    semester,
    semresult,
    /* Associations */
    _asohija: redirected to parent ZC_PADRE_DM
}
