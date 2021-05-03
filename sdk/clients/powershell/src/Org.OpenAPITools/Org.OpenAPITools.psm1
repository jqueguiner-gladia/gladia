#region Import functions

'API', 'Model', 'Private' | Get-ChildItem -Path {
    Join-Path $PSScriptRoot $_
} -Filter '*.ps1' | ForEach-Object {
    Write-Verbose "Importing file: $($_.BaseName)"
    try {
        . $_.FullName
    } catch {
        Write-Verbose "Can't import function!"
    }
}

#endregion


#region Initialize APIs

'Creating object: Org.OpenAPITools.Api.DefaultApi' | Write-Verbose
$Script:DefaultApi= New-Object -TypeName Org.OpenAPITools.Api.DefaultApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.UsApi' | Write-Verbose
$Script:UsApi= New-Object -TypeName Org.OpenAPITools.Api.UsApi -ArgumentList @($null)

'Creating object: Org.OpenAPITools.Api.UsersApi' | Write-Verbose
$Script:UsersApi= New-Object -TypeName Org.OpenAPITools.Api.UsersApi -ArgumentList @($null)


#endregion
