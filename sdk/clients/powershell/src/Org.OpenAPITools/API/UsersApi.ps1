function Invoke-UsersApiReadUserMeImageImageUncolorizationUsersMeGet {
    [CmdletBinding()]
    Param (
    )

    Process {
        'Calling method: UsersApi-ReadUserMeImageImageUncolorizationUsersMeGet' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:UsersApi.ReadUserMeImageImageUncolorizationUsersMeGet(
        )
    }
}

