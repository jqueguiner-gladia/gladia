function Invoke-DefaultApiReadUsersImageImageUncolorizationUsersGet {
    [CmdletBinding()]
    Param (
    )

    Process {
        'Calling method: DefaultApi-ReadUsersImageImageUncolorizationUsersGet' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:DefaultApi.ReadUsersImageImageUncolorizationUsersGet(
        )
    }
}

function Invoke-DefaultApiRootGet {
    [CmdletBinding()]
    Param (
    )

    Process {
        'Calling method: DefaultApi-RootGet' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:DefaultApi.RootGet(
        )
    }
}

