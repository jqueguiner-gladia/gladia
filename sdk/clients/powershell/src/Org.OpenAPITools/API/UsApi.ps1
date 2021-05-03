function Invoke-UsApiReadUserImageImageUncolorizationUsersUsernamePost {
    [CmdletBinding()]
    Param (
        [Parameter(Position = 0, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true, Mandatory = $true)]
        [String]
        ${username}
    )

    Process {
        'Calling method: UsApi-ReadUserImageImageUncolorizationUsersUsernamePost' | Write-Verbose
        $PSBoundParameters | Out-DebugParameter | Write-Debug

        $Script:UsApi.ReadUserImageImageUncolorizationUsersUsernamePost(
            ${username}
        )
    }
}

