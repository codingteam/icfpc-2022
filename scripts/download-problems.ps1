param (
    $ProblemCount = 10,
    $ProblemApiBase = "https://cdn.robovinci.xyz/imageframes",
    $OutputDirectory = "$PSScriptRoot/../problems"
)

foreach ($i in 1..$ProblemCount) {
    $url = "$ProblemApiBase/$i.png"
    Write-Host $url
    Invoke-RestMethod $url -OutFile "$OutputDirectory/$i.png"
}
