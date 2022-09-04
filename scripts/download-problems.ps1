param (
    $ProblemCount = 40,
    $ProblemApiBase = "https://cdn.robovinci.xyz/imageframes",
    $OutputDirectory = "$PSScriptRoot/../problems"
)

foreach ($i in 1..$ProblemCount) {
    $url = "$ProblemApiBase/$i.png"
    Write-Host $url
    Invoke-RestMethod $url -OutFile "$OutputDirectory/$i.png"

    $url = "$ProblemApiBase/$i.initial.json"
    Write-Host $url
    Invoke-RestMethod $url -OutFile "$OutputDirectory/$i.initial.json"

    $url = "$ProblemApiBase/$i.initial.png"
    Write-Host $url
    Invoke-RestMethod $url -OutFile "$OutputDirectory/$i.initial.png"

    $url = "$ProblemApiBase/$i.source.png"
    Write-Host $url
    Invoke-RestMethod $url -OutFile "$OutputDirectory/$i.source.png"
}
