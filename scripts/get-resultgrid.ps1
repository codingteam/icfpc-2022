param (
    $bash = 'C:\Program Files\Git\bin\bash.exe'
)

(& $bash "$PSScriptRoot/../get_all_results.sh" | ConvertFrom-Json).results | ForEach-Object {
    $_ | Add-Member -NotePropertyName 'diff' -NotePropertyValue ($_.min_cost - $_.overall_best_cost) -PassThru
} | Out-GridView
