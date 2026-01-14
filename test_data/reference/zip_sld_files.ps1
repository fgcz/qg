param(
    [Parameter(Mandatory=$true)][string]$indir,
    [Parameter(Mandatory=$true)][string]$outzip
)

Add-Type -AssemblyName System.IO.Compression.FileSystem

$indir = (Resolve-Path $indir).Path
$outzip = $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath($outzip)

if (Test-Path $outzip) { Remove-Item $outzip }

$zip = [System.IO.Compression.ZipFile]::Open($outzip, 'Create')

Get-ChildItem -Path $indir -Filter "*.sld" -Recurse | ForEach-Object {
    $relativePath = $_.FullName.Substring($indir.Length + 1)
    [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile(
        $zip, $_.FullName, $relativePath, 'Optimal'
    ) | Out-Null
}

$zip.Dispose()
Write-Host "Created $outzip with folder structure preserved"
