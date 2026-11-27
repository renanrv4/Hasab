
$ErrorActionPreference = "Stop"

# Diretórios do projeto
$projectRoot = Split-Path -Parent $PSScriptRoot
$casesDir = Join-Path $projectRoot "tests/parser/cases"
$outputDir = Join-Path $projectRoot "tests/parser/generated"

New-Item -ItemType Directory -Force -Path $outputDir | Out-Null

# Executa o parser e captura a AST
function Get-ParserOutput {
    param(
        [string]$Source
    )

    $psi = New-Object System.Diagnostics.ProcessStartInfo
    $psi.FileName = "cabal"
    $psi.Arguments = "run -v0 parser-test"
    $psi.WorkingDirectory = $projectRoot
    $psi.UseShellExecute = $false
    $psi.RedirectStandardInput = $true
    $psi.RedirectStandardOutput = $true
    $psi.RedirectStandardError = $true
    $psi.CreateNoWindow = $true

    $process = New-Object System.Diagnostics.Process
    $process.StartInfo = $psi

    [void]$process.Start()

    # Leitura assíncrona para evitar bloqueios
    $stdoutTask = $process.StandardOutput.ReadToEndAsync()
    $stderrTask = $process.StandardError.ReadToEndAsync()

    $process.StandardInput.WriteLine($Source)
    $process.StandardInput.Close()

    $process.WaitForExit()

    $stdout = $stdoutTask.Result
    $stderr = $stderrTask.Result
    $exitCode = $process.ExitCode

    $process.Dispose()

    if ($exitCode -ne 0) {
        throw "Erro ao executar parser:`n$stderr"
    }

    return $stdout.TrimEnd("`r", "`n")
}

# Percorre os arquivos de casos
$files = Get-ChildItem -Path $casesDir -Filter "*.txt" -File |
    Sort-Object Name

foreach ($file in $files) {

    Write-Host "Gerando testes: $($file.Name)"

    $content = Get-Content -LiteralPath $file.FullName -Raw

    # Separa os programas pelo delimitador ---
    $cases = $content -split '(?m)^[ \t]*---[ \t]*$'

    $testContent = ""
    $caseNumber = 0

    foreach ($case in $cases) {

        $source = $case.Trim()

        if ([string]::IsNullOrWhiteSpace($source)) {
            continue
        }

        $caseNumber++

        Write-Host "  Caso $caseNumber"

        $expected = Get-ParserOutput -Source $source

        $testContent += @"
# $($file.BaseName) - Caso $caseNumber
<
$source
`$ cabal run -v0 parser-test
>
$expected
>= 0


"@
    }

    $outputPath = Join-Path $outputDir "$($file.BaseName).test"

    [System.IO.File]::WriteAllText(
        $outputPath,
        $testContent,
        (New-Object System.Text.UTF8Encoding($false))
    )
}

Write-Host ""
Write-Host "Testes gerados com sucesso!"
