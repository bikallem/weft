# Install git, opam, OCaml 5.5 and the test tools of weft in the Windows VM
# (tools/windows-vm.sh opam). The script skips the steps that are complete.
# The tools agree with the freebsd job of .github/workflows/ci.yml.
$ErrorActionPreference = 'Stop'
# The progress bar makes Invoke-WebRequest slow.
$ProgressPreference = 'SilentlyContinue'
$env:OPAMYES = '1'

# PowerShell 5.1 does not stop when a program gives an error.
function Run {
  & $args[0] $args[1..($args.Count - 1)]
  if ($LASTEXITCODE -ne 0) { throw "$($args -join ' '): exit $LASTEXITCODE" }
}

# MinGit (Git for Windows with no installer). opam needs git for the pins of
# windtrap, and dune subst needs a git repository.
if (-not (Test-Path C:\git\cmd\git.exe)) {
  $zip = Join-Path $env:TEMP mingit.zip
  Invoke-WebRequest -UseBasicParsing -OutFile $zip `
    https://github.com/git-for-windows/git/releases/download/v2.56.0.windows.4/MinGit-2.56.0.4-64-bit.zip
  Expand-Archive $zip C:\git
  Remove-Item $zip
}

if (-not (Test-Path C:\opam\bin\opam.exe)) {
  New-Item -ItemType Directory -Force C:\opam\bin | Out-Null
  Invoke-WebRequest -UseBasicParsing -OutFile C:\opam\bin\opam.exe `
    https://github.com/ocaml/opam/releases/download/2.6.1/opam-2.6.1-x86_64-windows.exe
}

# The PATH of the machine is for the next start of sshd. This session gets
# the two directories at once.
$path = [Environment]::GetEnvironmentVariable('Path', 'Machine')
foreach ($dir in 'C:\git\cmd', 'C:\opam\bin') {
  if ($path -notlike "*$dir*") { $path = "$path;$dir" }
  if ($env:Path -notlike "*$dir*") { $env:Path = "$env:Path;$dir" }
}
[Environment]::SetEnvironmentVariable('Path', $path, 'Machine')

# With OPAMYES, opam init makes an internal Cygwin, and the switch gets
# MinGW-w64 from it.
if (-not (Test-Path (Join-Path $env:LOCALAPPDATA opam\config))) {
  Run opam init --bare --disable-sandboxing
}
if ((opam switch list --short) -notcontains 'default') {
  Run opam switch create default 5.5.0
}

# The fork of windtrap at the commit of flake.lock.
$wt = 'git+https://github.com/bikallem/windtrap.git#8054b03cb7c580b3048afde86dc819c27815c7ce'
Run opam pin add -n windtrap $wt
Run opam pin add -n ppx_windtrap $wt
Run opam install dune windtrap ppx_windtrap dscheck
