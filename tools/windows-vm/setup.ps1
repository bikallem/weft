# The first logon of the Windows VM (tools/windows-vm.sh install). The script
# enables the OpenSSH server with the key of the VM directory. The VM is only
# for tests, thus the script also stops the automatic updates and the real-time
# scan of Defender: an update can reboot the VM during a test, and the scan
# makes a build slow. The log goes to C:\weft-setup.log.
$ErrorActionPreference = 'Stop'
Start-Transcript -Path C:\weft-setup.log
$src = Split-Path -Parent $MyInvocation.MyCommand.Path

if (-not (Get-Service sshd -ErrorAction SilentlyContinue)) {
  Add-WindowsCapability -Online -Name OpenSSH.Server~~~~0.0.1.0
}

# sshd reads the keys of an administrator from this file. Only Administrators
# and SYSTEM can have access to it, or sshd ignores it.
$keys = 'C:\ProgramData\ssh\administrators_authorized_keys'
New-Item -ItemType Directory -Force -Path C:\ProgramData\ssh | Out-Null
Copy-Item (Join-Path $src 'authorized_keys') $keys
icacls.exe $keys /inheritance:r /grant 'Administrators:F' /grant 'SYSTEM:F'

# Windows Server 2025 has this rule, but only for the Private profile. The
# network of the VM is Public.
if (Get-NetFirewallRule -Name OpenSSH-Server-In-TCP -ErrorAction SilentlyContinue) {
  Set-NetFirewallRule -Name OpenSSH-Server-In-TCP -Profile Any -Enabled True
} else {
  New-NetFirewallRule -Name OpenSSH-Server-In-TCP -DisplayName 'OpenSSH Server' `
    -Direction Inbound -Protocol TCP -LocalPort 22 -Action Allow | Out-Null
}

# SConfig starts at each logon and waits for an answer on the console.
Set-SConfig -AutoLaunch $false

$au = 'HKLM:\SOFTWARE\Policies\Microsoft\Windows\WindowsUpdate\AU'
New-Item -Force -Path $au | Out-Null
Set-ItemProperty -Path $au -Name NoAutoUpdate -Type DWord -Value 1
try { Set-MpPreference -DisableRealtimeMonitoring $true } catch { $_ }

# Start sshd last: tools/windows-vm.sh install waits for SSH, and then the
# setup is complete.
Set-Service sshd -StartupType Automatic
Start-Service sshd
Stop-Transcript
