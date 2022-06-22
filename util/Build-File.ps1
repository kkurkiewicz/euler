# Module      :  Build-File.ps1
# Description :  Build File
# Copyright   :  (c) Kamil Kurkiewicz
# License     :  NONE
# 
# Maintainer  :  Kamil Kurkiewicz <k-kurkiewicz@outlook.com>
# Stability   :  provisional
# Portability :  portable
# 
# Builds a given Problem???.hs file


# 
# 
# NOTE Tested in only a cursory way
# (for details, see the definition of the nonterminal 'modid' in the Haskell report)
# 
$modid = "(([A-Z]([a-z]|[A-Z]|[0-9]|')*)\.)*([A-Z]([a-z]|[A-Z]|[0-9]|')*)"


# 
# 
# TODO -fglasgow-exts
# 
Set-Location 'C:\Users\kkurkiewicz\Desktop\Project Euler\euler\src\'
$Problem = Read-Host "Problem file (e.g. Problem019)"
Get-Content -LiteralPath ("Euler\" + $Problem + ".hs") | Select-String -Pattern "^module $modid where$" -NotMatch -Raw | Out-File (".\Euler\.~" + $Problem + ".hs")
Invoke-Expression ("ghc -o ..\out\" + $Problem + ".exe -O2 -no-keep-hi-files -no-keep-o-files Euler\.~" + $Problem + ".hs")
Remove-Item (".\Euler\.~" + $Problem + ".hs")

