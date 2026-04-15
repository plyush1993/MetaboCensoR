@echo off
set "REXE=C:\Program Files\R\R-4.5.0\bin\Rscript.exe"
"%REXE%" -e "options(shiny.launch.browser=TRUE); MetaboCensoR::run_metabocensor()"
pause