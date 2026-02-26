# MetaboCensoR <img src="metabocensor_logo_4x.png" align="right" height="180" width="160">

### Description :bookmark_tabs:
Shiny App for filtering redundant features in LC-MS peak table and associated .mgf file.

### Launch the App :rocket:
Shiny deployment:<br>
[**`MetaboCensoR`**](https://plyush1993.shinyapps.io/MetaboCensoR/) <br><br>
Run locally:
```r
cat("Checking required packages (auto-installing if missing)\n")
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load("shiny", "DT", "shinythemes", "shinyWidgets", "shinyjs", "vroom", "dplyr", "data.table", "tidyr", "plotly", "igraph", "scales", "tools", "ggplot2", "tibble", "shinyBS", "htmltools", "waiter", "shinycssloaders")
if (!requireNamespace("Spectra", quietly = TRUE)) BiocManager::install("Spectra")
if (!requireNamespace("MsBackendMgf", quietly = TRUE)) BiocManager::install("MsBackendMgf")

source("https://raw.githubusercontent.com/plyush1993/MetaboCensoR/refs/heads/main/app.R")
shiny::shinyApp(ui, server)
```
<br>

> [!IMPORTANT]
>The App was compiled using [R version 4.1.2](https://cran.r-project.org/bin/windows/base/old/4.1.2/) 
<br>

### Contact :mailbox_with_mail:
[⚠️**Issues**⚠️](https://github.com/plyush1993/MetaboCensoR/issues)
<div align="left">
  <span style="font-weight: bold; vertical-align: middle;"></span>
  <a href="mailto:plyushchenko.ivan@gmail.com"><img src="https://img.shields.io/badge/-4a9edc?style=for-the-badge&logo=gmail" height="28" alt="Email" style="vertical-align: middle;"/></a>
  <a href="https://github.com/plyush1993"><img src="https://img.shields.io/static/v1?style=for-the-badge&message=%20&color=181717&logo=GitHub&logoColor=FFFFFF&label=" height="28" alt="GH" style="vertical-align: middle;"/></a>
  <a href="https://orcid.org/0000-0003-3883-4695"><img src="https://img.shields.io/badge/-A6CE39?style=for-the-badge&logo=ORCID&logoColor=white" height="28" alt="ORCID" style="vertical-align: middle;"/></a>
</div>
