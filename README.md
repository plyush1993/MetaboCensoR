# MetaboCensoR <img src="metabocensor_logo_4x.png" align="right" height="180" width="160">

### Description :bookmark_tabs:
The [`R`](https://www.r-project.org/) [`Shiny`](https://www.shinyapps.io/) App for filtering redundant features in LC-MS peak table and .mgf file. 
- Read the output peak table from [`mzMine`](https://mzio.io/mzmine-news/), [`xcms`](https://www.bioconductor.org/packages/release/bioc/html/xcms.html), [`MS-DIAL`](https://systemsomicslab.github.io/compms/msdial/main.html), and `Default format`.<br>
Check compatibility with [examples of inputs](https://github.com/plyush1993/MetaboCensoR/tree/main/Input_Examples).
- Filter features detected in a blank with a widget.
- Filter Isotopes/Dimers, Adducts, Neutral Loses, In-Source Fragments, and generate annotation tables.
- Filter features by Zero/RSD/Mean/Min values with a widget.
- Filter peaks by *mz*, *RT*, RMD, and AMD with a widget.
- Compile the final filtered peak table, and filter [`MGF file`](https://fiehnlab.ucdavis.edu/projects/lipidblast/mgf-files) according to the final peak table.
- Instructions and details.

<details>
  <summary><b>Server Map&nbsp;&nbsp;</b></summary>
  <br/>
</a><a href="DOI:">
<img src="Server_Map.png" align="center" width="700" height="500">
</a>
</details><br>

### Launch the App :rocket:
**Shiny deployment**<br>
[**`plyush1993.shinyapps.io/MetaboCensoR`**](https://plyush1993.shinyapps.io/MetaboCensoR/) <br><br>
**Run locally**<br>
Install:
```r
if (!require("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}
if (!require("remotes", quietly = TRUE)) {
    install.packages("remotes")
}
remotes::install_github("plyush1993/MetaboCensoR", INSTALL_opts = "--no-multiarch")
```
Run:
```r
library(MetaboCensoR)
run_metabocensor()
```
<br>

> [!IMPORTANT]
>The App was compiled using [R version 4.1.2](https://cran.r-project.org/bin/windows/base/old/4.1.2/)<br/>
> [`Full App Session Info`](https://github.com/plyush1993/MetaboCensoR/blob/main/session_info.txt)
<br>

### Contact :mailbox_with_mail:
[⚠️**Issues**⚠️](https://github.com/plyush1993/MetaboCensoR/issues)
<div align="left">
  <span style="font-weight: bold; vertical-align: middle;"></span>
  <a href="mailto:plyushchenko.ivan@gmail.com"><img src="https://img.shields.io/badge/-4a9edc?style=for-the-badge&logo=gmail" height="28" alt="Email" style="vertical-align: middle;"/></a>
  <a href="https://github.com/plyush1993"><img src="https://img.shields.io/static/v1?style=for-the-badge&message=%20&color=181717&logo=GitHub&logoColor=FFFFFF&label=" height="28" alt="GH" style="vertical-align: middle;"/></a>
  <a href="https://orcid.org/0000-0003-3883-4695"><img src="https://img.shields.io/badge/-A6CE39?style=for-the-badge&logo=ORCID&logoColor=white" height="28" alt="ORCID" style="vertical-align: middle;"/></a>
</div>
