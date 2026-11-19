# Adduct List Reference Map

| Adduct list                                     | Description                                                                     |
| ----------------------------------------------- | ------------------------------------------------------------------------------- |
| [`adducts (nontarget).csv`](https://github.com/plyush1993/MetaboCensoR/blob/main/adducts%20(nontarget).csv)                       | Default MetaboCensoR list, recommended for general untargeted LC–MS processing. Adduct definitions adapted from [`nontarget`](https://github.com/blosloos/nontarget/tree/master) & [`enviPat`](https://github.com/blosloos/enviPat) R packages. |
| [`adducts (acdlabs).csv`](https://github.com/plyush1993/MetaboCensoR/blob/main/adducts%20(acdlabs).csv)                        | Smallest conservative list containing common and abundant adducts. Adduct definitions adapted from the [`ACD/Labs`](https://www.acdlabs.com/blog/common-adduct-and-fragment-ions-in-mass-spectrometry/) website.             |
| [`adducts (nontarget + 2xH2O).csv`](https://github.com/plyush1993/MetaboCensoR/blob/main/adducts%20(nontarget%20%2B%202xH2O).csv)               | Default nontarget list extended with additional double water-loss ion forms; suitable for natural-product datasets and used with the [`Orbi`](https://github.com/plyush1993/MetaboCensoR_Examples/tree/main/orbi) example dataset.    |
| [`adducts (MetaboCoreUtils).csv`](https://github.com/plyush1993/MetaboCensoR/blob/main/adducts%20(MetaboCoreUtils).csv)                 | Adduct definitions adapted from [`MetaboCoreUtils`](https://www.bioconductor.org/packages/release/bioc/html/MetaboCoreUtils.html) R package.                                |
| [`adducts (MS1FA).csv`](https://github.com/plyush1993/MetaboCensoR/blob/main/adducts%20(MS1FA).csv)                           | Adduct definitions adapted from [`MS1FA`](https://github.com/RuibingS/MS1FA_RShiny_dashboard).                                          |
| [`adducts (nontarget+enviPat+MetaboCoreUtils+MS1FA).csv`](https://github.com/plyush1993/MetaboCensoR/blob/main/adducts%20(nontarget%2BenviPat%2BMetaboCoreUtils%2BMS1FA).csv) | Most comprehensive merged list for broad adduct screening.                      |



| Adduct + Rules (for ISF & NL) list                                     | Description                                                                     |
| ----------------------------------------------- | ------------------------------------------------------------------------------- |
| [`adducts_rules (CAMERA basic).csv`](https://github.com/plyush1993/MetaboCensoR/blob/main/adducts_rules%20(CAMERA%20basic).csv)                       | Combined adduct, neutral-loss, and in-source fragment rules for MS1-only data, enabling removal of multiple types of ion redundancy in a single adduct-grouping step. Basic rules list. Rules definitions adapted from [`CAMERA`](https://www.bioconductor.org/packages/3.23/bioc/html/CAMERA.html) R package.|
| [`adducts_rules (CAMERA extended).csv`](https://github.com/plyush1993/MetaboCensoR/blob/main/adducts_rules%20(CAMERA%20extended).csv)                        | Extended rules list. Rules definitions adapted from [`CAMERA`](https://www.bioconductor.org/packages/3.23/bioc/html/CAMERA.html) R package. |
| [`adducts_rules (commonMZ).csv`](https://github.com/plyush1993/MetaboCensoR/blob/main/adducts_rules%20(commonMZ).csv)               | Most comprehensive rules list. Rules definitions adapted from [`commonMZ`](https://github.com/stanstrup/commonMZ/) R package. |
