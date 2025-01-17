---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# DIGCLASS

<!-- badges: start -->
[![R-CMD-check](https://github.com/cimentadaj/DIGCLASS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cimentadaj/DIGCLASS/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `DIGCLASS` R package aims to make translation between occupational social classes easy and comprehensive. It facilitates the translation of the International Standard Classification of Occupations (ISCO) from 1968, 1988 and 2008 to a wide range of social class schemes.

This package is a work in progress and has implemented currently these translations:

* **ISCO68**
  - [X] ISCO68 to ISCO88
  - [X] ISCO68 to ISCO08
  - [X] ISCO68 to SIOPS
  - [X] ISCO68 to ISEI
  - [X] ISCO68 to EGP11/EGP7/EGP5/EGP3
  - [X] ISCO68 to EGP11-MP

* **ISCO88**
  - [X] ISCO88 to ISEI
  - [X] ISCO88 to SIOPS
  - [X] ISCO88 to IPICS
  - [X] ISCO88 to MPS88
  - [X] ISCO88 to EGP11/EGP7/EGP5
  - [X] ISCO88 to EGP11-MP
  - [X] ISCO88 to OESCH16/OESCH8/OESCH5
  - [X] ISCO88 to ISCO88COM
  - [X] ISCO88 to ISCO08
  - [X] ISCO88 to ISCO68
  - [X] ISCO88 to ORDC
  - [X] ISCO88COM to ESEC - ISCO88COM must be 3 digits
  - [X] ISCO88COM to ESEC-MP - ISCO88COM must be 3 digits
  - [X] ISCO88COM to MSEC - ISCO88COM must be 3 digits
  - [X] ISCO88COM to WRIGHT
  - [X] ISCO88 to OEP (1-4 digits)

* **ISCO08**
  - [X] ISCO08 to ISCO88
  - [X] ISCO08 to ISEI
  - [X] ISCO08 to SIOPS
  - [X] ISCO08 to Microclasses
  - [X] ISCO08 to IPICS
  - [X] ISCO08 to OESCH16/OESCH8/OESCH5
  - [X] ISCO08 to ESEC - ISCO08 must be 3 digits
  - [X] ISCO08 to ESEC - ISCO08 must be 2 digits
  - [X] ISCO08 to ESEC-MP - ISCO08 must be 3 digits
  - [X] ISCO08 to MSEC - ISCO08 must be 3 digits
  - [X] ISCO08 to ESEG - ISCO08 must be 2 digits
  - [X] ISCO08 to OEP (1-4 digits)


* **ESCO**
  - [X] ESCO to ISCO08

* **Extras**
  - [X] Translation between major/submajor/minor/unit groups for ISCO68, ISCO88 and ISCO08
  - [X] Repair ISCO variables

## Installation

You can install the development version of DIGCLASS from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cimentadaj/DIGCLASS")
```

## Example

Here's an example of translating ISCO68 to ISEI and EGP class schemas:


``` r
library(DIGCLASS)
library(dplyr)

# Internal data for the European Social Survey round 6
# containing different ISCO variables
ess %>%
  transmute(
    isco68,
    isei = isco68_to_isei(isco68),
    egp = isco68_to_egp(isco68, self_employed, emplno),
    egp_labels = isco68_to_egp(isco68, self_employed, emplno, label = TRUE)
  )
#> # A tibble: 48,285 × 4
#>    isco68 isei  egp   egp_labels                            
#>    <chr>  <chr> <chr> <chr>                                 
#>  1 5890   35    6     'IVb: Self-employed with no employees'
#>  2 2120   67    2     'II: Lower Controllers'               
#>  3 7200   34    8     'VI: Skilled Worker'                  
#>  4 9310   32    8     'VI: Skilled Worker'                  
#>  5 6220   16    10    'VIIb: Farm Labor'                    
#>  6 6220   16    10    'VIIb: Farm Labor'                    
#>  7 9595   24    9     'VIIa: Unskilled Worker'              
#>  8 6000   46    11    'IVc: Self-employed Farmer'           
#>  9 6000   46    11    'IVc: Self-employed Farmer'           
#> 10 6220   16    10    'VIIb: Farm Labor'                    
#> # ℹ 48,275 more rows
```

The nomenclature of the function is `{origin}_to_{destination}` where `origin` is the origin class schema and `destination` is the destination class schema. The usual workflow is for you to type, for example `isco` and then hit `TAB` to get auto-completion on all possible translations.

For those class schemas that have labels, the `label` argument returns the labels instead of the class codes.

## Steps to add a new translation

1. Add two csv files respectively in `data-raw/social_classes/labels/` and `data-raw/social_classes/translation/` containing the labels and translation for the two schemas.

2. Run the script `data-raw/social_classes.R` (with the root directory in `data-raw/`)

3. Add a new function inside `R/` with the convention `{origin}_to_{destination}()` where origin and destination are the class schemas we're translating. Please have a look at other translation to recycle common functions to do translations.

4. Add proper documentation to the function

## Other R packages

This package has benefitted greatly from other open source packages that already pave the way for translation between social class schemas. In particular, we've learned a lot and borrowed code from all of these packages:

- [ISCOGEN](https://github.com/benjann/iscogen): Stata package
- [SocialPosition](https://cran.r-project.org/web/packages/SocialPosition/index.html): R package
- [occupar](https://github.com/DiogoFerrari/occupar/): R package


## Funding

This project has been funded through the European Commission’s JRC Centre for Advanced Studies and the project Social Classes in the Digital Age (DIGCLASS): [https://joint-research-centre.ec.europa.eu/tools-and-laboratories/centre-advanced-studies/digclass_en](https://joint-research-centre.ec.europa.eu/tools-and-laboratories/centre-advanced-studies/digclass_en)
