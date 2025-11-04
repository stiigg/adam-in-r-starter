# ADaM in R (R-only Scaffold)

### Setup
1. Install R >= 4.2.
2. In the project root, run:
   ```r
   install.packages(c("targets","tarchetypes","dplyr","tidyr","lubridate","readr","stringr","tibble","yaml","validate","testthat","xportr"))
   ```
3. Put SDTM-like CSVs into `data_raw/` as `dm.csv`, `ex.csv`.

### Run pipeline
```r
library(targets)
tar_make()
```
Outputs: `ADSL.xpt` and a QC summary.

### Tests
```r
install.packages("testthat")
source("R/derivations_adsl.R")
source("R/utils.R")
source("R/qc.R")
source("R/exports.R")
testthat::test_dir("tests/testthat")
```
