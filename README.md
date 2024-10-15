# queueGenerator

```
shiny::shinyApp(ui = qg::.buildQgUI , server = qg::.buildQgServer) -> qg
## Have a lot of fun!
qg
```

## Install:

https://gitlab.bfabric.org/proteomics/shiny-ms-apps-dockerized

## sample input

container | instument  |LC.      | position | type  | area
:---------|:-----------|:--------|:---------|-------|-- 
36104     | ASTRAL_1   | MCLASS  | 1:A,1    | vial. | P
  35270     | EXPLORIS_3 | VANQUISH| 1:A1     | vial. | M
35117     | TIMSTOF_1  | EVOSEP  | 1:A1     | plate | P
xxx

## Motivation:

anno 2010

* fgcz naming convention,
* injecting bfabric sample IDs and order IDs, ...
* automatic linking to meta data in bfabric ...
* on some point we want to have it in bfabric 
    
## Challenges 

* ((autoQC_user|QC)) - autoQC01, autoQC03, autoQC4; soon: ((autoQC05)), ((autoQC06)), ((autoQC07)), ((autoQC08))
* blanc/clean/wash runs
* plate, e.g., 96WP, / multiplate systems
* LC systems: `.eksigent()`, `.water()`, ..., `nanoElute()` ,  `.vanquish()`
* instrument control software, e.g., `XCalibur`, `HyStar`
* (block) randomization run order / across plates
* multi order
* acquisition of a sample set multiple times (with different methods, eg. pos and neg mode)
* DIL (metabolomics)
* ...
* Legacy code

## Workflow/Pipeline

0. user -> enters samples in bfabric
1. qg queries bfabric sample of project
2. ...


## Package Organization

* `inst/extdata/instrument.csv` - table instrument;area;system;lc

* `R/config.R` - general configuration, e.g., Hystar driver, insertSample FUN
* `R/configProteomics.R` - configuration for proteomics
* `R/configMetabolomics.R`  - configuration for metabolomics
* `R/lc-systems.R` - LC systems mainly how to write the possitions

## See also

* https://fgcz-intranet.uzh.ch/tiki-index.php?page=sw.queueGenerator
* runs as virtual machine: http://fgcz-c-072.uzh.ch:4001/extractPlateId/ 


## Next: functional test for all four use cases
