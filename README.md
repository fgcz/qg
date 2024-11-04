# queueGenerator

```{r}
52705270
```
## Install:

https://gitlab.bfabric.org/proteomics/shiny-ms-apps-dockerized

## sample input

container | instument  |LC.      | position | type  | area
:---------|:-----------|:--------|:---------|-------|-- 
36104     | ASTRAL_1   | MCLASS  | 1:A,1    | vial. | P
35270*    | EXPLORIS_3 | VANQUISH| 1:A1     | vial. | M
35117     | TIMSTOF_1  | EVOSEP  | 1:A1     | plate | P
35920     | EXPLORIS_1 | EVOSEP  | 1:A1     | vial. | P

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

## REST

```
$ BFABRICPY_CONFIG_ENV=PRODUCTION bfabric_flask.py --port 5002
```

## Next: functional test for all four use cases

#TODO

## Cronos
```
,Analysis Method,Source Tray,Source Vial,Sample Name,Xcalibur Method,Xcalibur Filename,Xcalibur Post Acquisition Program,Xcalibur Output Dir,Comment
1,"C:\Program Files (x86)\Chronos\Plugins\EvosepOne\Templates\Xcalibur\Xcalibur 30 SPD (44min, EV1106, EV1137).cam",EvoSlot 1,1,,,20240809_C35920_001_clean,C:\FGCZ\BioBeamer\biobeamer.bat,D:\Data2San\orders\Proteomics\EXPLORIS_2\analytic_20240809,
2,"C:\Program Files (x86)\Chronos\Plugins\EvosepOne\Templates\Xcalibur\Xcalibur 30 SPD (44min, EV1106, EV1137).cam",EvoSlot 1,2,,,20240809_C35920_002_autoQC01,C:\FGCZ\BioBeamer\biobeamer.bat,D:\Data2San\orders\Proteomics\EXPLORIS_2\analytic_20240809,
3,"C:\Program Files (x86)\Chronos\Plugins\EvosepOne\Templates\Xcalibur\Xcalibur 30 SPD (44min, EV1106, EV1137).cam",EvoSlot 1,3,,,20240809_C35920_003_S750451_35920_multiplexed1_fraction_1_phos,C:\FGCZ\BioBeamer\biobeamer.bat,D:\Data2San\orders\Proteomics\EXPLORIS_2\analytic_20240809,
4,"C:\Program Files (x86)\Chronos\Plugins\EvosepOne\Templates\Xcalibur\Xcalibur 30 SPD (44min, EV1106, EV1137).cam",EvoSlot 1,4,,,20240809_C35920_004_S750452_35920_multiplexed1_fraction_2_phos,C:\FGCZ\BioBeamer\biobeamer.bat,D:\Data2San\orders\Proteomics\EXPLORIS_2\analytic_20240809,
5,"C:\Program Files (x86)\Chronos\Plugins\EvosepOne\Templates\Xcalibur\Xcalibur 30 SPD (44min, EV1106, EV1137).cam",EvoSlot 1,5,,,20240809_C35920_005_S750453_35920_multiplexed1_fraction_3_phos,C:\FGCZ\BioBeamer\biobeamer.bat,D:\Data2San\orders\Proteomics\EXPLORIS_2\analytic_20240809,
```