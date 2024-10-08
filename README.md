# queueGenerator

```
shiny::shinyApp(ui = qg::.buildQgUI , server = qg::.buildQgServer) -> qg
## Have a lot of fun!
qg
```

## Install:

https://gitlab.bfabric.org/proteomics/shiny-ms-apps-dockerized


## sample input

container | instument |LC
:---------|:----------|:-------
p36104    | ASTRAL_1  | MCLASS

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

## See also

https://fgcz-intranet.uzh.ch/tiki-index.php?page=sw.queueGenerator
