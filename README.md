# queueGenerator (qg)

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


## Sample use cases

cont      | instument  |LC.               | position | type  | area
:---------|:-----------|:-----------------|:---------|-------|-- 
36104     | ASTRAL_1   | MCLASS           | 1:A,1    | vial. | P
35270     | EXPLORIS_3 | VANQUISH         | 1:A1     | vial. | M
35117     | TIMSTOF_1  | EVOSEP           | 1:A1     | plate | P
36946     | EXPLORIS_2 | EVOSEP           | 1:A1     | vial  | P
37142     | EXPORTIS_2 | EVOSEP           | 1:A1     | plate 479[34]  | P
37146     | EXPLORIS_2 | EVOSEP chronos   | 1:A1     | vial  | P
28073     | ASTRAL     | VANQUISH         |          |       |


## Usage

1. install python3 bfabricPy package

```
uv tool install bfabric-scripts   
```


2. run the SOAP REST Proxy

```
BFABRICPY_CONFIG_ENV=FGCZSTABLE  bfabric_flask.py
```

3.

set HOME `.Renviron`

```
login="$USER"
webservicepassword="eXXXe3XXX55dc39eXXXaeb763ec2XXXX"
bfabricposturl="http://127.0.0.1:5000/"
```

4.

install 

```
devtools::install_github("fgcz/qg")

```

and use the queue generator 
```
shiny::shinyApp(ui = qg::.buildQgUI , server = qg::.buildQgServer) -> qg
qg

## Have a lot of fun!
```


5. 


Read how to use it on the command line:

```
browserViewer("qg")
```