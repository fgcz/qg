#R 
# 2023 Maria dErrico
# 2024-07 Christian Panse <cp@fgcz.ethz.ch>

# Define server logic required

#' @title Build QG server
#' @inheritParams shiny::shinyServer
#' @import bfabricShiny
#' @import shiny
#' @export
#' @noRd
.buildQgServer <- function(input, output, session) {
  
  debugmode <- FALSE
  TIMEdebugmode <- FALSE
  
  
  configInstrument <- reactive({
   .readConfigInstrument()
  })
  
  output$instrumentTable <- DT::renderDataTable({
    DT::datatable(configInstrument(),
                  rownames = FALSE,
                  style = 'auto',
                  editable = FALSE,
                  options = list(paging = FALSE))
  })
  
  
  columnOrder <<- c("File Name",
                    "Path",
                    "Position",
                    "Inj Vol",
                    "L3 Laboratory",
                    "Sample ID",
                    "Sample Name",
                    "Instrument Method") 
  
  plate_idx <- c("Y", "G", "R", "B")
  currentdate <- format(Sys.time(), "%Y%m%d")
  
  bf <- callModule(bfabricShiny::bfabricLogin,
                   "bfabric8")
  
  
  ## TODO(cp): rename that rv to make it clear it is a global var
  rv <- reactiveValues(download_flag = 0, wuid = NULL)
  
  output$bfabricUser <- renderUI({
    if (require("bfabricShiny")){
      bfabricInput("bfabric8")
    }
  })
  
  posturl <- reactive({
    bfabricShiny:::.posturl()
  })
  
  user <- reactive({
    shiny::req(bf$login())
    shiny::req(bf$webservicepassword())
    
    try(
      u <- bfabricShiny::read(login = bf$login(),
                              webservicepassword = bf$webservicepassword(),
                              posturl = posturl(),
                              endpoint = 'user',
                              query = list(login = bf$login()))$res[[1]])
    validate(shiny::need(try(u$login == bf$login()),
                         "Please provide valid bfabric login and webservicepassword."))
    message(paste("Request started from user", u$login))
    
    return(u)
  })
  
  # UI ==========
  # ------ input area ------
  #R
  
  output$area <- renderUI({
    shiny::req(input$orderID)
    
    ccc <- container()[[1]]$technology[[1]]
    print(ccc)
    if (grepl("Proteomics", ccc)){
      area <- "Proteomics"
    }else{
      area <- "Metabolomics"
    }
    sprintf("DEBUG area: %s", area)
    selectInput(
      "area",
      "Area:",
      c("Proteomics", "Metabolomics"),
      multiple = FALSE,
      selected = area,
      selectize = FALSE
    )
  })
  
  
  # ------ input instrument ------
  output$instrument <- renderUI({
    shiny::req(input$orderID)
    shiny::req(input$area)
    
    instruments <- configInstrument()$instrument[configInstrument()$area == input$area] |> sort()
    
    selectInput(
      "instrument",
      "Instrument:",
      instruments,
      multiple = FALSE,
      selected = instruments,
      selectize = TRUE
    )
  })
  
  # ------ input system ------
  output$system <- renderUI({
    shiny::req(input$area)
    systems <- configInstrument()$system[configInstrument()$area == input$area &
                                           configInstrument()$instrument == input$instrument]
    
    selectInput(
      "system",
      "System:",
      unique(systems),
      multiple = FALSE,
      selected = unique(systems),
      selectize = FALSE
    )
  })
  
  output$lc <- renderUI({
    shiny::req(input$area)
    shiny::req(input$system)
    
    lc <- configInstrument()$lc[configInstrument()$area == input$area &
                                           configInstrument()$instrument == input$instrument &
                                           configInstrument()$system == input$system]
    
    selectInput(
      "lc",
      "LC:",
      unique(lc),
      multiple = FALSE,
      selected = unique(lc)[1],
      selectize = TRUE
    )
  })
  
  ## input queue configuration FUN =======
  output$selectqFUN <- renderUI({
    shiny::req(input$area)
    shiny::req(input$system)
    qg:::.readConfigInstrument() -> configDf
    
    
    ## Find Index of queue config functions
    (input$area == configDf$area &
        input$system == configDf$system &
        input$instrument == configDf$instrument &
        input$lc == configDf$lc &
        !is.na(configDf$func)) |> which() -> idxFunc
    
    configDf$func[idxFunc] -> availableConfigFunctions
    
    
    #browser()
    if (length(availableConfigFunctions) == 0){
      return(HTML("<p>No queue configuration available for this area and system</p>"))
    }
    
    shiny::selectInput(inputId = "qFUN", 
                       label = "Queue configuration:",
                       choices = availableConfigFunctions,
                       multiple = FALSE,
                       selected = availableConfigFunctions[1],
                       selectize = FALSE)
  })
  
  # input orderID ------------
  output$orderID <- renderUI({
    shiny::req(user())
    
    if (user()$login == 'cpanse'){
      return( selectInput(
        "orderID",
        "Order ID:",
        c("28073", "31741",  "35464", "34843", "34777", "34778",
          "35117", "35270", "35394", "38884", "37530", "39408", "39061", "39473") |> sort() |> unique(),
        selected = "31741",
        multiple = TRUE,
        selectize = FALSE
      ))
    }
    
    numericInput(
      "orderID",
      "container/order/project ID:",
      "",
      min = NA,
      max = NA,
      step = NA,
      width = NULL
    )
  })
  
  output$debug <- shiny::renderPrint({
    cat(paste0("R version:", R.version.string))
    cat(paste0(" | qg version:", packageVersion('qg')))
  })

  
  # input plateID ------------
  output$plateID <- renderUI({
    # shiny::req(input$orderID)
    #shiny::req(read_plateid())
    #shiny::req(user())
    
    if (is.null(read_plateid())){
      shiny::HTML("No plate IDs available")
    }else{
      selectInput(
        "plateID",
        "List of available plate IDs:",
        read_plateid(),
        selected = "",
        multiple = TRUE,
        selectize = TRUE,
        size = NULL,
        width = NULL
      )}
  })
  
  # input check sample selection -------------
  output$checkSampleSelection <- renderUI({
    shiny::req(input$orderID) 
    if (is.null(read_plateid())){
      shiny::checkboxInput("booleanSampleSelection",
                           "Subsetting samples", value = FALSE)
    }else{NULL}
  })
  
  
  # input injvol ------------ 
  output$injvol <- renderUI({
    shiny::req(input$orderID)
    
    #if(length(shiny::req(input$plateID)) > 0 || nrow(sampleOfContainer()) > 0){
    numericInput(
      "injvol",
      "Inj Vol",
      min = NA,
      max = NA,
      step = NA,
      width = NULL,
      value = NA)
  })
  
  #input randomization ------------
  output$randomization <- renderUI({
    shiny::req(input$orderID) 
    shiny::radioButtons("randomization", "Randomization:",
                        c("no" = "no",
                          "plate" = "plate",
                          "all" = "all"), inline = TRUE)
  })
  
  # input orderID ------------
  output$frequency <- renderUI({
    shiny::req(input$orderID) 
    selectInput(
      "frequency",
      "QC frequency:",
      c(1, 2, 4, 8, 16, 32, 36, 48, 64, 1024),
      selected = "16",
      multiple = FALSE,
      selectize = 16
    )
    
  })
  

  # input select sample ------------
  output$selectSampleSelection <- renderUI({
    shiny::req(sampleOfContainer)
    shiny::req(input$booleanSampleSelection)
    
    if (input$booleanSampleSelection){
      shiny::tagList(
        shiny::tags$h4("Available samples for your queue:"),
        shiny::tags$h5("use \"shift + click\" or \"click + drag\"  for selecting a block of consecutive samples"),
        shiny::tags$h5("use \"control + click\" to select multiple samples"),
        shiny::tags$h5("use \"control + click + drag\" to select multiple blocks of consecutive samples"),
        selectInput('selectedSample', 'Sample:',
                    sampleOfContainer()$"Sample Name",
                    size = 40, multiple = TRUE, selectize = FALSE)
      )
    }else{
      rv$selectedSampleinput <- sampleOfContainer()$"Sample Name"
    }
  })
  
  # input extratext (not used)  ------------
  output$extratext <- renderUI({
    shiny::req(input$orderID)
    shiny::req(read_plateid())
    shiny::req(input$area)
    shiny::req(input$instrument)
    list(textInput(
      "extratext",
      "(Optional) Suffix to the folder name in Data2San:",
      "",
      width = NULL
    ),
    helpText(paste0("if empty, the file path will be the following one: D:\\Data2San\\p", input$orderID, "\\", input$area, "\\", input$instrument, "\\", bf$login(), "_", currentdate))
    )
  })
  
  # input extrameasurement ------------
  output$extrameasurement <- renderUI({
    shiny::req(input$orderID)
    shiny::req(read_plateid())
    shiny::req(input$area)
    shiny::req(input$instrument)
    list(textInput(
      "extrameasurement",
      "(Optional) Suffix to the file name in case of duplicate measurements on the same samples:",
      "",
      width = NULL
    ),
    helpText("Note that the suffix above is applied to all samples for all selected plates"))
  })
  
  
  # output$download --------------------
  output$download <- renderUI({
    res <- composeTable()
    
    message(nrow(res))
    if (is.null(res) || nrow(res) == 0){
      msg <- "The download is not possible yet. "
      HTML(msg)
    }else{
      message(paste0("debug output$download rv$wuid=", rv$wuid))
      if (isFALSE(is.null(rv$wuid))){
        wuUrl <- paste0("window.open('https://fgcz-bfabric.uzh.ch/bfabric/userlab/show-workunit.html?id=",
                        rv$wuid, "', '_blank')")
        
        actionButton("download",
                     paste("go to B-Fabric workunit", rv$wuid),
                     onclick = wuUrl)
      }else{
        #if (file.exists(csvFilename())){
        actionButton('generate', 'Upload configuration to b-fabric')
        #  }
      }
    }
  })
  
  
  # FUNCTIONS ===========================
  
  # read_plateid FUN ------------
  read_plateid <- reactive({
    # shiny::req(user())
    shiny::req(input$orderID)
   
    shiny::withProgress(message = 'Reading plates of container', session = session,{
      res <- bfabricShiny::read(bf$login(),
                                bf$webservicepassword(),
                                posturl = posturl(),
                                endpoint = "plate",
                                query = list('containerid' = input$orderID))$res
      
    })
    plate_ids <- sapply(res, function(x) x$id)
    #if (length(plate_ids) == 0) return(NULL)
    #browser()
    #shiny::validate(
    #  shiny::need(try(length(plate_ids) > 0), "There are no plate defined for this order")
    #)
    
    if (length(plate_ids) > 0){
      return(sort(plate_ids))
    }
    
    NULL
  })
  
  read_sampletype <- function(sampleid){
    res <- bfabricShiny::read(bf$login(), bf$webservicepassword(),
                              posturl = posturl(),
                              endpoint = "sample",
                              query = list('id' = sampleid))
    if ( debugmode == TRUE ) {message(res)}
    if (is.null(res[[1]]$parent)){
      sampletype <- res[[1]]$type
      if (is.null(sampletype)){
        return("Unknown")
      } else {
        return(sampletype)
      }
    }
    read_sampletype(res[[1]]$parent[[1]]$id)
  }
  
  sampleOfContainer <- reactive({
    shiny::req(input$orderID)
    shiny::withProgress(message = 'Reading sample of container', session = session, {
      .readSampleOfContainer(input$orderID,
                             login = bf$login(),
                             webservicepassword = bf$webservicepassword(),
                             posturl = posturl())
    })
  })
  
  
  # read container from bfabric -----------------
  # bfabricShiny::read(login = login, webservicepassword = webservicepassword, posturl = bfabricposturl, endpoint = 'container', query = list('id' = list(34778, 35116)))$res -> rv
  container <- shiny::reactive({
    shiny::req(input$orderID)
    shiny::req(bf$login())
    shiny::req(bf$webservicepassword())
    
    login <- bf$login()
    webservicepassword <- bf$webservicepassword()
    bfabricposturl <- posturl()
    orderId <- input$orderID
    
    rv <- bfabricShiny::read(login = login,
                              webservicepassword = webservicepassword,
                              posturl = bfabricposturl,
                              endpoint = "container",
                              maxitems = 100,
                              query = list('id' = orderId))
    
    validate(need(length(rv$res) > 0, "Empty container result set."))
    
    validate(need(rv$res[[1]]$technology[[1]] %in% c('Metabolomics', 'Proteomics', 'Metabolomics/Biophysics'),
                  "The technology type of the container is not supported."))
    rv$res
  })
  
  filteredSampleOfContainer <- reactive({
    shiny::req(sampleOfContainer)
    
    if (length(rv$selectedSample) > 0){
      sampleOfContainer() |>
        subset(sampleOfContainer()$`Sample Name` %in% rv$selectedSample)
    }else{
      sampleOfContainer()
    }
  })
  
  ## ====== compose output table ==========
  composeTable <- reactive({
    shiny::req(input$instrument)
    shiny::req(input$injvol)
    shiny::req(input$area)
    #shiny::req(input$plateID)
    shiny::req(input$qFUN)
    SAVERDATA <- TRUE
  
    if (length(input$plateID) == 0){
      ## --------vial block (no plateid)------------ 
      ## we fetch all samples of a container
      QCrow <- "F"
      randomization <- FALSE
      if (input$randomization == 'plate'){
        randomization <- TRUE
      }
      
      filteredSampleOfContainer() |> 
        qg::.composeVialSampleTable(orderID = input$orderID,
                                instrument = input$instrument,
                                user = bf$login(),
                                injVol = input$injvol,
                                area = input$area,
                                lc = input$lc,
                                randomization = randomization) -> df
      
     
    }else{
      ## --------plate block ------------ 
      ## we iterate over the given plates
      QCrow <- "H"
      plateCounter <- 1
      shiny::withProgress(message = 'Reading sample of plate(s)',
                          value = 0, session = session, {
        input$plateID |>
          lapply(FUN = function(pid){
            shiny::incProgress(1 / length(input$plateID))
            readPlate(pid, login = bf$login(),
                      webservicepassword = bf$webservicepassword(),
                      posturl = posturl()) |>
              qg::.composePlateSampleTable(orderID = input$orderID,
                                           instrument = input$instrument,
                                           system = input$system,
                                           lc = input$lc,
                                           user = bf$login(),
                                           injVol = input$injvol,
                                           area = input$area,
                                           plateCounter = plateCounter,
                                           randomization = input$randomization) -> p
            
           
            # global counter
            plateCounter <<- plateCounter + 1
            ## TODO(cp): check if this is necessary
            #p[, columnOrder]
            p
          }) |> Reduce(f = rbind) -> df
      }) 
      
      if (input$randomization == "all"){
        set.seed(872436)
        df[sample(nrow(df)), ] -> df
      }
    }
    if (FALSE){
      tempfile(pattern = "fgcz_queue_generator_", fileext = ".RData") -> tf
      base::save(df, file = tf)
      message("Output table saved to ", tf)
    } 
    
    if (SAVERDATA) {
      
      filenameRData <- file.path("/tmp/", paste("test", input$area,
                                                input$lc,
                                                input$system,
                                                input$instrument,
                                                paste0("c", input$orderID[1], ".RData"),
                                                sep='-'))
      
      shiny::showNotification(paste0("Save file ", filenameRData))
      base::save(df, file = filenameRData)
    }
    
    ## ------injectSamples------
    ## here we evaluate the queue configuration function, and afterwards,
    ## replace placeholders like {run}, {date}, {container} in the filenames
    if (input$area == "Metabolomics") {
      do.call(
        what = input$qFUN,
        args = list(x = df, howOften = as.integer(input$frequency))
      ) |>
        qg::.interpolateFinalRows(container = input$orderID[1])
    } else {
      if (SAVERDATA) {
        # filenameRData <- file.path("/tmp/", paste0(input$area,"_c", input$orderID[1], ".RData"))
        # base::save(df, file = filenameRData)
      }
      #browser()
      do.call(
        what = input$qFUN,
        args = list(
          x = df,
          lc = input$lc,
          containerid = input$orderID[1],
          howOften = as.integer(input$frequency)
        )
      ) |>
        qg::.interpolateFinalRows(container = input$orderID[1])
    }
    
  })
  
  output$plotFirstPlate <- renderPlot({
    shiny::req(composeTable())
   
    if (shiny::req(input$system) == "Chronos" && shiny::req(input$lc) == "EVOSEP6x12x8"){
      op <- par(mar = c(0, 0, 0, 0))
      composeTable()$`Source Vial` |> as.integer() -> run
   
      ceiling(run / 12)-> y
      ((run - 1) %% 12) + 1  -> x 
      
      plot(x, y,
           xlim = c(1, 12), ylim = c(1, 8), type = 'n')
      
      text(x = x, y = y, labels = run, cex = 1.4)
      
      lines(x, y)
    }
  })
  
  output$outputKable <- DT::renderDataTable({
    shiny::req(composeTable())
    
   
    
    DT::datatable(composeTable(),
                  rownames = FALSE,
                  style = 'auto',
                  editable = FALSE,
                  options = list(paging = FALSE))
  })
  
  csvFilename <- reactive({
    tempfile(pattern = "fgcz_queue_generator_XCalibur", fileext = ".csv")
  })
  xmlFilename <- reactive({
    tempfile(pattern = "fgcz_queue_generator_Hystar.", fileext = ".xml")
  })
  
  
  # ObserveEvents ===================================
  
  # observe selected sample ---------------
  oeSelectedSample <- observeEvent(input$selectedSample, {
    shiny::req(input$booleanSampleSelection)
    
    if (input$booleanSampleSelection){
      rv$selectedSample <- input$selectedSample
    }
  })
  
  # upload to bfabric --------------
  bfabricUploadResource <- observeEvent(input$generate, {
    progress <- shiny::Progress$new(min = 0, max = 1)
    progress$set(message = "upload csv file to bfabric")
    on.exit(progress$close())
    
    filename <- NULL
    workunitname <- NULL
    resourcename <- NULL
    
    if(grepl("Hystar", input$qFUN)){
      composeTable() |>
        .toHystar(file = xmlFilename())
      filename <- xmlFilename()
      workunitname <- sprintf("Hystar-MS-configuration_orderID-%s", input$orderID[1])
      resourcename = sprintf("queue-C%s_%s.xml",
                             input$orderID[1],
                             format(Sys.time(), format="%Y%m%d-%H%M%S"))
    }else if(grepl("Chronos", input$qFUN)){
      message(msg <- paste0("Writing Chronos csv config file ",
                            csvFilename(), " ..."))
      
      
      df <- composeTable()
      paste0("EvoSlot ", df$`Source Tray`) -> df$`Source Tray`
      row.names <- 1:nrow(df) |> as.character()
      write.csv(df, file = csvFilename(),
                row.names = TRUE,
                quote = FALSE)
      filename <- csvFilename()
      workunitname <- sprintf("Chronos-MS-configuration_orderID-%s", input$orderID[1])
      resourcename = sprintf("queue-C%s_%s.csv",
                             input$orderID[1],
                             format(Sys.time(), format="%Y%m%d-%H%M%S"))
      
    } else{
      message(paste0("Writing XCalibur csv config file ",
                     csvFilename(), " ..."))
      
      ## to make it readable by XCalibur
      cat("Bracket Type=4\r\n",
          file = csvFilename(),
          append = FALSE)
      utils::write.table(composeTable(),
                         sep = ',',
                         file = csvFilename(),
                         row.names = FALSE,
                         append = TRUE,
                         quote = FALSE,
                         eol = '\r\n')
      filename <- csvFilename()
      workunitname <- sprintf("XCalibur-MS-configuration_orderID-%s", input$orderID[1])
      resourcename = sprintf("queue-C%s_%s.csv",
                             input$orderID[1],
                             format(Sys.time(), format="%Y%m%d-%H%M%S"))
    }
    
    
    message("Uploading queue config file to bfabric ...")
    progress$set(message = "uploading config file with plate info to bfabric")
    
    rr <- bfabricShiny::uploadResource(
      login = bf$login(),
      webservicepassword = bf$webservicepassword(),
      posturl = posturl(),
      containerid = input$orderID,
      applicationid = 319,
      status = "PENDING",
      description = "plate queue generator config file",
      workunitname = workunitname,
      resourcename = resourcename,
      file = filename
    )
    
    ## save wuid
    rv$wuid <- rr$workunit$res[[1]]$id
  })
}


#' Define queue generator UI ======
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel uiOutput hr
#' @importFrom shiny uiOutput htmlOutput NS br a
#' @importFrom shiny dataTableOutput
#' @export
.buildQgUI <-  function(){
  
  #ns <- NS(id)
  
  fluidPage(
    titlePanel(sprintf("MS Queue Generator (qg v%s)", packageVersion('qg'))), 
    sidebarLayout(
      sidebarPanel(
        uiOutput(("bfabricUser")),
        hr(),
        uiOutput(("orderID")),
        uiOutput(("area")),
        uiOutput(("instrument")),
        uiOutput(("system")),
        uiOutput(("lc")),
        uiOutput(("selectqFUN")),
        uiOutput(("plateID")),
        uiOutput(("checkSampleSelection")),
        uiOutput(("selectSampleSelection")),
        uiOutput(("injvol")),
        uiOutput(("randomization")),
        uiOutput(("frequency")),
        htmlOutput(("download")),
        hr(),
        a("B-fabric application 319 page", href="https://fgcz-bfabric.uzh.ch/bfabric/application/show.html?id=319"),
        br(),
        a("Internal queue generator tiki-wiki page", href="https://fgcz-intranet.uzh.ch/tiki-index.php?page=sw.queueGenerator"),
        br(),
        a('Report an issue', href='https://gitlab.bfabric.org/proteomics/qg/-/issues'),
        uiOutput('debug'),
        plotOutput('plotFirstPlate')
      ),
      mainPanel(
        tabsetPanel( 
          tabPanel("Queue Table", dataTableOutput("outputKable")),  
          tabPanel("Instrument Configuration", dataTableOutput("instrumentTable"))
        )
      )
    )
  )
}

#' @importFrom shiny shinyApp
#' @export
buildQg <- function(){
  #@ui <- .buildQgUI()
  #server <- .buildQgServer()
  app <- shiny::shinyApp(ui = .buildQgUI, server = .buildQgServer)
}
