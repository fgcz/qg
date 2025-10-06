#R
#options(shiny.fullstacktrace = TRUE)
shiny::shinyApp(ui = qg::.buildQgUI , server = qg::.buildQgServer) 