ui <- qg::.buildQgUI
server <- qg::.readSampleOfContainer
shiny::shinyApp(ui, server)
