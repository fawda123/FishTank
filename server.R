source('R/funcs.R')

library(reshape)
library(ggplot2)

# Define server logic required to generate and plot data
shinyServer(function(input, output) {
  
  # run models fir each scenario
  runmod <- reactive({
    
    # inputs
    scenario1 <- input$scenario1
    scenario2 <- input$scenario2
    
    # run models based on inputs
    run_mod(scenario1, 'scenario1')
    run_mod(scenario2, 'scenario2')
    
  })
    
  output$simplot <- renderPlot({
     
    toplo <- runmod()
    
    NULL
    
    }, height = 600, width = 900)

})