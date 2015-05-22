source('R/funcs.R')

library(reshape)
library(ggplot2)
library(dplyr)

# Define server logic required to generate and plot data
shinyServer(function(input, output) {
  
  # run models fir each scenario
  runmod <- eventReactive(input$runmod, {
    
    # action button, only applies to stuff in this chunk
    
    # inputs
    scenario1 <- input$scenario1
    scenario2 <- input$scenario2
    
    # run models based on inputs
    run_mod(scenario1, 'scenario1')
    run_mod(scenario2, 'scenario2')
    
    # combine data from output, assign scenario names to those selected
    data_format(scenario1, scenario2)
    
  })
    
  # first variable plot
  output$var1plot <- renderPlot({
     
    alldat <- runmod()
    
    # data to plot
    varsel <- input$var1
    plo_fun(varsel, alldat)
    
    }, height = 300, width = 600)

  # second variable plot
  output$var2plot <- renderPlot({
     
    alldat <- runmod()
    
    # data to plot
    varsel <- input$var2
    plo_fun(varsel, alldat)
    
    }, height = 300, width = 600)

  # third variable plot
  output$var3plot <- renderPlot({
     
    alldat <- runmod()
    
    # data to plot
    varsel <- input$var3
    plo_fun(varsel, alldat)
    
    }, height = 300, width = 600)

})