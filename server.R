source('R/funcs.R')

library(reshape)
library(ggplot2)
library(dplyr)
library(tidyr)
library(dygraphs)
library(xts)

# Define server logic required to generate and plot data
shinyServer(function(input, output, session) {
  
  # run models fir each scenario
  runmod <- eventReactive(input$runmod, {
    
    # action button, only applies to stuff in this chunk
    
    # inputs
    scenario1 <- input$scenario1
    scenario2 <- input$scenario2
    
    # progress
    progress <- shiny::Progress$new(session, min=1, max=1)
    progress$set(message = 'FishTank in progress...')
    on.exit(progress$close())
    
    # run models based on inputs
    run_mod(scenario1, 'scenario1')
    run_mod(scenario2, 'scenario2')
    
    # combine data from output, assign scenario names to those selected
    data_format(scenario1, scenario2)
    
  })
    
  # first variable plot
  output$var1plot <- renderDygraph({
     
    alldat <- runmod()
  
    # data to plot
    varsel <- input$var1
    plo_fun(varsel, alldat)
    
    })

  # second variable plot
  output$var2plot <- renderDygraph({
     
    alldat <- runmod()
    
    # data to plot
    varsel <- input$var2
    plo_fun(varsel, alldat)
    
    })

  # third variable plot
  output$var3plot <- renderDygraph({
     
    alldat <- runmod()
    
    # data to plot
    varsel <- input$var3
    plo_fun(varsel, alldat)
    
    })

})