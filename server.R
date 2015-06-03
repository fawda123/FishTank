source('R/funcs.R')

library(reshape)
library(ggplot2)
library(dplyr)
library(tidyr)
library(dygraphs)
library(xts)
library(htmltools)

# change this to FALSE to use preexisting results
# otherwise results are created running the model each time
onfly <- TRUE

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
    
    # run model on the fly 
    if(onfly){
      
      # run models based on inputs
      run_mod(scenario1, 'scenario1')
      run_mod(scenario2, 'scenario2')
      
      # combine data from output, assign scenario names to those selected
      data_format(scenario1, scenario2)
    
    # otherwise use pre-existing data for faster widget
    } else {
      
      data_format2(scenario1, scenario2)
       
    }
      
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
  
  # fourth variable plot
  output$var4plot <- renderDygraph({
     
    alldat <- runmod()
    
    # data to plot
    varsel <- input$var4
    plo_fun(varsel, alldat)
    
    })
  
  # scenario descriptions
  output$descrips1 <- renderText({
    sc1 <- input$scenario1
    read_descrips(sc1)
  })
  output$descrips2 <- renderText({
    sc2 <- input$scenario2
    read_descrips(sc2)
  })
  
  # table for input conditions
  # table of performance metrics
  output$initconds <- renderTable({
    
    sc1 <- input$scenario1
    sc2 <- input$scenario2
    
    tab_form(sc1, sc2)
    
  }, include.rownames = F)
  

  # parameter values
  output$initparms1 <- renderText({
    sc1 <- input$scenario1
    HTML(read_parms(sc1))
  })
  output$initparms2 <- renderText({
    sc2 <- input$scenario2
    read_parms(sc2)
  })
  
})