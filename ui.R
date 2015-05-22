library(shiny)
source('R/funcs.R')

# Define UI for application
shinyUI(fluidPage(
  
  theme = 'styles.css',
  
  # Application title
  headerPanel("FishTank"), 
    
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    
    actionButton('runmod', label = "Run model!", class = "color:red"),
    
    selectInput(inputId = 'scenario1', 
      label = h4('Scenario one'),
      choices = c('base', 'low', 'high'),
      selected = 'base'
    ),
   
    selectInput(inputId = 'scenario2', 
      label = h4('Scenario two'),
      choices = c('base', 'low', 'high'),
      selected = 'high'
    ),

    width = 3
    
  ),
  
  # main panel for variable selection
  mainPanel(

    # first variable
    column(9, 
      selectInput(inputId = 'var1',
        label = NULL,
        choices = labels_fun()$lngs, 
        selected = 'nitrate', 
        width = '600px'
      )
    ),
        
    plotOutput("var1plot", height = "100%"),

    # second variable
    column(9, 
      selectInput(inputId = 'var2',
        label = NULL,
        choices = labels_fun()$lngs, 
        selected = 'phytoplankton abundance 1', 
        width = '600px'
      )
    ),
        
    plotOutput("var2plot", height = "100%"),
    
    # third variable
    column(9, 
      selectInput(inputId = 'var3',
        label = NULL,
        choices = labels_fun()$lngs, 
        selected = 'oxygen', 
        width = '600px'
      )
    ),
        
    plotOutput("var3plot", height = "100%")
            
  )
  
))