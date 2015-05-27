library(shiny)
library(dygraphs)

source('R/funcs.R')

# names of input scenarios
scenarios <- list.dirs('scenarios', full.names = FALSE)[-1]

# Define UI for application
shinyUI(fluidPage(
  
  theme = 'styles.css',
  
  # Application title

  fluidRow(
    
    column(width = 1, offset = 0,
      img(src = "icon.png", height = 70, width = 70)
    ),
    
    column(width = 6, 
      h1('FishTank')
    )
    
  ),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    
    actionButton('runmod', label = "Run model!", class = "color:red"),
    
    selectInput(inputId = 'scenario1', 
      label = h4('Scenario one'),
      choices = scenarios,
      selected = scenarios[1]
    ),
   
    selectInput(inputId = 'scenario2', 
      label = h4('Scenario two'),
      choices = scenarios,
      selected = scenarios[2]
    ),

    width = 3
    
  ),
  
  # main panel for variable selection
  mainPanel(

    tabsetPanel(
      
      tabPanel("Model output", 
    
        HTML('<p></p>'),
        
        # first variable
        column(9, 
          selectInput(inputId = 'var1',
            label = NULL,
            choices = labels_fun()$lngs, 
            selected = 'nitrate', 
            width = '600px'
          )
        ),
        
        column(9,
          dygraphOutput("var1plot", height = "300px", width = "700px"),
          HTML('<p></p>')
          ),
        
        # second variable
        column(9, 
          selectInput(inputId = 'var2',
            label = NULL,
            choices = labels_fun()$lngs, 
            selected = 'phytoplankton abundance 1', 
            width = '600px'
          )
        ),
            
        column(9, 
          dygraphOutput("var2plot", height = "300px", width = "700px"),
          HTML('<p></p>')
        ),
        
        # third variable
        column(9, 
          selectInput(inputId = 'var3',
            label = NULL,
            choices = labels_fun()$lngs, 
            selected = 'oxygen', 
            width = '600px'
          )
        ),
         
        column(9,    
          dygraphOutput("var3plot", height = "300px", width = "700px"),
          HTML('<p></p>')
        )
      
      ),
    
    tabPanel("Initial conditions",
             
        tableOutput("initconds")
        
      )
    
    )
            
  )
  
))