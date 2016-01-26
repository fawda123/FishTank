library(shiny)
library(dygraphs)

source('R/funcs.R')

# names of input scenarios
scenarios <- list.dirs('scenarios', full.names = FALSE)[-1]

# Define UI for application
shinyUI(fluidPage(
  
  theme = 'styles.css',
  
  # main panel for variable selection
  mainPanel(width = 12,
      
  # spacing
  fluidRow(HTML('<p></p>')),
  
  # top controls
  fluidRow(
    
    column(width = 3,
      actionButton('runmod', label = img(src = "FishTankLogo_v2.png", width = 120))
    ),
    
    column(width = 4, 
      selectInput(inputId = 'scenario1', 
        label = h4('Scenario one'),
        choices = scenarios,
        selected = scenarios[4]
      )
    ),
    
    column(width = 4, 
      selectInput(inputId = 'scenario2', 
        label = h4('Scenario two'),
        choices = scenarios,
        selected = scenarios[3]
      )
    )
   
  ),
  
  #spacing
  fluidRow(HTML('<p></p>')),
    
    tabsetPanel(
      
      tabPanel("Model output", 
    
        HTML('<p></p>'),
        
        # first row of plots
        fluidRow(
          
          # first variable
          column(width = 6,
            
            selectInput(inputId = 'var1',
              label = NULL,
              choices = labels_fun()$lngs, 
              selected = 'Nitrate', 
              width = '600px'
            ),
                                   
            dygraphOutput("var1plot", height = "300px", width = "600px")
          
          ),
          
          # second variable
          column(width = 6, 
          
            selectInput(inputId = 'var2',
              label = NULL,
              choices = labels_fun()$lngs, 
              selected = 'Phosphate', 
              width = '600px'
            ),
      
            dygraphOutput("var2plot", height = "300px", width = "600px")
            
          )
          
        ), 
        
        HTML('<p></p>'),
        
        # second variable
        fluidRow(
          
          column(width = 6,
            
            selectInput(inputId = 'var3',
              label = NULL,
              choices = labels_fun()$lngs, 
              selected = 'Phytoplankton Abundance 1', 
              width = '600px'
            ),
                                   
            dygraphOutput("var3plot", height = "300px", width = "600px")
          
          ),
          
          column(width = 6, 
          
            selectInput(inputId = 'var4',
              label = NULL,
              choices = labels_fun()$lngs, 
              selected = 'Dissolved Oxygen', 
              width = '600px'
            ),
      
            dygraphOutput("var4plot", height = "300px", width = "600px")
            
          )
          
        )
        
      ),
    
    tabPanel("Scenario descriptions",
        
        h3('Scenario 1'),     
        htmlOutput("descrips1"),
        HTML('<br></br>'),
        h3('Scenario 2'),
        htmlOutput("descrips2")
      
      ),
      
    tabPanel("Initial conditions",
             
        tableOutput("initconds")
        
      ),
      
    tabPanel("Parameter settings",
        
        h3('Scenario 1'),   
        htmlOutput("initparms1"),
        HTML('<br></br>'),
        h3('Scenario 2'),
        htmlOutput("initparms2")
        
      )
    
    )
            
  )
  
))