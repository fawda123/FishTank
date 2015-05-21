library(shiny)

# Define UI for application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("FishTank"), 
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    
    submitButton(text = "Run!"),
    
    selectInput(inputId = 'scenario1', 
      label = h4('Select first scenario'),
      choices = c('base', 'low', 'high'),
      selected = 'base'
    ),
   
    selectInput(inputId = 'scenario2', 
      label = h4('Select second scenario'),
      choices = c('base', 'low', 'high'),
      selected = 'low'
    ),

    width = 3
    
  ),
  
  # Show the plot results
  mainPanel(
    plotOutput("simplot", width = "100%")
  )
  
))