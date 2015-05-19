library(shiny)

# Define UI for application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("FishTank"), 
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    submitButton(text = "Run!"),
    textInput("A1", "A1", value = "88562256"), 
    textInput("A2", "A2", value = "88562256"), 
    textInput("A3", "A3", value = "88562256"), 
    textInput("A4", "A4", value = "88562256"), 
    textInput("A5", "A5", value = "88562256"), 
    textInput("A6", "A6", value = "88562256"), 
    textInput("Qn1", "Qn1", value = "0.30649887E-8"),
    textInput("Qn2", "Qn2", value = "0.30649887E-8"),
    textInput("Qn3", "Qn3", value = "0.30649887E-8"),
    textInput("Qn4", "Qn4", value = "0.30649887E-8"),
    textInput("Qn5", "Qn5", value = "0.30649887E-8"),
    textInput("Qn6", "Qn6", value = "0.30649887E-8"),
    textInput("Qp1", "Qp1", value = "0.19438481E-9"),
    textInput("Qp2", "Qp2", value = "0.19438481E-9"),
    textInput("Qp3", "Qp3", value = "0.19438481E-9"),
    textInput("Qp4", "Qp4", value = "0.19438481E-9"),
    textInput("Qp5", "Qp5", value = "0.19438481E-9"),
    textInput("Qp6", "Qp6", value = "0.19438481E-9"),
    textInput("G1", "G1", value = "340"),
    textInput("G2", "G2", value = "34000"),
    textInput("NO3", "NO3", value = "0.9"),
    textInput("NH4", "NH4", value = "0.55"),
    textInput("PO4", "PO4", value = "0.0268"),
    textInput("DIC", "DIC", value = "2134"),
    textInput("O2", "O2", value = "172"),
    textInput("OM1_A", "OM1_A", value = "0.13"),
    textInput("OM2_A", "OM2_A", value = "2.7"),
    textInput("OM1_fp", "OM1_fp", value = "0.13"),
    textInput("OM2_fp", "OM2_fp", value = "2.7"),
    textInput("OM1_rp", "OM1_rp", value = "0"),
    textInput("OM2_rp", "OM2_rp", value = "0"),
    textInput("CDOM", "CDOM", value = "0.68"),
    textInput("Si", "Si", value = "7.34"),
    textInput("OM1_bc", "OM1_bc", value = "26"),
    textInput("OM2_bc", "OM2_bc", value = "123"),
    
    width = 3
    
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("simplot", width = "100%")
  )
))