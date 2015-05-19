source('R/funcs.R')

library(reshape)
library(ggplot2)

# Define server logic required to generate and plot data
shinyServer(function(input, output) {
  
  
  runmod <- reactive({
    
    # get values
    states <- c('A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'Qn1', 'Qn2', 'Qn3', 
      'Qn4', 'Qn5', 'Qn6', 'Qp1', 'Qp2', 'Qp3', 'Qp4', 'Qp5', 'Qp6', 
      'G1', 'G2', 'NO3', 'NH4', 'PO4', 'DIC', 'O2', 'OM1_A', 'OM2_A', 
      'OM1_fp', 'OM2_fp', 'OM1_rp', 'OM2_rp', 'CDOM', 'Si', 'OM1_bc', 
      'OM2_bc')

    tmp <- sapply(states, function(x) input[[x]])
    
    # save in a format for the input file
    format_input(tmp)
    
    # run model
    torun <- paste0(getwd(), '/EPACOM_GEM.exe')
    system(torun)

    # import the data
    fls <- vector("list", length = length(states))
    names(fls) <- states
    for(fl in states) fls[[fl]] <- read.table(paste0('R_PLOTS/', fl, '.txt'))
    
    # format data for plotting
    out <- do.call('rbind', fls)
    out$state <- gsub('\\.[0-9]*$', '', row.names(out))
    row.names(out) <- 1:nrow(out)
    names(out) <- c('step', 'val', 'state')
    out$step <- out$step/288
    out$state <- factor(out$state, levels = states)
    
    return(out)
    
  })
    
  output$simplot <- renderPlot({
     
    toplo <- runmod()
    
    p <- ggplot(toplo, aes(x = step, y = val, group = state)) + 
      geom_line() +
      facet_wrap(~ state, scales = 'free_y', ncol = 6) +
      theme_bw() + 
      theme(axis.title = element_blank())
    
    return(p)
    
    }, height = 600, width = 900)

})