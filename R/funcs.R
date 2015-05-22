# change the input conditions based on chosen scenario
# sc_in is chr string for name of input scenario, 'base', 'low', 'high'
create_input <- function(sc_in){

  init <- c(
    A1 = "88562256", 
    A2 = "88562256",
    A3 = "88562256", 
    A4 = "88562256", 
    A5 = "88562256", 
    A6 = "88562256",
    Qn1 = "0.30649887E-8",
    Qn2 = "0.30649887E-8",
    Qn3 = "0.30649887E-8",
    Qn4 = "0.30649887E-8",
    Qn5 = "0.30649887E-8",
    Qn6 = "0.30649887E-8",
    Qp1 = "0.19438481E-9",
    Qp2 = "0.19438481E-9",
    Qp3 = "0.19438481E-9",
    Qp4 = "0.19438481E-9",
    Qp5 = "0.19438481E-9",
    Qp6 = "0.19438481E-9",
    G1 = "340",
    G2 = "34000",
    NO3 = "0.9",
    NH4 = "0.55",
    PO4 = "0.0268",
    DIC = "2134",
    O2 = "172",
    OM1_A = "0.13",
    OM2_A = "2.7",
    OM1_fp = "0.13",
    OM2_fp = "2.7",
    OM1_rp = "0",
    OM2_rp = "0",
    CDOM = "0.68",
    Si = "7.34",
    OM1_bc = "26",
    OM2_bc = "123"
  )
  
  ##
  # scenarios 
  
  # baseline
  base <- c(
    NO3 = "0.9",
    NH4 = "0.55",
    PO4 = "0.0268"
  )
  
  # low nutrients
  low <- c(
    NO3 = "0",
    NH4 = "0",
    PO4 = "0"
  )
    
  # high nutrients
  high <- c(
    NO3 = "30",
    NH4 = "30",
    PO4 = "30"
  )
  
  # replace values in init w/ chosen scenario
  sc_in <- get(sc_in)
  init[names(sc_in)] <- sc_in
  
  return(init)
  
}

# a caller for state variable labels
labels_fun <- function(...){
  
  # shrt names, all other vectors will be ordered using shrt
  shrt <- c('A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'Qn1', 'Qn2', 'Qn3', 
    'Qn4', 'Qn5', 'Qn6', 'Qp1', 'Qp2', 'Qp3', 'Qp4', 'Qp5', 'Qp6', 
    'G1', 'G2', 'NO3', 'NH4', 'PO4', 'DIC', 'O2', 'OM1_A', 'OM2_A', 
    'OM1_fp', 'OM2_fp', 'OM1_rp', 'OM2_rp', 'CDOM', 'Si', 'OM1_bc', 
    'OM2_bc')

  # long names
  lngs <- c(
    A1 = 'phytoplankton abundance 1',
    A2 = 'phytoplankton abundance 2',
    A3 = 'phytoplankton abundance 3',
    A4 = 'phytoplankton abundance 4',
    A5 = 'phytoplankton abundance 5',
    A6 = 'phytoplankton abundance 6',
    Qn1 = 'cell nitrogen quota 1',
    Qn2 = 'cell nitrogen quota 2',
    Qn3 = 'cell nitrogen quota 3',
    Qn4 = 'cell nitrogen quota 4',
    Qn5 = 'cell nitrogen quota 5',
    Qn6 = 'cell nitrogen quota 6',
    Qp1 = 'cell phosphorus quota 1',
    Qp2 = 'cell phosphorus quota 2',
    Qp3 = 'cell phosphorus quota 3',
    Qp4 = 'cell phosphorus quota 4',
    Qp5 = 'cell phosphorus quota 5',
    Qp6 = 'cell phosphorus quota 6',
    G1 = 'zooplankton 1',
    G2 = 'zooplankton 2',
    OM1_A = 'particulate organic matter derived from phytoplankton',
    OM1_fp = 'particulate organic matter derived from zooplankton fecal pellets',
    OM1_rp = 'particulate organic matter from riverine particulates',
    OM1_bc = 'particulate organic matter from coastal ocean boundary condition',
    OM2_A = 'dissolved organic matter derived from phytoplankton',
    OM2_fp = 'dissolved organic matter derived from zooplankton',
    OM2_rp = 'dissolved organic matter from the rivers',
    OM2_bc = 'dissolved organic matter from coastal ocean boundary condition',
    NO3 = 'nitrate',
    NH4 = 'ammonium',
    PO4 = 'phosphate',
    CDOM = 'colored dissolved organic matter',
    Si = 'silica',
    O2 = 'oxygen',
    DIC = 'dissolved inorganic carbon'
  )
  lngs <- lngs[shrt]
  
  # unit values
  vals <- c(
    A1 = 	'cells m-3',
    A2 = 	'cells m-3',
    A3 = 	'cells m-3',
    A4 = 	'cells m-3',
    A5 = 	'cells m-3',
    A6 = 	'cells m-3',
    Qn1 = 	'mmol  N cell-1',
    Qn2 = 	'mmol  N cell-1',
    Qn3 = 	'mmol  N cell-1',
    Qn4 = 	'mmol  N cell-1',
    Qn5 = 	'mmol  N cell-1',
    Qn6 = 	'mmol  N cell-1',
    Qp1 = 	'mmol  P cell-1',
    Qp2 = 	'mmol  P cell-1',
    Qp3 = 	'mmol  P cell-1',
    Qp4 = 	'mmol  P cell-1',
    Qp5 = 	'mmol  P cell-1',
    Qp6 = 	'mmol  P cell-1',
    G1 = 	'individuals  m-3',
    G2 = 	'individuals  m-3',
    OM1_A = 	'mmol m-3',
    OM1_fp = 	'mmol m-3',
    OM1_rp = 	'mmol m-3',
    OM1_bc = 	'mmol m-3',
    OM2_A = 	'mmol m-3',
    OM2_fp = 	'mmol m-3',
    OM2_rp = 	'mmol m-3',
    OM2_bc = 	'mmol m-3',
    NO3 = 	'mmol m-3',
    NH4 = 	'mmol m-3',
    PO4 = 	'mmol m-3',
    CDOM = 	'ppb',
    Si = 	'mmol m-3',
    O2 = 	'mmol m-3',
    DIC = 	'mmol m-3'
  )
  vals <- vals[shrt]    
  
  # remove names
  names(lngs) <- NULL
  names(vals) <- NULL
    
  out <- list(shrt = shrt, lngs = lngs, vals = vals)
  return(out)

}

# formats the input list and saves as text file for model input
format_input <- function(ls_in){
  
  # make a data frame
  out <- data.frame(ls_in)
  out$var <- paste0('!', row.names(out))
  
  # save to outfl
  write.table(out, 'InitialConditions.txt', quote = FALSE, 
    row.names = FALSE, col.names = FALSE, 
    sep = '\t\t\t\t')
  
}

# move output and files and initial conditions file
# input_in is what input was chosen, a character string
mv_files <- function(input_in){
  
  # get the files names of results from model
  fls <- list.files('R_PLOTS/', pattern = '.txt', full.names = TRUE)
  
  # scenario 1 files
  if(input_in == 'scenario1'){
  
    file.copy(c('InitialConditions.txt', fls), 'R_PLOTS/scenario1/', overwrite = TRUE)
    
  } 
  
  # scenario 2 files
  if(input_in == 'scenario2'){
    
    file.copy(c('InitialConditions.txt', fls), 'R_PLOTS/scenario2/', overwrite = TRUE)
    
  }
  
  # remove the copied files
  file.remove(c('InitialConditions.txt', fls))
  
} 
  
# run the model, using output from create format
# sc_in is the name of the scenario
run_mod <- function(sc_in, input_in){
  
  states <- labels_fun()$shrt
  # get the input based on scenario, save the file
  input <- create_input(sc_in)
  sv_input <- format_input(input)
  
  # run model
  torun <- paste0(getwd(), '/EPACOM_GEM.exe')
  system(torun)
  
  # move and cleanup
  mv_files(input_in)
  
}
  
# format data for plotting, both scenarios
# sc_in1 and sc_in2 is to relabel the model outputs for the legend 
data_format <- function(sc_in1, sc_in2, ...){
  
  # state variables names and location of model results for each scenario
  states <- labels_fun()$shrt
  dirs <- dir('R_PLOTS')
  
  # empty vector for results
  out_all <- vector('list', length = length(dirs))
  names(out_all) <- dirs
  
  # import results for each directory
  for(dir in dirs){

    # import the data
    fls <- vector("list", length = length(states))
    names(fls) <- states
    for(fl in states) 
      fls[[fl]] <- read.table(paste0('R_PLOTS/', dir, '/', fl, '.txt'))
    
    # format data for plotting
    out <- do.call('rbind', fls)
    out$state <- gsub('\\.[0-9]*$', '', row.names(out))
    row.names(out) <- 1:nrow(out)
    names(out) <- c('step', 'val', 'state')

    out_all[[dir]] <- out
    
  }

  # combine results from each scenario
  out_all <- melt(out_all, id.vars = names(out_all[[1]]))
  names(out_all)[names(out_all) %in% 'L1'] <- 'scenario'
  out_all$scenario <- factor(out_all$scenario, labels = c(sc_in1, sc_in2))
  out_all$step <- out_all$step/288
  out_all$state <- factor(out_all$state, levels = states)

  return(out_all)
  
}

# the plotting function
# varsel is input name, all dat is input data
plo_fun <- function(varsel, alldat){

  varsel <- labels_fun()$shrt[labels_fun()$lngs %in% varsel]
  toplo <- filter(alldat, alldat$state %in% varsel) 
  
  ylab <- expr_fun(varsel)
  
  # make xts
  toplo <- select(toplo, -state)
  toplo <- spread(toplo, scenario, val)
  toplo <- as.matrix(toplo[, -1])
  toplo <- as.xts(toplo, order.by = as.Date(1:nrow(toplo), origin = '2000-01-01'))

  dygraph(toplo, ylab = ylab, group = 'group') %>% 
      dyRangeSelector
    

}

# formatting the labels from labels_fun as expressions for plots
expr_fun <- function(lab_in){
 
  sel <- which(labels_fun()$shrt == lab_in)
  val <- labels_fun()$vals[sel]
  
#   if(grepl('-', val)){
#     val <- strsplit(val, '-')[[1]]
#     val <- bquote(.(val[1]) ^ .(paste0('-', val[2])))
#   }
    
  return(val)
   
}
    