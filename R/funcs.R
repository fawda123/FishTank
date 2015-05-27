# a caller for state variable labels
labels_fun <- function(...){
  
  # shrt names, all other vectors will be ordered using shrt
  shrt <- c('Chla','A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'Qn1', 'Qn2', 'Qn3', 
    'Qn4', 'Qn5', 'Qn6', 'Qp1', 'Qp2', 'Qp3', 'Qp4', 'Qp5', 'Qp6', 
    'G1', 'G2', 'IOPpar', 'NO3', 'NH4', 'PO4', 'DIC', 'O2', 'OM1_A', 'OM2_A', 
    'OM1_fp', 'OM2_fp', 'OM1_rp', 'OM2_rp', 'CDOM', 'Si', 'OM1_bc', 
    'OM2_bc')

  # long names
  lngs <- c(
    Chla = 'chlorophyll a',
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
    IOPpar = 'photosynthetically available radiation', 
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
    Chla = 'mg m-3',
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
    IOPpar = 'percent',
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

# import an initial conditions file for state variables, format as data frame
# sc_in is name of folder in scenarios folder, 
import_init <- function(sc_in){
  
  dat <- read.table(paste0('scenarios/', sc_in, '/InitialConditions.txt'))
  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  dat[, 2] <- gsub('^!', '', dat[, 2])
  names(dat) <- c('strt', 'shrt')
  
  return(dat)
  
}

# move the input files from the scenario folders to root to run model
# sc_in is text from input indicating the name of the scenairo
mvinp_fls <- function(sc_in){
  
  # get the files names of results from model
  path <- paste0('scenarios/', sc_in, '/')
  fls <- list.files(path, full.names = TRUE)
  file.copy(fls, getwd(), overwrite = TRUE)
  
}

# move output and files and initial conditions file
# input_in is what input was chosen, a character string
mvout_fls <- function(input_in){
  
  # get the files names of results from model
  fls <- list.files('R_PLOTS/', pattern = '.txt', full.names = TRUE)
  fls <- c('InitialConditions.txt', 'GEM_InputFile', fls)
  
  # path to move to
  pathmv <- 'R_PLOTS/scenario1'
  if(input_in == 'scenario2') pathmv <- 'R_PLOTS/scenario2'
    
  # copy, then remove the files to the path
  file.copy(fls, pathmv, overwrite = TRUE)
  file.remove(fls)
  
} 
  
# run the model, using output from create format
# sc_in is the name of the scenario
run_mod <- function(sc_in, input_in){
  
  # move the input files
  mvinp_fls(sc_in)
  
  # run model
  system('EPACOM_GEM.exe')
  
  # move output and cleanup
  mvout_fls(input_in)
  
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

# function to format input conditions for each scenario for table
tab_form <- function(sc1, sc2){

  ls1 <- import_init(sc1)
  ls2 <- import_init(sc2)
   
  out <- data.frame(Variable = labels_fun()$lng, Value = labels_fun()$val, shrt = labels_fun()$shrt)
  out <- merge(out, ls1, by = 'shrt', all.x = TRUE)
  out <- merge(out, ls2, by = 'shrt', all.x = TRUE)
  out <- out[, -1]
  names(out)[c(3, 4)] <- c(sc1, sc2)

  return(out)
  
}    