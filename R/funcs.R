# a caller for state variable labels
labels_fun <- function(...){
  
  # shrt names, all other vectors will be ordered using shrt
  shrt <- c('Chla','A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'Qn1', 'Qn2', 'Qn3', 
    'Qn4', 'Qn5', 'Qn6', 'Qp1', 'Qp2', 'Qp3', 'Qp4', 'Qp5', 'Qp6', 
    'G1', 'G2', 'IOPpar', 'NO3', 'NH4', 'PO4', 'DIC', 'O2', 'OM1_A', 'OM2_A', 
    'OM1_fp', 'OM2_fp', 'OM1_rp', 'OM2_rp', 'CDOM', 'Si', 'OM1_bc', 
    'OM2_bc','uN1','uN2','uN3','uN4','uN5','uN6','uP1','uP2','uP3','uP4','uP5','uP6',
    'uE1','uE2','uE3','uE4','uE5','uE6','uA1','uA2','uA3','uA4','uA5','uA6','x1A',
    'x2A','y1A','y2A','x1fp','x2fp','y1fp','y2fp')

  # long names
  lngs <- c(
    Chla = 'Chlorophyll a',
    A1 = 'Phytoplankton Abundance 1',
    A2 = 'Phytoplankton Abundance 2',
    A3 = 'Phytoplankton Abundance 3',
    A4 = 'Phytoplankton Abundance 4',
    A5 = 'Phytoplankton Abundance 5',
    A6 = 'Phytoplankton Abundance 6',
    Qn1 = 'Cell Nitrogen Quota 1',
    Qn2 = 'Cell Nitrogen Quota 2',
    Qn3 = 'Cell Nitrogen Quota 3',
    Qn4 = 'Cell Nitrogen Quota 4',
    Qn5 = 'Cell Nitrogen Quota 5',
    Qn6 = 'Cell Nitrogen Quota 6',
    Qp1 = 'Cell Phosphorus Quota 1',
    Qp2 = 'Cell Phosphorus Quota 2',
    Qp3 = 'Cell Phosphorus Quota 3',
    Qp4 = 'Cell Phosphorus Quota 4',
    Qp5 = 'Cell Phosphorus Quota 5',
    Qp6 = 'Cell Phosphorus Quota 6',
    G1 = 'Zooplankton Abundance 1',
    G2 = 'Zooplankton Abundance 2',
    IOPpar = 'Photosynthetically Available Radiation', 
    OM1_A = 'Particulate Organic Matter from Phytoplankton',
    OM1_fp = 'Particulate Organic Matter from Zooplankton Fecal Pellets',
    OM1_rp = 'Particulate Organic Matter from River',
    OM1_bc = 'Particulate organic matter from Boundary',
    OM2_A = 'Dissolved Organic Matter from Phytoplankton',
    OM2_fp = 'Dissolved Organic Matter from Zooplankton',
    OM2_rp = 'Dissolved Organic Matter from River',
    OM2_bc = 'Dissolved Organic Matter from Boundary',
    NO3 = 'Nitrate',
    NH4 = 'Ammonium',
    PO4 = 'Phosphate',
    CDOM = 'Colored Dissolved Organic Matter',
    Si = 'Dissolved Silica',
    O2 = 'Dissolved Oxygen',
    DIC = 'Dissolved Inorganic Carbon',
    uN1 = 'Specific Growth Rate Due to N 1',
    uN2 = 'Specific Growth Rate Due to N 2',
    uN3 = 'Specific Growth Rate Due to N 3',
    uN4 = 'Specific Growth Rate Due to N 4',
    uN5 = 'Specific Growth Rate Due to N 5',
    uN6 = 'Specific Growth Rate Due to N 6',
    uP1 = 'Specific Growth Rate Due to P 1',
    uP2 = 'Specific Growth Rate Due to P 2',
    uP3 = 'Specific Growth Rate Due to P 3',
    uP4 = 'Specific Growth Rate Due to P 4',
    uP5 = 'Specific Growth Rate Due to P 5',
    uP6 = 'Specific Growth Rate Due to P 6',
    uE1 = 'Specific Growth Rate Due to PAR 1',
    uE2 = 'Specific Growth Rate Due to PAR 2',
    uE3 = 'Specific Growth Rate Due to PAR 3',
    uE4 = 'Specific Growth Rate Due to PAR 4',
    uE5 = 'Specific Growth Rate Due to PAR 5', 
    uE6 = 'Specific Growth Rate Due to PAR 6',
    uA1 = 'Specific Growth Rate 1',
    uA2 = 'Specific Growth Rate 2',
    uA3 = 'Specific Growth Rate 3',
    uA4 = 'Specific Growth Rate 4',
    uA5 = 'Specific Growth Rate 5',
    uA6 = 'Specific Growth Rate 6',
    x1A = 'C:P Stoichiometry of OM1A',
    x2A = 'C:P Stoichiometry of OM2A',
    y1A = 'N:P Stoichiometry to OM1A',
    y2A = 'N:P Stoichiometry to OM2A',
    x1fp = 'C:P of from Fecal Pellets',
    x2fp = 'C:P for DOM from Zooplankton',
    y1fp = 'N:P of from Fecal Pellets',
    y2fp = 'N:P for DOM from Zooplankton'
  )
  lngs <- lngs[shrt]
  
  # unit values
  vals <- c(
    Chla = 'mg m-3',
    A1 = 	'log cells m-3',
    A2 = 	'log cells m-3',
    A3 = 	'log cells m-3',
    A4 = 	'log cells m-3',
    A5 = 	'log cells m-3',
    A6 = 	'log cells m-3',
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
    DIC = 	'mmol m-3',
    uN1 = 'd-1',
    uN2 = 'd-1',
    uN3 = 'd-1',
    uN4 = 'd-1',
    uN5 = 'd-1',
    uN6 = 'd-1',
    uP1 = 'd-1',
    uP2 = 'd-1',
    uP3 = 'd-1',
    uP4 = 'd-1',
    uP5 = 'd-1',
    uP6 = 'd-1',
    uE1 = 'd-1',
    uE2 = 'd-1',
    uE3 = 'd-1',
    uE4 = 'd-1',
    uE5 = 'd-1',
    uE6 = 'd-1',
    uA1 = 'd-1',
    uA2 = 'd-1',
    uA3 = 'd-1',
    uA4 = 'd-1',
    uA5 = 'd-1',
    uA6 = 'd-1',
    x1A = 'unitless',
    x2A = 'unitless',
    y1A = 'unitless',
    y2A = 'unitless',
    x1fp = 'unitless',
    x2fp = 'unitless',
    y1fp = 'unitless',
    y2fp = 'unitless'
  )
  vals <- vals[shrt]
  
  # alphabetize by long name
  ords <- order(lngs)
  shrt <- shrt[ords]
  lngs <- lngs[ords]
  vals <- vals[ords]
  
  # remove names
  names(lngs) <- NULL
  names(vals) <- NULL
  
  out <- list(shrt = shrt, lngs = lngs, vals = vals)
  return(out)

}

# import an initial conditions file for state variables, format as data frame
# sc_in is name of folder in scenarios folder 
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
  system('runmod.bat')
  
  # move output and cleanup
  mvout_fls(input_in)
  
}
 
# function to get model step and output step from parm file
get_steps <- function(sc_in){

  # get parm file, find dT_out
  parms <- read_parms(sc_in)  
  parms <- parms[grep('dT_out', parms)]
  
  # timestep
  dT <- strsplit(parms, ' ')[[1]][1]
  dT <- as.numeric(gsub('^<br>', '', dT))
  
  # output step
  dT_out <- strsplit(parms, ' ')[[1]][2]
  dT_out <- as.numeric(gsub('\t\t!-', '', dT_out))
  
  return(c(dT, dT_out))
  
}

# format data for plotting, both scenarios, used with results on the fly
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
      fls[[fl]] <- read.table(paste0('R_PLOTS/', dir, '/', fl, '.txt'))[, c(1, 2)]
    
    # format data for plotting
    out <- do.call('rbind', fls)
    out$state <- gsub('\\.[0-9]*$', '', row.names(out))
    row.names(out) <- 1:nrow(out)
    names(out) <- c('step', 'val', 'state')

    # get timestep info for the scenario
    steps <- get_steps(get(paste0('sc_in', gsub('scenario', '', dir))))
    out$step <- out$step * steps[1]
      
    out_all[[dir]] <- out
    
  }

  # combine results from each scenario
  out_all <- melt(out_all, id.vars = names(out_all[[1]]))
  names(out_all)[names(out_all) %in% 'L1'] <- 'scenario'
  out_all$scenario <- factor(out_all$scenario, labels = c(sc_in1, sc_in2))
  out_all$state <- factor(out_all$state, levels = states)

  return(out_all)
  
}

# format data for plotting, for pre-allocated output
# sc_in1 and sc_in2 is to relabel the model outputs for the legend 
data_format2 <- function(sc_in1, sc_in2, ...){

  # state variables names and location of model results for each scenario
  states <- labels_fun()$shrt
  dirs <- dir('scenarios_proc')
  dirs <- dirs[dirs %in% c(sc_in1, sc_in2)]
  
  # empty vector for results
  out_all <- vector('list', length = length(dirs))
  names(out_all) <- dirs
  
  # import results for each directory
  for(dir in dirs){
    
    # import the data
    fls <- vector("list", length = length(states))
    names(fls) <- states
    for(fl in states) 
      fls[[fl]] <- read.table(paste0('scenarios_proc/', dir, '/', fl, '.txt'))[, c(1, 2)]
    
    # format data for plotting
    out <- do.call('rbind', fls)
    out$state <- gsub('\\.[0-9]*$', '', row.names(out))
    row.names(out) <- 1:nrow(out)
    names(out) <- c('step', 'val', 'state')

    # get timestep info for the scenario
    steps <- get_steps(dir)
    out$step <- out$step * steps[1]
      
    out_all[[dir]] <- out
    
  }

  # combine results from each scenario
  out_all <- melt(out_all, id.vars = names(out_all[[1]]))
  names(out_all)[names(out_all) %in% 'L1'] <- 'scenario'
  out_all$scenario <- factor(out_all$scenario, labels = c(sc_in1, sc_in2))
  out_all$state <- factor(out_all$state, levels = states)

  return(out_all)
  
}

# divide or multiply output for better axis format
# toplo and varsel are inputs, used in plo_fun
form_axis <- function(toplo, varsel){
  
  # phyto
  if(grepl('^A[0-9]', varsel)){
    toplo$val <- log10(toplo$val)
  }
 
#   # cell nitrogen quota
#   if(grepl('Qn[0-9]', varsel)){
#     toplo$val <- toplo$val * 1e9
#   }
#   
#   # cell phosphorus quota
#   if(grepl('Qp[0-9]', varsel)){
#     toplo$val <- toplo$val * 1e10
#   }
#     
#   # specific growth rate
#   if(grepl('uA[0-9]', varsel)){
#     toplo$val <- toplo$val * 1e9
#   }
#   
#   # zoops
#   if(grepl('G[0-9]', varsel)){
#     toplo$val <- toplo$val / 1e3
#   }
  
  return(toplo)
  
}
  

# the plotting function
# varsel is input name, all dat is input data
plo_fun <- function(varsel, alldat){

  varsel <- labels_fun()$shrt[labels_fun()$lngs %in% varsel]
  toplo <- filter(alldat, alldat$state %in% varsel) 

  # reformat units
  toplo <- form_axis(toplo, varsel)
  
  # change label format (doesn't work for dygraphs)
  ylab <- expr_fun(varsel)
  
  # make xts
  toplo <- select(toplo, -state)
  toplo <- spread(toplo, scenario, val)
  step <- as.POSIXct(toplo$step, origin = '2005-01-02') 
  toplo <- as.matrix(toplo[, -1])
  toplo <- as.xts(toplo, order.by = step)

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

# read description files and prep for output tab
# sc_in is the name of the scenario
read_descrips <- function(sc_in){
  
  out <- readLines(paste0('scenarios/', sc_in, '/description.txt'), warn = FALSE)
  out <- paste0('<br>', out, '</br>')
  out <- c(paste0('<h4>', sc_in, '</h4>'), out)
  
  return(out)
  
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

# function to read parameter input file
# sc_in is name of input scenario
read_parms <- function(sc_in){
  
  out <- readLines(paste0('scenarios/', sc_in, '/GEM_InputFile'), warn = FALSE)
  out <- paste0('<br>', out, '</br>')
  out <- c(paste0('<h4>', sc_in, '</h4>'), out)
  
  return(out)
  
}
