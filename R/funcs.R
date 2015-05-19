# formats the input list and saves as text file for model input
format_input <- function(ls_in, outfl = 'InitialConditions.txt'){
  
  # make a data frame
  out <- data.frame(ls_in)
  out$var <- paste0('!', row.names(out))
  
  # save to outfl
  write.table(out, outfl, quote = FALSE, 
    row.names = FALSE, col.names = FALSE, 
    sep = '\t\t\t\t')
  
}