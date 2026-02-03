# Functions for processing individual data for survival analyses
# Based on original code from Sarah SUpp and Ellen K. Bledsoe

### LIBRARIES ### ==============================================================

library(tidyverse)
library(RCurl)
library(RMark)
library(tsibble)

### DATA FUNCTIONS ### =========================================================

create_trmt_hist = function(dat, tags, prd) {
  
  MARK_data = data.frame("ch" = 1,
                         "censored" = 1,
                         "tags" = 1)
  
  outcount = 0
  
  for (t in 1:length(tags)) {
    capture_history = "" # create empty string
    
    for (p in 1:length(prd)) {
      
      tmp <- which(dat$id == tags[t] & dat$period == prd[p])
      
      if (nrow(dat[tmp, ]) == 0) {
        state = "0"
        capture_history = paste(capture_history, state, sep = "")
      } else {
        state = "1"
        capture_history = paste(capture_history, state, sep = "")

      }
    }
    censored = 1
    
    outcount = outcount + 1
    MARK_data[outcount, ] <- c(capture_history, censored, tags[t])
    print(tags[t])
    
  }
  
  return(MARK_data)
  
}

sp_trapping_history = function(data, sp, path){
  
    dat = data |> drop_na(id) |> filter(species == sp) |> 
      distinct(id,period, .keep_all = TRUE) 
    periods_all = seq(min(data$period), max(data$period))
    tags_all = unique(dat$id)
    mark_trmt_all = create_trmt_hist(dat, tags_all, periods_all)
    filename = paste(path, sp, "_captures_all.csv", sep="")
    write.csv(mark_trmt_all,filename)

  return(mark_trmt_all)
}

survival_output = function(species, start_period, end_period, path){

  capture_data = read_csv(paste(path,species,"_captures_all.csv", sep=""), 
                          col_types = cols(ch = col_character()))
  capture_data = capture_data |> rename(key = ...1, freq = censored) |> 
    select(-c(tags))
  
  # process the data 
  capture.pr <- process.data(capture_data, model = "CJS")
  
  # Setup model structures for each parameter
  Phi.dot = list(formula = ~ 1)
  # force use of an identity matrix by putting '-1' in formula
  Phi.time = list(formula = ~ -1 + time) 
  
  p.dot = list(formula = ~ 1)
  
  # constant survival and recapture rates
  Phi.dot.p.dot = mark(capture.pr, 
                       model.parameters = list(Phi = Phi.dot, p = p.dot))
  
  # time varying survival and constant recapture rates
  Phi.time.p.dot = mark(capture.pr, 
                        model.parameters = list(Phi = Phi.time, p = p.dot))
  
  time_phi = Phi.time.p.dot$results$real
  p_result = time_phi |> tail(1)
  
  AICc_dot = Phi.dot.p.dot$results$AICc
  AICc_t = Phi.time.p.dot$results$AICc
  model_aics_p = data.frame(sp = species,
                            constant_AICc = AICc_dot,
                            time_AICc = AICc_t,
                            p.dot = p_result)
  
  phi_time = time_phi |> filter(row_number() <= n()-1)
  phi_time$period = seq(start_period+1,end_period)
  mdat = read_csv("newmooncodes.csv")
  phi_time = phi_time |> left_join(mdat,phi_time, by='period')
  
  write.csv(phi_time, paste(path,species,"period_survival.csv",sep=""))
  write.csv(model_aics_p, paste(path,species, "survival_AICs.csv", sep=""))
  return(phi_time)
}



##########################
# Left over functions from Ellen's code
##########################
run.ms <- function(S_dot = list(formula = ~ 1), 
                   S_stratum = list(formula =  ~ -1 + stratum + PB_time), 
                   p_dot = list(formula =  ~ 1), 
                   p_stratum = list(formula =  ~ -1 + stratum + PB_time), 
                   Psi_s = list(formula =  ~ -1 + stratum:tostratum + PB_time, link = "logit")) {
  
  # RMark function for Portal data
  if (is.null(S_dot)) {
    S.stratum = S_stratum
  } else if (is.null(S_stratum)) {
    S.dot = S_dot
  } else {
    S.stratum = S_stratum
    S.dot = S_dot
  }
  
  if (is.null(p_dot)) {
    p.stratum = p_stratum
  } else if (is.null(p_stratum)) {
    p.dot = p_dot
  } else {
    p.stratum = p_stratum
    p.dot = p_dot
  }
  
  Psi.s = Psi_s
  
  # Create model list and run assortment of models
  ms.model.list <- create.model.list("Multistrata")
  
  ms.results <- mark.wrapper(ms.model.list,
                            data = ms.pr, ddl = ms.ddl,
                            options="SIMANNEAL")
  
  # Return model table and list of models
  return(ms.results)
  
}


