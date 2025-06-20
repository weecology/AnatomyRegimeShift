library(marked)
library(tidyverse)


DM_survival = survival_output(species="DM")
DO_survival = survival_output(species="DO")
DS_survival = survival_output(species="DS")
PB_survival = survival_output(species="PB")
PP_survival = survival_output(species="PP")

all_species = bind_rows(DM_survival, DO_survival, DS_survival, PB_survival, PP_survival)
write.csv(all_species, "species_survival_end2015.csv")
  
survival_output = function(species){
  
  capture_data = read_csv(paste(species,"_captures_annual.csv", sep=""), 
                          col_types = cols(ch = col_character()))
  years = unique(capture_data$year)
  survival_ts = data.frame(year=numeric(),
                         species = character(),
                         survival = numeric(),
                         recap = numeric())
  for (y in 1:length(years)) {

    print(paste("PROCESSING...", years[y]))
    capture_history = capture_data |> filter(year == years[y])
    tryCatch(
      {cjs.m1 <- crm(capture_history)
      Phi = exp(cjs.m1$results$beta$Phi)/(1+exp(cjs.m1$results$beta$Phi)) # real Phi (survival) estimate by hand
      p = exp(cjs.m1$results$beta$p)/(1+exp(cjs.m1$results$beta$p)) # real p (capture probability) estimate by hand
      newrow = list(years[y], species, Phi, p)},
      error = function(e) {newrow = list(years[y], species, 0, 0) })
    survival_ts[nrow(survival_ts) + 1,] = newrow
  }
  write.csv(survival_ts, paste(species,"_survival.csv", sep=""))
  return(survival_ts)
}



