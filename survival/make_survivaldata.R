# Making Data for Analyses
# Based on original code from Sarah Supp and Ellen K. Bledsoe

### LIBRARIES ### ==============================================================
source("./survival/survival_functions.R")


#----------------------------------------------------------------
#
# Calculate Species Survival Data
#
#----------------------------------------------------------------

# SET path for saving files

path = "./survival/"

# SET species list for processing

dominant_sp = c("DM", "DO", "DS", "PB", "PP")

## FORMAT raw data for survival processing

# SKME: to improve this, need to convert to newmooncodes and use the survival calc that
# takes into account differences in time between captures for missed periods. Would
# possibly allow 2021 to get added back in and would correct survival estimates for
# time periods where gaps are more prevalent

all = read.csv("raw_suppformat_rodents.csv")
controls_all = all |> filter(plot_type == "Control", year > 1977 & year < 2020, species %in% dominant_sp)
start_period = min(controls_all$period)
end_period = max(controls_all$period)

## GENERATE capture history for every unique individual
# Makes file: paste(sp,"_captures_all.csv")
# generates capture history for each unique ID for every month over the 1977-2020 time span


# note survivorship calculations crash < 1979 for PP, should manage in 
# survivor code. --check to see if this is still true SKME 2/2/2026

DM_data = sp_trapping_history(controls_all, 'DM')
DO_data = sp_trapping_history(controls_all, 'DO')
DS_data = sp_trapping_history(controls_all,'DS')
PB_data = sp_trapping_history(controls_all, 'PB')
PP_data = sp_trapping_history(controls_all, 'PP')

# CALCULATE time-varying survival estimate and AIC for static vs. time-varying models
# writes to files: paste( species,"period_survival.csv")) &
#                  paste(species, "survival_AICs.csv"

## todo: need to update survival function with code in Rmark_survival_test.csv
## todo: reformat survival_functions to read from survival folder?
DM_survival = survival_output(species="DM", start_period, end_period)
DO_survival = survival_output(species="DO", start_period, end_period)
DS_survival = survival_output(species="DS", start_period, end_period)
PB_survival = survival_output(species="PB", start_period, end_period)
PP_survival = survival_output(species="PP", start_period, end_period)

# format survival time-series
allspecies = data.frame()

for(spcode in dominant_sp){
data = read_csv(paste(path,spcode,"period_survival.csv", sep=""))
data = data |> mutate(species = rep(spcode, n()))
allspecies = rbind(allspecies,data)
}
allspecies = allspecies |> select(-c(...1,...9,fixed,note,period))
write.csv(allspecies, paste(path,"species_survival.csv",sep=""), row.names=FALSE)




