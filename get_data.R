# Making Data for Analyses
# Based on original code from Sarah Supp and Ellen K. Bledsoe

### LIBRARIES ### ==============================================================
source("functions.R")

##################################################################
# GET DATA 
# Run if first time making data. Makes "raw_suppformat_rodents.csv".
# If file already exists, skip to #Make Capture History time slices
##########################################################

get_rawdata()

#----------------------------------------------------------------
#
# Calculate Species Survival Data
#
#----------------------------------------------------------------

# SKME: to improve this, need to convert to newmooncodes and use the survival calc that
# takes into account differences in time between captures for missed periods. Would
# possibly allow 2021 to get added back in and would correct survival estimates for
# time periods where gaps are more prevalent

all = read.csv("raw_suppformat_rodents.csv")
dominant_sp = c("DM", "DO", "DS", "PB", "PP")
controls_all = all |> filter(plot_type == "Control", year > 1977 & year < 2020, species %in% dominant_sp)
start_period = min(controls_all$period)
end_period = max(controls_all$period)

# Generates capture history for every unique individual
# for a calendar year (Jan-Dec). Makes file for each species.
# makes a 0 entry if no individuals caught that year

# note survivorship calculations crash < 1979 for PP, should manage in survivor code.

DM_data = sp_trapping_history(controls_all, 'DM')
DO_data = sp_trapping_history(controls_all, 'DO')
DS_data = sp_trapping_history(controls_all,'DS')
PB_data = sp_trapping_history(controls_all, 'PB')
PP_data = sp_trapping_history(controls_all, 'PP')

# generates time-varying survival estimate and AIC for static vs. time-varying models
# writes to file

## need to update survival function with code in Rmark_survival_test.csv
DM_survival = survival_output(species="DM", start_period, end_period)
DO_survival = survival_output(species="DO", start_period, end_period)
DS_survival = survival_output(species="DS", start_period, end_period)
PB_survival = survival_output(species="PB", start_period, end_period)
PP_survival = survival_output(species="PP", start_period, end_period)

# format survival time-series
allspecies = data.frame()
for(spcode in dominant_sp){
data = read_csv(paste(spcode,"period_survival.csv", sep=""))
data = data |> mutate(species = rep(spcode, n()))
allspecies = rbind(allspecies,data)
}
allspecies = allspecies |> select(-c(...1,...9,fixed,note,censusdate,period))
write.csv(allspecies, "species_survival.csv", row.names=FALSE)

#----------------------------------------------------------------
#
# Calculate New Individuals
#
#----------------------------------------------------------------

# ToDO: 
# 1) we have years with all counts but no new counts. Need to check 
#     that there really weren't new individuals (i.e. all recaps)
# 2) early years do not have *. Might need to create a new function 
#     that IDs 'newcaps' from the first appearance of a tag (with time 
#     buffer from last seen). This is probably already (or partially)
#     in the Supp code for cleaning tags. Using this would let us push 
#     back caps to 1977.
#

annual_new = get_newcounts(controls_all)


