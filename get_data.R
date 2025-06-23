# Making Data for Analyses
# Based on original code from Sarah Supp and Ellen K. Bledsoe

### LIBRARIES ### ==============================================================
library(portalr)
source("functions.R")
library(ggplot2)

##################################################################
# GET DATA 
# Run if first time making data. Makes "raw_suppformat_rodents.csv".
# If file already exists, skip to #Make Capture History time slices
##########################################################

get_rawdata()

#----------------------------------------------------------------
#
# Make Capture History time slices
#
#----------------------------------------------------------------
all = read.csv("raw_suppformat_rodents.csv")
controls_all = all |> filter(plot_type == "Control", year < 2020)

# Generates capture history for every unique individual
# for a calendar year (Jan-Dec). Makes file for each species.
# makes a 0 entry if no individuals caught that year

# note survivorship calculations crash < 1979 for PP, should manage in survivor code.

DM_data = sp_trapping_history(controls_all, 'DM')
DO_data = sp_trapping_history(controls_all, 'DO')
DS_data = sp_trapping_history(controls_all,'DS')
PB_data = sp_trapping_history(controls_all, 'PB')
PP_data = sp_trapping_history(controls_all, 'PP')


DM_survival = survival_output(species="DM")
DO_survival = survival_output(species="DO")
DS_survival = survival_output(species="DS")
PB_survival = survival_output(species="PB")
PP_survival = survival_output(species="PP")

all_species = bind_rows(DM_survival, DO_survival, DS_survival, PB_survival, PP_survival)
write.csv(all_species, "species_survival_end2015.csv")

ggplot(all_species, aes(x=year, y=survival)) +
  geom_line(aes(color = species)) +
  geom_vline(xintercept = 1984) +
  geom_vline(xintercept = 1999) +
  geom_vline(xintercept = 2010)
  

