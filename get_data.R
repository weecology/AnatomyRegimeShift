# Making Data for Analyses
# Based on original code from Sarah Supp and Ellen K. Bledsoe

### LIBRARIES ### ==============================================================
library(portalr)
library(RCurl)
source("functions.R")



##################################################################
# GET DATA 
# Run if first time making data. Makes "raw_suppformat_rodents.csv".
# If file already exists, skip to #Make Capture History time slices
##########################################################

get_rawdata(max_year=2015, min_period = 6)

#----------------------------------------------------------------
#
# Make Capture History time slices
#
#----------------------------------------------------------------
all = read.csv("raw_suppformat_rodents.csv")
controls_all = all |> filter(plot_type == "Control")

# Generates capture history for every unique individual
# for a calendar year (Jan-Dec). Makes one species files
# note survivorship calculations crash < 1979 for PP, should manage in survivor code.

# sp_trapping_history crashes when no individuals in a year
# currently managing by filtering input data here, but should
# modify sp_trapping_history to change output if no individuals


DM_data = sp_trapping_history(all, 'DM')

DO_filtered = all |> filter(year < 2016)
DO_data = sp_trapping_history(DO_filtered, 'DO')

DS_filtered = all |> filter(year < 1995)
DS_data = sp_trapping_history(DS_filtered,'DS')

PB_filtered = all |> filter(year > 1994)
PB_data = sp_trapping_history(PB_filtered, 'PB')

PP_filtered = all |> filter(year > 1980)
PP_data = sp_trapping_history(PP_filtered, 'PP')
