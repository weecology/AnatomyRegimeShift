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
# note survivorship calculations crash < 1979 for PP, > 1994 for DS, and
# <1994 for PB. Need to work previous data filters into survivorship code

PP_data = sp_trapping_history(all, 'PP')
DM_data = sp_trapping_history(all, 'DM')
DO_data = sp_trapping_history(all, 'DO')
DS_data = sp_trapping_history(all,'DS')
PB_data = sp_trapping_history(all, 'PB')

