# Making Data for Analyses
# Based on original code from Sarah Supp and Ellen K. Bledsoe

### LIBRARIES ### ==============================================================

library(tidyverse)
library(portalr)
library(RCurl)
source("functions.R")

##########################################################
# DATA PREP
##########################################################

#---------------------------------------------------------
# Clean the Data
#---------------------------------------------------------

# rodent file from repo
rodents <- getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent.csv")
rdat <- read.csv(text = rodents, header = TRUE, na.strings = c(""), stringsAsFactors = FALSE)

# species file
species <- getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent_species.csv")
sdat <- read.csv(text = species, header = TRUE, na.strings = c(""), stringsAsFactors = FALSE)

# trapping file from repo
trapping <- getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent_trapping.csv")
tdat <- read.csv(text = trapping, header = TRUE, stringsAsFactors = FALSE)

# make it match Sarah Supp's data structure to use her code
all <- repo_data_to_Supp_data(rdat, sdat) %>% 
  filter(year <= 2015 & period > 4)

# make table of plots and treatment types
plots_and_treatments <- unique(all[c("plot", "plot_type")]) %>% arrange(plot)
all_plots_all_years <- all %>% tidyr::expand(nesting(plot, plot_type), year)

# Find and Remove Periods with One Day of Trapping

# summarize trapping
trap_count <- tdat %>%
  group_by(period) %>%
  summarise(count = sum(sampled))
bad_periods <- filter(trap_count, count < 20) # periods that weren't fully trapped
bad_periods <- as.list(bad_periods$period)

# don't use periods with only one day of trapping
all_no_incomplete = all[-which(all$period %in% bad_periods),] 

#################################################################################
### Warning: the code that is commented out below might crash your computer!  ###
#################################################################################
#                                                                               #
#       The `create_trmt_hist` function to create mark_trmt_all (line 237)      #
#       takes a long time to run. If you don't want to run it, you can          #
#       read in the results from the GitHub repo (lines 240-1) and then         #
#       run the RMark code -OR- skip to line 294 for all RMark results          #
#                                                                               #
#################################################################################


sp_trapping_history(all, 'PP')
sp_trapping_history(all, 'DM')
sp_trapping_history(all, 'DO')
sp_trapping_history(all,'DS')
sp_trapping_history(all, 'PB')


PP_only <- filter(all, species == 'PP') |> distinct(id,period, .keep_all = TRUE)
DM_only <- filter(all_clean, species == 'DM')  |> distinct(id,period, .keep_all = TRUE)
DS_only <- filter(all_clean, species == 'DS')  |> distinct(id,period, .keep_all = TRUE)
DO_only <- filter(all_clean, species == 'DO')  |> distinct(id,period, .keep_all = TRUE)
PB_only <- filter(all_clean, species == 'PB') |> distinct(id,period, .keep_all = TRUE)


#-----------------------------------------------------------
# Clean repeat tags in same period
#-----------------------------------------------------------

# Rarely an individual moves plots during a period, resulting in
# a tag repeated for the same period code. This code finds those reepeats
# and selects the first capture

# Create a set of capture histories by treatment and by plot if needed
tags_all = unique(PP_only$id)
periods_all = seq(min(PP_only$period), max(PP_only$period))

#################################################################################
### Warning: the code that is commented out below might crash your computer!  ###
#################################################################################
#                                                                               #
#       The `create_trmt_hist` function to create mark_trmt_all (line 237)      #
#       takes a long time to run. If you don't want to run it, you can          #
#       read in the results from the GitHub repo (lines 240-1) and then         #
#       run the RMark code -OR- skip to line 294 for all RMark results          #
#                                                                               #
#################################################################################


