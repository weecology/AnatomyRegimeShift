# Making Data for Analyses
# Based on original code from Sarah Supp and Ellen K. Bledsoe

### LIBRARIES ### ==============================================================
library(portalr)
library(RCurl)
source("functions.R")



##################################################################
# DATA PREP
##########################################################

#---------------------------------------------------------
# Load Data
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

newmoon = getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/moon_dates.csv")
mdat = read.csv(text = newmoon, header = TRUE, stringsAsFactors = FALSE)

#------------------------------------------------------------------------
# Clean data
#-----------------------------------------------------------------------

# add newmoonnumber to rodent table
merged = rdat |> inner_join(mdat)
# make it match Sarah Supp's data structure to use her code
all <- repo_data_to_Supp_data(merged, sdat) %>% 
  filter(year <= 2015 & period > 6)
write.csv(all, "raw_suppformat_rodents.csv") # so you don't have to keep running this bit


#----------------------------------------------------------------
#
# Make Capture History time slices
#
#----------------------------------------------------------------
all = read.csv("raw_suppformat_rodents.csv")
controls_all = all |> filter(plot_type == "Control")

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


PP_records = all |> filter(year != 1979)
PP_data = sp_trapping_history(PP_records, 'PP')
DM_data = sp_trapping_history(all, 'DM')
DO_data = sp_trapping_history(all, 'DO')

DS_records = all |> filter(year < 1995)
DS_data = sp_trapping_history(DS_records,'DS')

PB_records = all |> filter(year > 1994)
PB_data = sp_trapping_history(PB_records, 'PB')


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

# don't use periods with only one day of trapping for non-survival analyses
all_no_incomplete = all[-which(all$period %in% bad_periods),] 

