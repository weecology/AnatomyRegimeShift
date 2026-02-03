# Making Data for Analyses
# Based on original code from Sarah Supp and Ellen K. Bledsoe

# NOTE: For each month, uses all available control plots, short and long-term

### LIBRARIES ### ==============================================================
library(dplyr)
library(portalr)

##################################################################
# GET DATA 
# Run if first time making data. Makes "raw_suppformat_rodents.csv".
# If file already exists, can skip this step and run data processing 
# files for specific data types:
#   survival/get_survivaldata.R
#   newcap/get_newcaps.R
##########################################################

get_rawdata = function(){
  # can't currently get universal tags from portalr, so this function downloads
  # data and supporting tables, adds newmooncodes to the rodent data, subsets to desired
  # treatments and time period (max year, min period) and converts
  # data format to work with Sarah Supp's individual capture history processing code.
  # It outputs a .csv file so you don't have to keep running this function which is slow
  
  # rodent file from repo
  rodents <- getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent.csv")
  rdat <- read.csv(text = rodents, header = TRUE, na.strings = c(""), stringsAsFactors = FALSE)
  
  # species file
  species <- getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent_species.csv")
  sdat <- read.csv(text = species, header = TRUE, na.strings = c(""), stringsAsFactors = FALSE)
  
  # trapping file from repo
  trapping <- getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent_trapping.csv")
  tdat <- read.csv(text = trapping, header = TRUE, stringsAsFactors = FALSE)
  
  # newmoon files for converting census numbers to newmoons for time series
  newmoon = getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/moon_dates.csv")
  mdat = read.csv(text = newmoon, header = TRUE, stringsAsFactors = FALSE)
  
  # add newmoonnumber to rodent table
  merged = rdat |> inner_join(mdat)
  
  # make it match Sarah Supp's data structure to use her code
  all <- repo_data_to_Supp_data(merged, sdat)
  
  write.csv(all, paste("raw_suppformat_rodents.csv", sep="")) # so you don't have to keep running this bit
  write.csv(mdat, "newmooncodes.csv")
}

repo_data_to_Supp_data <- function(data, species_data){
  
  # function to convert rodent data downloaded from the PortalData repo
  # to match up with the format of Sarah Supp's data to use her functions
  # EKB
  
  # get only target species
  target <- species_data$speciescode[species_data$censustarget == 1]
  
  data <- data %>% 
    filter(period > 0, #remove negative periods and periods after plot switch
           plot > 0, species %in% target) # remove non-target animals
  
  ## make dataframe look like Sarah's raw data
  
  # new columns
  data$Treatment_Number = NA
  data$east = NA
  data$north = NA
  data$plot_type = NA
  
  # remove unneeded columns
  data <- select(data, -recordID, -day, -note1, -age, -testes, -lactation, -hfl,
                 -(prevrt:note4))
  
  # reorganize columns
  data <- data[, c("year", "month", "newmoonnumber", "period", "Treatment_Number", 
                   "plot", "stake", "east", "north", "species", "sex", 
                   "reprod", "vagina", "nipples", "pregnant", "wgt",
                   "tag", "note2", "ltag", "note3", "note5", "id", "plot_type")]
  
  
  # add a plot_type column for easier plotting down the road
  for (i in 1:length(data$period)){
    if (data$newmoonnumber[i] < 468){
      if (data$plot[i] %in% c(1, 2, 4, 8, 9, 11, 12, 14, 17, 22)){
        data$Treatment_Number[i] = 1
        data$plot_type[i] = 'Control'
      } else if (data$plot[i] %in% c(3, 6, 13, 15, 18, 19, 20, 21 )){
        data$Treatment_Number[i] = 2
        data$plot_type[i] = 'Krat_Exclosure'
      } else {
        data$Treatment_Number[i] = 3 # Plots 5, 7, 10, 16, 23, 24
        data$plot_type[i] = 'Removal'
      } 
    } else {
      if (data$plot[i] %in% c(4,5,6,7,11,13,14,17,18,24)){
        data$Treatment_Number[i] = 1
        data$plot_type[i] = 'Control'
      } else if (data$plot[i] %in% c(2,3,8,15,19,20,21,22)){
        data$Treatment_Number[i] = 2
        data$plot_type[i] = 'Krat_Exclosure'
      } else {
        data$Treatment_Number[i] = 3
        data$plot_type[i] = 'Removal'
      }
    }
  }
  return(data)
} 


get_rawdata()