# Functions for processing individual data for survival analyses
# Based on original code from Sarah SUpp and Ellen K. Bledsoe

### LIBRARIES ### ==============================================================

library(tidyverse)
library(portalr)
library(RCurl)
library(marked)

### DATA FUNCTIONS ### =========================================================

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

sp_trapping_history = function(data, sp){
  
    dat = data |> drop_na(id) |> filter(species == sp) |> distinct(id,period, .keep_all = TRUE) 
    periods_all = seq(min(data$period), max(data$period))
    tags_all = unique(dat$id)
    mark_trmt_all = create_trmt_hist(dat, tags_all, periods_all)
    filename = paste(sp,"_captures_all.csv", sep="")
    write.csv(mark_trmt_all,filename)

  return(mark_trmt_all)
}

  
survival_output = function(species){
  
  capture_data = read_csv(paste(species,"_captures_annual.csv", sep=""), 
                          col_types = cols(ch = col_character()))
  capture_data = capture_data |> filter(tags != 0)
  years = unique(capture_data$year)
  survival_ts = data.frame(year=numeric(),
                           species = character(),
                           survival = numeric(),
                           recap = numeric(),
                           n_id = numeric())
  for (y in 1:length(years)) {
    
    print(paste("PROCESSING...", years[y]))
    capture_history = capture_data |> filter(year == years[y])
    n_id = length(unique(capture_history$tags))
    tryCatch(
      {cjs.m1 <- crm(capture_history)
      Phi = exp(cjs.m1$results$beta$Phi)/(1+exp(cjs.m1$results$beta$Phi)) # real Phi (survival) estimate by hand
      p = exp(cjs.m1$results$beta$p)/(1+exp(cjs.m1$results$beta$p)) # real p (capture probability) estimate by hand
      newrow = list(years[y], species, Phi, p, n_id)},
      error = function(cond) {newrow <<- list(years[y], species, 0, 0, n_id) })
    survival_ts[nrow(survival_ts) + 1,] = newrow
  }
  write.csv(survival_ts, paste(species,"_survival.csv", sep=""))
  return(c(survival_ts,cjs.m1))
}

get_newcounts = function(data){
  new_individuals = controls_all |> 
    group_by(species, year) |> 
    filter(note2 == "*") |>  count()
  all_individuals = controls_all |> group_by(species,year) |> count() 
  all_individuals = all_individuals |> 
    left_join(new_individuals, by=join_by(year,species)) |>
    rename(all = n.x, new = n.y) |>
    mutate(per_new = new/all)
  write.csv(all_individuals, "percent_newcaps.csv")
  return(all_individuals)
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


