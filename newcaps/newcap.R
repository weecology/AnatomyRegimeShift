
#----------------------------------------------------------------
#
# Calculate New Individuals
#
#----------------------------------------------------------------

# For all species on controls, calculates total number of
# individual caught each newmoon and the number of new captures
# new captures are determined by finding the min(time) for each
# unique ID and then for each newmoonnumber, counting the newIDs 
# captured in that session.

library(dplyr)
library(tidyr)
library(tsibble)
library(lubridate)

# SET path
path = "./newcaps/"

# SET species
sp_set = c("DS", "DO", "DM", "PB", "PP", "RM", "PF")

# READ data and FORMAT time and replace species = NA w/ NE
# TODO: empty species codes would also be read as NA. Need to fix

data = read.csv("raw_suppformat_rodents.csv")
data = data|> 
  mutate(time = yearmonth(paste(year,month, sep=" ")), 
         species = replace_na(species, "NE"))

# SUBSET to control data and years of interest
controls = data |> filter(plot_type == "Control", year < 2020, species %in% sp_set)

# FIND first capture of each unique tag
first_captures = controls |> 
  group_by(id) |> 
  filter(time == min(time)) |>
  ungroup()

# COUNT number of new captures for each newmoon and species
new_caps = first_captures |> 
  group_by(newmoonnumber, time, species) |> 
  summarise(new_caps=n())

# COUNT number of captures (new and recaps) by species and newmoon 
all_captures = controls |> group_by(newmoonnumber, time, species) |> 
  summarise(all_caps=n())

# MERGE count data and CALCULATE relative number of new captures
capture_counts = all_captures |> full_join(new_caps) |> 
  mutate(new_caps = replace_na(new_caps, 0), 
         relative_new = new_caps/all_caps,
         month = month(time)) 

month_avg = capture_counts |>
  group_by(month, species) |> 
  summarise(month_mean = mean(relative_new), month_std = sd(relative_new))

capture_output = capture_counts |> full_join(month_avg)
  
  
write.csv(capture_output, paste(path,"percent_newcaps.csv", sep=""), 
          row.names=FALSE)
