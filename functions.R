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
  
  # I left the code that codes capture history by treatment (A=control, B= krat excl,
  # C=rodent excl even though I filtered down to controls only in case I wanted to add that back in 
  # for some reason)

  MARK_data = data.frame("year" = 1,
                         "ch" = 1,
                         "censored" = 1,
                         "tags" = 1)
  
  outcount = 0
  
  for (t in 1:length(tags)) {
    unique_year = unique(dat$year)
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

 #   tmp2 <- which(dat$id == tags[t])
    censored = 1
    
    outcount = outcount + 1
    MARK_data[outcount, ] <- c(unique_year,capture_history, censored, tags[t])
    
  }
  
  return(MARK_data)
  
}

sp_trapping_history = function(data, sp){
  
# need to change it skips create_trmt_hist when no records in
# timeslice
  
  ########### Testing
  # sp = 'PB'
  # data = controls_all
  # y = 1
  #################
  
  unique_years = unique(data$year)
  years_captures = data.frame(year = integer(),
                              ch = character(),
                              censored = integer(),
                              tags = character())
  for (y in 1:length(unique_years)){
    year_target = unique_years[y]
    time_slice = data |> filter(year == year_target) |> drop_na(id)
    dat = filter(time_slice, species == sp) |> distinct(id,period, .keep_all = TRUE) 
    if (nrow(dat) == 0){
      mark_trmt_all = data.frame("year" = year_target,
                             "ch" = 0,
                             "censored" = 0,
                             "tags" = 0)
      } else {
      periods_all = seq(min(time_slice$period), max(time_slice$period))
      tags_all = unique(dat$id)
      mark_trmt_all = create_trmt_hist(dat, tags_all, periods_all)
      }
    years_captures = rbind(years_captures, mark_trmt_all)
    print(year_target)
    }
  filename = paste(sp,"_captures_annual.csv", sep="")
  write.csv(years_captures,filename)

  return(years_captures)
}
  
survival_output = function(species){
  
  capture_data = read_csv(paste(species,"_captures_annual.csv", sep=""), 
                          col_types = cols(ch = col_character()))
  years = unique(capture_data$year)
  survival_ts = data.frame(year=numeric(),
                           species = character(),
                           survival = numeric(),
                           recap = numeric())
  for (y in 1:length(years)) {
    
    print(paste("PROCESSING...", years[y]))
    capture_history = capture_data |> filter(year == years[y])
    tryCatch(
      {cjs.m1 <- crm(capture_history)
      Phi = exp(cjs.m1$results$beta$Phi)/(1+exp(cjs.m1$results$beta$Phi)) # real Phi (survival) estimate by hand
      p = exp(cjs.m1$results$beta$p)/(1+exp(cjs.m1$results$beta$p)) # real p (capture probability) estimate by hand
      newrow = list(years[y], species, Phi, p)},
      error = function(cond) {newrow <<- list(years[y], species, 0, 0) })
    survival_ts[nrow(survival_ts) + 1,] = newrow
  }
  write.csv(survival_ts, paste(species,"_survival.csv", sep=""))
  return(survival_ts)
}


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


### PLOTTING FUNCTIONS ### =====================================================

plot_PB_timeseries_by_treament <- function(data){
  
  # function for plotting C. baileyi avg. abundance per plot
  # through the time series by plot treatment type
  # Figure S4
  
  y_axis_title <- expression(atop(paste(italic("C. baileyi"), " individuals"),
                                  "(mean per plot)"))
  
  plot <- ggplot(data, aes(x = year, y = avg_ind_per_prd, color = plot_type, group = plot_type)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      geom_errorbar(aes(ymin = ymin, ymax = ymax), width = .5) +
      scale_color_manual(values = cbbPalette) +
      xlab("Year") +
      ylab(y_axis_title) +
      labs(color = "Plot type") +
      theme_classic() +
      theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
            axis.title.x = element_text(size = 12, margin = margin(t = 10)),
            axis.title.y = element_text(size = 12, margin = margin(r = 10)),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            legend.title = element_blank(),
            plot.margin = margin(r = 15, l = 10))
  
  return(plot)
  
}

plot_PP_regression <- function(data, model){
  
  # function for plotting C. penicillatus residual abundances
  # against C. baileyi average abundance per plot
  # Figure 1c and Figure S1c
  
  x_axis_title <- expression(paste(italic("C. baileyi"), " individuals (mean per plot)"))
  y_axis_title <- expression(paste(italic("C. penicillatus"), " residual abundance"))
  
  # plot 1c
  plot <- ggplot(data, aes(x = PB_avg_indiv, y = PP_residuals)) +
    geom_hline(aes(yintercept = 0), color = 'dark gray') +
    geom_smooth(aes(y = fitted(model)),  size = 1, color = "black") +
    geom_point(size = 2) +
    xlab(x_axis_title) +
    ylab(y_axis_title) +
    labs(subtitle = 'c') +
    theme_classic()+
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          plot.subtitle = element_text(size = 10, hjust = -.075),
          axis.line = element_line(size = .25),
          axis.title.x = element_text(size = 9, margin = margin(t = 10)),
          axis.title.y = element_text(size = 9, margin = margin(r = 5)),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          plot.margin = margin(l = 25))
  
  return(plot)
  
}


plot_PB_timeseries <- function(data){
  
  # function for plotting C. baileyi average abundance per plot
  # Figure 1a and Figure S1a
  
  y_axis_title <- expression(atop(paste(italic("C. baileyi"), " individuals"),
                                  "(mean per plot)"))
  
  plot <- ggplot(data, aes(x = year, y = PB_avg_indiv)) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 1995, xmax = 1998,
             ymin = -Inf, ymax = Inf) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 2008, xmax = 2010,
             ymin = -Inf, ymax = Inf) +
    geom_point(size = 2) +
    geom_line() +
    xlab("Year") +
    ylab(y_axis_title) +
    labs(subtitle = expression(paste("a", italic("    C. baileyi")))) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          plot.subtitle = element_text(size = 10, hjust = -.1),
          axis.line = element_line(size = .25),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 9, margin = margin(r = 5)),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          plot.margin = margin(r = 15, l = 5, b = 15))
  
  return(plot)
  
}


plot_PP_residuals_timeseries <- function(data){
  
  # function for plotting C. penicillatus residual
  # abundances through time
  # Figure 1b and Figure S1b
  
  y_axis_title <- expression(atop("Residual abundance"),
                             phantom('W'))
  
  plot <- ggplot(data, aes(x = year, y = PP_residuals)) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 1995, xmax = 1998,
             ymin = -Inf, ymax = Inf) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 2008, xmax = 2010,
             ymin = -Inf, ymax = Inf) +
    geom_hline(aes(yintercept = 0), color = 'black') +
    geom_point(size = 2) +
    geom_line()+
    xlab("Year") +
    ylab(y_axis_title) +
    labs(subtitle = expression(paste("b", italic("    C. penicillatus")))) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          plot.subtitle = element_text(size = 10, hjust = -.1),
          axis.line = element_line(size = .25),
          axis.title.x = element_text(size = 9, margin = margin(t = 10)),
          axis.title.y = element_text(size = 9, margin = margin(r = 5)),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          plot.margin = margin(r = 15, l = 5))
  
  return(plot)
  
}


prep_RMark_data_for_plotting <- function(data){
  
  # prep RMark results for plotting
  data$time = c("Before", "After", "Before", "After", "Before", "After", NA, 
                         "Before", "After", "Before", "After", "Before", "After",
                         "Before", "After", "Before", "After", "Before", "After")
  
  # add descriptive columns
  data$metric = rep("S", nrow(data))
  data$metric[7] = "p"
  data$metric[8:19] = "Psi"
  
  data$stratum = c("A", "A", "B", "B", "C", "C", NA, 
                            "AB", "AB",  "AC", "AC", "BA", "BA",
                            "BC", "BC", "CA", "CA", "CB", "CB")
  data$Treatment = c("Control", "Control", "KR Exclosure", "KR Exclosure", "Removal", "Removal", NA,
                              "Control to KR Exclosure", "Control to KR Exclosure", "Control to Removal", "Control to Removal",
                              "KR Exclosure to Control", "KR Exclosure to Control", "KR Exclosure to Removal", "KR Exclosure to Removal",
                              "Removal to Control", "Removal to Control", "Removal to KR Exclosure", "Removal to KR Exclosure")
  
  data <- data %>% 
    filter(metric != "p", stratum == "A" | stratum == "B" | stratum == "AB" | stratum == "BA")
  data$time <- factor(data$time, levels = c("Before", "After"))
  
  return(data)
  
}


plot_estimated_survival <- function(data){
  
  # plot estimated survival metrics from RMark
  # Figure 2a and Figure S2a
  
  x_axis_title <- expression(paste(italic("C. baileyi"), " establishment"))
  
  plot <- ggplot(data[(data$metric == "S"),], color = Treatment) +
    geom_pointrange(aes(x = time, y = estimate, 
                        ymin = (estimate - se), ymax = (estimate + se), 
                        color = Treatment), 
                    position = position_dodge(.1), size = .75) +
    scale_colour_manual(values = cbbPalette) + 
    xlab(x_axis_title) +
    ylab("Residency") + 
    labs(subtitle = 'a') +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          plot.subtitle = element_text(size = 14, hjust = -.35, vjust = -.5),
          axis.line = element_line(size = .25),
          axis.title.x = element_text(size = 12, margin = margin(t = 10)),
          axis.title.y = element_text(size = 12, margin = margin(r = 10)),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.position = "top",
          legend.title = element_blank(),
          plot.margin = margin(l = 5, t = 20))
  
  return(plot)
  
}


plot_transition_probability <- function(data){
  
  # plot transition probability  metrics from RMark
  # Figure 2b and Figure S2b
  
  x_axis_title <- expression(paste(italic("C. baileyi"), " establishment"))
  
  plot <- ggplot(data[(data$metric == "Psi"),]) +
    geom_pointrange(aes(x = time, y = estimate,
                        ymin = (estimate - se), ymax = (estimate + se), 
                        color = Treatment), 
                    position = position_dodge(.1), size = .75) +
    scale_colour_manual(values = cbbPalette) + 
    xlab(x_axis_title) +
    ylab("Transition probability") +
    guides(color = guide_legend(nrow = 2)) +
    labs(subtitle = 'b') +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          plot.subtitle = element_text(size = 14, hjust = -.35, vjust = -.5),
          axis.line = element_line(size = .25),
          axis.title.x = element_text(size = 12, margin = margin(t = 10)),
          axis.title.y = element_text(size = 12, margin = margin(r = 10)),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.position = "top", 
          legend.title = element_blank(),
          plot.margin = margin(l = 20))
  
  return(plot)
  
}


plot_new_PP_individuals <- function(data){
  
  # plot number of new (i.e., untagged) PP individuals per year by treatment
  # Figure 2c and Figure S2c
  
  # rename plot_treatments for plotting
  data$plot_type <- plyr::revalue(data$plot_type, c("Krat_Exclosure" = "KR Exclosure"))
  
  y_axis_title <- expression(atop(paste(italic("C. penicillatus"), " individuals")))
  
  plot <- ggplot(data, aes(x = year,
                           y = avg_plot_sum_by_year,
                           color = plot_type,
                           group = plot_type)) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 1995, xmax = 1998,
             ymin = -Inf, ymax = Inf) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 2008, xmax = 2010,
             ymin = -Inf, ymax = Inf) +
    scale_color_manual(values = cbbPalette, name = "Plot Type") +
    geom_point(size = 2.5) +
    geom_line() +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = .5) +
    ylab(y_axis_title) +
    xlab("Year") +
    labs(subtitle = 'c') +
    #guides(color = guide_legend(override.aes = list(size = 3))) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          plot.subtitle = element_text(size = 14, hjust = -.14, vjust = -.5), 
          axis.title.x = element_text(size = 12, margin = margin(t = 10)),
          axis.title.y = element_text(size = 12, margin = margin(r = -10)),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.position = "top", 
          legend.title = element_blank(),
          plot.margin = margin(r = 10, t = 15))
  
  return(plot)
  
}


plot_energy_ratio <- function(data){
  
  # function for plotting KR exclosure:control energy ratio
  # Figure 3 & Figure S3
  
  y_axis_title <- expression(atop("Energy ratio",
                                  "(kangaroo rat exclosure:control)"))
  
  plot <- ggplot(data, aes(year, EX_to_CO_ratio, group = 1)) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 1995, xmax = 1998,
             ymin = -Inf, ymax = Inf) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 2008, xmax = 2010, 
             ymin = -Inf, ymax = Inf) +
    geom_point(size = 2) +
    geom_line() +
    ylab(y_axis_title) +
    xlab("Year") + 
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          axis.title.x = element_text(size = 10, margin = margin(t = 10)),
          axis.title.y = element_text(size = 10, margin = margin(r = 10)),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          plot.margin = margin(10, 15, 10, 10))
  
  return(plot)
  
}


plot_avg_competitors <- function(data){
  
  # function for plotting avg of total competitors per treatment
  # Figure S5
  
  # rename plot_treatments for plotting
  data$plot_type <- plyr::revalue(data$plot_type, 
                                  c("Krat_Exclosure" = "KR Exclosure"))
  
  y_axis_title <- expression(atop("Total competitors",
                             "(mean per plot)"))
  
  plot <- ggplot(data, aes(x = year,
                           y = avg_plot_sum_by_year,
                           color = plot_type,
                           group = plot_type)) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 1995, xmax = 1998,
             ymin = -Inf, ymax = Inf) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 2008, xmax = 2010,
             ymin = -Inf, ymax = Inf) +
    scale_color_manual(values = cbbPalette, name = "Plot Type") +
    geom_point(size = 2.5) +
    geom_line() +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = .5) +
    ylab(y_axis_title) +
    xlab("Year") +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          plot.subtitle = element_text(size = 14, hjust = -.14, vjust = -.5), 
          axis.title.x = element_text(size = 12, margin = margin(t = 10)),
          axis.title.y = element_text(size = 12, margin = margin(r = 5)),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.position = "top", 
          legend.title = element_blank(),
          plot.margin = margin(r = 10, l = 5))
  
  return(plot)
  
}
