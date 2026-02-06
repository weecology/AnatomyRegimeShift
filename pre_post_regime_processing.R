library(ggplot2)
library(dplyr)
library(tidyr)
library(tsibble)
library(lubridate)


plot_format = function(plot_data, regime_data){
  min_y = min(plot_data$standardized_caps)
  max_y = max(plot_data$standardized_caps)
  min_x = regime$transition_start
  max_x = regime$transition_end
  title = unique(regime_data$regime)
   ggplot(plot_data, aes(x = time, y = standardized_caps)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +
  annotate('rect',
    xmin = min_x,
    xmax = max_x,
    ymin = min_y,
    ymax = max_y,
    alpha = .6,
    fill = 'red') +
  ggtitle(title) +
  facet_wrap(vars(species))
}
data = read.csv("./newcaps/percent_newcaps.csv")


regimes = read.csv("regime_dates.csv") #dates from Christensen

dominant_sp = c("DM", "DS", "DO", "PP", "PB")

data = data |> mutate(time = yearmonth(time)) |> 
  filter(species %in% dominant_sp) |>
  select(-c(relative_new,month)) |> 
  complete(nesting(newmoonnumber, time), species, fill=list(all_caps=0, new_caps=0)) |>
  mutate(month = month(time))

regimes = regimes |> mutate(transition_start = yearmonth(transition_start),
                            transition_end = yearmonth(transition_end),
                            start = yearmonth(start))
regimes_data = data.frame()
for (regime_name in regimes$regime){
  regime = regimes |> filter(regime == regime_name)
  pre_regime_data = data |> 
    filter(time >= (regime$transition_start - 24) , time < regime$transition_start)
  after_pre_data = data |> 
    filter(time >= (regime$transition_start) , time <= (regime$transition_end + 24))
  month_means = data |> 
    filter(time >= (regime$start) , time < regime$transition_start) |>
    group_by(species, month) |> 
    summarise(month_mean = mean(new_caps),
              month_sd = sd(new_caps))
  month_means = month_means |> mutate(month_sd = ifelse(month_sd == 0, 1, month_sd)) 
  
  pre_regime_data = pre_regime_data |> left_join(month_means)
  after_pre_data = after_pre_data |> left_join(month_means)
  
  regime_data = rbind(pre_regime_data, after_pre_data) 
  regime_data = regime_data |> mutate(standardized_caps = (new_caps - month_mean)/ month_sd,
                                      regime = regime_name)
  regimes_data = rbind(regimes_data, regime_data)
  
}

write.csv(regimes_data, "regime_standardized_newcaps.csv")

regime_data = read.csv("regime_standardized_newcaps.csv")
regime_data = regime_data |> mutate(time = yearmonth(time))
regimes = read.csv("regime_dates.csv") #dates from Christensen
regimes = regimes |> mutate(transition_start = yearmonth(transition_start),
                            transition_end = yearmonth(transition_end),
                            start = yearmonth(start))

  plot_data = regime_data |> filter(regime == "DS_regime")
  regime = regimes |> filter(regime == "DS_regime")
 plot_format(plot_data, regime)

 
 plot_data = regime_data |> filter(regime == "flood_regime")
 regime = regimes |> filter(regime == "flood_regime")
 plot_format(plot_data, regime)
 
 plot_data = regime_data |> filter(regime == "PB_regime")
 regime = regimes |> filter(regime == "PB_regime")
 plot_format(plot_data, regime)
