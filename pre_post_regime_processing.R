library(ggplot2)
library(dplyr)
library(tidyr)
library(tsibble)
library(lubridate)


plot_format = function(plot_data, regime_data){
  min_y = min(plot_data$standardized_value)
  max_y = max(plot_data$standardized_value)
  min_x = regime$transition_start
  max_x = regime$transition_end
  title = unique(regime_data$regime)
  ggplot(plot_data, aes(x = time, y = standardized_value)) +
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



make_regime_standardized_data = function(data, regimes_dates, variable){
regimes_data = data.frame()
unique_regimes = unique(regimes_dates$regime)
variable = as.name(variable)
for (regime_name in unique_regimes){
  regime = regimes_dates |> filter(regime == regime_name)
  pre_regime_data = data |> 
    filter(time >= (regime$transition_start - 24) , time < regime$transition_start)
  after_pre_data = data |> 
    filter(time >= (regime$transition_start) , time <= (regime$transition_end + 24))
  month_means = data |> 
    filter(time >= (regime$start) , time < regime$transition_start) |>
    group_by(species, month) |> 
    summarise(month_mean = mean({{variable}}),
              month_sd = sd({{variable}}))
  month_means = month_means |> mutate(month_sd = ifelse(month_sd == 0, 1, month_sd)) 
  
  pre_regime_data = pre_regime_data |> left_join(month_means)
  after_pre_data = after_pre_data |> left_join(month_means)
  
  regime_data = rbind(pre_regime_data, after_pre_data) 
  regime_data = regime_data |> mutate(standardized_value = ({{variable}} - month_mean)/ month_sd,
                                      regime = regime_name)
  regimes_data = rbind(regimes_data, regime_data)
  
}
return(regimes_data)
}

regimes = read.csv("regime_dates.csv") #dates from Christensen
regimes = regimes |> mutate(transition_start = yearmonth(transition_start),
                            transition_end = yearmonth(transition_end),
                            start = yearmonth(start))

dominant_sp = c("DM", "DS", "DO", "PP", "PB")

cap_data = read.csv("./newcaps/newcap_data.csv")
cap_data = cap_data |> mutate(time = yearmonth(time)) 
newcap_standard = make_regime_standardized_data(cap_data,regimes,"new_caps")
write.csv(newcap_standard, "regime_standardized_newcaps.csv")

survival_data = read.csv("./survival/regime_survival.csv")
survival_data = survival_data |> mutate(time = yearmonth(time)) 
survival_standard = make_regime_standardized_data(data,regimes, "estimate")
write.csv(survival_standard, "regime_standardized_survival.csv")


#regime_data = read.csv("regime_standardized_newcaps.csv")
regime_data = read.csv("regime_standardized_survival.csv")
regime_data = regime_data |> filter(species != "DO")
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
