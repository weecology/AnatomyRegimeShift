library(ggplot2)
library(dplyr)

survival_data = read.csv("./survival/species_survival.csv")

survival = survival_data |> 
  mutate(time = yearmonth(newmoondate), 
         raw_deviation = (estimate - month_mean)/month_sd) |>
  filter(time > yearmonth("1978 Feb") & time < yearmonth("2020 Dec"))

DS_regime = survival |> filter(time > yearmonth("1981 Sep"), time < yearmonth("1985 Oct"))
ggplot(DS_regime, aes(x = time, y = raw_deviation)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = yearmonth("1983 Sep"), color = 'red') +
  geom_vline(xintercept = yearmonth("1983 Oct"), color = 'red') +
  annotate('rect', xmin = yearmonth("1983 Dec"), xmax = yearmonth("1984 Jul"),
           ymin = -4.5, ymax = 4.5, alpha = .2, fill = 'red') +
  facet_wrap(vars(species))

ggplot(survival, aes(x = time, y = raw_deviation)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = yearmonth("1983 Sep"), color = 'red') +
  geom_vline(xintercept = yearmonth("1983 Oct"), color = 'red') +
  annotate('rect', xmin = yearmonth("1983 Dec"), xmax = yearmonth("1984 Jul"),
           ymin = -4.5, ymax = 4.5, alpha = .2, fill = 'red') +
  annotate('rect', xmin = yearmonth("1988 Oct"), xmax = yearmonth("1996 Jan"),
           ymin = -4.5, ymax = 4.5, alpha = .2, fill = 'blue') +
  geom_vline(xintercept = yearmonth("1999 Aug"), color = 'orange') +
  annotate('rect', xmin = yearmonth("1998 Sep"), xmax = yearmonth("1999 Dec"),
           ymin = -4.5, ymax = 4.5, alpha = .2, fill = 'orange') +
  annotate('rect', xmin = yearmonth("2009 Jun"), xmax = yearmonth("2010 Sep"),
           ymin = -4.5, ymax = 4.5, alpha = .2, fill = 'aquamarine') +
  facet_wrap(vars(species))

flood_regime = survival |> filter(time > yearmonth("1996 Sep"), time < yearmonth("2001 Dec"))
ggplot(flood_regime, aes(x = time, y = raw_deviation)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = yearmonth("1999 Aug"), color = 'orange') +
  annotate('rect', xmin = yearmonth("1998 Sep"), xmax = yearmonth("1999 Dec"),
           ymin = -4.5, ymax = 4.5, alpha = .2, fill = 'orange') +
  facet_wrap(vars(species))

pb_regime = survival |> filter(time > yearmonth("2007 Jun"), time < yearmonth("2012 Sep"))
ggplot(pb_regime, aes(x = time, y = raw_deviation)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +
  annotate('rect', xmin = yearmonth("2009 Jun"), xmax = yearmonth("2010 Sep"),
           ymin = -4.5, ymax = 4.5, alpha = .2, fill = 'aquamarine') +
  facet_wrap(vars(species))


library("gratia")
library("mgcv")

model_checking = function(model_output){
  gam.check(model_output)
  acf(resid(model_output))
  pacf(resid(model_output))
}
# defining the seasonal knows used for all analyses

knots = list(c(.5,12.5))

data_species = survival |> filter(species == "PP") |> group_by(month,year) |> 
  summarise(mean=mean(estimate)) |> ungroup()

test = gam(mean ~ s(year, k=40, bs='cr') + s(month, k=12, bs="cc") + ti(year, month, bs=c("cr","cc")), 
           data=data_species, family=gaussian(), method="REML", knots=knots)
summary(test)
appraise(test)
draw(test, residuals=TRUE)
