library(ggplot2)
library(dplyr)

survival = read.csv("./survival/species_survival.csv")

survival = survival |> mutate(month = month(newmoondate), year = year(newmoondate))

ggplot(regime_DS, aes(x=censusdate, y=estimate)) +
  geom_line() + geom_point() +
  geom_vline(xintercept= as.Date("1983-09-27"),color='red') +
  geom_vline(xintercept = as.Date("1983-10-03"), color='red') +
  facet_wrap(vars(species))

regime_flood = survival |>
  filter(newmoondate > as.Date("1995-8-14") & newmoondate < as.Date("2003-08-14"))
ggplot(regime_flood, aes(x=censusdate, y=estimate)) +
  geom_line() + geom_point() +
  geom_vline(xintercept= as.Date("1999-08-14"),color='red') +
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
