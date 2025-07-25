library(ggplot2)

survival = read_csv("DMperiod_survival.csv")

ggplot(survival, aes(x=censusdate, y=estimate)) +
  geom_line() + geom_point() +
  geom_vline(xintercept = as.Date("1984-10-01"), color='red') +
  geom_vline(xintercept = as.Date("1999-08-01"), color='red')

new_caps = read.csv("percent_newcaps.csv")

ggplot(new_caps, aes(x=year, y=per_new)) +
  geom_line(aes(color = species)) + geom_point() +
  geom_vline(xintercept = 1984) +
  geom_vline(xintercept = 1999) +
  geom_vline(xintercept = 2010) +
  facet_grid(rows=vars(species))

