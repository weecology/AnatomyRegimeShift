library(ggplot2)

survival = read.csv("species_survival.csv")

ggplot(survival, aes(x=year, y=survival)) +
  geom_line(aes(color = species)) +
  geom_vline(xintercept = 1984) +
  geom_vline(xintercept = 1999) +
  geom_vline(xintercept = 2010)
