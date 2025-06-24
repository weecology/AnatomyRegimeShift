library(ggplot2)

survival = read.csv("species_survival.csv")

ggplot(survival, aes(x=year, y=survival)) +
  geom_line(aes(color = species)) + geom_point() +
  geom_vline(xintercept = 1984) +
  geom_vline(xintercept = 1999) +
  geom_vline(xintercept = 2010) +
  facet_grid(rows=vars(species))

new_caps = read.csv("percent_newcaps.csv")

ggplot(new_caps, aes(x=year, y=per_new)) +
  geom_line(aes(color = species)) + geom_point() +
  geom_vline(xintercept = 1984) +
  geom_vline(xintercept = 1999) +
  geom_vline(xintercept = 2010) +
  facet_grid(rows=vars(species))

