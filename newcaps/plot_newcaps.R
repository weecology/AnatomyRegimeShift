library(ggplot2)
library(dplyr)
library(tsibble)

recaps = read.csv("./newcaps/percent_newcaps.csv")

dominants = c("DM", "DO", "DS", "PP", "PB")
recaps = recaps |> 
  mutate(time = yearmonth(time), 
         rel_deviation = (relative_new - month_relmean)/month_relstd,
         raw_deviation = (new_caps - month_rawmean)/month_rawstd) |>
  filter(time > yearmonth("1978 Feb") & time < yearmonth("2020 Dec"))

species_data = recaps |> filter(species %in% dominants)

DS_regime = species_data |> filter(time > yearmonth("1981 Sep"), time < yearmonth("1985 Oct"))
ggplot(DS_regime, aes(x = time, y = raw_deviation)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = yearmonth("1983 Sep"), color = 'red') +
  geom_vline(xintercept = yearmonth("1983 Oct"), color = 'red') +
  annotate('rect', xmin = yearmonth("1983 Dec"), xmax = yearmonth("1984 Jul"),
           ymin = -4.5, ymax = 4.5, alpha = .2, fill = 'red') +
   facet_wrap(vars(species))

ggplot(species_data, aes(x = time, y = deviation)) +
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

flood_regime = species_data |> filter(time > yearmonth("1996 Sep"), time < yearmonth("2001 Dec"))

ggplot(flood_regime, aes(x = time, y = raw_deviation)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = yearmonth("1999 Aug"), color = 'orange') +
  annotate('rect', xmin = yearmonth("1998 Sep"), xmax = yearmonth("1999 Dec"),
           ymin = -4.5, ymax = 4.5, alpha = .2, fill = 'orange') +
  facet_wrap(vars(species))

pb_regime = species_data |> filter(time > yearmonth("2007 Jun"), time < yearmonth("2012 Sep"))
ggplot(pb_regime, aes(x = time, y = raw_deviation)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +
  annotate('rect', xmin = yearmonth("2009 Jun"), xmax = yearmonth("2010 Sep"),
           ymin = -4.5, ymax = 4.5, alpha = .2, fill = 'aquamarine') +
  facet_wrap(vars(species))
