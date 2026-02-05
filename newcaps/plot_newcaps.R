library(ggplot2)
library(dplyr)
library(tsibble)

recaps = read.csv("./newcaps/percent_newcaps.csv")

dominants = c("DM", "DO", "DS", "PP", "PB")
recaps = recaps |> 
  mutate(time = yearmonth(time), 
         deviation = (relative_new - month_mean)/month_std) |>
  filter(time > yearmonth("1978 Feb") & time < yearmonth("2020 Dec"))

species_data = recaps |> filter(species %in% dominants)
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

  