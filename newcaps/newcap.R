get_newcounts = function(data){
  data = data|> mutate(time = yearmonth(paste(year,month, sep=" ")))
  new_individuals = data |> 
    group_by(species, time) |> 
    filter(note2 == "*") |>  count()
  all_individuals = data |> group_by(species,time) |> count() 
  all_individuals = all_individuals |> 
    full_join(new_individuals, by=join_by(time,species)) |>
    rename(allN = n.x, newN=n.y) |> 
    mutate(newN = replace_na(newN,0), relNew = newN/allN)
  write.csv(all_individuals, "percent_newcaps.csv")
  return(all_individuals)
}
