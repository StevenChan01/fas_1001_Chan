library(tidyverse)
library(rvest)
library(lubridate)

URL <- "https://en.wikipedia.org/wiki/Statistics_of_the_COVID-19_pandemic_in_the_United_States#covid-19-pandemic-in-united-states-by-location"

page <- URL |> 
read_html()

covid <- page |>  html_node("table.wikitable") |> html_table()


# nettoyage de données

covid <- covid |> select("Location[i]","Cases[ii]","Deaths[iii]") |> 
  rename(Etat ="Location[i]",
  Cas = "Cases[ii]",
  Mort = "Deaths[iii]") 


covid <- covid[-c(1,58,59), ]
      

covid_clean <- covid |> 
  mutate(across(c(Cas, Mort), ~ str_replace_all(
    ., "[[:punct:]vi[:punct:]vii[:punct:]viii]", "")),Etat = 
      str_replace_all(Etat, "\\[v[ivx]+\\]", ""))


covid_num <- covid_clean |> mutate(Cas = as.numeric(Cas))

covid_sup <- covid_num |> filter(Cas > 3000000)

covid_inf <- covid_num |> filter(Cas < 900000)




covid_sup |> ggplot(aes(x = Etat, y = Cas)) + geom_bar(stat = "identity") + 
  coord_flip() +
  labs(title = "Les État les plus infectés", 
       x = "États", y= "Le nombre d'infection")


covid_inf |> ggplot(aes(x = Etat, y = Cas)) + geom_bar(stat = "identity") + 
  coord_flip() + labs(title = "Les États les moins infectés", x = "États", 
                      y = "le nombre d'infection")








