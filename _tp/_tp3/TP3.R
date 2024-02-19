library(quanteda)
library(tidyverse)
library(fs)
library(crayon)
library(ggplot2)



#Dictionnaire anglais

lexicoder <- dictionary(file = "policy_agendas_english.lcd",
                        format = "yoshikoder")

# fonction

run_dictionary <- function(data, text, dictionary) {
  tictoc::tic()
  if ( is.data.frame(data) != "TRUE") {
    stop(crayon::yellow('the argument "data" needs to be a dataframe'))
  }
  data <- data %>% dplyr::mutate(text = {{text}})
  if ( is.character(data$text) != "TRUE") {
    stop(crayon::yellow('The variable "text" needs to be a character vector'))
  }
  corpus <- quanteda::tokens(data$text)
  if ( quanteda::is.dictionary(dictionary) != "TRUE") {
    stop(crayon::yellow('Your "dictionary" needs to be in a dictionary format\n For more information:" https://quanteda.io/reference/dictionary.html'))
  }
  dfm    <- quanteda::dfm(quanteda::tokens_lookup(corpus, dictionary, nested_scope = "dictionary"))
  message(crayon::green("100% expressions/words found"))
  dataFinal   <- quanteda::convert(dfm, to = "data.frame")
  tictoc::toc()
  return(dataFinal)
}

#importation des data de  28 mai 2019
library(readr)
data_parl_1 <- read_csv("2019-5-28.csv")
View(data_parl)

data_28 <- data_parl_1 %>% filter(speakerparty == "Conservative")


#importation des data de 27 mai
library(readr)
data_27 <- read_csv("2019-5-27.csv")
View(data_27)

data_27 <- data_27 %>% filter(speakerparty == "Conservative")

#importation des data de 29 mai

data_parl_29 <- read_csv("2019-5-29.csv")
View(data_parl_29)

data_29 <- data_parl_29 %>% filter(speakerparty == "Conservative")

# fusion 

data_conser <- bind_rows(data_27,data_28, data_29)

# nettoyage de données

data_clean <- data_conser %>% 
  select(speakerparty,speechtext) %>% 
  mutate(speechtext = tolower(speechtext)) %>%
           na.omit()
speech <- data_clean %>% select(speechtext)




data_object <- run_dictionary(data       = data_clean, 
               text       = speechtext, 
               dictionary = lexicoder) |> 
  bind_cols(data_clean) |> select(-c(doc_id,speechtext)) %>% 
  pivot_longer(!speakerparty, names_to = "categorie", values_to="n") |> 
  ungroup() %>%  group_by(speakerparty,categorie)%>% 
  filter(n>5) %>% summarise(n=sum(n)) %>%  mutate(
    categorie = case_when(categorie == "macroeconomics" ~ "Économie",
   categorie == "crime" ~ "Crime",
   categorie == "healthcare" ~ "Assurance maladie",
   categorie == "transportation" ~ "Transportation",
   categorie == "social_welfare" ~ "Sécurité sociale",
   categorie == "religion" ~ "Religion",
   categorie == "land-water-management" ~ "Contrôle des terres et eaux",
   categorie == "labour" ~ "Emplois",
   categorie == "foreign_trade" ~ "Exportation",
   categorie == "environment" ~ "Environement",
   categorie == "education" ~ "Éducation",
   categorie == "civil_rights" ~ "Droit civil",
    T ~ as.character(categorie)))%>%  na.omit() 



data_object %>% ggplot(aes(x = n, y = categorie))+ geom_bar(
  stat= "identity", position = "dodge", na.rm = TRUE) + scale_x_continuous(
    breaks = seq (0,140, by = 20)) + 
  labs( title = "Enjeux mentionné par le Parti conservateur du 27 au 29 mai",
    x = "Nombre de fois mentionné",
       y = "Enjeux") + 
  theme_bw() + theme(panel.grid.major.x = element_blank())
                   


+ theme(title = element_text(size = 20),
                       legend.text = element_text(size = 20),
                       axis.text = element_text(size = 20, color = "black"))







