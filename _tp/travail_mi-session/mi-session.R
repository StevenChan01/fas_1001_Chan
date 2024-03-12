library(quanteda)
library(tidyverse)
library(fs)
library(crayon)
library(ggplot2)
library(readr)


lexicoder <- dictionary(file = "policy_agendas_english.lcd",
                        format = "yoshikoder")


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

# importation de data

X2019_4_1 <- read_csv("2019/4/2019-4-1.csv")
View(X2019_4_1)

X2019_4_2 <- read_csv("2019/4/2019-4-2.csv")

X2019_4_3 <- read_csv("2019/4/2019-4-3.csv")

X2019_4_4 <- read_csv("2019/4/2019-4-5.csv")

X2019_4_5 <- read_csv("2019/4/2019-4-5.csv")

X2019_4_8 <- read_csv("2019/4/2019-4-8.csv")

X2019_4_9 <- read_csv("2019/4/2019-4-9.csv")

data_04_1 <- X2019_4_1 %>% filter (speakerparty == "Conservative")

data_04_02 <- X2019_4_2 %>%  filter (speakerparty == "Conservative")

data_04_03 <-  X2019_4_3 %>%  filter ( speakerparty == "Conservative")

data_04_04 <-  X2019_4_4 %>%  filter(str_detect(speakerparty, "Conservative"))

data_04_05 <-  X2019_4_5 %>%  filter (speakerparty == "Conservative")

data_04_08 <-  X2019_4_8 %>%  filter(speakerparty == "Conservative")

data_04_09 <-  X2019_4_9 %>%  filter(speakerparty == "Conservative")

data <-  bind_rows(data_04_1, data_04_02, data_04_03, data_04_04, data_04_05,
                   data_04_08, data_04_09)


data1 <-  data %>% filter(speakername == "Pierre Poilievre"| 
                              speakername == "Erin O'Toole"| 
                              speakername == "Candice Bergen"|
                              speakername == "Andrew Scheer")

data_clean <- data1%>% select(speakername, speechtext, speakerparty)


# Nombre de parole

nb_parole <- data1%>% count(speakername)

parole_graph <- nb_parole %>% ggplot(aes(x = reorder(
  speakername, - n), y = n)) + geom_bar(stat = "identity") + 
labs(title = "Nombre de parole par politiciens", x = "Politiciens", 
     y = element_blank()) + theme_bw()

 parole_graph  
 
# nettoyage

data_dic <- data1%>% 
  select(speechtext, speakerparty) %>% 
  mutate(speechtext = tolower(speechtext)) %>%
  na.omit()

#dictionnaire

data_object <- run_dictionary(data       = data_dic, 
                              text       = speechtext, 
                              dictionary = lexicoder) |> 
  bind_cols(data_dic) |> select(-c(doc_id,speechtext)) %>% 
  pivot_longer(!speakerparty, names_to = "categorie", values_to="n") |> 
  ungroup() %>%  group_by(speakerparty,categorie) %>% summarise(n=sum(n)) %>%  mutate(
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
  
data_sup <- data_object %>% filter(n > 100)

data_moy <- data_object %>% filter(n < 100, n > 15)

data_inf <- data_object %>% filter(n < 15)

  
data_sup %>% ggplot(aes(x = n, y = categorie))+ geom_bar(
  stat= "identity", position = "dodge", na.rm = TRUE) + 
  labs( title = "Enjeux mentionné par le Parti conservateur",
        x = "Nombre de fois mentionné",
        y = "Enjeux") + 
  theme_bw() + theme(panel.grid.major.x = element_blank(), title = element_text(size = 8),
                     axis.text = element_text(size = 8, color = "black"))


data_moy %>% ggplot(aes(x = n, y = categorie))+ geom_bar(
  stat= "identity", position = "dodge", na.rm = TRUE) + 
  labs( title = "Enjeux mentionné par le Parti conservateur",
        x = "Nombre de fois mentionné",
        y = "Enjeux") + 
  theme_bw() + theme(panel.grid.major.x = element_blank(), title = element_text(size = 8),
                     axis.text = element_text(size = 8, color = "black"))


data_inf %>% ggplot(aes(x = n, y = categorie))+ geom_bar(
  stat= "identity", position = "dodge", na.rm = TRUE) + 
  labs( title = "Enjeux mentionné par le Parti conservateur",
        x = "Nombre de fois mentionné",
        y = "Enjeux") + 
  theme_bw() + theme(panel.grid.major.x = element_blank(), title = element_text(size = 8),
                     axis.text = element_text(size = 8, color = "black"))


