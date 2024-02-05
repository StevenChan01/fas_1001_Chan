library(readr)
library(tidyverse)
library(ggplot2)
election_Canada<- read_csv("_tp/_tp2/electoral_study_Canada.csv")
View(election_Canada)



niveau <- election_Canada %>% select(
  ces04_MBS_DH,ces08_MBS_DH,MBS11_D2h) %>% 
  rename(
    niv_conf_04 = ces04_MBS_DH, 
    niv_conf_08 = ces08_MBS_DH, 
    niv_conf_11 = MBS11_D2h)%>% mutate("2004" = mean(niv_conf_04,na.rm = TRUE),
          "2008" = mean(niv_conf_08, na.rm =TRUE),
          "2011" = mean(niv_conf_11, na.rm = TRUE))%>% 
  summarise(across(starts_with("20"), ~ first(.))) %>% 
  pivot_longer(cols = c("2004", "2008", "2011"), 
               names_to = "années", values_to = "moyenne")

view(niveau)


                               


elec_2019 <- read_csv("_tp/_tp2/data/elec_2019.csv")
View(elec_2019)

niveau_2019 <- elec_2019 %>% select(pes19_conf_inst1_1) %>% 
  rename(niv_conf_19 = pes19_conf_inst1_1) %>% mutate(moy19 = mean(niv_conf_19, na.rm=TRUE))%>% summarise(
      "2019"= first(moy19)) %>% pivot_longer(
        cols= "2019", names_to = "années", values_to = "moyenne")


niveau_2019

data_clean <- rbind(niveau, niveau_2019)

ggplot(data_clean,aes(x = années,y = moyenne)) +
  geom_point()+geom_smooth() + labs(x= "Années d'élection canadiennes",
                    y = "moyenne du niveau de confiance",
       title = "Niveau de confiance des Canadiens envers le fédéral") + 
  theme_minimal()




