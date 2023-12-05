
####
library(ggplot2)
library(tidyr)
library(dplyr)
library(skimr)
####
load("C:/Universidad/ANALEC/Examen/ELSOC_Long_2016_2022_v1.00.RData")
datos <- elsoc_long_2016_2022.2
rm(elsoc_long_2016_2022.2)
datos <- datos %>% 
  mutate_all(~ifelse(. %in% c(-666, -777, -888, -999), NA, .))
####
# interés en la política: c13
# habla de política en casa: c14_01
# se informa por los medios: c14_02
# aborto: c37_02
# restricciones ingreso migrante: c37_05
# capitalización individual: c37_04
# voto boric/kast: c53
# el que pague mas mejor educación : d02_02
## Es justo que las personas de altos ingresos tengan una mejor educación para sus hijos que las personas con ingresos más bajos
####
conc <- datos %>% 
  select(idencuesta, ola,
         c13, c14_01, c14_02, c37_02, c37_04, c37_05, d02_02) %>% 
  mutate(ola_label = as.factor(case_when(
    ola == 1 ~ "2016",
    ola == 2 ~ "2017",
    ola == 3 ~ "2018",
    ola == 4 ~ "2019",
    ola == 5 ~ "2021",
    ola == 6 ~ "2022",
  )))

conc_label <- conc %>% 
  mutate(across(
    c(c13, c14_01, c14_02, c37_02, c37_04, c37_05, d02_02), 
    ~ case_when(
      . == 1 ~ "Totalmente en desacuerdo", 
      . == 2 ~ "En desacuerdo", 
      . == 3 ~ "Ni de acuerdo ni en desacuerdo", 
      . == 4 ~ "De acuerdo", 
      . == 5 ~ "Totalmente de acuerdo", 
      TRUE ~ as.character(.)
    )
  ))

voto <- datos %>% 
  filter(!is.na(c53)) %>% 
  mutate(votó_por = case_when(
    c53 == 1 ~ "Gabriel Boric",
    c53 == 2 ~ "José Antonio Kast"
  ))
####

ggplot(conc, aes(x = ola_label, y = c37_02))

