##### Armado de base final #####

library(data.table)
library(dplyr)
library(purrr)
library(readr)

# Importo las bases 

Data <- list.files(path = "./Data/Party_import_exact/",
pattern = "*.csv", 
full.names = T) %>% 
        map_df(~read_csv(.), .id = "Ext_number") 

Base <- rio::import(here::here("Data", "Base Candidatos LA.xlsx"))

# Quito las comillas producto de la extracción
Data$Candidate <- gsub("'","",Data$Candidate)
Data$Party     <- gsub("'","",Data$Party)

# Promedio el valor de las extracciones
IPartido <- doBy::summary_by(Data, indicador~ GEO_code + Year + Candidate + Party, 
                             FUN = mean, na.rm=T)

names(IPartido) <- c("GEO_code", "Year", "Candidate_exact", "Party_exact", "Indicador")

# Mergeo con la base original
Base <- dplyr::left_join(Base, IPartido)

# Debo asignar a los candidatos independientes el valor de 1
Base$Indicador <- ifelse(Base$Party=="Independiente", 1, Base$Indicador)


Base$Indicador_ponderado <- Base$Indicador * Base$Votes_perc

# Creo la base a nivel sistema
Sistema <- doBy::summary_by(Base, Indicador_ponderado~ Country + Year, 
                            FUN = sum, na.rm=T)


rio::export(Base, here::here("Bases Gtrend", "Base_Partido_exact.csv"), format = "csv")
rio::export(Sistema, here::here("Bases Gtrend", "Base_Sistema_exact.csv"), format = "csv")





## Hago lo mismo para las bases con terminos relacionados

Data <- list.files(path = "./Data/Party_import_related/",
                   pattern = "*.csv", 
                   full.names = T) %>% 
        map_df(~read_csv(.), .id = "Ext_number") 

Base <- rio::import(here::here("Data", "Base Candidatos LA.xlsx"))


# Promedio el valor de las extracciones
IPartido <- doBy::summary_by(Data, indicador~ GEO_code + Year + Candidate + Party, 
                             FUN = mean, na.rm=T)

names(IPartido) <- c("GEO_code", "Year", "Candidate", "Party", "Indicador")

# Mergeo con la base original
Base <- dplyr::left_join(Base, IPartido)

# Debo asignar a los candidatos independientes el valor de 1
Base$Indicador <- ifelse(Base$Party=="Independiente", 1, Base$Indicador)


Base$Indicador_ponderado <- Base$Indicador * Base$Votes_perc

# Creo la base a nivel sistema
Sistema <- doBy::summary_by(Base, Indicador_ponderado~ Country + Year, 
                            FUN = sum, na.rm=T)


rio::export(Base, here::here("Bases Gtrend", "Base_Partido_related.csv"), format = "csv")
rio::export(Sistema, here::here("Bases Gtrend", "Base_Sistema_related.csv"), format = "csv")


