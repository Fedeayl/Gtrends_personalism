#### CALCULOS CON BASE DE DATO IMPORTADA
source(here::here("Code", "Funciones.R"))

Base <- rio::import(here::here("Data", "Base Candidatos LA.xlsx"))
Daily <- rio::import(here::here("Data","Daily", "Daily_2023-03-15.csv"))


P_index <- function(Extraction, Data){
        
        require(dplyr)
        require(doBy)
        
        x <- Extraction
        y <- Data
        
        # Resumo por año y sumo hits
        Sum <- doBy::summary_by(x, hits~keyword+geo+year, FUN=sum, na.rm=T)
        
        # Voy a Data, para agregar las nuevas columnas con la extracción resumida
        y <- left_join(y, Sum, by=c("Candidate"="keyword", "Year"="year", "GEO_code"="geo"))
        y <- y %>% rename (hits.candidate = hits.sum)
        
        y <- left_join(y, Sum, by=c("Party" = "keyword", "Year"="year", "GEO_code"="geo"))
        y <- y %>% rename (hits.party = hits.sum)
        
        # Y calcvulo el indicador
        y$indicador <-  y$hits.candidate / (y$hits.candidate + y$hits.party)
        
        return(y)
}


# Nivel partido
Index <- P_index(Extraction=Daily, Data=Base)


# Nivel sistema
Index_sistema <- left_join(Base, Index)

Index_sistema$Index_ponderado <- Index_sistema$indicador*Index_sistema$Votes_perc

Index_sistema <- doBy::summary_by(Index_sistema, Index_ponderado~Country+Year, 
                              FUN = sum, na.rm=T)

rio::export(Index_sistema,file=here::here("Data","System", "System_2023-03-15.csv"), format = "csv")

