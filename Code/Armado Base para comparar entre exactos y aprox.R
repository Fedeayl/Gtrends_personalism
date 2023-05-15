#### COMPARACIÓN EXTRACCIONES ####
library(data.table)
library(dplyr)
library(purrr)
library(readr)
# El objetivo es comparar las extracciones realizadas con Gtrends utilizando el término exacto
# y aquellas con las aproximaciones del algoritmo de Google

DataE <- list.files(path = "./Data/System_exact/",
                   pattern = "*.csv", 
                   full.names = T) %>% 
        map_df(~read_csv(.), .id = "Ext_number") 

names(DataE) <- c("Ext_number","Country", "Year", "PI_EXACT", "GEO_code")

#### Reparo las entradas con solo GEO_code ####
DataE[79:nrow(DataE),] <- DataE[79:nrow(DataE),] %>%  mutate(Country = case_when(GEO_code == "AR" ~ "ARGENTINA",
                                               GEO_code == "BO" ~ "BOLIVIA",
                                               GEO_code == "BR" ~ "BRASIL",
                                               GEO_code == "CO" ~ "COLOMBIA",
                                               GEO_code == "CL" ~ "CHILE",
                                               GEO_code == "CR" ~ "COSTA RICA",
                                               GEO_code == "EC" ~ "ECUADOR",
                                               GEO_code == "DO" ~ "REPÚBLICA DOMINICANA",
                                               GEO_code == "SV" ~ "EL SALVADOR",
                                               GEO_code == "GT" ~ "GUATEMALA",
                                               GEO_code == "HN" ~ "HONDURAS",
                                               GEO_code == "MX" ~ "MEXICO",
                                               GEO_code == "NI" ~ "NICARAGUA",
                                               GEO_code == "PA" ~ "PANAMA",
                                               GEO_code == "PY" ~ "PARAGUAY",
                                               GEO_code == "PE" ~ "PERU",
                                               GEO_code == "UY" ~ "URUGUAY",
                                               GEO_code == "VE" ~ "VENEZUELA"))
##########################################
                      
                         
DataE <- doBy::summary_by(DataE, PI_EXACT~ Country +Year, FUN=mean, na.rm=T)

names(DataE) <- c("Country", "Year", "PI_Exc")


DataA <- list.files(path = "./Data/System/",
                    pattern = "*.csv", 
                    full.names = T) %>% 
        map_df(~read_csv(.), .id = "Ext_number") 

names(DataA) <- c("Ext_number","Country", "Year", "PI_Alg")

DataA <- doBy::summary_by(DataA, PI_Alg~ Country +Year, FUN=mean, na.rm=T)
names(DataA) <- c("Country", "Year", "PI_Alg")


Data <- dplyr::left_join(DataE, DataA)

rio::export(Data, here::here("Data", "Base_comparacion_exactvsaprox.csv"),format="csv")
# Simple Scatterplot

with(Data, plot(x=PI_Exc, y=PI_Alg, main="Scatterplot", 
     xlab="Exact term ", ylab="Algoritm terms", pch=19, xlim = c(0, 1), ylim=c(0,1)))
abline(a = 0, b=1)

with(Data,cor(PI_Exc, PI_Alg, use = "complete.obs"))


