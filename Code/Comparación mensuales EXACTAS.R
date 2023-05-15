### Gráficos con distintos períodos de extracción para términos exactos

library(dplyr)
library(purrr)
library(readr)
library(ggplot2)
source(here::here("Code", "Funciones.R"))
# Importo las bases con las extracciones
DataA <- list.files(path = "./Data/Daily_comparativa_exactos/",
                    pattern = "*.csv", 
                    full.names = T) %>% 
        map_df(~read_csv(.), .id = "Ext_number") 

# Importo la base inicial
Base <- rio::import(here::here("Data", "Base Candidatos LA.xlsx"))

# Conservo las variables de interés
Data <- dplyr::select(DataA, c("date", "keyword", "geo", "Ext_number", "hits"))

# Armo la base resultante con el promedio de hits diarios
Data <- tidyr::pivot_wider(Data, names_from = "Ext_number", values_from = "hits", values_fn = sum)
Data$hits <- rowMeans(Data[, c(4:ncol(Data))], na.rm = TRUE)

# Me quedo con las variables útiles
Data <- dplyr::select(Data, c("date", "keyword", "geo", "hits"))
Data$year <- as.numeric(format(Data$date,'%Y'))
Data <- year_fix(Data)

Data<- Data %>% group_by(geo, year) %>% 
        mutate(E_date = as.Date(max(date, na.rm = T), format="%d/%m/%Y"))

Data$date <- as.Date(Data$date,format="%d/%m/%Y")


# Para un mes
Data1 <- Data
Data1$one <-  as.Date(Data1$E_date,format="%d/%m/%Y")-30
Data1 <- Data1 %>% group_by(geo, year) %>% filter(date >= one & date < E_date)
ONE_index <- P_index_exact(Data1, Base)
names(ONE_index) <- c("Country", "Year", "One")


# Para dos meses
Data2 <- Data
Data2$two <-  as.Date(Data2$E_date,format="%d/%m/%Y")-60
Data2 <- Data2 %>% group_by(geo, year) %>% filter(date >= two & date <E_date)
TWO_index <- P_index_exact(Data2, Base)
names(TWO_index) <- c("Country", "Year", "Two")

# Para tres meses
Data3 <- Data
Data3$three <-  as.Date(Data3$E_date,format="%d/%m/%Y")-90
Data3 <- Data3 %>% group_by(geo, year) %>% filter(date >= three & date <E_date)
THREE_index <- P_index_exact(Data3, Base)
names(THREE_index) <- c("Country", "Year", "Three")

# Para seis meses
Data6 <- Data
Data6$six <-  as.Date(Data6$E_date,format="%d/%m/%Y")-180
Data6 <- Data6 %>% group_by(geo, year) %>% filter(date >= six & date <E_date)
SIX_index <- P_index_exact(Data6, Base)
names(SIX_index) <- c("Country", "Year", "Six")

INDEX <- left_join(ONE_index, TWO_index)
INDEX <- left_join(INDEX, THREE_index)
INDEX <- left_join(INDEX, SIX_index)

INDEX <- tidyr::pivot_longer(INDEX, cols=c("One", "Two", "Three", "Six"))



G1 <- ggplot(INDEX, aes(x=as.numeric(Year), y=value, color=name))+
        geom_line()+
        facet_wrap("Country",  ncol = 3) +
        labs(title = "") + 
        ylim (0,1) +
        xlab("")+
        ylab("Índice de personalismo sistémico")+
        labs(color = "") + 
        labs(title = "") +
        theme_minimal() +
        theme(plot.margin = margin(10,30,10,10),
              axis.text.x = element_text(angle = -0, vjust = 0, hjust = 0, size = 6),
              axis.text.y = element_text(size = 6),
              legend.position= "bottom",
              text=element_text(size=10, family="Cambria"))
G1








