library(data.table)
library(dplyr)
library(purrr)
library(readr)

# Importo y junto todos los dataset correspondiente al cálculo nivel sistema
Data <- list.files(path = "./Data/System/",
                   pattern = "*.csv", 
                   full.names = T) %>% 
        map_df(~read_csv(.), .id = "Ext_number") 

names(Data) <- c("Ext_number","Country", "Year", "Personalism_index")

# Resumo promediando para cada elección
Personalism <- doBy::summary_by(Data, Personalism_index~ Country +Year, FUN=mean, na.rm=T)

# Vemos el comportamiento por extracción
Base_wide <- as.data.frame(tidyr::spread(Data, key = Ext_number, value = Personalism_index))


# Variable número de elección

Data <- Data %>%  mutate(elect = case_when(Country == "ARGENTINA" & Year == "2007" ~ 1,
                                Country == "ARGENTINA" & Year == "2011" ~ 2,
                                Country == "ARGENTINA" & Year == "2015" ~ 3,
                                Country == "ARGENTINA" & Year == "2019" ~ 4,
                                Country == "BOLIVIA" & Year == "2005" ~ 1,
                                Country == "BOLIVIA" & Year == "2009" ~ 2,
                                Country == "BOLIVIA" & Year == "2014" ~ 3,
                                Country == "BOLIVIA" & Year == "2019" ~ 4,
                                Country == "BOLIVIA" & Year == "2020" ~ 5,
                                Country == "BRASIL" & Year == "2006" ~ 1,
                                Country == "BRASIL" & Year == "2010" ~ 2,
                                Country == "BRASIL" & Year == "2014" ~ 3,
                                Country == "BRASIL" & Year == "2018" ~ 4,
                                Country == "BRASIL" & Year == "2022" ~ 5,
                                Country == "CHILE" & Year == "2005" ~ 1,
                                Country == "CHILE" & Year == "2009" ~ 2,
                                Country == "CHILE" & Year == "2013" ~ 3,
                                Country == "CHILE" & Year == "2017" ~ 4,
                                Country == "CHILE" & Year == "2021" ~ 5,
                                Country == "COLOMBIA" & Year == "2006" ~ 1,
                                Country == "COLOMBIA" & Year == "2010" ~ 2,
                                Country == "COLOMBIA" & Year == "2014" ~ 3,
                                Country == "COLOMBIA" & Year == "2018" ~ 4,
                                Country == "COLOMBIA" & Year == "2022" ~ 5,
                                Country == "COSTA RICA" & Year == "2006" ~ 1,
                                Country == "COSTA RICA" & Year == "2010" ~ 2,
                                Country == "COSTA RICA" & Year == "2014" ~ 3,
                                Country == "COSTA RICA" & Year == "2018" ~ 4,
                                Country == "COSTA RICA" & Year == "2022" ~ 5,
                                Country == "ECUADOR" & Year == "2006" ~ 1,
                                Country == "ECUADOR" & Year == "2009" ~ 2,
                                Country == "ECUADOR" & Year == "2013" ~ 3,
                                Country == "ECUADOR" & Year == "2017" ~ 4,
                                Country == "ECUADOR" & Year == "2021" ~ 5,
                                Country == "EL SALVADOR" & Year == "2004" ~ 1,
                                Country == "EL SALVADOR" & Year == "2009" ~ 2,
                                Country == "EL SALVADOR" & Year == "2014" ~ 3,
                                Country == "EL SALVADOR" & Year == "2019" ~ 4,
                                Country == "GUATEMALA" & Year == "2007" ~ 1,
                                Country == "GUATEMALA" & Year == "2011" ~ 2,
                                Country == "GUATEMALA" & Year == "2015" ~ 3,
                                Country == "GUATEMALA" & Year == "2019" ~ 4,
                                Country == "HONDURAS" & Year == "2005" ~ 1,
                                Country == "HONDURAS" & Year == "2009" ~ 2,
                                Country == "HONDURAS" & Year == "2013" ~ 3,
                                Country == "HONDURAS" & Year == "2017" ~ 4,
                                Country == "HONDURAS" & Year == "2021" ~ 5,
                                Country == "MEXICO" & Year == "2006" ~ 1,
                                Country == "MEXICO" & Year == "2012" ~ 2,
                                Country == "MEXICO" & Year == "2018" ~ 3,
                                Country == "NICARAGUA" & Year == "2006" ~ 1,
                                Country == "NICARAGUA" & Year == "2011" ~ 2,
                                Country == "NICARAGUA" & Year == "2016" ~ 3,
                                Country == "NICARAGUA" & Year == "2021" ~ 4,
                                Country == "PANAMA" & Year == "2004" ~ 1,
                                Country == "PANAMA" & Year == "2009" ~ 2,
                                Country == "PANAMA" & Year == "2014" ~ 3,
                                Country == "PANAMA" & Year == "2019" ~ 4,
                                Country == "PARAGUAY" & Year == "2008" ~ 1,
                                Country == "PARAGUAY" & Year == "2013" ~ 2,
                                Country == "PARAGUAY" & Year == "2018" ~ 3,
                                Country == "PERU" & Year == "2006" ~ 1,
                                Country == "PERU" & Year == "2011" ~ 2,
                                Country == "PERU" & Year == "2016" ~ 3,
                                Country == "PERU" & Year == "2021" ~ 4,
                                Country == "REPÚBLICA DOMINICANA" & Year == "2004" ~ 1,
                                Country == "REPÚBLICA DOMINICANA" & Year == "2008" ~ 2,
                                Country == "REPÚBLICA DOMINICANA" & Year == "2012" ~ 3,
                                Country == "REPÚBLICA DOMINICANA" & Year == "2016" ~ 4,
                                Country == "REPÚBLICA DOMINICANA" & Year == "2020" ~ 5,
                                Country == "URUGUAY" & Year == "2004" ~ 1,
                                Country == "URUGUAY" & Year == "2009" ~ 2,
                                Country == "URUGUAY" & Year == "2014" ~ 3,
                                Country == "URUGUAY" & Year == "2019" ~ 4,
                                Country == "VENEZUELA" & Year == "2006" ~ 1,
                                Country == "VENEZUELA" & Year == "2012" ~ 2,
                                Country == "VENEZUELA" & Year == "2013" ~ 3,
                                Country == "VENEZUELA" & Year == "2018" ~ 4
                                ))


library(ggplot2)
G1 <- ggplot(Data, aes(x=as.numeric(Ext_number), y=Personalism_index))+
        geom_line()+
        facet_grid(Country~elect) +
        labs(title = "") + 
        ylim (0,1) +
        xlab("")+
        ylab("Índice de personalismo sistémico")+
        labs(color = "") + 
        labs(title = "") +
        theme_minimal() +
        theme(plot.margin = margin(10,30,10,10),
              axis.text.x = element_text(angle = -0, vjust = 0, hjust = 0, size = 3),
              axis.text.y = element_text(size = 3),
              legend.position= "bottom",
              text=element_text(size=5, family="Cambria"))

jpeg(filename = here::here("Figures", "evolExt.jpg"), 
     width = 1500, height = 3000, res = 300)
G1
dev.off()

