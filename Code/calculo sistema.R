########## CÁLCULO DEL ÍNDICE A NIVEL SISTÉMICO ##########

Base <- rio::import(here::here("Data", "Base Candidatos LA.xlsx"))
Extr <- rio::import(here::here("Data", "Extraccion1-feb12.csv"))

Base <- left_join(Base, Extr)

Base$Index_ponderado <- Base$indicador*Base$`Votes%_1st`

Sistema <- doBy::summary_by(Base, Index_ponderado~Country+Year, 
                 FUN = sum, na.rm=T)

G1 <- ggplot(Sistema, aes(x=as.numeric(Year), y=Index_ponderado.sum))+
        geom_col()+
        facet_wrap("Country",  ncol = 3) +
        labs(title = "") +
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

