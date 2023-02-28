########## CÁLCULO DEL ÍNDICE A NIVEL SISTÉMICO ##########
library(ggplot2)
Extr <- rio::import(here::here("Data", "System_2023-02-18.csv"))


G1 <- ggplot(Extr, aes(x=as.numeric(Year), y=Index_ponderado.sum))+
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


jpeg(filename = here::here("Figures", "g1-feb18.jpg"), 
     width = 1500, height = 1900, res = 300)
G1
dev.off()
