########## CÁLCULO DEL ÍNDICE A NIVEL SISTÉMICO ##########

Extr <- rio::import(here::here("Data", "Index_Sistema_2023-02-18.csv"))


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

# Exporto el resultado
rio::export(Sistema, file = here::here("Data", "Ext1-feb12.csv"),format = "csv")


jpeg(filename = here::here("Figures", "g1-feb12.jpg"), 
     width = 1500, height = 1900, res = 300)
G1
dev.off()
